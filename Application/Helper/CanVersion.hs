module Application.Helper.CanVersion where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Exts
import GHC.Records
import GHC.Generics
import Data.Maybe
import Generated.Types
import Application.Script.Prelude
import IHP.Log as Log
import IHP.Log.Types
import Data.Aeson.Types
import Data.ByteString.Lazy.Internal
import Database.PostgreSQL.Simple as PG (Only,Query)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Data.Text.Encoding ( encodeUtf8 )
import Data.Text.Read as T (decimal)
-- import Application.Helper.VersionTree
import Application.Helper.WorkflowProgress
import Text.Printf (printf)

import IHP.Pagination.Types as PT

today :: IO Day -- :: (year,month,day)
today = getCurrentTime >>= return . utctDay

-- e denotes a temporal component and s its history of change
class (Show e, KnownSymbol (GetTableName e), e ~ GetModelByTableName (GetTableName e), PrimaryKey (GetTableName e) ~ Integer, Record e,
    CanCreate e, CanUpdate e, Fetchable (QueryBuilder (GetTableName e))  e, FromRow e,
    HasField "id" e (Id e), Show (PrimaryKey (GetTableName e)), HasField "refHistory" e (Id History),SetField "refHistory" e (Id History),HasTxnLog e,
    Show s, KnownSymbol (GetTableName s), s ~ GetModelByTableName (GetTableName s), PrimaryKey (GetTableName s) ~ Integer, Record s,
    CanCreate s, CanUpdate s, Fetchable (QueryBuilder (GetTableName s))  s, FromRow s,
    HasField "id" s (Id s), Show (PrimaryKey (GetTableName s)), HasField "refEntity" s (Id e),SetField "refEntity" s (Id e),
    HasField "refValidfromversion" s (Id Version), SetField "refValidfromversion" s (Id Version),
    HasField "refValidthruversion" s (Maybe(Id Version)), SetField "refValidthruversion" s (Maybe (Id Version)),
    HasField "content" s Text, SetField "content" s Text, HasTxnLog s) => CanVersion e s 
    where

    getKey :: s -> Integer
    default getKey :: s -> Integer
    getKey m = case decimal $ recordToInputValue m of
                                    Left _ -> -1
                                    Right ( i , _) -> i
    getAccessor :: (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s)))
    getWorkFlowState :: WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s))
    getWorkFlowState wfp = getAccessor wfp
    setWorkFlowState :: WorkflowProgress -> Maybe (StateKeys (Id e)(Id s)) -> WorkflowProgress
    updateWfpV :: (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s))) -> WorkflowProgress -> UUID -> Id e -> Maybe Integer -> Maybe (Id s) -> Value
    updateWfpV accessor wfp hId eId vIdMB sIdMB = fromJust $ decode $ encode $ setWorkFlowState wfp (Just ((fromJust $ accessor wfp) { history= Just hId, entity = Just eId, version= vIdMB, state= sIdMB } ))
    initialWfpV:: (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s))) -> UUID -> Value
    initialWfpV accessor h = fromJust $ decode $ encode $ setWorkFlowState workflowProgressDefault (Just ((fromJust $ accessor workflowProgressDefault) { history= Just h} ))
    getStatehistoryIdMB :: (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s))) -> WorkflowProgress -> Maybe UUID
    getStatehistoryIdMB accessor wfp = history =<< accessor wfp
    getStateVersionIdMB :: (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s))) ->WorkflowProgress -> Maybe Integer
    getStateVersionIdMB accessor wfp = version =<< accessor wfp
    getEntityIdMB :: (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s))) ->WorkflowProgress -> Maybe (Id e)
    getEntityIdMB  accessor wfp = entity =<< accessor wfp
    getStateIdMB :: (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s))) ->WorkflowProgress -> Maybe (Id s)
    getStateIdMB  accessor wfp = state =<< accessor wfp

    commitState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context ) => (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s))) -> Workflow -> IO (Either Text Text)
    commitState accessor workflow = do
        let workflowId = get #id workflow
        Log.info $ "ToCOmmitWF wf=" ++ show workflowId
        let wfpMB = getWfp workflow
        Log.info ("ende"::String)
        putStrLn "Ende commit"
        case wfpMB of
            Just wfp -> do
                case getStatehistoryIdMB accessor wfp of
                    Just h -> case getStateVersionIdMB accessor wfp of
                        Just v -> case getEntityIdMB accessor wfp of
                            Just e -> case getStateIdMB accessor wfp of
                                Just s -> withTransaction do 
                                    Log.info $ "committing h:" ++ show h
                                    hUnlocked :: History <- fetch (Id h)
                                    hUnlocked |> set #refOwnedByWorkflow Nothing |> updateRecord
                                    Log.info $ "Unlocked h:" ++ show h
                                    Log.info $ "version=" ++ show v
                                    Log.info $ "entity=" ++ show e
                                    newVersion :: Version <- fetch (Id v)
                                    newVersion |>set #committed True |> updateRecord
                                    Log.info $ "commit version v: " ++ show v
                                    w <- workflow |> set #workflowStatus "committed" |> updateRecord
                                    Log.info $ "commit workflow w: " ++ show w
                                    sOld :: [s] <- query @ s |> filterWhere (#refEntity, e) |>
                                        filterWhereSql(#refValidfromversion,"<> " ++ encodeUtf8( show v)) |>
                                        filterWhere(#refValidthruversion,Nothing) |> fetch
                                    case head sOld of
                                        Just sOld -> do
                                                sUpd :: s <- sOld |> set #refValidthruversion (Just (Id v)) |> updateRecord
                                                Log.info $ "predecessor state terminated" ++ show s
                                        Nothing -> Log.info ("no predecessor state" ::String)
                                    case getShadowed accessor wfp of
                                        Nothing -> Log.info ("No version" ::String)
                                        Just (shadow,shadowed) -> do
                                            updated :: [Version]<- sqlQuery "update versions v set ref_shadowedby = ? where id in ? returning * " (v, In shadowed)
                                            forEach updated (\v -> Log.info $ "updated" ++ show v)
                                    commitTransaction
                                    pure (Left "commit successful")
                                    -- redirectTo $ ShowWorkflowAction workflowId
                                Nothing -> do
                                    pure $ Right $ "cannot commit: state is null h=" ++ show h ++ "v=" ++ show v ++ "e=" ++ show e
                            Nothing -> do
                                pure $ Right $ "cannot commit: entity is null h=" ++ show h ++ "v=" ++ show v 
                        Nothing -> do
                            pure $ Right $ "cannot commit: version is null h=" ++ show h
                    Nothing -> do
                        pure $ Right "cannot commit: history is null"
            Nothing -> do
                pure $ Right "SHOULDN'T: empty progress data"

    createHistory :: (?modelContext::ModelContext, ?context::context, LoggingProvider context ) => (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s))) -> Workflow -> s -> IO (s,StateKeys (Id e)(Id s))
    createHistory accessor workflow state = do
        Log.info $ "createHistory for workflow: " ++ show (get #id workflow)
        history ::History <- newRecord |> set #historyType (get #historyType workflow) |> set #refOwnedByWorkflow (Just $ get #id workflow)|> createRecord
        let historyUUID ::UUID = bubu $ get #id history
                                    where bubu (Id uuid) = uuid
        version :: Version <- newRecord |> set #refHistory (get #id history) |>  set #validfrom (get #validfrom workflow) |> createRecord
        let versionId :: Integer = bubu $ get #id version
                                    where bubu (Id intid) = intid
            histoType = get #historyType workflow
        entity ::e <- newRecord |> set #refHistory (get #id history) |> createRecord
        state ::s <- state |> set #refEntity (get #id entity) |> set #refValidfromversion (get #id version) |> createRecord
        let entityId = get #id entity
            stateId = get #id state
            cruE :: PersistenceLog = mkPersistenceLogState $ mkInsertLog entityId
            cruS :: PersistenceLog = mkPersistenceLogState $ mkInsertLog stateId
            cruW :: PersistenceLog = mkPersistenceLogState $ mkInsertLog $ get #id workflow
            cruH :: PersistenceLog = mkPersistenceLogState $ mkInsertLog $ get #id history
            cruV :: PersistenceLog  = mkPersistenceLogState $ mkInsertLog $ get #id version
            pl :: [PersistenceLog] = [cruE, cruS, cruW, cruH, cruV]
            sk :: StateKeys (Id e)(Id s) = stateKeysDefault {history = Just historyUUID, version = Just versionId, entity = Just entityId, state = Just stateId}
            wfp = setWorkFlowState (WorkflowProgress Nothing Nothing Nothing []) $ Just sk 
            progress = toJSON wfp {plog = pl}
        uptodate ::Workflow <- workflow |> set #progress progress |> updateRecord
        Log.info ("hier ist Workflow mit JSON " ++ show (get #progress uptodate))
        pure (state,sk)
    
    getShadowed :: (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s))) -> WorkflowProgress -> Maybe (Integer,[Integer])
    getShadowed accessor wfp = shadowed $ fromJust $ accessor wfp 
    setShadowed :: (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s))) -> WorkflowProgress -> (Integer,[Integer]) -> WorkflowProgress

    mutateHistory :: (?modelContext::ModelContext, ?context::context, LoggingProvider context) => (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s))) -> Workflow -> s -> IO (Workflow, s)
    mutateHistory accessor workflow state = do
        let wfprogress :: WorkflowProgress = fromJust $ getWfp workflow
        let versionIdMB = getStateVersionIdMB accessor wfprogress
        Log.info $ "mutateHistory Update histoType/wfprogress/versionid" ++ show (get #historyType workflow) ++ show wfprogress ++ "/" ++ show versionIdMB
        case versionIdMB of
            Just v -> do
                Log.info ("mutateHistory Update existing Version" :: String)
                pure (workflow,state)
            Nothing -> do
                Log.info ("mutateHistory Update new Version" :: String)
                let validfrom = tshow $ get #validfrom workflow
                    entityId :: (Id e) =  get #refEntity state
                entity::e <- fetch entityId
                version :: Version <- newRecord |> set #refHistory (get #refHistory entity) |> set #validfrom (get #validfrom workflow) |> createRecord
                newState :: s <- newRecord |> set #refEntity entityId |> set #refValidfromversion (get #id version) |>
                    set #content (get #content state) |> createRecord
                let wfpNew :: Value = updateWfpV accessor wfprogress (fromId $ get #refHistory entity) entityId (Just $ fromId $ get #id version) (Just $ get #id newState )
                workflow :: Workflow <- workflow |> set #progress wfpNew   |> updateRecord  
                Log.info $ "mutateHistory Update new Version = " ++  show (get #id newState)           
                pure (workflow,newState)

    queryImmutableState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context )=> Id Version -> IO (Id s)
    queryImmutableState versionId =  do
        mstate <- query @s |> filterWhere (#refValidfromversion, versionId) |> fetchOne
        pure $ get #id mstate

    queryVersionMutableValidfrom :: (?modelContext::ModelContext, ?context::context, LoggingProvider context) => (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s))) -> Workflow -> IO (Workflow,Version,[Version])
    queryVersionMutableValidfrom accessor workflow = do
        Log.info $ "queryVersionMutableValidfrom Workflow=" ++ show workflow
        let wfprogress :: WorkflowProgress = fromJust $ getWfp workflow
            validfrom = tshow $ get #validfrom workflow
        Log.info $ "queryVersionMutableValidfrom workflowProgress:" ++ show wfprogress
        let historyId =  fromJust $ case get #historyType workflow of
                HistorytypeContract -> getStatehistoryIdMB contract wfprogress
                HistorytypePartner -> getStatehistoryIdMB partner wfprogress
                HistorytypeTariff -> getStatehistoryIdMB tariff wfprogress
        Log.info $ "queryVersionMutableValidfrom HistoryId:" ++ show historyId ++ "validfrom" ++ show validfrom
        let q :: Query = "SELECT * FROM versions v WHERE v.id in (SELECT max(id) FROM versions where ref_history = ? and validfrom <= ?)"
            p :: (Id History, Text) = (Id historyId, validfrom)
        vs :: [Version]  <- sqlQuery  q p
        let versionId = get #id $ fromJust $ head vs 
        let q2 :: Query = "SELECT * FROM versions v WHERE ref_history = ? and v.id > ? and validfrom > ?"
        let p2 :: (Id History, Id Version,Text) = (Id historyId, versionId, validfrom)
        shadowed :: [Version]  <- sqlQuery  q2 p2
        let shadowedIds :: [Integer] = map (getKey . get #id) shadowed  
        Log.info ( "queryVersionMutableValidfrom versionId / shadowed / workflowId =" ++ show versionId ++ "/" ++ show shadowedIds ++ "/" ++ show (get #id workflow))
        workflow :: Workflow <- setWfp workflow (setShadowed accessor wfprogress (getKey versionId, shadowedIds)) |> updateRecord
        Log.info ("queryVersionMutableValidfrom progress=" ++ show (getWfp workflow ))
        pure (workflow, fromJust $ head vs, shadowed)
            where getKey (Id key) = key

    queryMutableState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context )=> (WorkflowProgress ->  Maybe (StateKeys (Id e)(Id s)))-> Workflow -> IO (Workflow, s,[Version])
    queryMutableState accessor workflow =  do
        Log.info $ "queryMutableState workflow=" ++ show (get #progress workflow)
        (workflow,version,shadowed) :: (Workflow, Version,[Version]) <- queryVersionMutableValidfrom accessor workflow 
        Log.info $ "queryMutableState version=" ++ show version ++ " shadowed " ++ show shadowed
        Log.info $ "queryMutableState wfp=" ++ show ( fromJust $ getWfp workflow )
        let wfp = fromJust $ getWfp workflow 
            h = fromJust $ getStatehistoryIdMB accessor wfp
            e = fromJust $ getEntityIdMB accessor wfp
            v = get #id version
        mstate <- query @s |> filterWhere(#refEntity, e) |> filterWhereSql (#refValidfromversion, encodeUtf8("<= " ++ show v)) |>
                            queryOr 
                                 (filterWhereSql (#refValidthruversion, encodeUtf8("> " ++ show v)))
                                 (filterWhereSql (#refValidthruversion, "is null")) |> fetchOne
        Log.info ("queryMutableState shadow/shadowed=" ++ show version ++ " / " ++ show shadowed)
        pure (workflow, mstate,shadowed)

    runMutation :: (?modelContext::ModelContext, ?context::context, LoggingProvider context, Show s, CanVersion e s) => (WorkflowProgress -> Maybe (StateKeys (Id e)(Id s))) -> User -> HistoryType -> s -> Day -> Text -> IO()
    runMutation accessor usr histoType s validfrom newContent = do
        Log.info $ ">>>>>>>>>>>>>>> runMutation start" ++ show histoType 
        let eId :: Id e = get #refEntity s
        Log.info $ "runMutation entity =" ++ show eId
        entity :: e <- fetch eId
        let hid :: (Id History) = get #refHistory entity
            wfpJ :: Value = updateWfpV accessor workflowProgressDefault (fromId hid) eId Nothing Nothing 
        Log.info $ "runMutation history = " ++ show hid
        workflow <- newRecord |> set #refUser (get #id usr) |> set #historyType histoType |> set #workflowType WftypeUpdate |> 
            set #progress wfpJ |> set #validfrom validfrom |> createRecord
        Log.info $ "runMutation wfmut1 = " ++ show workflow
        (workflow,state,shadowed) :: (Workflow, s,[Version]) <- queryMutableState accessor workflow
        Log.info $ "runMutation MUTABLE/SHADOWED=" ++ show state ++ "/" ++ show shadowed
        let  newState = state |> set #content newContent
        (workflow,newState) :: (Workflow,s) <- mutateHistory getAccessor workflow state
        Log.info $ ">>>>>>>>>>>>>>> NACH MUTATE histotype " ++ show histoType ++ " progress" ++ show (get #progress workflow)
        Log.info $ "Workflow für commit:" ++ show  workflow
        result <- commitState accessor workflow 
        case result of
            Left msg -> Log.info $ "SUCCESS:"++ msg
            Right msg -> Log.info $ "ERROR:" ++ msg
    
        Log.info $ ">>>>>>>>>>>>>>> NACH COMMITMUTATATION " ++ show histoType
            
queryEntityStateByValidFromMaxTxn :: HistoryType -> String -> Query 
queryEntityStateByValidFromMaxTxn historyType fieldList =  do
    fromString $ printf qryFS entityState entity where
        limitOffset = case fieldList of
            "es.*" -> "limit ? offset ? "
            "count(*)" -> ""
        qryFS = "select " ++ fieldList ++ " from %s es join %s e on es.ref_entity = e.id where (e.ref_history, ref_validfromversion) in " ++
            "(select h.id, MAX(v.id) from versions v join histories h on v.ref_history = h.id " ++
            "where h.history_type = ?  and v.validfrom <= ? and v.createdat < ? " ++
              "group by h.id ) " ++ limitOffset
        (entity::String,entityState::String) = case historyType of
            HistorytypeContract -> ("contracts","contract_states")
            HistorytypePartner -> ("partners","partner_states")
            HistorytypeTariff -> ("tariffs","tariff_states")

countStatesByValidFromMaxTxn :: (?context::context, ?modelContext::ModelContext, FromField b1, ToField c, ToField b2, LoggingProvider context, Show c, Show b2,Show b1) => HistoryType -> b2 -> c -> IO b1
countStatesByValidFromMaxTxn historyType valid maxtxn= do
    let qry = queryEntityStateByValidFromMaxTxn historyType "count(*)"
    Log.info $ "Count QRY " ++ show qry      
    [Only count] <- sqlQuery qry (historyType, valid, maxtxn)
    Log.info $ "Count " ++ show count  
    pure count    

selectStatesByValidFromMaxTxn :: (?modelContext :: ModelContext,?context::context, LoggingProvider context, CanVersion r rs) => HistoryType -> Day -> UTCTime -> PT.Options -> PT.Pagination -> IO ([rs],Pagination)
selectStatesByValidFromMaxTxn historyType valid maxtxn options pagination = do            
    Log.info $ "Pagination "  ++ show pagination     
    result :: [rs] <- sqlQuery (queryEntityStateByValidFromMaxTxn historyType "es.*") (historyType,valid, maxtxn, pageSize pagination, ((currentPage pagination) -1) * pageSize pagination)
    pure (result,pagination {currentPage=(currentPage pagination) +1})

instance CanVersion Contract ContractState where
    getAccessor :: (WorkflowProgress -> Maybe (StateKeys (Id'"contracts")(Id' "contract_states")))
    getAccessor = contract
    setShadowed :: (WorkflowProgress ->  Maybe (StateKeys (Id'"contracts")(Id' "contract_states"))) -> WorkflowProgress -> (Integer,[Integer]) -> WorkflowProgress
    setShadowed accessor wfp shadow = let new :: StateKeys (Id'"contracts")(Id' "contract_states") = fromJust $ accessor wfp 
        in wfp {contract = Just $ new { shadowed = Just shadow }}
    setWorkFlowState :: WorkflowProgress -> Maybe (StateKeys (Id'"contracts")(Id' "contract_states")) -> WorkflowProgress
    setWorkFlowState wfp s = wfp  {contract = s} 
instance CanVersion Partner PartnerState where
    getAccessor :: (WorkflowProgress ->Maybe (StateKeys (Id'"partners")(Id' "partner_states")))
    getAccessor = partner
    setShadowed :: (WorkflowProgress ->  Maybe (StateKeys (Id'"partners")(Id' "partner_states"))) -> WorkflowProgress -> (Integer,[Integer]) -> WorkflowProgress
    setShadowed accessor wfp shadow = let new :: StateKeys (Id'"partners")(Id' "partner_states") = fromJust $ accessor wfp 
        in wfp {partner = Just $ new { shadowed = Just shadow }}
    setWorkFlowState :: WorkflowProgress ->Maybe (StateKeys (Id'"partners")(Id' "partner_states")) -> WorkflowProgress
    setWorkFlowState wfp s = wfp  {partner = s} 
instance CanVersion Tariff TariffState where
    getAccessor :: (WorkflowProgress ->Maybe (StateKeys (Id'"tariffs")(Id' "tariff_states")))
    getAccessor = tariff
    setShadowed :: (WorkflowProgress ->  Maybe (StateKeys (Id'"tariffs")(Id' "tariff_states"))) -> WorkflowProgress -> (Integer,[Integer]) -> WorkflowProgress
    setShadowed accessor wfp shadow = let new :: StateKeys (Id'"tariffs")(Id' "tariff_states") = fromJust $ accessor wfp 
        in wfp {tariff = Just $ new { shadowed = Just shadow }}
    setWorkFlowState :: WorkflowProgress ->Maybe (StateKeys (Id'"tariffs")(Id' "tariff_states")) -> WorkflowProgress
    setWorkFlowState wfp s = wfp  {tariff = s} 


putPartnerState :: (?context::ControllerContext, ?modelContext :: ModelContext) => (Id ContractState) -> (Id PartnerState) -> [PersistenceLog]-> IO([PersistenceLog])
putPartnerState contractStateId partnerStateId pLog = do
    contractState <- fetch contractStateId
    partnerState <- fetch partnerStateId
    contract <- fetch (get #refEntity contractState) 
    newContractPartner :: ContractPartner <- newRecord |> set #refHistory (get #refHistory contract) |> createRecord
    newContractPartnerState :: ContractPartnerState <- newRecord |> set #refEntity (get #id newContractPartner) |> 
        set #refContract (get #id contractState) |> set #refPartner (get #id partnerState) |>
        set #refValidfromversion (get #refValidfromversion contractState) |> set #refValidthruversion Nothing |> createRecord
    let cpsLog = ( mkPersistenceLogState $ mkInsertLog $ get #id newContractPartnerState ) : pLog
    setSuccessMessage "new ContractStatePartnerState"
    pure cpsLog