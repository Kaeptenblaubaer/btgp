module Application.Helper.CanVersion where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


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
import Application.Helper.WorkflowEnvironment
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
    getAccessor :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s)))
    getWorkFlowState :: WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))
    getWorkFlowState wfe = getAccessor wfe
    setWorkFlowState :: WorkflowEnvironment -> Maybe (StateKeys (Id e)(Id s)) -> WorkflowEnvironment
    updateWfpV :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> WorkflowEnvironment -> UUID -> Id e -> Maybe Integer -> Maybe (Id s) -> Value
    updateWfpV accessor wfe hId eId vIdMB sIdMB = fromJust $ decode $ encode $ setWorkFlowState wfe (Just ((fromJust $ accessor wfe) { history= Just hId, entity = Just eId, version= vIdMB, state= sIdMB } ))
    initialWfpV:: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> UUID -> Value
    initialWfpV accessor h = fromJust $ decode $ encode $ setWorkFlowState workflowEnvironmentDefault (Just ((fromJust $ accessor workflowEnvironmentDefault) { history= Just h} ))
    getStatehistoryIdMB :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> WorkflowEnvironment -> Maybe UUID
    getStatehistoryIdMB accessor wfe = history =<< accessor wfe
    getStateVersionIdMB :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) ->WorkflowEnvironment -> Maybe Integer
    getStateVersionIdMB accessor wfe = version =<< accessor wfe
    getEntityIdMB :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) ->WorkflowEnvironment -> Maybe (Id e)
    getEntityIdMB  accessor wfe = entity =<< accessor wfe
    getStateIdMB :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) ->WorkflowEnvironment -> Maybe (Id s)
    getStateIdMB  accessor wfe = state =<< accessor wfe

    commitState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context ) => (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> Workflow -> IO (Either Text Text)
    commitState accessor workflow = do
        let workflowId = get #id workflow
        Log.info $ "ToCOmmitWF wf=" ++ show workflowId
        let wfpMB = getWfe workflow
        Log.info ("ende"::String)
        putStrLn "Ende commit"
        case wfpMB of
            Just wfe -> do
                case getStatehistoryIdMB accessor wfe of
                    Just h -> case getStateVersionIdMB accessor wfe of
                        Just v -> case getEntityIdMB accessor wfe of
                            Just e -> case getStateIdMB accessor wfe of
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
                                    case getShadowed accessor wfe of
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

    createHistory :: (?modelContext::ModelContext, ?context::context, LoggingProvider context ) => (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> Workflow -> s -> IO (s,StateKeys (Id e)(Id s))
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
            wfe = setWorkFlowState (WorkflowEnvironment Nothing Nothing Nothing Nothing []) $ Just sk 
            progress = toJSON wfe {plog = pl}
        uptodate ::Workflow <- workflow |> set #progress progress |> updateRecord
        Log.info ("hier ist Workflow mit JSON " ++ show (get #progress uptodate))
        pure (state,sk)
    
    getShadowed :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> WorkflowEnvironment -> Maybe (Integer,[Integer])
    getShadowed accessor wfe = shadowed $ fromJust $ accessor wfe 
    setShadowed :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> WorkflowEnvironment -> (Integer,[Integer]) -> WorkflowEnvironment

    mutateHistory :: (?modelContext::ModelContext, ?context::context, LoggingProvider context) => (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> Workflow -> s -> IO (Workflow, s)
    mutateHistory accessor workflow state = do
        let wfprogress :: WorkflowEnvironment = fromJust $ getWfe workflow
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

    queryVersionMutableValidfrom :: (?modelContext::ModelContext, ?context::context, LoggingProvider context) => (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> Workflow -> IO (Workflow,Version,[Version])
    queryVersionMutableValidfrom accessor workflow = do
        Log.info $ "queryVersionMutableValidfrom Workflow=" ++ show workflow
        let wfprogress :: WorkflowEnvironment = fromJust $ getWfe workflow
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
        workflow :: Workflow <- setWfe workflow (setShadowed accessor wfprogress (getKey versionId, shadowedIds)) |> updateRecord
        Log.info ("queryVersionMutableValidfrom progress=" ++ show (getWfe workflow ))
        pure (workflow, fromJust $ head vs, shadowed)
            where getKey (Id key) = key

    queryMutableState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context )=> (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s)))-> Workflow -> IO (Workflow, s,[Version])
    queryMutableState accessor workflow =  do
        Log.info $ "queryMutableState workflow=" ++ show (get #progress workflow)
        (workflow,version,shadowed) :: (Workflow, Version,[Version]) <- queryVersionMutableValidfrom accessor workflow 
        Log.info $ "queryMutableState version=" ++ show version ++ " shadowed " ++ show shadowed
        Log.info $ "queryMutableState wfe=" ++ show ( fromJust $ getWfe workflow )
        let wfe = fromJust $ getWfe workflow 
            h = fromJust $ getStatehistoryIdMB accessor wfe
            e = fromJust $ getEntityIdMB accessor wfe
            v = get #id version
        mstate <- query @s |> filterWhere(#refEntity, e) |> filterWhereSql (#refValidfromversion, encodeUtf8("<= " ++ show v)) |>
                            queryOr 
                                 (filterWhereSql (#refValidthruversion, encodeUtf8("> " ++ show v)))
                                 (filterWhereSql (#refValidthruversion, "is null")) |> fetchOne
        Log.info ("queryMutableState shadow/shadowed=" ++ show version ++ " / " ++ show shadowed)
        pure (workflow, mstate,shadowed)

    createCreationWorkflow :: (?modelContext::ModelContext, ?context::context, LoggingProvider context, Show s, CanVersion e s) => (WorkflowEnvironment -> Maybe (StateKeys (Id e)(Id s))) -> User -> HistoryType -> Day -> IO(Workflow)
    createCreationWorkflow accessor usr histoType validfrom = do
        wf ::Workflow <- newRecord |> set #refUser (get #id usr) |> set #historyType histoType |> set #validfrom validfrom |>  set #workflowType WftypeNew |> createRecord
        pure wf
        
    createUpdateWorkflow :: (?modelContext::ModelContext, ?context::context, LoggingProvider context, Show s, CanVersion e s) => (WorkflowEnvironment -> Maybe (StateKeys (Id e)(Id s))) -> User -> HistoryType -> s -> Day -> IO(Workflow)
    createUpdateWorkflow accessor usr histoType s validfrom = do
        let eId :: Id e = get #refEntity s
        Log.info $ "runMutation entity =" ++ show eId
        entity :: e <- fetch eId
        let hid :: (Id History) = get #refHistory entity
            wfpJ :: Value = updateWfpV accessor workflowEnvironmentDefault (fromId hid) eId Nothing Nothing 
        Log.info $ "runMutation history = " ++ show hid
        workflow <- newRecord |> set #refUser (get #id usr) |> set #historyType histoType |> set #workflowType WftypeUpdate |> 
            set #progress wfpJ |> set #validfrom validfrom |> createRecord
        Log.info $ "runMutation wfmut1 = " ++ show workflow
        pure workflow

    runMutation :: (?modelContext::ModelContext, ?context::context, LoggingProvider context, Show s, CanVersion e s) => (WorkflowEnvironment -> Maybe (StateKeys (Id e)(Id s))) -> User -> HistoryType -> s -> Day -> Text -> IO(Workflow)
    runMutation accessor usr histoType s validfrom newContent = do
        Log.info $ ">>>>>>>>>>>>>>> runMutation start" ++ show histoType 
        workflow <- createUpdateWorkflow accessor usr histoType s validfrom
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
        pure workflow

            
queryEntityStateByValidFromMaxTxn :: HistoryType -> String -> Query 
queryEntityStateByValidFromMaxTxn historyType fieldList =  do
    fromString $ printf qryFS entityState entity where
        limitOffset = case fieldList of
            "es.*" -> "limit ? offset ? "
            "count(*)" -> ""
            _ -> ""
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

selectStatesByValidFromMaxTxn :: (?modelContext :: ModelContext,?context::context, LoggingProvider context, CanVersion relation relationState) => HistoryType -> Day -> UTCTime -> PT.Options -> PT.Pagination -> IO ([relationState],Pagination)
selectStatesByValidFromMaxTxn historyType valid maxtxn options pagination = do            
    Log.info $ "Pagination "  ++ show pagination     
    result :: [relationState] <- sqlQuery (queryEntityStateByValidFromMaxTxn historyType "es.*") (historyType,valid, maxtxn, pageSize pagination, ((currentPage pagination) -1) * pageSize pagination)
    pure (result,pagination {currentPage=(currentPage pagination) +1})

instance CanVersion Contract ContractState where
    getAccessor :: (WorkflowEnvironment -> Maybe (StateKeys (Id'"contracts")(Id' "contract_states")))
    getAccessor = contract
    setShadowed :: (WorkflowEnvironment ->  Maybe (StateKeys (Id'"contracts")(Id' "contract_states"))) -> WorkflowEnvironment -> (Integer,[Integer]) -> WorkflowEnvironment
    setShadowed accessor wfe shadow = let new :: StateKeys (Id'"contracts")(Id' "contract_states") = fromJust $ accessor wfe 
        in wfe {contract = Just $ new { shadowed = Just shadow }}
    setWorkFlowState :: WorkflowEnvironment -> Maybe (StateKeys (Id'"contracts")(Id' "contract_states")) -> WorkflowEnvironment
    setWorkFlowState wfe s = wfe  {contract = s} 
instance CanVersion Partner PartnerState where
    getAccessor :: (WorkflowEnvironment ->Maybe (StateKeys (Id'"partners")(Id' "partner_states")))
    getAccessor = partner
    setShadowed :: (WorkflowEnvironment ->  Maybe (StateKeys (Id'"partners")(Id' "partner_states"))) -> WorkflowEnvironment -> (Integer,[Integer]) -> WorkflowEnvironment
    setShadowed accessor wfe shadow = let new :: StateKeys (Id'"partners")(Id' "partner_states") = fromJust $ accessor wfe 
        in wfe {partner = Just $ new { shadowed = Just shadow }}
    setWorkFlowState :: WorkflowEnvironment ->Maybe (StateKeys (Id'"partners")(Id' "partner_states")) -> WorkflowEnvironment
    setWorkFlowState wfe s = wfe  {partner = s} 
instance CanVersion Tariff TariffState where
    getAccessor :: (WorkflowEnvironment ->Maybe (StateKeys (Id'"tariffs")(Id' "tariff_states")))
    getAccessor = tariff
    setShadowed :: (WorkflowEnvironment ->  Maybe (StateKeys (Id'"tariffs")(Id' "tariff_states"))) -> WorkflowEnvironment -> (Integer,[Integer]) -> WorkflowEnvironment
    setShadowed accessor wfe shadow = let new :: StateKeys (Id'"tariffs")(Id' "tariff_states") = fromJust $ accessor wfe 
        in wfe {tariff = Just $ new { shadowed = Just shadow }}
    setWorkFlowState :: WorkflowEnvironment ->Maybe (StateKeys (Id'"tariffs")(Id' "tariff_states")) -> WorkflowEnvironment
    setWorkFlowState wfe s = wfe  {tariff = s} 

instance CanVersion ContractPartner ContractPartnerState where
    getAccessor :: (WorkflowEnvironment ->Maybe (StateKeys (Id'"contract_partners")(Id' "contract_partner_states")))
    getAccessor = contractPartner

class (CanVersion sourceEntity sourceState, CanVersion targetEntity targetState, CanVersion relation relationState, HasField "refSource" relationState (Id sourceState), SetField "refSource" relationState (Id sourceState), HasField "refTarget" relationState (Id targetState), SetField "refTarget" relationState (Id targetState)) => CanVersionRelation sourceEntity sourceState targetEntity targetState relation relationState
    where
    putRelState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context) => (WorkflowEnvironment ->  Maybe (StateKeys (Id relation)(Id relationState))) -> (Id sourceState) -> (Id targetState) -> [PersistenceLog]-> IO([PersistenceLog])
    putRelState accessor sid tid pLog = do
        src :: sourceState <- fetch sid
        tgt :: targetState <- fetch tid
        srcEntity :: sourceEntity <- fetch (get #refEntity src) 
        newRelation :: relation <- newRecord |> set #refHistory (get #refHistory srcEntity) |> createRecord
        newRelationState :: relationState <- newRecord |> set #refEntity (get #id newRelation) |> 
            set #refSource sid |> set #refTarget tid |>
            set #refValidfromversion (get #refValidfromversion src) |> set #refValidthruversion Nothing |> createRecord
        let cpsLog = ( mkPersistenceLogState $ mkInsertLog $ get #id newRelationState ) : pLog
        -- setSuccessMessage "new ContractStatePartnerState"
        pure cpsLog

instance CanVersionRelation Contract ContractState Partner PartnerState ContractPartner ContractPartnerState