module Application.Model.CanVersion where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


import GHC.Exts
import GHC.Records
import GHC.Generics
import Data.Functor
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
import Application.Model.WorkflowEnvironment as WorkflowEnvironment
    ( HasTxnLog(mkInsertLog, mkPersistenceLogState),
      PersistenceLog,
      WorkflowEnvironment(plog),
      StateKeys(history, version, entity, state, shadowed),
      fromId,
      stateKeysDefault,
      workflowEnvironmentDefault,
      getWfe,
      setWfe,
      getPLog,
      setPLog )
import Text.Printf (printf)

import IHP.Pagination.Types as PT

today :: IO Day -- :: (year,month,day)
today = getCurrentTime Data.Functor.<&> utctDay

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
    updateWfpV :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> WorkflowEnvironment -> Id History -> Id e -> Maybe (Id Version) -> Maybe (Id s) -> Value
    updateWfpV accessor wfe hId eId vIdMB sIdMB = fromJust $ decode $ encode $ setWorkFlowState wfe (Just ((fromJust $ accessor wfe) { history= Just hId, entity = Just eId, version= vIdMB, state= sIdMB } ))
    initialWfpV:: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> Id History -> Value
    initialWfpV accessor h = fromJust $ decode $ encode $ setWorkFlowState workflowEnvironmentDefault (Just ((fromJust $ accessor workflowEnvironmentDefault) { history= Just h} ))
    getStatehistoryIdMB :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> WorkflowEnvironment -> Maybe (Id History)
    getStatehistoryIdMB accessor wfe = history =<< accessor wfe
    getStateVersionIdMB :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) ->WorkflowEnvironment -> Maybe (Id Version)
    getStateVersionIdMB accessor wfe = version =<< accessor wfe
    getEntityIdMB :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) ->WorkflowEnvironment -> Maybe (Id e)
    getEntityIdMB  accessor wfe = entity =<< accessor wfe
    getStateIdMB :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) ->WorkflowEnvironment -> Maybe (Id s)
    getStateIdMB  accessor wfe = state =<< accessor wfe

    commitState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context ) => StateKeys (Id e)(Id s) -> Workflow -> IO (Either Text Text)
    commitState wfe workflow = do
        let workflowId = get #id workflow
        Log.info $ "ToCOmmit wfe=" ++ show wfe
        case history wfe of
            Just h -> case version wfe of
                Just v -> case entity wfe of
                    Just e -> case state wfe of
                        Just s -> withTransaction do 
                            Log.info $ "committing h:" ++ show h
                            hUnlocked :: History <- fetch h
                            hUnlocked |> set #refOwnedByWorkflow Nothing |> updateRecord
                            Log.info $ "Unlocked h:" ++ show h
                            Log.info $ "version=" ++ show v
                            Log.info $ "entity=" ++ show e
                            newVersion :: Version <- fetch v
                            newVersion |>set #committed True |> updateRecord
                            Log.info $ "commit version v: " ++ show v
                            w <- workflow |> set #workflowStatus "committed" |> updateRecord
                            Log.info $ "commit workflow w: " ++ show w
                            sOld :: [s] <- query @ s |> filterWhere (#refEntity, e) |>
                                filterWhereSql(#refValidfromversion,"<> " ++ encodeUtf8( show v)) |>
                                filterWhere(#refValidthruversion,Nothing) |> fetch
                            case head sOld of
                                Just sOld -> do
                                        sUpd :: s <- sOld |> set #refValidthruversion (Just v) |> updateRecord
                                        Log.info $ "predecessor state terminated" ++ show s
                                Nothing -> Log.info ("no predecessor state to terminate" ::String)
                            case shadowed wfe of
                                Nothing -> Log.info ("No version" ::String)
                                Just (shadow,shadowed) -> do
                                    updated :: [Version]<- sqlQuery "update versions v set ref_shadowedby = ? where id in ? returning * " (v, In shadowed)
                                    forEach updated (\v -> Log.info $ "updated" ++ show v)
                            commitTransaction
                            pure (Left "commit successful")
                        Nothing -> do
                            pure $ Right $ "cannot commit: state is null h=" ++ show h ++ "v=" ++ show v ++ "e=" ++ show e
                    Nothing -> do
                        pure $ Right $ "cannot commit: entity is null h=" ++ show h ++ "v=" ++ show v 
                Nothing -> do
                    pure $ Right $ "cannot commit: version is null h=" ++ show h
            Nothing -> do
                pure $ Right "cannot commit: history is null"

    createHistory :: (?modelContext::ModelContext, ?context::context, LoggingProvider context ) => Id Workflow -> HistoryType -> Day -> s -> IO (s,StateKeys (Id e)(Id s),[PersistenceLog])
    createHistory workflowId historyType validFrom state = do
        history ::History <- newRecord |> set #historyType historyType |> set #refOwnedByWorkflow (Just workflowId) |> createRecord
        let historyId = get #id history
        version :: Version <- newRecord |> set #refHistory (get #id history) |>  set #validfrom validFrom |> createRecord
        entity ::e <- newRecord |> set #refHistory (get #id history) |> createRecord
        state ::s <- state |> set #refEntity (get #id entity) |> set #refValidfromversion (get #id version) |> createRecord
        let versionId = get #id version
            entityId = get #id entity
            stateId = get #id state
            cruE :: PersistenceLog = mkPersistenceLogState $ mkInsertLog entityId
            cruS :: PersistenceLog = mkPersistenceLogState $ mkInsertLog stateId
            cruW :: PersistenceLog = mkPersistenceLogState $ mkInsertLog  workflowId
            cruH :: PersistenceLog = mkPersistenceLogState $ mkInsertLog $ get #id history
            cruV :: PersistenceLog  = mkPersistenceLogState $ mkInsertLog $ get #id version
            pl :: [PersistenceLog] = [cruE, cruS, cruW, cruH, cruV]
            sk :: StateKeys (Id e)(Id s) = stateKeysDefault {history = Just historyId, version = Just versionId, entity = Just entityId, state = Just stateId}
        pure (state,sk, pl)
    
    getShadowed :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> WorkflowEnvironment -> Maybe (Id Version,[Id Version])
    getShadowed accessor wfe = shadowed $ fromJust $ accessor wfe 
    setShadowed :: (WorkflowEnvironment ->  Maybe (StateKeys (Id e)(Id s))) -> WorkflowEnvironment -> (Id Version,[Id Version]) -> WorkflowEnvironment

    mutateHistory :: (?modelContext::ModelContext, ?context::context, LoggingProvider context) => StateKeys (Id e)(Id s) -> Day -> s -> IO (s, StateKeys (Id e)(Id s),[PersistenceLog])
    mutateHistory wfe validfrom state = do
        Log.info $ "mutateHistory Update wfe" ++ show wfe
        let historyId = fromJust $ history wfe
            entityId = fromJust $ entity wfe
        case version wfe of
            Just v -> do
                Log.info $ show "mutateHistory Update existing Version/state"
                currentState <- state |> updateRecord 
                pure (currentState,wfe,[])
            Nothing -> do
                Log.info $ show "mutateHistory Update new Version"
                newVersion :: Version <- newRecord |> set #refHistory historyId |> set #validfrom validfrom |> createRecord
                let versionId :: Id Version = get #id newVersion
                newState :: s <- state |> set #refEntity entityId |> set #refValidfromversion versionId |> createRecord
                let stateId = get #id newState
                    wfeUpd :: StateKeys (Id e)(Id s) = wfe { version= Just versionId , state = Just stateId }  
                    cruS :: PersistenceLog = mkPersistenceLogState $ mkInsertLog stateId
                    cruV :: PersistenceLog  = mkPersistenceLogState $ mkInsertLog versionId
                    pl :: [PersistenceLog] = [cruS, cruV]
                Log.info $ "mutateHistory Update new Version = " ++  show (get #id newState) ++  " wfeUpd " ++ show wfeUpd      
                pure (newState, wfeUpd, pl )

    queryImmutableState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context )=> Id Version -> IO (Id s)
    queryImmutableState versionId =  do
        mstate <- query @s |> filterWhere (#refValidfromversion, versionId) |> fetchOne
        pure $ get #id mstate

    queryVersionMutableValidfrom :: (?modelContext::ModelContext, ?context::context, LoggingProvider context) => StateKeys (Id e)(Id s) -> Day -> IO (StateKeys (Id e)(Id s), Version,[Version])
    queryVersionMutableValidfrom wfe validfrom = do
        let historyId =  fromJust $ history wfe
        let q :: Query = "SELECT * FROM versions v WHERE v.id in (SELECT max(id) FROM versions where ref_history = ? and validfrom <= ?)"
            p :: (Id History, Day) = (historyId, validfrom)
        vs :: [Version]  <- sqlQuery  q p
        let versionId = get #id $ fromJust $ head vs 
        let q2 :: Query = "SELECT * FROM versions v WHERE ref_history = ? and v.id >= ? and validfrom >= ?"
        let p2 :: (Id History, Id Version,Day) = (historyId, versionId, validfrom)
        shadowed :: [Version]  <- sqlQuery  q2 p2
        let shadowedIds :: [Id Version] = map (get #id) shadowed
        pure (wfe { shadowed = Just (get #id (fromJust $ head vs), shadowedIds)}, fromJust $ head vs, shadowed)

    queryMutableState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context )=> StateKeys (Id e)(Id s)-> Day -> IO (StateKeys (Id e)(Id s), s,[Version])
    queryMutableState wfe validfrom =  do
        (wfe,version,shadowed) :: (StateKeys (Id e) (Id s), Version,[Version]) <- queryVersionMutableValidfrom wfe validfrom
        let h = fromJust $ history wfe
            e = fromJust $ entity wfe
            v = get #id version
        mstate :: s <- query @s |> filterWhere(#refEntity, e) |> filterWhereSql (#refValidfromversion, encodeUtf8("<= " ++ show v)) |>
                            queryOr 
                                 (filterWhereSql (#refValidthruversion, encodeUtf8("> " ++ show v)))
                                 (filterWhereSql (#refValidthruversion, "is null")) |> fetchOne
        pure (wfe {state=Just (get #id mstate)}, mstate,shadowed)

    createCreationWorkflow :: (?modelContext::ModelContext, ?context::context, LoggingProvider context, Show s, CanVersion e s) => (WorkflowEnvironment -> Maybe (StateKeys (Id e)(Id s))) -> User -> HistoryType -> Day -> IO Workflow
    createCreationWorkflow accessor usr histoType validfrom = do
        wf ::Workflow <- newRecord |> set #refUser (get #id usr) |> set #historyType histoType |> set #validfrom validfrom |>  set #workflowType WftypeNew |> createRecord
        pure wf
        
    createUpdateWorkflow :: (?modelContext::ModelContext, ?context::context, LoggingProvider context, Show s, CanVersion e s) => (WorkflowEnvironment -> Maybe (StateKeys (Id e)(Id s))) -> User -> HistoryType -> s -> Day -> IO Workflow
    createUpdateWorkflow accessor usr histoType s validfrom = do
        let eId :: Id e = get #refEntity s
        Log.info $ "runMutation entity =" ++ show eId
        entity :: e <- fetch eId
        let hid :: (Id History) = get #refHistory entity
            wfpJ :: Value = updateWfpV accessor workflowEnvironmentDefault hid eId Nothing Nothing 
        Log.info $ "runMutation history = " ++ show hid
        workflow <- newRecord |> set #refUser (get #id usr) |> set #historyType histoType |> set #workflowType WftypeUpdate |> 
            set #progress wfpJ |> set #validfrom validfrom |> createRecord
        Log.info $ "runMutation wfmut1 = " ++ show workflow
        pure workflow

    runMutation :: (?modelContext::ModelContext, ?context::context, LoggingProvider context, Show s, CanVersion e s) => StateKeys (Id e)(Id s) -> s -> Day -> Text -> IO (s,StateKeys (Id e)(Id s),[PersistenceLog])
    runMutation wfe state validfrom newContent = do
        (wfe,state,shadowed) :: (StateKeys (Id e)(Id s), s,[Version]) <- queryMutableState wfe validfrom
        Log.info $ "runMutation MUTABLE/SHADOWED=" ++ show wfe ++ " ////shadowed /// " ++ show shadowed
        let  newState = state |> set #content newContent 
        result@(newState,wfe,pl) :: (s,StateKeys (Id e)(Id s),[PersistenceLog]) <- mutateHistory wfe validfrom newState
        pure result

            
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
    result :: [relationState] <- sqlQuery (queryEntityStateByValidFromMaxTxn historyType "es.*") (historyType,valid, maxtxn, pageSize pagination, (currentPage pagination -1) * pageSize pagination)
    pure (result,pagination {currentPage= currentPage pagination +1})
class (CanVersion sourceEntity sourceState, CanVersion targetEntity targetState, CanVersion relation relationState, HasField "refTarget" relationState (Id targetState), SetField "refTarget" relationState (Id targetState)) => CanVersionRelation sourceEntity sourceState targetEntity targetState relation relationState
    where
    putRelState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context) => Id sourceState -> Id targetState -> IO (relationState, StateKeys(Id relation) (Id relationState),[PersistenceLog] )
    putRelState sid tid = do
        Log.info $ tshow "enter commitState" 
        src :: sourceState <- fetch sid
        tgt :: targetState <- fetch tid
        entity :: sourceEntity <- fetch (get #refEntity src) 
        version :: sourceEntity <- fetch (get #refEntity src) 
        let historyId = get #refHistory entity
            versionId = get #refValidfromversion src
        newRelation :: relation <- newRecord |> set #refHistory historyId |> createRecord
        newRelationState :: relationState <- newRecord |> set #refEntity (get #id newRelation) |> set #refTarget tid |>
            set #refValidfromversion versionId |> set #refValidthruversion Nothing |> createRecord
        let entityId = get #id newRelation
            stateId = get #id newRelationState
            cruE :: PersistenceLog = mkPersistenceLogState $ mkInsertLog entityId
            cruS :: PersistenceLog = mkPersistenceLogState $ mkInsertLog stateId
            pl :: [PersistenceLog] = [cruE, cruS]
            sk :: StateKeys (Id relation)(Id relationState) = stateKeysDefault {history = Just historyId, version = Just versionId, entity = Just entityId, state = Just stateId}
        return (newRelationState, sk, pl)

