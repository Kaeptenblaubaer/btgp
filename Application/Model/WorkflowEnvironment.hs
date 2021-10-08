module Application.Model.WorkflowEnvironment where
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
import Database.PostgreSQL.Simple ( Query, ToRow )
import Data.Text.Encoding ( encodeUtf8 )
import Data.Text.Read as T (decimal)

getCurrentWorkflow :: (?context::ControllerContext, ?modelContext::ModelContext) => IO Workflow
getCurrentWorkflow  = do
    id <- getSessionUUID "workflowId"
    Log.info $ "current workflowid = " ++ show id
    wf :: Workflow <- fetch (Id (fromJust id))
    pure wf

getCurrentWorkflowId :: (?context::ControllerContext, ?modelContext::ModelContext) => IO (Id Workflow)
getCurrentWorkflowId = do
        workflow :: Workflow <- getCurrentWorkflow
        pure (get #id workflow)
    
setCurrentWorkflowId :: (?context::ControllerContext) => Workflow -> IO ()
setCurrentWorkflowId workflow = do
    oldid <- getSessionUUID "workflowId"
    Log.info $ "old workflowid = " ++ show oldid
    setSession "workflowId" (show (get #id workflow)) 
    newid <- getSessionUUID "workflowId"
    Log.info $ "current workflowid = " ++ show newid

getWfe :: Workflow -> Maybe WorkflowEnvironment
getWfe workflow  =  decode $ encode $ get #progress workflow

setWfe :: Workflow -> WorkflowEnvironment -> Workflow
setWfe wf wfe = wf |> set #progress ( fromJust $ decode $ encode wfe )


getPLog :: Workflow -> [PersistenceLog] 
getPLog workflow = maybe  [] (plog) (decode $ encode $ get #progress workflow)

setPLog :: Workflow -> [PersistenceLog] -> Workflow
setPLog workflow  pl =  do
    let wfe = case decode $ encode $ get #progress workflow of
            Nothing -> workflowEnvironmentDefault {plog=pl}
            Just wfe -> wfe {plog=pl++plog wfe}
    setWfe workflow wfe

fromId :: Id' table -> PrimaryKey table
fromId (Id key) = key

data StateKeys entityId stateId = StateKeys  {
    history :: Maybe (Id History) , version :: Maybe (Id Version), entity :: Maybe entityId, state :: Maybe stateId, shadowed :: Maybe (Integer,[Integer])} deriving (Show, Generic)
stateKeysDefault = StateKeys Nothing Nothing Nothing Nothing Nothing 

data WorkflowEnvironment = WorkflowEnvironment {
    contract :: Maybe (StateKeys (Id' "contracts")(Id' "contract_states")),
    partner:: Maybe (StateKeys (Id' "partners")(Id' "partner_states")) , 
    tariff :: Maybe (StateKeys (Id' "tariffs")(Id' "tariff_states")),
    adress :: Maybe (StateKeys (Id' "adresses")(Id' "adress_states")),
    contractPartner:: Maybe (StateKeys (Id' "contract_partners")(Id' "contract_partner_states")) ,
    contractTariff:: Maybe (StateKeys (Id' "contract_tariffs")(Id' "contract_tariff_states")) ,
    tariffPartner:: Maybe (StateKeys (Id' "tariff_partners")(Id' "tariff_partner_states")) , 
    partnerAdress:: Maybe (StateKeys (Id' "partner_adresses")(Id' "partner_adress_states")) , 
    plog :: [PersistenceLog]
    } deriving (Show, Generic)

workflowEnvironmentDefault = WorkflowEnvironment (Just stateKeysDefault) (Just stateKeysDefault) (Just stateKeysDefault)
    (Just stateKeysDefault) (Just stateKeysDefault) (Just stateKeysDefault) (Just stateKeysDefault) (Just stateKeysDefault) [] 

instance FromJSON WorkflowEnvironment
instance ToJSON WorkflowEnvironment
instance FromJSON (StateKeys  (Id' "adresses")(Id' "adress_states"))
instance ToJSON (StateKeys  (Id' "adresses")(Id' "adress_states"))
instance FromJSON (StateKeys  (Id' "contracts")(Id' "contract_states"))
instance ToJSON (StateKeys  (Id' "contracts")(Id' "contract_states"))
instance FromJSON (StateKeys  (Id' "contract_partners")(Id' "contract_partner_states"))
instance ToJSON (StateKeys  (Id' "contract_partners")(Id' "contract_partner_states"))
instance FromJSON (StateKeys  (Id' "contract_tariffs")(Id' "contract_tariff_states"))
instance ToJSON (StateKeys  (Id' "contract_tariffs")(Id' "contract_tariff_states"))
instance FromJSON (StateKeys  (Id' "partners")(Id' "partner_states"))
instance ToJSON (StateKeys  (Id' "partners")(Id' "partner_states"))
instance FromJSON (StateKeys  (Id' "partner_adresses")(Id' "partner_adress_states"))
instance ToJSON (StateKeys   (Id' "partner_adresses")(Id' "partner_adress_states"))
instance FromJSON (StateKeys  (Id' "tariffs")(Id' "tariff_states"))
instance ToJSON (StateKeys  (Id' "tariffs")(Id' "tariff_states"))
instance FromJSON (StateKeys  (Id' "tariff_partners")(Id' "tariff_partner_states"))
instance ToJSON (StateKeys  (Id' "tariff_partners")(Id' "tariff_partner_states"))

data PersistenceLog =
    WorkflowPL (CRULog (Id Workflow)) | HistoryPL (CRULog (Id History)) | VersionPL (CRULog (Id Version)) | ContractPL (CRULog (Id Contract)) | ContractStatePL (CRULog (Id ContractState)) | 
    PartnerPL (CRULog (Id Partner))| PartnerStatePL (CRULog (Id PartnerState))| ContractPartnerPL (CRULog (Id ContractPartner)) | ContractPartnerStatePL (CRULog (Id ContractPartnerState)) |
    ContractTariffPL (CRULog (Id ContractTariff)) | ContractTariffStatePL (CRULog (Id ContractTariffState)) |
    TariffPL (CRULog (Id Tariff)) | TariffStatePL (CRULog (Id TariffState)) |
    TariffPartnerPL (CRULog (Id TariffPartner)) | TariffPartnerStatePL (CRULog (Id TariffPartnerState)) | 
    AdressPL (CRULog (Id Adress)) | AdressStatePL (CRULog (Id AdressState)) |
    PartnerAdressPL (CRULog (Id PartnerAdress)) | PartnerAdressStatePL (CRULog (Id PartnerAdressState)) 
    deriving (Generic, Show)

instance ToJSON PersistenceLog
instance FromJSON PersistenceLog

instance ToJSON (Id' "workflows") where
    toJSON (Id key) = object ["workflowId" .= key]
instance FromJSON (Id' "workflows") where
    parseJSON = withObject "workflowId" $ \o ->
        Id <$> o .: "workflowId"
instance ToJSON (Id' "histories") where
    toJSON (Id key) = object ["historyId" .= key]
instance FromJSON (Id' "histories") where
    parseJSON = withObject "historyId" $ \o ->
        Id <$> o .: "historyId"
instance ToJSON (Id' "versions") where
    toJSON (Id key) = object ["versionId" .= key]
instance FromJSON (Id' "versions") where
    parseJSON = withObject "versionId" $ \o ->
        Id <$> o .: "versionId"
instance ToJSON (Id' "contracts") where
    toJSON (Id key) = object ["contractId" .= key]
instance FromJSON (Id' "contracts") where
    parseJSON = withObject "contractId" $ \o ->
        Id <$> o .: "contractId"
instance ToJSON (Id' "contract_states") where
    toJSON (Id key) = object ["contractStateId" .= key]
instance FromJSON (Id' "contract_states") where
    parseJSON = withObject "contractStateId" $ \o ->
        Id <$> o .: "contractStateId"
instance ToJSON (Id' "adresses") where
    toJSON (Id key) = object ["adressId" .= key]
instance FromJSON (Id' "adresses") where
    parseJSON = withObject "adressId" $ \o ->
        Id <$> o .: "adressId"
instance ToJSON (Id' "partners") where
    toJSON (Id key) = object ["partnerId" .= key]
instance FromJSON (Id' "partners") where
    parseJSON = withObject "partnerId" $ \o ->
        Id <$> o .: "partnerId"
instance ToJSON (Id' "partner_states") where
    toJSON (Id key) = object ["partnerStateId" .= key]
instance FromJSON (Id' "partner_states") where
    parseJSON = withObject "partnerStateId" $ \o ->
        Id <$> o .: "partnerStateId"
instance ToJSON (Id' "adress_states") where
    toJSON (Id key) = object ["adressStateId" .= key]
instance FromJSON (Id' "adress_states") where
    parseJSON = withObject "adressStateId" $ \o ->
        Id <$> o .: "adressStateId"
instance ToJSON (Id' "tariffs") where
    toJSON (Id key) = object ["tariffId" .= key]
instance FromJSON (Id' "tariffs") where
    parseJSON = withObject "tariffId" $ \o ->
        Id <$> o .: "tariffId"
instance ToJSON (Id' "tariff_states") where
    toJSON (Id key) = object ["tariffStateId" .= key]
instance FromJSON (Id' "tariff_states") where
    parseJSON = withObject "tariffStateId" $ \o ->
        Id <$> o .: "tariffStateId"

instance ToJSON (Id' "contract_partners") where
    toJSON (Id key) = object ["contractPartnerId" .= key]
instance FromJSON (Id' "contract_partners") where
    parseJSON = withObject "contractPartnerId" $ \o ->
        Id <$> o .: "contractPartnerId"
instance ToJSON (Id' "contract_partner_states") where
    toJSON (Id key) = object ["contractPartnerStateId" .= key]
instance FromJSON (Id' "contract_partner_states") where
    parseJSON = withObject "contractPartnerStateId" $ \o ->
        Id <$> o .: "contractPartnerStateId"

instance ToJSON (Id' "contract_tariffs") where
    toJSON (Id key) = object ["contractTariffId" .= key]
instance FromJSON (Id' "contract_tariffs") where
    parseJSON = withObject "contractTariffId" $ \o ->
        Id <$> o .: "contractTariffId"
instance ToJSON (Id' "contract_tariff_states") where
    toJSON (Id key) = object ["contractTariffStateId" .= key]
instance FromJSON (Id' "contract_tariff_states") where
    parseJSON = withObject "contractTariffStateId" $ \o ->
        Id <$> o .: "contractTariffStateId"

instance ToJSON (Id' "tariff_partners") where
    toJSON (Id key) = object ["tariffPartnerId" .= key]
instance FromJSON (Id' "tariff_partners") where
    parseJSON = withObject "tariffPartnerId" $ \o ->
        Id <$> o .: "tariffPartnerId"
instance ToJSON (Id' "tariff_partner_states") where
    toJSON (Id key) = object ["tariffPartnerStateId" .= key]
instance FromJSON (Id' "tariff_partner_states") where
    parseJSON = withObject "tariffPartnerStateId" $ \o ->
        Id <$> o .: "tariffPartnerStateId"

instance ToJSON (Id' "partner_adresses") where
    toJSON (Id key) = object ["partnerAdressId" .= key]
instance FromJSON (Id' "partner_adresses") where
    parseJSON = withObject "partnerAdressId" $ \o ->
        Id <$> o .: "partnerAdressId"
instance ToJSON (Id' "partner_adress_states") where
    toJSON (Id key) = object ["partnerAdressStateId" .= key]
instance FromJSON (Id' "partner_adress_states") where
    parseJSON = withObject "partnerAdressStateId" $ \o ->
        Id <$> o .: "partnerAdressStateId"
data CRULog a = Inserted  { key::a }| Updated { old::a , new :: a } deriving (Generic, Show,Read)
instance (ToJSON a) => ToJSON (CRULog a)
instance (FromJSON a) => FromJSON (CRULog a)

class (KnownSymbol (GetTableName rec), rec ~ GetModelByTableName (GetTableName rec), Record rec, FilterPrimaryKey (GetTableName rec),CanCreate rec,Fetchable (QueryBuilder (GetTableName rec))  rec, FromRow rec,
    HasField "id" rec (Id rec), Show rec, Show (PrimaryKey (GetTableName rec)) , ToJSON (PrimaryKey (GetTableName rec))) => HasTxnLog rec 
    where
    mkInsertLog :: Id rec -> CRULog (Id rec)
    mkInsertLog id = Inserted id
    mkPersistenceLogState :: CRULog (Id rec) -> PersistenceLog
    mkUpdateLog :: rec -> rec -> CRULog (Id rec)
    mkUpdateLog old new = Updated (get #id old) (get #id new)
    commit ::  (?modelContext::ModelContext, ?context::context, LoggingProvider context )  => CRULog (Id rec) -> IO()
    commit cruLog = do
        case cruLog of
            Inserted key -> do
                rec <- fetch key
                Log.info $ "huhu INSERTED " ++ show rec
            Updated old new -> do
                recOld <- fetch old
                Log.info $ "huhu OLD " ++ show recOld
                recNew <- fetch old
                Log.info $ "huhu OLD " ++ show recNew

instance HasTxnLog Workflow where
    mkPersistenceLogState :: CRULog (Id Workflow) -> PersistenceLog
    mkPersistenceLogState cru = WorkflowPL cru
instance HasTxnLog History where
    mkPersistenceLogState :: CRULog (Id History) -> PersistenceLog
    mkPersistenceLogState cru = HistoryPL cru
instance HasTxnLog Version where
    mkPersistenceLogState :: CRULog (Id Version) -> PersistenceLog
    mkPersistenceLogState cru = VersionPL cru
instance HasTxnLog Contract where
    mkPersistenceLogState :: CRULog (Id Contract) -> PersistenceLog
    mkPersistenceLogState cru = ContractPL cru
instance HasTxnLog ContractState where
    mkPersistenceLogState :: CRULog (Id ContractState) -> PersistenceLog
    mkPersistenceLogState cru = ContractStatePL cru
instance HasTxnLog Partner where
    mkPersistenceLogState :: CRULog (Id Partner) -> PersistenceLog
    mkPersistenceLogState cru = PartnerPL cru
instance HasTxnLog PartnerState where
    mkPersistenceLogState :: CRULog (Id PartnerState) -> PersistenceLog
    mkPersistenceLogState cru = PartnerStatePL cru
instance HasTxnLog Tariff where 
    mkPersistenceLogState :: CRULog (Id Tariff) -> PersistenceLog
    mkPersistenceLogState cru = TariffPL cru
instance HasTxnLog TariffState where 
    mkPersistenceLogState :: CRULog (Id TariffState) -> PersistenceLog
    mkPersistenceLogState cru = TariffStatePL cru
instance HasTxnLog Adress where 
    mkPersistenceLogState :: CRULog (Id Adress) -> PersistenceLog
    mkPersistenceLogState cru = AdressPL cru
instance HasTxnLog AdressState where 
    mkPersistenceLogState :: CRULog (Id AdressState) -> PersistenceLog
    mkPersistenceLogState cru = AdressStatePL cru
instance HasTxnLog ContractPartner where 
    mkPersistenceLogState :: CRULog (Id ContractPartner) -> PersistenceLog
    mkPersistenceLogState cru = ContractPartnerPL cru
instance HasTxnLog ContractPartnerState where 
    mkPersistenceLogState :: CRULog (Id ContractPartnerState) -> PersistenceLog
    mkPersistenceLogState cru = ContractPartnerStatePL cru
instance HasTxnLog ContractTariff where 
    mkPersistenceLogState :: CRULog (Id ContractTariff) -> PersistenceLog
    mkPersistenceLogState cru = ContractTariffPL cru
instance HasTxnLog ContractTariffState where 
    mkPersistenceLogState :: CRULog (Id ContractTariffState) -> PersistenceLog
    mkPersistenceLogState cru = ContractTariffStatePL cru
instance HasTxnLog TariffPartner where 
    mkPersistenceLogState :: CRULog (Id TariffPartner) -> PersistenceLog
    mkPersistenceLogState cru = TariffPartnerPL cru
instance HasTxnLog TariffPartnerState where 
    mkPersistenceLogState :: CRULog (Id TariffPartnerState) -> PersistenceLog
    mkPersistenceLogState cru = TariffPartnerStatePL cru
instance HasTxnLog PartnerAdress where 
    mkPersistenceLogState :: CRULog (Id PartnerAdress) -> PersistenceLog
    mkPersistenceLogState cru = PartnerAdressPL cru
instance HasTxnLog PartnerAdressState where 
    mkPersistenceLogState :: CRULog (Id PartnerAdressState) -> PersistenceLog
    mkPersistenceLogState cru = PartnerAdressStatePL cru

