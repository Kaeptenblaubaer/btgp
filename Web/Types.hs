module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data ContractStatesController
    = ContractStatesAction
    | NewContractStateAction
    | ShowContractStateAction { contractStateId :: !(Id ContractState) }
    | CreateContractStateAction
    | EditContractStateAction { contractStateId :: !(Id ContractState) }
    | UpdateContractStateAction { contractStateId :: !(Id ContractState) }
    | DeleteContractStateAction { contractStateId :: !(Id ContractState) }
    | UpdateContractStatePartnerStateAction { contractStateId :: !(Id ContractState)}
    deriving (Eq, Show, Data)

data WorkflowsController
    = WorkflowsAction
    | NewWorkflowAction
    | ShowWorkflowAction { workflowId :: !(Id Workflow) }
    | CreateWorkflowAction
    | EditWorkflowAction { workflowId :: !(Id Workflow) }
    | UpdateWorkflowAction { workflowId :: !(Id Workflow) }
    | DeleteWorkflowAction { workflowId :: !(Id Workflow) }
    | NextWorkflowAction 
    deriving (Eq, Show, Data)

data PartnersController
    = PartnersAction
    | NewPartnerAction
    | ShowPartnerAction { partnerId :: !(Id Partner) }
    | CreatePartnerAction
    | EditPartnerAction { partnerId :: !(Id Partner) }
    | UpdatePartnerAction { partnerId :: !(Id Partner) }
    | DeletePartnerAction { partnerId :: !(Id Partner) }
    deriving (Eq, Show, Data)

data PartnerStatesController
    = PartnerStatesAction
    | SelectPartnerStateAction 
    | NewPartnerStateAction
    | ShowPartnerStateAction { partnerStateId :: !(Id PartnerState) }
    | CreatePartnerStateAction
    | EditPartnerStateAction { partnerStateId :: !(Id PartnerState) }
    | UpdatePartnerStateAction { partnerStateId :: !(Id PartnerState) }
    | DeletePartnerStateAction { partnerStateId :: !(Id PartnerState) }
    deriving (Eq, Show, Data)

data HistoriesController
    = HistoriesAction
    | NewHistoryAction
    | ShowHistoryAction { historyId :: !(Id History) }
    | CreateHistoryAction
    | EditHistoryAction { historyId :: !(Id History) }
    | UpdateHistoryAction { historyId :: !(Id History) }
    | DeleteHistoryAction { historyId :: !(Id History) }
    deriving (Eq, Show, Data)

data TariffStatesController
    = TariffStatesAction
    | NewTariffStateAction
    | ShowTariffStateAction { tariffStateId :: !(Id TariffState) }
    | CreateTariffStateAction
    | EditTariffStateAction { tariffStateId :: !(Id TariffState) }
    | UpdateTariffStateAction { tariffStateId :: !(Id TariffState) }
    | DeleteTariffStateAction { tariffStateId :: !(Id TariffState) }
    deriving (Eq, Show, Data)

data AdressStatesController
    = AdressStatesAction
    | NewAdressStateAction
    | ShowAdressStateAction { adressStateId :: !(Id AdressState) }
    | CreateAdressStateAction
    | EditAdressStateAction { adressStateId :: !(Id AdressState) }
    | UpdateAdressStateAction { adressStateId :: !(Id AdressState) }
    | DeleteAdressStateAction { adressStateId :: !(Id AdressState) }
    deriving (Eq, Show, Data)
