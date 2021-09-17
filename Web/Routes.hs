module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute ContractStatesController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id ContractState))
    allowedMethodsForAction actionName =
            case actionName of
                "UpdateContractStatePartnerStateAction" -> [GET]
                _ -> [GET, POST, HEAD]

instance AutoRoute WorkflowsController where
    allowedMethodsForAction actionName =
            case actionName of
                "NextWorkflowAction" -> [GET, POST]
                _ -> [GET, POST, HEAD]

instance AutoRoute PartnerStatesController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id PartnerState))

instance AutoRoute HistoriesController

