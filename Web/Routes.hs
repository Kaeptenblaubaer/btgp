module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute ContractStatesController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id ContractState))

instance AutoRoute WorkflowsController

instance AutoRoute PartnerStatesController where
    autoRoute = autoRouteWithIdType (parseIntegerId @(Id PartnerState))
