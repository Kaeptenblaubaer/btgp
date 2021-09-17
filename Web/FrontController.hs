module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Histories
import Web.Controller.PartnerStates
import Web.Controller.Workflows
import Web.Controller.ContractStates
import Web.Controller.Static

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        -- Generator Marker
        , parseRoute @HistoriesController
        , parseRoute @PartnerStatesController
        , parseRoute @WorkflowsController
        , parseRoute @ContractStatesController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
