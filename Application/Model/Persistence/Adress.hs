module Application.Model.Persistence.Adress where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}

import Application.Script.Prelude
import Data.Maybe
import Application.Model.CanVersion 
import Application.Model.WorkflowEnvironment
import Generated.Types

instance CanVersion Adress AdressState where
    getAccessor :: (WorkflowEnvironment ->Maybe (StateKeys (Id'"adresses")(Id' "adress_states")))
    getAccessor = adress
    setShadowed :: (WorkflowEnvironment ->  Maybe (StateKeys (Id'"adresses")(Id' "adress_states"))) -> WorkflowEnvironment -> (Id Version,[Id Version]) -> WorkflowEnvironment
    setShadowed accessor wfe shadow = let new :: StateKeys (Id'"adresses")(Id' "adress_states") = fromJust $ accessor wfe 
        in wfe {adress = Just $ new { shadowed = Just shadow }}
    setWorkFlowState :: WorkflowEnvironment ->Maybe (StateKeys (Id'"adresses")(Id' "adress_states")) -> WorkflowEnvironment
    setWorkFlowState wfe s = wfe  {adress = s} 
