module Application.Model.Persistence.Partner where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Application.Script.Prelude
import Application.Model.CanVersion
import Application.Model.WorkflowEnvironment
import Generated.Types
import Application.Model.Persistence.Adress

instance CanVersion Partner PartnerState where
    getAccessor :: (WorkflowEnvironment ->Maybe (StateKeys (Id'"partners")(Id' "partner_states")))
    getAccessor = partner
    setShadowed :: (WorkflowEnvironment ->  Maybe (StateKeys (Id'"partners")(Id' "partner_states"))) -> WorkflowEnvironment -> (Id Version,[Id Version]) -> WorkflowEnvironment
    setShadowed accessor wfe shadow = let new :: StateKeys (Id'"partners")(Id' "partner_states") = fromJust $ accessor wfe 
        in wfe {partner = Just $ new { shadowed = Just shadow }}
    setWorkFlowState :: WorkflowEnvironment ->Maybe (StateKeys (Id'"partners")(Id' "partner_states")) -> WorkflowEnvironment
    setWorkFlowState wfe s = wfe  {partner = s} 

instance CanVersion PartnerAdress PartnerAdressState where
    getAccessor :: (WorkflowEnvironment ->Maybe (StateKeys (Id'"partner_adresses")(Id' "partner_adress_states")))
    getAccessor = partnerAdress

instance CanVersionRelation Partner PartnerState Adress AdressState PartnerAdress PartnerAdressState
