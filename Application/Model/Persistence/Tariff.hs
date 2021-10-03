module Application.Model.Persistence.Tariff where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Application.Script.Prelude
import Application.Model.CanVersion
import Application.Model.WorkflowEnvironment
import Generated.Types
import Application.Model.Persistence.Partner

instance CanVersion Tariff TariffState where
    getAccessor :: (WorkflowEnvironment ->Maybe (StateKeys (Id'"tariffs")(Id' "tariff_states")))
    getAccessor = tariff
    setShadowed :: (WorkflowEnvironment ->  Maybe (StateKeys (Id'"tariffs")(Id' "tariff_states"))) -> WorkflowEnvironment -> (Integer,[Integer]) -> WorkflowEnvironment
    setShadowed accessor wfe shadow = let new :: StateKeys (Id'"tariffs")(Id' "tariff_states") = fromJust $ accessor wfe 
        in wfe {tariff = Just $ new { shadowed = Just shadow }}
    setWorkFlowState :: WorkflowEnvironment ->Maybe (StateKeys (Id'"tariffs")(Id' "tariff_states")) -> WorkflowEnvironment
    setWorkFlowState wfe s = wfe  {tariff = s} 


instance CanVersion TariffPartner TariffPartnerState where
    getAccessor :: (WorkflowEnvironment ->Maybe (StateKeys (Id'"tariff_partners")(Id' "tariff_partner_states")))
    getAccessor = tariffPartner

instance CanVersionRelation Tariff TariffState Partner PartnerState TariffPartner TariffPartnerState
