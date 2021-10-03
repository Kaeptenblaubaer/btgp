module Application.Model.Persistence.Contract where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Application.Script.Prelude
import Application.Model.CanVersion
import Application.Model.WorkflowEnvironment
import Generated.Types
import Application.Model.Persistence.Partner
import Application.Model.Persistence.Tariff

instance CanVersion Contract ContractState where
    getAccessor :: (WorkflowEnvironment -> Maybe (StateKeys (Id'"contracts")(Id' "contract_states")))
    getAccessor = contract
    setShadowed :: (WorkflowEnvironment ->  Maybe (StateKeys (Id'"contracts")(Id' "contract_states"))) -> WorkflowEnvironment -> (Integer,[Integer]) -> WorkflowEnvironment
    setShadowed accessor wfe shadow = let new :: StateKeys (Id'"contracts")(Id' "contract_states") = fromJust $ accessor wfe 
        in wfe {contract = Just $ new { shadowed = Just shadow }}
    setWorkFlowState :: WorkflowEnvironment -> Maybe (StateKeys (Id'"contracts")(Id' "contract_states")) -> WorkflowEnvironment
    setWorkFlowState wfe s = wfe  {contract = s} 

instance CanVersion ContractPartner ContractPartnerState where
    getAccessor :: (WorkflowEnvironment ->Maybe (StateKeys (Id'"contract_partners")(Id' "contract_partner_states")))
    getAccessor = contractPartner

instance CanVersion ContractTariff ContractTariffState where
    getAccessor :: (WorkflowEnvironment ->Maybe (StateKeys (Id'"contract_tariffs")(Id' "contract_tariff_states")))
    getAccessor = contractTariff

instance CanVersionRelation Contract ContractState Partner PartnerState ContractPartner ContractPartnerState
instance CanVersionRelation Contract ContractState Tariff TariffState ContractTariff ContractTariffState
