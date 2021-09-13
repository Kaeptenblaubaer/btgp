#!/usr/bin/env run-script
module Application.Script.TestDB where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}

import Application.Helper.CanVersion
import Application.Helper.WorkflowProgress
import Application.Script.Prelude
import IHP.Log as Log


run :: Script
run = do
    cps :: ContractPartnerState <- query @ContractPartnerState |> fetchOne
    Log.info $ "Fetch CPS " ++ show cps
    let cId :: (Id ContractState) = get #refContract cps
    c <- fetch cId
    cagg :: Include "contractPartnerStates" ContractState <- fetch cId >>= fetchRelated #contractPartnerStates
    let bubu = get #id cagg 
        baba = get #contractPartnerStates cagg 
    Log.info $ "Fetch c " ++ show c
    Log.info $ "Fetch CAGG " ++ show cagg