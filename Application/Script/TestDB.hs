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
    let cId :: (Id ContractState) = get #refSource cps
    c <- fetch cId
    let pId :: (Id PartnerState) = get #refTarget cps
    p <- fetch pId
    bubu :: [PersistenceLog] <- putRelState contractPartner cId pId []
    Log.info $ "fetched cs / ps=" ++ show cId ++ "/" ++ show pId ++ show bubu
    cagg :: Include "contractPartnerStates" ContractState <- fetch cId >>= fetchRelated #contractPartnerStates
    let bubu = get #id cagg 
        baba :: [ContractPartnerState] = get #contractPartnerStates cagg
        bobo = head baba
    Log.info $ "Fetch c " ++ show c
    Log.info $ "Fetch CAGG " ++ show cagg ++ "length baba=" ++ show (length baba) 
    Log.info $ "Fetch CAGG " ++ show bobo
    forEach baba (\cps -> do
        Log.info $ "CPS-Loop " ++ show cps
        ps <- fetch (get #refTarget cps) 
        Log.info $ "PartnerState " ++ show (get #content ps)
        )