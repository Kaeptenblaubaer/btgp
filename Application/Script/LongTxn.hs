#!/usr/bin/env run-script
module Application.Script.LongTxn where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}

import Application.Script.Prelude
import IHP.Log as Log
import Data.Maybe
import IHP.Pagination.Types as PT
import Application.Helper.Controller

run :: Script
run = do
    usr :: User <- query @User |> fetchOne 
    let validfrom0 :: Day = fromGregorian 2020 12 1
    wfc ::Workflow <- createCreationWorkflow contract usr HistorytypeContract validfrom0
    wfp ::Workflow <- createCreationWorkflow partner usr HistorytypePartner validfrom0
    wft ::Workflow <- createCreationWorkflow tariff usr HistorytypeTariff validfrom0
    wfa ::Workflow <- createCreationWorkflow adress usr HistorytypeAdress validfrom0
    let c0 :: ContractState = newRecord |> set #content "First contract"
        p0 :: PartnerState = newRecord |> set #content "FIRST PARTNER"
        t0 :: TariffState = newRecord |> set #content "First Tariff"
        a0 :: AdressState = newRecord |> set #content "First Adress"

-- Create an Adress
    ask@(adressState,adressKeys,pLogA)::(AdressState, StateKeys (Id Adress)(Id AdressState),[PersistenceLog]) <- createHistory (get #id wfa) HistorytypeAdress validfrom0 a0
    Log.info $ "create History adress " ++ show adressKeys
    let wfenvA = workflowEnvironmentDefault {adress=Just adressKeys, plog = pLogA}
    wfa :: Workflow <- wfa |> set #progress (toJSON wfenvA )|> updateRecord 
    result <- fetch (get #id wfa) >>= commitState adressKeys
    Log.info $ "adress " ++ show result

-- Create a partner and attach the pdress
    psk@(partnerState,partnerKeys,pLogP)::(PartnerState, StateKeys (Id Partner)(Id PartnerState),[PersistenceLog]) <- createHistory (get #id wfp) HistorytypePartner validfrom0 p0
    Log.info $ "create History partner " ++ show partnerKeys
    let wfenvP = workflowEnvironmentDefault {partner=Just partnerKeys, plog = pLogP}
    wfp <- wfp |> set #progress (toJSON wfenvP) |> updateRecord  
    wfp <- fetch (get #id wfp)

-- Attach the adress
    (partnerAdressState,partnerAdressKeys,pLogPA):: (PartnerAdressState,StateKeys (Id PartnerAdress) (Id PartnerAdressState),[PersistenceLog]) <- putRelState (get #id partnerState) (get #id adressState) 
    Log.info $ "putrelstates partnerAdress " ++ show partnerAdressKeys
    let wfenvPA = wfenvP {partnerAdress=Just(partnerAdressKeys), plog = pLogP ++ pLogPA}
    Log.info $ "wfenv partneraDRESS = " ++ show wfenvPA
    wfp <- wfp |> set #progress (toJSON wfenvPA) |> updateRecord
    result <- fetch (get #id wfp) >>= commitState partnerKeys
    Log.info $ "partner + partneradress " ++ show result

-- Create a tariff and attach the partner

    tsk@(tariffState,tariffKeys,pLogT)::(TariffState, StateKeys (Id Tariff)(Id TariffState),[PersistenceLog]) <- createHistory (get #id wft) HistorytypeTariff validfrom0 t0
    Log.info $ "create History tariff " ++ show tariffKeys
    let wfenvT = workflowEnvironmentDefault {tariff=Just tariffKeys, plog = pLogT}
    wft <- wft |> set #progress (toJSON wfenvT) |> updateRecord  
    wft <- fetch (get #id wft)
-- Attach the partner    
    (tariffPartnerState,tariffPartnerKeys,pLogTP):: (TariffPartnerState,StateKeys (Id TariffPartner) (Id TariffPartnerState),[PersistenceLog]) <- putRelState (get #id tariffState) (get #id partnerState) 
    Log.info $ "putrelstates tariffpartner " ++ show tariffPartnerKeys
    let wfenvTP = wfenvT  {tariffPartner=Just(tariffPartnerKeys), plog = pLogT ++ pLogTP}
    wft <- wft |> set #progress (toJSON wfenvTP) |> updateRecord  
    result <- fetch (get #id wft) >>= commitState tariffKeys
    Log.info $ show "tariff + tariffpartner " ++ show result

-- Create a contract and attach the partner and the tariff
    csk@(contractState,contractKeys,pLogC)::(ContractState, StateKeys (Id Contract)(Id ContractState),[PersistenceLog]) <- createHistory (get #id wft) HistorytypeContract validfrom0 c0
    Log.info $ "create History contract " ++ show contractKeys
    let wfenvC = workflowEnvironmentDefault {contract=Just contractKeys, plog = pLogC}
    wfc <- wfc |> set #progress (toJSON wfenvC) |> updateRecord  
    wfc <- fetch (get #id wfc)
-- attach the partner    
    (contractPartnerState,contractPartnerKeys,pLogCP):: (ContractPartnerState,StateKeys (Id ContractPartner) (Id ContractPartnerState),[PersistenceLog]) <- putRelState (get #id contractState) (get #id partnerState) 
    Log.info $ "putrelstates contractpartner " ++ show contractPartnerKeys
    let wfenvCP = wfenvC  {contractPartner=Just(contractPartnerKeys), plog = pLogC ++ pLogCP}
    wfc <- wfc |> set #progress (toJSON wfenvCP) |> updateRecord 
    Log.info $ show "contract + contractpartner " ++ show result
-- attach the tariff
    (contractTariffState,contractTariffKeys,pLogCT):: (ContractTariffState,StateKeys (Id ContractTariff) (Id ContractTariffState),[PersistenceLog]) <- putRelState (get #id contractState) (get #id tariffState) 
    Log.info $ "putrelstates contracttariff " ++ show contractTariffKeys
    let wfenvCT = wfenvC  {contractTariff=Just(contractTariffKeys), plog = pLogC ++ pLogCT}
    wfc <- wfc |> set #progress (toJSON wfenvCT) |> updateRecord  
    result <- fetch (get #id wfc) >>= commitState contractKeys
    Log.info $ show "contract + contracttariff " ++ show result


--
--    let validfrom1 :: Day = fromGregorian 2021 7 1
-- 
--    workflowCM <- runMutation contract usr HistorytypeContract (fst csk) validfrom1 "1st mutatated ContractState"
--    commitState contract workflowCM
--    case result of
--        Left msg -> Log.info $ "SUCCESS 1CM:"++ msg
--        Right msg -> Log.info $ "ERROR 1CM:" ++ msg
----
--    workflowPM <- runMutation partner usr HistorytypePartner (fst psk) validfrom1  "1st mutatated PartnerState"
--    commitState partner workflowPM
--    case result of
--        Left msg -> Log.info $ "SUCCESS:"++ msg
--        Right msg -> Log.info $ "ERROR:" ++ msg
--
--    workflowTM <- runMutation tariff usr HistorytypeTariff (fst tsk) validfrom1 "1st mutatated TariffState"
--    commitState tariff workflowTM
--    case result of
--        Left msg -> Log.info $ "SUCCESS:"++ msg
--        Right msg -> Log.info $ "ERROR:" ++ msg
--
--
--    let validfrom2 :: Day = fromGregorian 2021 5 1
-- 
--    workflowCM <- runMutation contract usr HistorytypeContract (fst csk) validfrom2 "2nd mutatated ContractState"
--    commitState contract workflowCM
--    case result of
--        Left msg -> Log.info $ "SUCCESS 2CM:"++ msg
--        Right msg -> Log.info $ "ERROR 2CM :" ++ msg
----
--
----
----    forEach (persistenceLogC ++ persistenceLogP ++ persistenceLogT) \pl -> do
----        Log.info $ "Logged plog:" ++ show pl
----        case pl of
----            WorkflowPL cru -> commit cru
----            HistoryPL cru -> commit cru
----            VersionPL cru -> commit cru
----            ContractPL cru -> commitState cru
----            PartnerPL cru -> commit cru
----            TariffPL cru -> commit cru
----        Log.info ("Logged nach Commit" :: String)
----
--    today <- today
--    now <- getCurrentTime
--    countP <- countStatesByValidFromMaxTxn HistorytypePartner today now
--    let o :: PT.Options = defaultPaginationOptions
--        p :: PT.Pagination = Pagination
--                {
--                    currentPage = 1
--                ,   totalItems = countP
--                ,   pageSize = 3
--                ,   window = windowSize o
--                }
--        showPage = \pag -> Log.info $ "Page " ++ show (currentPage pag)
--    pStates :: ([PartnerState],pagination) <- selectStatesByValidFromMaxTxn HistorytypePartner today now o p
--    showPage $ snd pStates
--    forEach pStates \s -> do
--        Log.info $ "pState= " ++ show s
--
--    pStates :: ([PartnerState],pagination) <- selectStatesByValidFromMaxTxn HistorytypePartner today now o $ snd pStates
--    showPage $ snd pStates
--    forEach pStates \s -> do
--        Log.info $ "pState= " ++ show s
--
--    pStates :: ([PartnerState],pagination) <- selectStatesByValidFromMaxTxn HistorytypePartner today now o $ snd pStates
--    showPage $ snd pStates
--    forEach pStates \s -> do
--        Log.info $ "pState= " ++ show s