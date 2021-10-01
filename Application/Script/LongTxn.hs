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

    ask@(adressState,adressKeys)::(AdressState, StateKeys (Id Adress)(Id AdressState)) <- createHistory adress wfa a0
    Log.info $ show $ snd ask
    result <- fetch (get #id wfa) >>= commitState adress
    Log.info $ show result

    psk@(partnerState,partnerKeys)::(PartnerState, StateKeys (Id Partner)(Id PartnerState)) <- createHistory partner wfp p0
    wfp <- fetch (get #id wfp)
    putRelState partnerAdress (get #id partnerState) (get #id adressState) wfp
    Log.info $ show $ snd psk
    result <- fetch (get #id wfp) >>= commitState partner
    Log.info $ show result

    tsk@(tariffState,tariffKeys)::(TariffState, StateKeys (Id Tariff)(Id TariffState)) <- createHistory tariff wft t0
    wft <- fetch (get #id wft)
    Log.info $ show $ snd tsk
    putRelState tariffPartner (get #id tariffState) (get #id partnerState) wft
    result <- fetch (get #id wft) >>= commitState tariff
    Log.info $ show result
    
    csk@(contractState,contractKeys)::(ContractState, StateKeys (Id Contract)(Id ContractState)) <- createHistory contract wfc c0
    wfc <- fetch (get #id wfc)
    Log.info $ show $ snd csk
    Log.info $ show result
    putRelState contractPartner (get #id contractState) (get #id partnerState) wfc
    putRelState contractTariff (get #id contractState) (get #id tariffState) wfc
    result <- fetch (get #id wfc) >>= commitState contract
    Log.info $ show result

    let validfrom1 :: Day = fromGregorian 2021 7 1
 
    workflowCM <- runMutation contract usr HistorytypeContract (fst csk) validfrom1 "1st mutatated ContractState"
    commitState contract workflowCM
    case result of
        Left msg -> Log.info $ "SUCCESS 1CM:"++ msg
        Right msg -> Log.info $ "ERROR 1CM:" ++ msg
--
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
    let validfrom2 :: Day = fromGregorian 2021 5 1
 
    workflowCM <- runMutation contract usr HistorytypeContract (fst csk) validfrom2 "2nd mutatated ContractState"
    commitState contract workflowCM
    case result of
        Left msg -> Log.info $ "SUCCESS 2CM:"++ msg
        Right msg -> Log.info $ "ERROR 2CM :" ++ msg
--
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