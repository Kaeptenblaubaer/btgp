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
    wenv ::Workflow <- createCreationWorkflow partner usr HistorytypePartner validfrom0
    wft ::Workflow <- createCreationWorkflow tariff usr HistorytypeTariff validfrom0
    let c0 :: ContractState = newRecord |> set #content "initial"
        p0 :: PartnerState = newRecord |> set #content "FIRST PARTNER"
        t0 :: TariffState = newRecord |> set #content "initial"
    csk@(contractState,contractKeys)::(ContractState, StateKeys (Id Contract)(Id ContractState)) <- createHistory contract wfc c0
    Log.info $ show $ snd csk
    result <- fetch (get #id wfc) >>= (\s -> commitState contract s)
    Log.info $ show result
    psk@(partnerState,partnerKeys)::(PartnerState, StateKeys (Id Partner)(Id PartnerState)) <- createHistory partner wenv p0
    Log.info $ show $ snd psk
    result <- fetch (get #id wenv) >>= (\s -> commitState partner s)
    Log.info $ show result
    tsk@(tariffState,tariffKeys)::(TariffState, StateKeys (Id Tariff)(Id TariffState)) <- createHistory tariff wft t0
    Log.info $ show $ snd csk
    result <- fetch (get #id wft) >>= (\s -> commitState tariff s)
    Log.info $ show result
    
    let validfrom1 :: Day = fromGregorian 2021 7 1
    workflowCM <- runMutation contract usr HistorytypeContract (fst csk) validfrom1 "1st mutatated ContractState"
    newContractPartner :: ContractPartner <- newRecord |> set #refHistory (Id (fromJust (history contractKeys))) |> createRecord
    newContractPartnerState :: ContractPartnerState <- newRecord |> set #refEntity (get #id newContractPartner) |> 
        set #refSource (get #id contractState) |> set #refTarget (get #id partnerState) |>
           set #refValidfromversion (get #refValidfromversion contractState) |> set #refValidthruversion Nothing |> createRecord
    cpsPLog <- putRelState contractPartner (get #id contractState) (get #id partnerState) (getPLog workflowCM)
    newContractTariff :: ContractTariff <- newRecord |> set #refHistory (Id (fromJust (history contractKeys))) |> createRecord
    newContractTariffState :: ContractTariffState <- newRecord |> set #refEntity (get #id newContractTariff) |> 
        set #refSource (get #id contractState) |> set #refTarget (get #id tariffState) |>
           set #refValidfromversion (get #refValidfromversion contractState) |> set #refValidthruversion Nothing |> createRecord
    ctsPLog <- putRelState contractTariff (get #id contractState) (get #id tariffState) (getPLog workflowCM)
    newTariffPartner :: TariffPartner <- newRecord |> set #refHistory (Id (fromJust (history tariffKeys))) |> createRecord
    newTariffPaqrtnerState :: TariffPartnerState <- newRecord |> set #refEntity (get #id newTariffPartner) |> 
        set #refSource (get #id tariffState) |> set #refTarget (get #id partnerState) |>
           set #refValidfromversion (get #refValidfromversion contractState) |> set #refValidthruversion Nothing |> createRecord
    tpsPLog <- putRelState contractTariff (get #id contractState) (get #id tariffState) (getPLog workflowCM)
    result <- commitState contract (setPLog workflowCM tpsPLog)
    case result of
        Left msg -> Log.info $ "SUCCESS:"++ msg
        Right msg -> Log.info $ "ERROR:" ++ msg
    Log.info $ ">>>>>>>>>>>>>>> NACH COMMITMUTATATION " ++ show HistorytypeContract 
--    runMutation partner usr HistorytypePartner (fst psk) validfrom1  "mutatated PartnerState"
--    runMutation tariff usr HistorytypeTariff (fst tsk)  validfrom1  "mutatated TariffState"
--
    let validfrom2 :: Day = fromGregorian 2021 6 1
    runMutation contract usr HistorytypeContract (fst csk) validfrom2 "2nd mutatated ContractState"
--    runMutation partner usr HistorytypePartner (fst psk) validfrom2  "Once mutated First Partner"
--    runMutation tariff usr HistorytypeTariff (fst tsk)  validfrom2  "mutatated TariffState"
--
    let validfrom3 :: Day = fromGregorian 2021 8 1
    runMutation contract usr HistorytypeContract (fst csk) validfrom3 "3rd mutatated ContractState"
--    runMutation partner usr HistorytypePartner (fst psk) validfrom3  "Twice mutated  First Partner"
--    runMutation tariff usr HistorytypeTariff (fst tsk)  validfrom3  "mutatated TariffState"
--
--    forEach (persistenceLogC ++ persistenceLogP ++ persistenceLogT) \pl -> do
--        Log.info $ "Logged plog:" ++ show pl
--        case pl of
--            WorkflowPL cru -> commit cru
--            HistoryPL cru -> commit cru
--            VersionPL cru -> commit cru
--            ContractPL cru -> commitState cru
--            PartnerPL cru -> commit cru
--            TariffPL cru -> commit cru
--        Log.info ("Logged nach Commit" :: String)
--
    today <- today
    now <- getCurrentTime
    countP <- countStatesByValidFromMaxTxn HistorytypePartner today now
    let o :: PT.Options = defaultPaginationOptions
        p :: PT.Pagination = Pagination
                {
                    currentPage = 1
                ,   totalItems = countP
                ,   pageSize = 3
                ,   window = windowSize o
                }
        showPage = \pag -> Log.info $ "Page " ++ show (currentPage pag)
    pStates :: ([PartnerState],pagination) <- selectStatesByValidFromMaxTxn HistorytypePartner today now o p
    showPage $ snd pStates
    forEach pStates \s -> do
        Log.info $ "pState= " ++ show s

    pStates :: ([PartnerState],pagination) <- selectStatesByValidFromMaxTxn HistorytypePartner today now o $ snd pStates
    showPage $ snd pStates
    forEach pStates \s -> do
        Log.info $ "pState= " ++ show s

    pStates :: ([PartnerState],pagination) <- selectStatesByValidFromMaxTxn HistorytypePartner today now o $ snd pStates
    showPage $ snd pStates
    forEach pStates \s -> do
        Log.info $ "pState= " ++ show s