module Web.Controller.PartnerStates where

import Web.Controller.Prelude
import Web.View.PartnerStates.Select
import Web.View.PartnerStates.Index
import Web.View.PartnerStates.New
import Web.View.PartnerStates.Edit
import Web.View.PartnerStates.Show
import IHP.Log as Log
import IHP.Pagination.Types 

instance Controller PartnerStatesController where
    action PartnerStatesAction = do
        workFlow <- getCurrentWorkflow
        today <- today
        now <- getCurrentTime
        let page = paramOrDefault @Int 1 "page"
            pageSize = paramOrDefault @Int (maxItems defaultPaginationOptions ) "maxItems"
        count <- countStatesByValidFromMaxTxn HistorytypeContract today now

        let pagination = Pagination
                {
                    currentPage = page
                ,   totalItems = count
                ,   pageSize = pageSize
                ,   window = windowSize defaultPaginationOptions 
                }
        Log.info $ "pagination=" ++ show pagination
        (partnerStates, pagination) <- selectStatesByValidFromMaxTxn HistorytypePartner today now defaultPaginationOptions pagination
        render IndexView { .. }

    action SelectPartnerStateAction = do
        workflow <- getCurrentWorkflow
        let wfId = get #id workflow
        let sidMB = case getWfp workflow of
                Just wfp -> case get #historyType workflow of
                    HistorytypeContract -> getStateIdMB contract wfp
                    _ -> Nothing
        case sidMB of
            Just sid -> Log.info $ "sid for selection " ++ show sid
            _ -> Log.info ("No sid found" :: String)      
        today <- today
        now <- getCurrentTime
        let page = paramOrDefault @Int 1 "page"
            pageSize = paramOrDefault @Int (maxItems defaultPaginationOptions ) "maxItems"
        count <- countStatesByValidFromMaxTxn HistorytypeContract today now

        let pagination = Pagination
                {
                    currentPage = page
                ,   totalItems = count
                ,   pageSize = pageSize
                ,   window = windowSize defaultPaginationOptions 
                }
        Log.info $ "pagination=" ++ show pagination
        (partnerStates, pagination) <- selectStatesByValidFromMaxTxn HistorytypePartner today now defaultPaginationOptions pagination
        render SelectView { .. }

    action NewPartnerStateAction = do
        let partnerState = newRecord
        render NewView { .. }

    action ShowPartnerStateAction { partnerStateId } = do
        partnerState <- fetch partnerStateId
        render ShowView { .. }

    action EditPartnerStateAction { partnerStateId } = do
        partnerState <- fetch partnerStateId
        render EditView { .. }

    action UpdatePartnerStateAction { partnerStateId } = do
        partnerState <- fetch partnerStateId
        partnerState
            |> buildPartnerState
            |> ifValid \case
                Left partnerState -> render EditView { .. }
                Right partnerState -> do
                    partnerState <- partnerState |> updateRecord
                    setSuccessMessage "PartnerState updated"
                    redirectTo EditPartnerStateAction { .. }

    action CreatePartnerStateAction = do
        let partnerState = newRecord @PartnerState
        partnerState
            |> buildPartnerState
            |> ifValid \case
                Left partnerState -> render NewView { .. } 
                Right partnerState -> do
                    partnerState <- partnerState |> createRecord
                    setSuccessMessage "PartnerState created"
                    redirectTo PartnerStatesAction

    action DeletePartnerStateAction { partnerStateId } = do
        partnerState <- fetch partnerStateId
        deleteRecord partnerState
        setSuccessMessage "PartnerState deleted"
        redirectTo PartnerStatesAction

buildPartnerState partnerState = partnerState
    |> fill @["refValidfromversion","refValidthruversion","refEntity","content"]
