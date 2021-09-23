module Web.Controller.Workflows where

import Web.Controller.Prelude
import Web.View.Workflows.Index
import Web.View.Workflows.New
import Web.View.Workflows.Edit
import Web.View.Workflows.Show
import IHP.Log as Log
import Data.Maybe

instance Controller WorkflowsController where
    action WorkflowsAction = do
        workflows <- query @Workflow |> fetch
        render IndexView { .. }

    action NewWorkflowAction = do
        today <- today
        let workflow = newRecord |> set #validfrom today
        render NewView { .. }

    action ShowWorkflowAction { workflowId } = do
        workflow <- fetch workflowId
        render ShowView { .. }

    action EditWorkflowAction { workflowId } = do
        workflow <- fetch workflowId
        render EditView { .. }

    action UpdateWorkflowAction { workflowId } = do
        workflow <- fetch workflowId
        workflow
            |> buildWorkflow
            |> ifValid \case
                Left workflow -> render EditView { .. }
                Right workflow -> do
                    workflow <- workflow |> updateRecord
                    setSuccessMessage "Workflow updated"
                    redirectTo EditWorkflowAction { .. }

    action CreateWorkflowAction = do
        let workflow = newRecord @Workflow
        workflow
            |> buildWorkflow
            |> ifValid \case
                Left workflow -> render NewView { .. } 
                Right workflow -> do
                    usr :: User <- query @User |> fetchOne
                    Log.info (show usr)
                    workflow <- workflow  |> set #refUser (get #id usr) |> set #progress ( fromJust $ decode $ encode workflowEnvironmentDefault ) |>createRecord
                    let workflowId = get #id workflow
                    setSuccessMessage "Workflow created"
                    setCurrentWorkflowId workflow
                    redirectTo NextWorkflowAction

    action DeleteWorkflowAction { workflowId } = do
        workflow <- fetch workflowId
        deleteRecord workflow
        setSuccessMessage "Workflow deleted"
        redirectTo WorkflowsAction

    action NextWorkflowAction = do
        workflow <- getCurrentWorkflow
        let workflowId = get #id workflow
        Log.info $ "nextaction Workflow" ++ show workflowId
        Log.info $ ("NextWF wf="++ show workflow)
        case getWfEnv workflow of
            Just wenv ->case get #workflowType workflow of
                WftypeNew -> case get #historyType workflow of
                    HistorytypeContract -> case getStateIdMB contract wenv of
                        Nothing -> redirectTo NewContractStateAction 
                        Just sid -> do
                            let cmd = paramText "Workflow"
                            case cmd of
                                "SelPartnerState" -> redirectTo $ SelectPartnerStateAction
                                "UpdateContractStatePartnerState" -> do
                                    let partnerStateId :: (Id PartnerState) = (Id (param "partnerStateId"))
                                    Log.info $ "UpdateContractStatePartnerState cs=" ++ show sid ++ " ps=" ++ show partnerStateId
                                    pLog <-putRelState contractPartner sid partnerStateId (getPLog workflow)
                                    workflowUpd <- setPLog workflow pLog |> updateRecord
                                    redirectTo $ EditContractStateAction sid
                                "Next" -> redirectTo $ EditContractStateAction sid
                                "Suspend" -> redirectTo $ ShowWorkflowAction workflowId
                                "Commit" -> do
                                    commitState contract workflow
                                    setSuccessMessage "ContractState committed"
                                    redirectTo $ ShowWorkflowAction workflowId
                                unknown -> do
                                    setErrorMessage $ unknown ++ " is notknown command."
                                    redirectTo $ ShowWorkflowAction workflowId
                --     HistorytypePartner -> case getStateIdMB partner wenv of
                --         Nothing -> redirectTo NewPartnerAction 
                --         Just sid -> redirectTo $ EditPartnerAction sid
                --     HistorytypeTariff -> case getStateIdMB tariff wenv of
                --         Nothing -> redirectTo NewTariffAction 
                --         Just sid -> redirectTo $ EditTariffAction sid
                -- WftypeUpdate -> redirectUpdateState workflow
            Nothing -> do
                setErrorMessage $ "SHOULDN'T: progress is null workflowId= " ++ show workflowId
                redirectTo $ ShowWorkflowAction workflowId

buildWorkflow workflow = workflow
    |> fill @["refUser","historyType","workflowType","validfrom","workflowStatus"]
