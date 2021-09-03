module Web.Controller.ContractStates where

import Web.Controller.Prelude
import Web.View.ContractStates.Index
import Web.View.ContractStates.New
import Web.View.ContractStates.Edit
import Web.View.ContractStates.Show
instance Controller ContractStatesController where
    action ContractStatesAction = do
        contractStates <- query @ContractState |> fetch
        render IndexView { .. }

    action NewContractStateAction = do
        let contractState = newRecord
        render NewView { .. }

    action ShowContractStateAction { contractStateId } = do
        contractState <- fetch contractStateId
        render ShowView { .. }

    action EditContractStateAction { contractStateId } = do
        contractState <- fetch contractStateId
        render EditView { .. }

    action UpdateContractStateAction { contractStateId } = do
        contractState <- fetch contractStateId
        contractState
            |> buildContractState
            |> ifValid \case
                Left contractState -> render EditView { .. }
                Right contractState -> do
                    contractState <- contractState |> updateRecord
                    setSuccessMessage "ContractState updated"
                    workflowId <- getCurrentWorkflowId
                    redirectToPath (pathTo (NextWorkflowAction { .. }) <> "&Workflow=" ++ paramText "Workflow")

    action CreateContractStateAction = do
        let contractState = newRecord @ContractState
        contractState
            |> buildContractState
            |> ifValid \case
                Left contractState -> render NewView { .. } 
                Right contractState -> do
                    workflow <- getCurrentWorkflow
                    contractState <- createHistory contract workflow  contractState
                    setSuccessMessage "ContractState created"
                    redirectTo ContractStatesAction

    action DeleteContractStateAction { contractStateId } = do
        contractState <- fetch contractStateId
        deleteRecord contractState
        setSuccessMessage "ContractState deleted"
        redirectTo ContractStatesAction

buildContractState contractState = contractState
    |> fill @["refValidfromversion","refValidthruversion","refEntity","content"]
