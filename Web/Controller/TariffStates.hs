module Web.Controller.TariffStates where

import Web.Controller.Prelude
import Web.View.TariffStates.Index
import Web.View.TariffStates.New
import Web.View.TariffStates.Edit
import Web.View.TariffStates.Show

instance Controller TariffStatesController where
    action TariffStatesAction = do
        tariffStates <- query @TariffState |> fetch
        render IndexView { .. }

    action NewTariffStateAction = do
        let tariffState = newRecord
        render NewView { .. }

    action ShowTariffStateAction { tariffStateId } = do
        tariffState <- fetch tariffStateId
        render ShowView { .. }

    action EditTariffStateAction { tariffStateId } = do
        tariffState <- fetch tariffStateId
        render EditView { .. }

    action UpdateTariffStateAction { tariffStateId } = do
        tariffState <- fetch tariffStateId
        tariffState
            |> buildTariffState
            |> ifValid \case
                Left tariffState -> render EditView { .. }
                Right tariffState -> do
                    tariffState <- tariffState |> updateRecord
                    setSuccessMessage "TariffState updated"
                    redirectTo EditTariffStateAction { .. }

    action CreateTariffStateAction = do
        let tariffState = newRecord @TariffState
        tariffState
            |> buildTariffState
            |> ifValid \case
                Left tariffState -> render NewView { .. } 
                Right tariffState -> do
                    tariffState <- tariffState |> createRecord
                    setSuccessMessage "TariffState created"
                    redirectTo TariffStatesAction

    action DeleteTariffStateAction { tariffStateId } = do
        tariffState <- fetch tariffStateId
        deleteRecord tariffState
        setSuccessMessage "TariffState deleted"
        redirectTo TariffStatesAction

buildTariffState tariffState = tariffState
    |> fill @["refValidfromversion","refValidthruversion","refEntity","content"]
