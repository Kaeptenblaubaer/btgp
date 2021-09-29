module Web.Controller.AdressStates where

import Web.Controller.Prelude
import Web.View.AdressStates.Index
import Web.View.AdressStates.New
import Web.View.AdressStates.Edit
import Web.View.AdressStates.Show

instance Controller AdressStatesController where
    action AdressStatesAction = do
        adressStates <- query @AdressState |> fetch
        render IndexView { .. }

    action NewAdressStateAction = do
        let adressState = newRecord
        render NewView { .. }

    action ShowAdressStateAction { adressStateId } = do
        adressState <- fetch adressStateId
        render ShowView { .. }

    action EditAdressStateAction { adressStateId } = do
        adressState <- fetch adressStateId
        render EditView { .. }

    action UpdateAdressStateAction { adressStateId } = do
        adressState <- fetch adressStateId
        adressState
            |> buildAdressState
            |> ifValid \case
                Left adressState -> render EditView { .. }
                Right adressState -> do
                    adressState <- adressState |> updateRecord
                    setSuccessMessage "AdressState updated"
                    redirectTo EditAdressStateAction { .. }

    action CreateAdressStateAction = do
        let adressState = newRecord @AdressState
        adressState
            |> buildAdressState
            |> ifValid \case
                Left adressState -> render NewView { .. } 
                Right adressState -> do
                    adressState <- adressState |> createRecord
                    setSuccessMessage "AdressState created"
                    redirectTo AdressStatesAction

    action DeleteAdressStateAction { adressStateId } = do
        adressState <- fetch adressStateId
        deleteRecord adressState
        setSuccessMessage "AdressState deleted"
        redirectTo AdressStatesAction

buildAdressState adressState = adressState
    |> fill @["refValidfromversion","refValidthruversion","refEntity","content"]
