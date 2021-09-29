module Web.View.AdressStates.Edit where
import Web.View.Prelude

data EditView = EditView { adressState :: AdressState }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={AdressStatesAction}>AdressStates</a></li>
                <li class="breadcrumb-item active">Edit AdressState</li>
            </ol>
        </nav>
        <h1>Edit AdressState</h1>
        {renderForm adressState}
    |]

renderForm :: AdressState -> Html
renderForm adressState = formFor adressState [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refEntity)}
    {(textField #content)}
    {submitButton}
|]
