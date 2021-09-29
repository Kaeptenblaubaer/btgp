module Web.View.AdressStates.New where
import Web.View.Prelude

data NewView = NewView { adressState :: AdressState }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={AdressStatesAction}>AdressStates</a></li>
                <li class="breadcrumb-item active">New AdressState</li>
            </ol>
        </nav>
        <h1>New AdressState</h1>
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
