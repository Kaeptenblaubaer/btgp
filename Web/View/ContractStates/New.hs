module Web.View.ContractStates.New where
import Web.View.Prelude

data NewView = NewView { contractState :: ContractState }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ContractStatesAction}>ContractStates</a></li>
                <li class="breadcrumb-item active">New ContractState</li>
            </ol>
        </nav>
        <h1>New ContractState</h1>
        {renderForm contractState}
    |]

renderForm :: ContractState -> Html
renderForm contractState = formFor contractState [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refEntity)}
    {(textField #content)}
    {submitButton}
|]
