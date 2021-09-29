module Web.View.TariffStates.New where
import Web.View.Prelude

data NewView = NewView { tariffState :: TariffState }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={TariffStatesAction}>TariffStates</a></li>
                <li class="breadcrumb-item active">New TariffState</li>
            </ol>
        </nav>
        <h1>New TariffState</h1>
        {renderForm tariffState}
    |]

renderForm :: TariffState -> Html
renderForm tariffState = formFor tariffState [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refEntity)}
    {(textField #content)}
    {submitButton}
|]
