module Web.View.TariffStates.Edit where
import Web.View.Prelude

data EditView = EditView { tariffState :: TariffState }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={TariffStatesAction}>TariffStates</a></li>
                <li class="breadcrumb-item active">Edit TariffState</li>
            </ol>
        </nav>
        <h1>Edit TariffState</h1>
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
