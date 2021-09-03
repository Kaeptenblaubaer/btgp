module Web.View.PartnerStates.New where
import Web.View.Prelude

data NewView = NewView { partnerState :: PartnerState }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PartnerStatesAction}>PartnerStates</a></li>
                <li class="breadcrumb-item active">New PartnerState</li>
            </ol>
        </nav>
        <h1>New PartnerState</h1>
        {renderForm partnerState}
    |]

renderForm :: PartnerState -> Html
renderForm partnerState = formFor partnerState [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refEntity)}
    {(textField #content)}
    {submitButton}
|]
