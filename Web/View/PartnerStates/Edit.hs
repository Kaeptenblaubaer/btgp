module Web.View.PartnerStates.Edit where
import Web.View.Prelude

data EditView = EditView { partnerState :: PartnerState }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PartnerStatesAction}>PartnerStates</a></li>
                <li class="breadcrumb-item active">Edit PartnerState</li>
            </ol>
        </nav>
        <h1>Edit PartnerState</h1>
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
