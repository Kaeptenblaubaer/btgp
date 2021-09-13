module Web.View.ContractStates.Edit where
import Web.View.Prelude

data EditView = EditView { contractState :: ContractState, detailPartnerStates::[ContractPartnerState]}
-- , contractPartnerStates :: ContractPartnerStates

instance View EditView where
    html EditView { .. } = [hsx|

        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ContractStatesAction}>ContractStates</a></li>
                <li class="breadcrumb-item active">Edit ContractState</li>
            </ol>
        </nav>
        <h1>Edit ContractState</h1>
        {renderForm contractState detailPartnerStates}
    |]

renderForm :: ContractState -> [ContractPartnerState] -> Html
renderForm contractState partners = formFor contractState [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refEntity)}
    {(textField #content)}
    {forEach partners renderPartnerState}
    <div><p>Select Partner
    <select name = "Select Partner" >
        <option value="SelectPremiumPayer">Premium Payer</option>
        <option value="SelectInsuredPerson">Insured Person</option>
    </select></p>
    </div>
    {workflowMenu}
    {submitButton}
|]

renderPartnerState :: ContractPartnerState -> Html
renderPartnerState contractPartnerState = [hsx|
    <div>
       Bubu { get #id contractPartnerState }
    </div>
|]