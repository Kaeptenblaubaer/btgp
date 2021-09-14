module Web.View.ContractStates.Edit where
import Web.View.Prelude

data EditView = EditView { contractState :: ContractState,
    contractPartnerStates ::[ContractPartnerState] , partnerStates :: [PartnerState]}

instance View EditView where
    html EditView { .. } = [hsx|

        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ContractStatesAction}>ContractStates</a></li>
                <li class="breadcrumb-item active">Edit ContractState</li>
            </ol>
        </nav>
        <h1>Edit ContractState</h1>
        {renderForm contractState partnerStates}
    |]

renderForm :: ContractState -> [PartnerState] -> Html
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

renderPartnerState :: PartnerState -> Html
renderPartnerState partnerState = [hsx|
    <div>
       { get #content partnerState }
    </div>
|]