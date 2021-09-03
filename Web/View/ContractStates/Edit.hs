module Web.View.ContractStates.Edit where
import Web.View.Prelude

data EditView = EditView { contractState :: ContractState }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ContractStatesAction}>ContractStates</a></li>
                <li class="breadcrumb-item active">Edit ContractState</li>
            </ol>
        </nav>
        <h1>Edit ContractState</h1>
        {renderForm contractState}
    |]

renderForm :: ContractState -> Html
renderForm contractState = formFor contractState [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refEntity)}
    {(textField #content)}
    {workflowMenu}
    {submitButton}
|]
