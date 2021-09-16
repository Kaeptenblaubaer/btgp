module Web.View.Workflows.New where
import Web.View.Prelude

data NewView = NewView { workflow :: Workflow }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WorkflowsAction}>Workflows</a></li>
                <li class="breadcrumb-item active">New Workflow</li>
            </ol>
        </nav>
        <h1>New Workflow</h1>
        {renderForm workflow}
    |]

instance CanSelect HistoryType where
    type SelectValue HistoryType = HistoryType
    selectValue value = value
    selectLabel = tshow

instance CanSelect WorkflowType where
    type SelectValue WorkflowType = WorkflowType
    selectValue value = value
    selectLabel = tshow


allht = allEnumValues @ HistoryType
allwt = allEnumValues @ WorkflowType

renderForm :: Workflow -> Html
renderForm workflow = formFor workflow [hsx|
    {(textField #refUser)}
    {(selectField #historyType allht)}
    {(selectField #workflowType allwt)}
    {(textField #progress)}
    {(dateField #validfrom)}
    {(textField #workflowStatus)}
    {workflowMenu emptyHtml}
    {submitButton}
|]

