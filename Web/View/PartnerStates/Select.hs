module Web.View.PartnerStates.Select where
import Web.View.Prelude
import Web.Controller.Workflows


data SelectView = SelectView { partnerStates :: [PartnerState], pagination :: Pagination , wfId :: Id Workflow}

instance View SelectView where
    html SelectView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={PartnerStatesAction}>PartnerStates</a></li>
            </ol>
        </nav>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>PartnerState</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach partnerStates renderPartnerState}</tbody>
            </table>
            {renderPagination pagination}
        </div>
    |]


renderPartnerState :: PartnerState -> Html
renderPartnerState partnerState = [hsx|
    <p><form action={ NextWorkflowAction} method="POST">
        <input type="hidden" name="Workflow" value="UpdateContractStatePartnerState" />
        <input type="text" name="partnerStateId" value={show (get #id partnerState)} />
        <button class="btn btn-primary">Select</button>
    </form></p> 
|]

--        "Workflow=UpdateContractStatePartnerState" ++ "&partnerStateId=" ++ show (get #id partnerState)

