module Web.View.PartnerStates.Select where
import Web.View.Prelude

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
    <tr>
        <td>{get #content partnerState}</td>
        <td><a href={(pathTo (NextWorkflowAction) <> "?Workflow=" ++ "UpdateContractStatePartnerState" ) }>Select</a></td>
    </tr>
|]
