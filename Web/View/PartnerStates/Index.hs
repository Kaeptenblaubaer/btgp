module Web.View.PartnerStates.Index where
import Web.View.Prelude

data IndexView = IndexView { partnerStates :: [PartnerState], pagination :: Pagination }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={PartnerStatesAction}>PartnerStates</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewPartnerStateAction} class="btn btn-primary ml-4">+ New</a></h1>
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
        <td>{partnerState}</td>
        <td><a href={ShowPartnerStateAction (get #id partnerState)}>Show</a></td>
        <td><a href={EditPartnerStateAction (get #id partnerState)} class="text-muted">Edit</a></td>
        <td><a href={DeletePartnerStateAction (get #id partnerState)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
