module Web.View.Histories.Index where
import Web.View.Prelude

data IndexView = IndexView { histories :: [History] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={HistoriesAction}>Histories</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewHistoryAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>History</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach histories renderHistory}</tbody>
            </table>
        </div>
    |]


renderHistory :: History -> Html
renderHistory history = [hsx|
    <tr>
        <td>{history}</td>
        <td><a href={ShowHistoryAction (get #id history)}>Show</a></td>
        <td><a href={EditHistoryAction (get #id history)} class="text-muted">Edit</a></td>
        <td><a href={DeleteHistoryAction (get #id history)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
