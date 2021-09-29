module Web.View.TariffStates.Index where
import Web.View.Prelude

data IndexView = IndexView { tariffStates :: [TariffState] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={TariffStatesAction}>TariffStates</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewTariffStateAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>TariffState</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach tariffStates renderTariffState}</tbody>
            </table>
        </div>
    |]


renderTariffState :: TariffState -> Html
renderTariffState tariffState = [hsx|
    <tr>
        <td>{tariffState}</td>
        <td><a href={ShowTariffStateAction (get #id tariffState)}>Show</a></td>
        <td><a href={EditTariffStateAction (get #id tariffState)} class="text-muted">Edit</a></td>
        <td><a href={DeleteTariffStateAction (get #id tariffState)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
