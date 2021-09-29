module Web.View.AdressStates.Index where
import Web.View.Prelude

data IndexView = IndexView { adressStates :: [AdressState] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={AdressStatesAction}>AdressStates</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewAdressStateAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>AdressState</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach adressStates renderAdressState}</tbody>
            </table>
        </div>
    |]


renderAdressState :: AdressState -> Html
renderAdressState adressState = [hsx|
    <tr>
        <td>{adressState}</td>
        <td><a href={ShowAdressStateAction (get #id adressState)}>Show</a></td>
        <td><a href={EditAdressStateAction (get #id adressState)} class="text-muted">Edit</a></td>
        <td><a href={DeleteAdressStateAction (get #id adressState)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
