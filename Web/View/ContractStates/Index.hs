module Web.View.ContractStates.Index where
import Web.View.Prelude

data IndexView = IndexView { contractStates :: [ContractState] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={ContractStatesAction}>ContractStates</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewContractStateAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>ContractState</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach contractStates renderContractState}</tbody>
            </table>
        </div>
    |]


renderContractState :: ContractState -> Html
renderContractState contractState = [hsx|
    <tr>
        <td>{contractState}</td>
        <td><a href={ShowContractStateAction (get #id contractState)}>Show</a></td>
        <td><a href={EditContractStateAction (get #id contractState)} class="text-muted">Edit</a></td>
        <td><a href={DeleteContractStateAction (get #id contractState)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
