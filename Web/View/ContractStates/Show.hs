module Web.View.ContractStates.Show where
import Web.View.Prelude

data ShowView = ShowView { contractState :: ContractState }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ContractStatesAction}>ContractStates</a></li>
                <li class="breadcrumb-item active">Show ContractState</li>
            </ol>
        </nav>
        <h1>Show ContractState</h1>
        <p>{contractState}</p>
    |]
