module Web.View.AdressStates.Show where
import Web.View.Prelude

data ShowView = ShowView { adressState :: AdressState }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={AdressStatesAction}>AdressStates</a></li>
                <li class="breadcrumb-item active">Show AdressState</li>
            </ol>
        </nav>
        <h1>Show AdressState</h1>
        <p>{adressState}</p>
    |]
