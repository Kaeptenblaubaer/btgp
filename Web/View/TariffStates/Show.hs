module Web.View.TariffStates.Show where
import Web.View.Prelude

data ShowView = ShowView { tariffState :: TariffState }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={TariffStatesAction}>TariffStates</a></li>
                <li class="breadcrumb-item active">Show TariffState</li>
            </ol>
        </nav>
        <h1>Show TariffState</h1>
        <p>{tariffState}</p>
    |]
