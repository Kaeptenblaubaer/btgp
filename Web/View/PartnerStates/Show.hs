module Web.View.PartnerStates.Show where
import Web.View.Prelude

data ShowView = ShowView { partnerState :: PartnerState }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PartnerStatesAction}>PartnerStates</a></li>
                <li class="breadcrumb-item active">Show PartnerState</li>
            </ol>
        </nav>
        <h1>Show PartnerState</h1>
        <p>{partnerState}</p>
    |]
