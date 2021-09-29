module Web.View.ContractStates.Show where
import Web.View.Prelude

data ShowView = ShowView { contractState :: ContractState,
    contractPartnerStates ::[ContractPartnerState] , partnerStates :: [PartnerState],
    contractTariffStates ::[ContractTariffState] , tariffStates :: [TariffState]}

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ContractStatesAction}>ContractStates</a></li>
                <li class="breadcrumb-item active">Show ContractState</li>
            </ol>
        </nav>
        <h1>Show ContractState</h1>
       {renderDetails contractState partnerStates tariffStates}
    |]

renderDetails :: ContractState -> [PartnerState] ->  [TariffState] -> Html
renderDetails contractState partners tariffs = formFor contractState [hsx|
    {get #content contractState}
    {forEach partners renderPartnerState}
    {forEach tariffs renderTariffState}
|]

renderPartnerState :: PartnerState -> Html
renderPartnerState partnerState = [hsx|
    <div>
       { get #content partnerState }
    </div>
|]

renderTariffState :: TariffState -> Html
renderTariffState tariffState = [hsx|
    <div>
       { get #content tariffState }
    </div>
|]

selectPartnerWorkflowOption = [hsx|
    <option value="SelPartnerState">Select Partner</option>
|]
