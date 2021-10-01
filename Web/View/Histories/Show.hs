module Web.View.Histories.Show where
import Web.View.Prelude
import Data.Tree
data ShowView = ShowView { history :: History , versions :: [Version], states :: [ContractState] }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={HistoriesAction}>Histories</a></li>
                <li class="breadcrumb-item active">Show History</li>
            </ol>
        </nav>
        <script src="app.js"/>
        <h1>Show History</h1>
        <h2 > Tree representation of bitemporal versioning histories</h2>
        <p>reference time is displayed vertically running down from old to new. Indentation represents
        invalidation of timelines by retrospective mutatations </p>
        <p>Click on the arrow(s) to open or close the tree branches, that is: show or hide invalidated timelines</p>
        <ul id="history" >
          { forEach (fst $ mkForest versions []) (renderTree states 0)} 
        </ul>
    |]

renderTree :: [ContractState] -> Integer -> Tree Version -> Html
renderTree states lvl n  = case subForest n of
       [] ->  [hsx| 
                    <li>
                            {renderLabel states lvl n}
                    </li>
                |]
       _  ->  [hsx|  
                    <li>{renderLabel states lvl n} 
                        <span class="caret" ></span>
                        <ul class="nested">
                          { forEach (subForest n) (renderTree states (lvl+1))} 
                        </ul>
                    </li>
                |]


renderLabel :: [ContractState] -> Integer -> Tree Version  -> Html
renderLabel states lvl n = case lvl of
    0 -> renderMutableVersion states ( rootLabel n)
    _ -> renderImmutableVersion states ( rootLabel n)

renderMutableVersion :: [ContractState] -> Version -> Html
renderMutableVersion states version =  [hsx| <div><table>
            <tr><th>id</th><th>valid from</th><th>show</th><th>command</th></tr>
            <tr>
                <td  style="width:1%">{get #id version}</td><td style="width:2%">{get #validfrom version}</td>
                <td style="width:3%"><a href={getStateId version states}>Show stateM</a></td>
                <td style="width:18%"><a href={pathTo(NewWorkflowAction) <> "?historyId=" ++ (show (get #refHistory version))}>Start Mutation Workflow</a></td>
            </tr></table></div>
          |] 

renderImmutableVersion :: [ContractState] -> Version -> Html
renderImmutableVersion states version =  [hsx| <div><table>
            <tr><th>id</th><th>valid from</th><th>show</th></tr>
            <tr>
                <td  style="width:1%">{get #id version}</td><td style="width:2%">{get #validfrom version}</td>
                <td style="width:3%"><a href={getStateId version states}>Show stateI</a></td><td style="width:18%"/>
            </tr></table></div>
          |]  



compId2refId :: Version -> Version -> Bool
compId2refId v ref = (Just $ get #id v) == get #refShadowedby ref

isChild node parent = compId2refId node parent  

mkTree root [] = (Node root [],[]) 
mkTree root rest = let  (children,rest2) = partition (isChild root) rest
                        (subforest,rest3) = mkSubForest children [] rest2 
    in (Node root subforest,rest3)

mkSubForest :: [Version] -> [Tree Version] -> [Version] -> ( [Tree Version], [Version])
mkSubForest [] accu rest = (accu, rest)
mkSubForest (fstChild:siblings) accu rest  = 
        let (tree,newrest) = mkTree fstChild rest 
        in mkSubForest siblings (accu++[tree]) newrest

mkForest ::  [Version] -> [Tree Version] -> ( [Tree Version], [Version])
mkForest  [] akku               = (akku,[])
mkForest  (head:tail) akku      =
        let (tree,rest) = mkTree head tail 
        in mkForest rest (akku ++ [tree]) 


getStateId v states = case filter (\s-> get #refValidfromversion s == get #id v) states of
        [] -> ContractStatesAction
        s : _ -> ShowContractStateAction (get #id s)

