module Web.View.Workflows.Show where
import Web.View.Prelude
import IHP.ControllerPrelude 
import Data.Text(replace, stripPrefix)
import Data.Maybe ( fromJust )

data ShowView = ShowView { workflow :: Workflow }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WorkflowsAction}>Workflows</a></li>              
                <li class="breadcrumb-item active">Show Workflow</li>
            </ol>
        </nav> 
        <h1>Show Workflow</h1>
        <style type="text/css">
            jsoneditor {
              width: 500px;
              height: 500px;   
            }
        </style>
        <div id="jsoneditor"></div> 
        <table class="table"><thead>
          <tr><td>Commands</td></tr>
        </thead>
        <tbody>
          <tr>
            <td><a href={pathTo (NextWorkflowAction) <> "?Command=Commit"}>Commit</a></td>
            <td><a href={pathTo (NextWorkflowAction) <> "?Command=Rollback"}>Rollback</a></td>
            <td><a href={pathTo (NextWorkflowAction) <> "?Command=Suspend"}>Suspend</a></td>
            <td><a href={pathTo (NextWorkflowAction) <> "?Command=Resume"}>Resume</a></td>
          </tr>
          </tbody></table>
        <script data-api-key= {show $ encode $ get #progress workflow }>
          var progress = document.currentScript.dataset.apiKey;
          progress = progress.substring(1,progress.length-1)
          // create the editor
          const container = document.getElementById('jsoneditor')
          const options = {}
          const editor = new JSONEditor(container, options)
          go()
  
          function go () {
            editor.set(JSON.parse(progress.replace(/\\/g,"")))
          }

        </script>
    |]