module Application.Helper.Controller (module CanVersion, module WorkflowEnvironment, encodeUtf8) where
import IHP.ControllerPrelude
import Application.Model.CanVersion as CanVersion
import Application.Model.WorkflowEnvironment as WorkflowEnvironment
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe
import Application.Model.Persistence.Adress
import Application.Model.Persistence.Partner
import Application.Model.Persistence.Tariff
import Application.Model.Persistence.Contract
import Generated.Types ( Workflow, Workflow'(progress) )
-- Here you can add functions which are available in all your controllers




