module App where

import Control.Monad.Eff
import Data.JSON
import Data.Maybe
import Data.Monoid

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

import App.ToDoItem
import App.Ajax

-- * Define actions

data Action = DoNothing -- ^ Just sit and do nothing

-- * Define state

type State = {
    todoList :: ToDoList
}

initialState :: State
initialState = { todoList: ToDoList { _todoItems: []} }

-- * Handle actions

performAction :: T.PerformAction _ Action (T.Action _ State)
performAction _ DoNothing = T.modifyState id

-- * Rendering the list

render :: T.Render State _ Action
render ctx s _ = T.div [ A.className "app-container" ] [ T.ul' current ]
  where
    current  = itemList s.todoList <> [newItem]
    itemList (ToDoList l) = itemRow <$> l._todoItems
    newItem = itemRow $
        ToDoItem { _todoIdent: Nothing, _todoText: "", _todoDone: false }
    itemRow :: ToDoItem -> T.Html _
    itemRow (ToDoItem i) = T.li' $
        [ T.input [ A._type "text", A.className "item-row", A.value i._todoText ] []
        , T.input ([ A._type "checkbox" ] <> checkedParam) []
        ]
      where
        checkedParam = if i._todoDone
                        then [ A.checked "true" ]
                        else []

-- * Main

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render
        # T.componentWillMount DoNothing

main = do
    let component = T.createClass spec
    T.render component {}
