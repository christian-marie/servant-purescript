module App where

import Control.Monad
import Control.Monad.Eff
import qualified Data.Array as Arr
import Data.JSON
import Data.List
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Monoid
import Data.Tuple
import Data.Tuple.Nested

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Internal as T
import qualified Thermite.Types as T

import Debug.Trace
import Debug.Foreign

import Data.List.Utils
import App.ToDoItem
import App.Ajax

-- * Extensions to Thermite

-- | Generic data-whatever attribute
_dataAttr :: forall action. String -> String -> T.Prop action
_dataAttr sub = T.unsafeAttribute ("data-" <> sub)

-- | data-todoitem
_dataToDoItem :: forall action. ToDoItem -> T.Prop action
_dataToDoItem (ToDoItem i) = case i._todoIdent of
    (Just ident) -> _dataAttr "todoitem" ident
    Nothing      -> _dataAttr "todoitem" ""

-- * Some unsafe routines

-- | Get key code for an event
foreign import getKeyCode
  "function getKeyCode(e) {\
  \  return e.keyCode;\
  \}" :: T.KeyboardEvent -> Number

-- | Stringifies a JSON-friendly object to JSON
foreign import stringify
"""
function stringify(o) {
  return JSON.stringify(o);
}
""" :: forall a. a -> String

-- | JSON stringify something we got

-- * Define actions

-- | Actions that can affect the page state and layout
data Action = GetList -- ^ Just get the latest list
            | DoNothing -- ^ Just sit and do nothing
            | UpdateText (Maybe UUID) String -- ^ Update a todo item's text
            | SubmitItem ToDoItem -- ^ Submit a new or updated item

-- * Define state

-- | State only contains the current ToDoList
type State = {
    todoList :: ToDoList
}

-- | Initial state is an empty ToDoList with one blank ToDoItem to fill in
initialState :: State
initialState = { todoList: addNewItem blankToDoList }

-- | Set todo list
setTodoList :: ToDoList -> T.Action _ State Unit
setTodoList l = T.setState { todoList: l }

-- * Handle actions

-- | Handle all available actions
performAction :: T.PerformAction _ Action (T.Action _ State)
performAction _ DoNothing        = return unit
performAction _ GetList          = getList
performAction _ (UpdateText i t) = updateItemText i t
performAction _ (SubmitItem x)   = sendItem x

-- | Update state for a given item
updateItemText :: Maybe UUID -> String -> T.Action _ State Unit
updateItemText u t = T.modifyState (\o ->
    { todoList: updateListItemText o.todoList u t })

-- | Modify current AJAX method so that it only passes through the success ResponseData to a callback.
-- This is needed to make it work with T.async.
responseOnly
    :: forall eff. 
       (SuccessFn eff -> FailureFn eff -> Eff (xhr :: XHREff | eff) Unit)
    -> ((ResponseData -> Eff (xhr :: XHREff | eff) Unit) -> Eff (xhr :: XHREff | eff) Unit)
responseOnly orig = flip orig err <<< success
  where
    success fn a b c = fn $ stringify a
    err a b c        = return unit

-- | Get current todo list
getList :: T.Action _ State Unit
getList = do
    r <- T.async (responseOnly getitems)
    setTodoList (addNewItem <<< result $ r)

-- | Send a ToDoItem to the server
sendItem :: ToDoItem -> T.Action _ State Unit
sendItem (ToDoItem i) = do
    r <- T.async (responseOnly <<< sender i._todoIdent <<< encode $ ToDoItem i)
    setTodoList (addNewItem <<< result $ r)
  where
    sender (Just ident) = putitemsWithUuid ident
    sender _            = postitems

-- | Decode result and return appropriate ToDoList
result :: String -> ToDoList
result = fromMaybe blankToDoList <<< decode

-- * DOM events

-- | onchange
handleTodoTextChange :: T.FormEvent -> Action
handleTodoTextChange e = UpdateText (strToIdent $ getToDoItemIdent e) (getValue e)
  where
    todoItem = getToDoItemFromUI (getToDoItemIdent e)

-- | onkeyup
handleTodoTextKeyUp :: T.KeyboardEvent -> Action
handleTodoTextKeyUp e = case getKeyCode e of
    13 -> case getValue e of
        "" -> DoNothing
        _  -> SubmitItem $
            getToDoItemFromUI (getToDoItemIdent e)
    _  -> DoNothing

-- | onblur
handleTodoTextBlur :: T.FocusEvent -> Action
handleTodoTextBlur e = SubmitItem $
    getToDoItemFromUI (getToDoItemIdent e)

-- | Checkbox click
handleTodoCheckboxChange :: T.MouseEvent -> Action
handleTodoCheckboxChange e = SubmitItem $
    getToDoItemFromUI (getToDoItemIdent e)

-- | Get entire ToDoItem from user interface properties
getToDoItemFromUI :: String -> ToDoItem
getToDoItemFromUI i = ToDoItem { _todoIdent: strToIdent i, _todoText: txt, _todoDone: done }
  where
    txt  = getToDoItemText i
    done = getToDoItemDone i

-- | Turns a string identifier into a proper Maybe for sending/using against state
strToIdent :: String -> Maybe UUID
strToIdent "" = Nothing
strToIdent x  = Just x

-- | Get value from a DOM element
foreign import getValue 
  "function getValue(e) {\
  \  return e.target.value;\
  \}" :: forall event. event -> String

-- | Get data-todoitem attribute from a DOM element
foreign import getToDoItemIdent
"""
function getToDoItemIdent (e) {
    if (e.target.getAttribute('data-todoitem')) {
        return e.target.getAttribute('data-todoitem');
    }
    return "";
}
""" :: forall event. event -> String

-- | Get text value from a todoitem DOM element
foreign import getToDoItemText
"""
function getToDoItemText (ident) {
    var t = document.querySelector("input[type='text'][data-todoitem='" + ident + "']");
    if (!!t) {
        return t.value
    }
    return "";
}
""" :: String -> String

-- | Get checkbox value from a todoitem DOM element
foreign import getToDoItemDone
"""
function getToDoItemDone (ident) {
    var t = document.querySelector("input[type='checkbox'][data-todoitem='" + ident + "']");
    if (!!t) {
        return t.checked
    }
    return false;
}
""" :: String -> Boolean

-- * Rendering the list

-- | Render user interface HTML
render :: T.Render State _ Action
render ctx s _ = do
    container [ T.ul' current
              , T.p' [ remaining ] ]
  where
    container = T.div [ A.className "app-container" ]
    remaining = T.text $ (show $ inState false s.todoList) <> " remaining"

    current  = itemList s.todoList
    itemList (ToDoList l) = itemRow <$> l._todoItems
    itemRow :: ToDoItem -> T.Html _
    itemRow (ToDoItem i) = T.li' $
        [ T.input [ A._type "text"
                  , A.className "item-row"
                  , A.value i._todoText
                  , T.onChange ctx handleTodoTextChange
                  , T.onKeyUp  ctx handleTodoTextKeyUp
                  , T.onBlur   ctx handleTodoTextBlur
                  , _dataToDoItem $ ToDoItem i ] []
        , T.input ([ A._type "checkbox"
                   , _dataToDoItem $ ToDoItem i
                   , T.onClick ctx handleTodoCheckboxChange ] <> checkedParam) []
        ]
      where
        checkedParam = if i._todoDone
                        then [ A.checked "true" ]
                        else []

-- * Main

-- | Specification for HTML UI
spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render
        # T.componentWillMount GetList

main = do
    let component = T.createClass spec
    T.render component {}
