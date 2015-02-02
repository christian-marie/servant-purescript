module App where

import Control.Monad.Eff
import qualified Data.Array as Arr
import Data.JSON
import Data.List
import Data.Maybe
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

-- * Define actions

data Action = GetList -- ^ Just get the latest list
            | DoNothing -- ^ Just sit and do nothing
            | UpdateText (Maybe UUID) String -- ^ Update a todo item's text
            | SubmitItem ToDoItem -- ^ Submit a new or updated item

-- * Define state

type State = {
    todoList :: ToDoList
}

blankToDoList :: ToDoList
blankToDoList = ToDoList { _todoItems: [] }

initialState :: State
initialState = { todoList: addNewItem blankToDoList }

newToDoItem :: ToDoItem
newToDoItem = ToDoItem { _todoIdent: Nothing, _todoText: "", _todoDone: false }

addNewItem :: ToDoList -> ToDoList
addNewItem (ToDoList l) = ToDoList { _todoItems: l._todoItems <> [newToDoItem] }

getItemByUuid :: ToDoList -> Maybe UUID -> Maybe ToDoItem
getItemByUuid (ToDoList l) i = case Arr.filter isThisIt l._todoItems of
    (it:_) -> Just it
    _      -> Nothing
  where
    isThisIt (ToDoItem it) = it._todoIdent == i

-- * Handle actions

-- | Handle all available actions
performAction :: T.PerformAction _ Action (T.Action _ State)
performAction _ DoNothing        = return unit
performAction _ GetList          = getList
performAction _ (UpdateText i t) = updateItemText i t
performAction _ (SubmitItem x)   = sendItem x

-- | Update state for a given item
updateItemText :: Maybe UUID -> String -> T.Action _ State Unit
updateItemText u t = do
    let aaa = fprintUnsafe [show u, t]
    T.modifyState (\o -> do
        let qqq = fprintUnsafe o.todoList
        let qqr = fprintUnsafe $ updateListItemText o.todoList u t
        { todoList: updateListItemText o.todoList u t })

-- | Update ToDoList item text
updateListItemText :: ToDoList -> Maybe UUID -> String -> ToDoList
updateListItemText (ToDoList l) u t = ToDoList { _todoItems: newToDoItems l }
  where
    newToDoItems l =
        case getItemByUuid (ToDoList l) i of
            Just item -> replace [item] [new item] l._todoItems
            _         -> l._todoItems <> [new newToDoItem]
    new (ToDoItem it) = ToDoItem { _todoIdent: i, _todoText: t, _todoDone: it._todoDone }

-- | Modify current AJAX method so that it only passes through the success ResponseData to a callback.
-- This is needed to make it work with T.async.
responseOnly
    :: forall eff. 
       (SuccessFn eff -> FailureFn eff -> Eff (xhr :: XHREff | eff) Unit)
    -> ((ResponseData -> Eff (xhr :: XHREff | eff) Unit) -> Eff (xhr :: XHREff | eff) Unit)
responseOnly orig = (flip orig (\_ _ d -> return unit)) <<< (\fn a _ _ -> fn a)

-- | Get current todo list
getList :: T.Action _ State Unit
getList = do
    r <- T.async <<< responseOnly $ getitems
    case decode r :: Maybe ToDoList of
        Just l -> T.modifyState (\o ->
            { todoList: addNewItem l })
        _      -> T.modifyState (\o ->
            { todoList : addNewItem blankToDoList })

-- | Send a ToDoItem to the server
sendItem :: ToDoItem -> T.Action _ State Unit
sendItem (ToDoItem i) =
    case i._todoIdent of
        Just ident -> do
            r <- T.async <<< responseOnly $ putitemsWithUuid ident (encode $ ToDoItem i)
            case decode r :: Maybe ToDoList of
                Just l -> T.modifyState (\o ->
                    { todoList: addNewItem l })
                _      -> T.modifyState (\o ->
                    { todoList : addNewItem blankToDoList })
        _ -> do
            r <- T.async <<< responseOnly $ postitems (encode $ ToDoItem i)
            case decode r :: Maybe ToDoList of
                Just l -> T.modifyState (\o ->
                    { todoList: addNewItem l })
                _      -> T.modifyState (\o ->
                    { todoList : addNewItem blankToDoList })

-- * DOM events

-- | onchange
handleTodoChange :: T.FormEvent -> Action
handleTodoChange e = UpdateText (strToIdent $ getToDoItemIdent e) (getValue e)
  where
    todoItem = getToDoItemFromUI (getToDoItemIdent e)

-- | onkeyup
handleTodoKeyUp :: T.KeyboardEvent -> Action
handleTodoKeyUp e = case getKeyCode e of
    13 -> SubmitItem todoItem
    _  -> DoNothing
  where
    todoItem = getToDoItemFromUI (getToDoItemIdent e)

-- | onblur
handleTodoBlur :: T.FocusEvent -> Action
handleTodoBlur e = SubmitItem todoItem
  where
    todoItem = getToDoItemFromUI (getToDoItemIdent e)

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

foreign import getValue 
  "function getValue(e) {\
  \  return e.target.value;\
  \}" :: forall event. event -> String

foreign import getToDoItemIdent
"""
function getToDoItemIdent (e) {
    if (e.target.getAttribute('data-todoitem')) {
        return e.target.getAttribute('data-todoitem');
    }
    return "";
}
""" :: forall event. event -> String

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
render ctx s _ = T.div [ A.className "app-container" ] [ T.ul' current ]
  where
    current  = itemList s.todoList
    itemList (ToDoList l) = itemRow <$> l._todoItems
    itemRow :: ToDoItem -> T.Html _
    itemRow (ToDoItem i) = T.li' $
        [ T.input [ A._type "text"
                  , A.className "item-row"
                  , A.value i._todoText
                  , T.onChange ctx handleTodoChange
                  , T.onKeyUp  ctx handleTodoKeyUp
                  , T.onBlur   ctx handleTodoBlur
                  , _dataToDoItem $ ToDoItem i ] []
        , T.input ([ A._type "checkbox"
                   , _dataToDoItem $ ToDoItem i ] <> checkedParam) []
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
