module App.ToDoItem where

import qualified Data.Array as Arr
import Data.Foldable
import Data.JSON
import Data.List.Utils
import Data.Maybe
import Data.Monoid
import Debug.Foreign

-- * Data types

-- | Type alias for server-side UUIDs
type UUID = String

-- | Type for a single To Do Item
data ToDoItem = ToDoItem {
                  _todoIdent :: Maybe UUID,
                  _todoText  :: String,
                  _todoDone  :: Boolean}

-- | Eq instance for ToDoItem
instance todoItemEq :: Eq ToDoItem where
    (==) (ToDoItem a) (ToDoItem b) = and $
        [ a._todoIdent == b._todoIdent
        , a._todoText  == b._todoText
        , a._todoDone  == b._todoDone ]
    (/=) a b = (a == b) == false

-- | Show instance for ToDoItem
instance todoItemShow :: Show ToDoItem where
    show (ToDoItem a) = "(ToDoItem "
            <> show a._todoIdent
            <> " "
            <> a._todoText
            <> " "
            <> show a._todoDone
            <> ")"

-- | Marshalls ToDoItem to JSON
instance todoItemToJSON :: ToJSON ToDoItem where
    toJSON (ToDoItem i) = object $
        [ "ident" .= jsonIdent i._todoIdent
        , "text"  .= i._todoText
        , "done"  .= i._todoDone ]

-- | Marshalls UUID to JSON
jsonIdent :: Maybe UUID -> JValue
jsonIdent u = case u of
    Just u' -> toJSON u'
    _       -> JNull

-- | Unmarshalls ToDoItem from JSON
instance todoItemFromJSON :: FromJSON ToDoItem where
    parseJSON (JObject o) = do
        ident <- o .:? "ident"
        text  <- o .: "text"
        done  <- o .: "done"
        return $ ToDoItem { _todoIdent: ident, _todoText: text, _todoDone: done }
    parseJSON _          = fail "Cannot parse ToDoItem"

-- | Type covering an entire To Do List
newtype ToDoList = ToDoList { _todoItems :: [ToDoItem] }

-- | Marshalls ToDoList from JSON
instance todoListFromJSON :: FromJSON ToDoList where
    parseJSON (JObject o) = do
        list <- o .: "_todoItems"
        return $ ToDoList { _todoItems: list }
    parseJSON _          = fail "Cannot parse ToDoList"

-- | Generates an empty ToDoList
blankToDoList :: ToDoList
blankToDoList = ToDoList { _todoItems: [] }

-- | Generates a fresh ToDoItem
newToDoItem :: ToDoItem
newToDoItem = ToDoItem { _todoIdent: Nothing, _todoText: "", _todoDone: false }

-- | Insert a blank ToDoItem at the bottom of a ToDoList
addNewItem :: ToDoList -> ToDoList
addNewItem (ToDoList l) = ToDoList { _todoItems: l._todoItems <> [newToDoItem] }

-- | Update ToDoList item text
updateListItemText :: ToDoList -> Maybe UUID -> String -> ToDoList
updateListItemText (ToDoList l) u t = ToDoList { _todoItems: newToDoItems l }
  where
    newToDoItems l =
        case getItemByUuid (ToDoList l) u of
            Just item -> replace [item] [makeNewItem item] l._todoItems
            _         -> l._todoItems <> [makeNewItem newToDoItem]
    makeNewItem (ToDoItem it) = ToDoItem { _todoIdent: u, _todoText: t, _todoDone: it._todoDone }

-- | Get item from a list by UUID
getItemByUuid :: ToDoList -> Maybe UUID -> Maybe ToDoItem
getItemByUuid (ToDoList l) i = case Arr.filter isThisIt l._todoItems of
    (it:_) -> Just it
    _      -> Nothing
  where
    isThisIt (ToDoItem it) = it._todoIdent == i

-- | Filter to a given state
filterState :: Boolean -> ToDoList -> ToDoList
filterState state (ToDoList l) = ToDoList { _todoItems: Arr.filter inStateF l._todoItems }
  where
    inStateF (ToDoItem i) = isJust i._todoIdent && i._todoDone == state

-- | Number of items in a given state
inState :: Boolean -> ToDoList -> Number
inState state (ToDoList l) = Arr.length $
    Arr.filter inStateF l._todoItems
  where
    inStateF (ToDoItem i) = isJust i._todoIdent && i._todoDone == state

