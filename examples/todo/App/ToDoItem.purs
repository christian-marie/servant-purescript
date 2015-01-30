module App.ToDoItem where

import Data.JSON
import Data.Maybe
import Data.Monoid

-- * Data types

type UUID = String

-- | Type for a single To Do Item
data ToDoItem = ToDoItem {
                  _todoIdent :: Maybe UUID,
                  _todoText  :: String,
                  _todoDone  :: Boolean }

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
