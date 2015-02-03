module Test.Main where

import Control.Monad.Error.Class
import Data.Array.Unsafe
import Data.Either
import Data.List.Utils
import Data.Maybe
import Data.Maybe.Unsafe
import Test.Unit

import App.ToDoItem

main = runTest do
    test "peeks at a list properly" do
        let testList = ToDoList {
                _todoItems: [ ToDoItem { _todoIdent: (Just "foo"), _todoText: "Foo", _todoDone: false } 
                            , ToDoItem { _todoIdent: (Just "bar"), _todoText: "Bar", _todoDone: false } ]}
        assert "should be foo" $ 
            ((getItemByUuid testList (Just "foo")) == (Just $ ToDoItem { _todoIdent: (Just "foo"), _todoText: "Foo", _todoDone: false }))
    test "peeks at a list properly 2" do
        let testList = ToDoList {
                _todoItems: [ ToDoItem { _todoIdent: (Just "foo"), _todoText: "Foo", _todoDone: false } 
                            , ToDoItem { _todoIdent: Nothing, _todoText: "Bar", _todoDone: false } ]}
        assert "should be foo" $
            ((getItemByUuid testList Nothing) == (Just $ ToDoItem { _todoIdent: Nothing, _todoText: "Bar", _todoDone: false }))
    test "replaces ToDoItems in a list properly" do
        let testList = ToDoList {
                _todoItems: [ ToDoItem { _todoIdent: (Just "foo"), _todoText: "Foo", _todoDone: false } 
                            , ToDoItem { _todoIdent: (Just "bar"), _todoText: "Bar", _todoDone: false } ]}
        case updateListItemText testList (Just "foo") "BAZ BAZ BAZ" of
            ToDoList l ->
                case head l._todoItems of
                    ToDoItem i ->
                        assert "text of first entry should have changed" $ i._todoText == "BAZ BAZ BAZ"
                    _ -> throwError "not a ToDoItem"
            _ -> throwError "not a ToDoList"
    test "replaces ToDoItems in a weird list properly" do
        let ll = [ ToDoItem { _todoIdent: (Just "bumf"), _todoText: "Foo", _todoDone: false } 
                 , ToDoItem { _todoIdent: Nothing, _todoText: "Bar", _todoDone: false } ]
        let testList = ToDoList { _todoItems: ll }
        case updateListItemText testList Nothing "BAZ BAZ BAZ" of
            ToDoList l ->
                case last l._todoItems of
                    ToDoItem i ->
                        assert "text of last entry should have changed" $ i._todoText == "BAZ BAZ BAZ"
                    _ -> throwError "not a ToDoItem"
            _ -> throwError "not a ToDoList"
