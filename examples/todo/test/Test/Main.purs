module Test.Main where

import Test.Unit

main = runTest do
    test "replaces ToDoItems in a list properly" $ do
        let testList = ToDoList {
            _todoItems: [ ToDoItem { _todoIdent: (Just "foo"), _todoText: "Foo", _todoDone: false } 
                        , ToDoItem { _todoIdent: (Just "bar"), _todoText: "Bar", _todoDone: false } 
                        ] }
        let newList = updateListItemText testList (Just "foo") "BAZ BAZ BAZ"
        assert "text of first entry should have changed" $ (head newList._todoItems)._todoText == "BAZ BAZ BAZ"