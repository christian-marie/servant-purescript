{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens               hiding ((.=))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Data.Aeson
import qualified Data.Foldable              as F
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import qualified Data.Sequence              as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.UUID
import           Data.UUID.V4
import           GHC.Generics
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Servant.Common.Text
import           Servant.JQuery
import           Servant.PureScript
import           System.FilePath
import           System.FilePath.Glob
import           System.Process

-- * JSON & Text translation for UUID

instance ToJSON UUID where
    toJSON = String . T.pack . toString

instance FromJSON UUID where
    parseJSON (String s) = return . fromJust . fromString . T.unpack $ s
    parseJSON _          = error "Cannot parse UUID"

instance FromText UUID where
    fromText = fromString . T.unpack

instance ToText UUID where
    toText = T.pack . toString

-- * ToDoList types

-- | Type for a single To Do Item
data ToDoItem = ToDoItem
    { _todoIdent :: Maybe UUID
    , _todoText  :: Text
    , _todoDone  :: Bool
    } deriving (Eq, Show)

makeLenses ''ToDoItem

instance ToJSON ToDoItem where
    toJSON i = object
                [ T.pack "ident" .= ident
                , T.pack "text"  .= (String $ i ^. todoText)
                , T.pack "done"  .= (i ^. todoDone)
                ]
      where
        ident = case i ^. todoIdent of
            Just i' -> toJSON i'
            _       -> Null

instance FromJSON ToDoItem where
    parseJSON (Object o) = ToDoItem <$> o .:? "ident" <*> o .: "text" <*> o .: "done"
    parseJSON _          = error "Cannot parse ToDoItem"

-- | Type covering an entire To Do List
newtype ToDoList = ToDoList { _todoItems :: [ToDoItem] }
    deriving (Generic, Eq, Show)

makeLenses ''ToDoList

instance ToJSON ToDoList

-- * Application State

-- | Start app state
newList :: IO (TVar ToDoList)
newList = newTVarIO $ ToDoList []

-- * Internal API

-- | Get list
getList :: MonadIO m => TVar ToDoList -> ExceptT String m ToDoList
getList = liftIO . readTVarIO

-- | Add item to item list
addItem :: MonadIO m => TVar ToDoList -> ToDoItem -> ExceptT String m ToDoList
addItem l i = do
    uuid <- liftIO nextRandom
    liftIO . atomically $ do
        oldList <- readTVar l
        let i' = i { _todoIdent = Just uuid }
        let n = ToDoList $ (oldList ^. todoItems) <> [i']
        writeTVar l n
        return n

-- | Update a single item
updateItem :: MonadIO m => TVar ToDoList -> ToDoItem -> ExceptT String m ToDoList
updateItem l i = do
    oldList <- liftIO $ readTVarIO l
    oldItem <- tryGetExistingItem l i
    liftIO . atomically $ do
        let u = ToDoList . F.toList .
                S.update (fromJust $ elemIndex oldItem (oldList ^. todoItems)) i .
                S.fromList $ oldList ^. todoItems
        writeTVar l u
        return u

-- | Delete a single item
deleteItem :: MonadIO m => TVar ToDoList -> ToDoItem -> ExceptT String m ToDoList
deleteItem l i = do
    oldList <- liftIO $ readTVarIO l
    oldItem <- tryGetExistingItem l i
    liftIO . atomically $ do
        let delList = ToDoList (delete oldItem (oldList ^. todoItems))
        writeTVar l delList
        return delList

-- | Try and get an existing item
tryGetExistingItem :: (MonadIO m) => TVar ToDoList -> ToDoItem -> ExceptT String m ToDoItem
tryGetExistingItem l i = do
    oldList <- liftIO $ readTVarIO l
    let oldItem = hasExistingItem oldList i
    unless (isJust oldItem) (throwError "Not found")
    return . fromJust $ oldItem

-- | Perform UUID equivalence operation find on todo list items
hasExistingItem :: ToDoList -> ToDoItem -> Maybe ToDoItem
hasExistingItem l i = find existingOp (l ^. todoItems)
  where
    existingOp x = x ^. todoIdent == i ^. todoIdent

-- * Servant API

type ToDoAPI = "items" :> Get ToDoList
          :<|> "items" :> ReqBody ToDoItem :> Post ToDoList
          :<|> "items" :> Capture "uuid" UUID :> Get ToDoItem
          :<|> "items" :> Capture "uuid" UUID :> ReqBody ToDoItem :> Put ToDoList
          :<|> "items" :> Capture "uuid" UUID :> Delete
          :<|> Raw

toDoAPI :: Proxy ToDoAPI
toDoAPI = Proxy

-- * Servant Server

appPath :: FilePath
appPath = "examples/todo"

tmp :: FilePath
tmp = appPath </> "temp"

www :: FilePath
www = appPath </> "www"

server :: TVar ToDoList -> Server ToDoAPI
server l = gl :<|> ai :<|> gi :<|> ui :<|> di :<|> serveDirectory www
  where
    gl = do
        r <- runExceptT $ getList l
        case r of
            Right list -> return list
            Left e     -> throwError (500, e)
    ai i = do
        r <- runExceptT $ addItem l i
        case r of
            Right list -> return list
            Left e     -> throwError (500, e)
    gi :: UUID -> EitherT (Int, String) IO ToDoItem
    gi u = do
        r <- runExceptT $ getList l
        case r of
            Right list -> do
                let testItem = ToDoItem (Just u) "" False
                let actualItem = hasExistingItem list testItem
                case actualItem of
                    Just i -> return i
                    _      -> throwError (404, "Not found")
            Left e     -> throwError (500, e)
    ui u i = do
        let i' = i { _todoIdent = Just u }
        r <- runExceptT $ updateItem l i'
        case r of
            Right list            -> return list
            Left "Item not found" -> throwError (404, "Not found")
            Left e                -> throwError (500, e)
    di u = do
        let testItem = ToDoItem (Just u) "" False
        r <- runExceptT $ deleteItem l testItem
        case r of
            Right _               -> return ()
            Left "Item not found" -> throwError (404, "Not found")
            Left e                -> throwError (500, e)

runServer :: TVar ToDoList -> Int -> IO ()
runServer l port = do
    putStrLn $ "About to run on " <> show port
    run port (serve toDoAPI $ server l)

-- * Generating JS

jsGL :<|> jsAI :<|> jsGI :<|> jsUI :<|> jsDI :<|> _ = jquery toDoAPI

writePS :: FilePath -> [AjaxReq] -> IO ()
writePS fp = writeFile fp . generatePSModule defaultSettings "App.Ajax"

-- * Main

-- | Download and get Bower dependencies
bower :: IO [FilePath]
bower = do
    void . system $ "cd " <> appPath <> " && bower prune && bower install"
    (matches, _) <- globDir [compile $ appPath </> "bower_components/*/src/**/*.purs"] "."
    return $ head matches

-- | Run PureScript compiler
psc :: [FilePath] -> [FilePath] -> FilePath -> IO ()
psc dep mod dest = do
    let cmd = concat
            [ "psc --module=App --main=App "
            , unwords dep
            , " "
            , unwords mod
            , " > "
            , dest ]
    putStrLn cmd
    void $ system cmd

-- | App files
appFiles :: FilePath -> IO [FilePath]
appFiles src = do
    (matches, _) <- globDir [compile "**.purs", compile "**/*.purs"] src
    return . nub . concat $ matches

-- | Build and run everything
main :: IO ()
main = do
    let ajaxPS = tmp </> "App.Ajax.purs"
    let appPS = appPath </> "App.purs"
    let appJS = www </> "app.js"

    -- Write Purescript module
    writePS ajaxPS [ jsGL, jsAI, jsGI, jsUI, jsDI ]

    -- Get dependencies and compile PureScript
    sourceFiles <- bower
    af <- appFiles (appPath </> "App")
    df <- appFiles (appPath </> "Data")
    psc sourceFiles (af <> df <> [ajaxPS, appPS]) appJS

    -- Initialise state and run app
    newList >>= flip runServer 8080
