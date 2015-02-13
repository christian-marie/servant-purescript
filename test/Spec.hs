{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Monad
import           Data.Either         (isRight)
import           Data.Proxy
import qualified Language.PureScript as P
import           Test.Hspec
import qualified Text.Parsec         as TP

import           Servant.API
import           Servant.JQuery
import           Servant.PureScript

type TestAPI = "simple" :> ReqBody String :> Post Bool
          :<|> "has.extension" :> Get Bool

type TopLevelRawAPI = "something" :> Get Int
                  :<|> Raw

type HeaderHandlingAPI = "something" :> Header "Foo" String :> Get Int
                  :<|> Raw

headerHandlingProxy :: Proxy HeaderHandlingAPI
headerHandlingProxy = Proxy

main :: IO ()
main = hspec .
    describe "generateJS" $ do
        it "should generate valid purescript" $ do
            let (postSimple :<|> getHasExtension ) = jquery (Proxy :: Proxy TestAPI)
            shouldParse $ generatePSModule defaultSettings "Foo" [postSimple]
            shouldParse $ generatePSModule defaultSettings "Foo" [getHasExtension]
        it "should use non-empty function names" $ do
            let (_ :<|> topLevel) = jquery (Proxy :: Proxy TopLevelRawAPI)
            let m = generatePSModule defaultSettings "Foo" [topLevel "GET"]
            shouldParse m
            m `shouldContain` "get :: "
        it "should generate valid header variables" $ do
            let (gs :<|> _) = jquery headerHandlingProxy
            let m = generatePSModule defaultSettings "Bar" [gs]
            shouldParse m
            m `shouldContain` "foo: headerFoo"

shouldParse :: String -> Expectation
shouldParse =
    (`shouldSatisfy` isRight) . (TP.runParser P.parseModule (P.ParseState 0) "" <=< P.lex "")
