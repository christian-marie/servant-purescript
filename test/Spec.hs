{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.Either (isRight)
import Data.Maybe
import Data.Proxy
import qualified Language.PureScript as P
import System.Process
import Test.Hspec
import qualified Text.Parsec as TP
import qualified Text.Parsec.Expr as TP

import Servant.API
import Servant.JQuery
import Servant.PureScript

type TestAPI = [sitemap|
POST    /simple                  String -> Bool
GET     /has.extension           Bool
|]

type TopLevelRawAPI = "something" :> Get Int
                  :<|> Raw

type HeaderHandlingAPI = "something" :> Header "Foo" String
                                     :> Get Int

headerHandlingProxy :: Proxy HeaderHandlingAPI
headerHandlingProxy = Proxy

spec = describe "Servant.JQuery"
    generateJSSpec

generateJSSpec :: Spec
generateJSSpec = describe "generateJS" $ do
    it "should generate valid javascript" $ do
        let (postSimple :<|> getHasExtension ) = jquery (Proxy :: Proxy TestAPI)

        let r1 = generatePSModule defaultSettings "Foo" [postSimple]
        print r1
        
        let r2 = generatePSModule defaultSettings "Foo" [getHasExtension]
        print r2

        let out1 = do
                l <- P.lex "" r1
                TP.runParser P.parseModule (P.ParseState 0) "" l

        let out2 = do
                l <- P.lex "" r2
                TP.runParser P.parseModule (P.ParseState 0) "" l

        out1 `shouldSatisfy` isRight
        out2 `shouldSatisfy` isRight

    it "should use non-empty function names" $ do
        let (_ :<|> topLevel) = jquery (Proxy :: Proxy TopLevelRawAPI)
        
        let m = generatePSModule defaultSettings "Foo" [topLevel "GET"]
        print m
        
        let out = do
                l <- P.lex "" m
                TP.runParser P.parseModule (P.ParseState 0) "" l
        
        out `shouldSatisfy` isRight




main :: IO ()
main = hspec spec

