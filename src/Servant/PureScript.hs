{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.PureScript (
  jquery,
  generatePSModule,
  generatePS,
  PSSettings(..),
  baseURL,
  defaultSettings
) where

import Control.Lens
import Data.List
import Data.Monoid
import Servant.JQuery

data PSSettings = PSSettings {
    _baseURL :: String
}

makeLenses ''PSSettings

generatePSModule :: PSSettings -> String -> [AjaxReq] -> String
generatePSModule settings mname reqs = "module " <> mname <> " where"
    <> "\n"
    <> "\n" <> intercalate "\n" (map (generatePS settings) reqs)

generatePS :: PSSettings -> AjaxReq -> String
generatePS settings req = "\n"
    <> "foreign import " <> fname
    <> "\n" <> "\"\"\""
    <> "\n" <> jsFun
    <> "\n" <> "\"\"\" :: " <> typeDec
    <> "\n"
  where
    args = captures
        <> map (view argName) queryParams
        <> body
        <> map (toValidFunctionName . (<>) "header" . headerArgName) hs
        <> ["onSuccess", "onError"]
    
    captures = map captureArg . filter isCapture $ req ^. reqUrl.path
    
    hs = req ^. reqHeaders
    
    queryParams = req ^.. reqUrl.queryStr.traverse
    
    body = ["body" | req ^. reqBody]
    
    fname = req ^. funcName
    
    method = req ^. reqMethod
    
    url = settings ^. baseURL <> urlArgs <> queryArgs
    
    urlArgs = jsSegments $ req ^.. reqUrl.path.traverse
    
    queryArgs = if null queryParams then "" else " + \"?" <> jsParams queryParams <> "\""
    
    dataBody = if req ^. reqBody then "data: JSON.stringify(body)," else ""
    
    renderedReqHeaders =
        if null hs
            then ""
            else "headers: {" <> headersStr <> "},"
    
    headersStr = intercalate "," (map headerStr hs)
    headerStr :: HeaderArg -> String
    headerStr h = "\"" <> headerArgName h <> "\": " <> show h
    
    typeDec = "forall a b eff. Fn" <> show (length args) <> " Foreign "
        <> unwords (map toTypeDec args) <> " (eff Unit)"
    
    toTypeDec "onSuccess" = "(b)"
    toTypeDec "onError"   = "(b)"
    toTypeDec _           = "(a)"
    
    jsFun = "function " <> fname <> " (" <> intercalate "," args <> ") {"
        <> "\n" <> "return function(){"
        <> "\n" <> jsFunInternal
        <> "\n" <> "};"
        <> "\n" <> "}"

    jsFunInternal = "$.ajax({"
        <> "\n" <> "url: \"" <> url <> "\","
        <> "\n" <> "type: \"" <> method <> "\","
        <> "\n" <> "success: onSuccess,"
        <> "\n" <> "error: onError,"
        <> "\n" <> renderedReqHeaders
        <> "\n" <> dataBody
        <> "\n" <> "});"

defaultSettings :: PSSettings
defaultSettings = PSSettings ""


