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
import Data.Char
import Data.List
import Data.Monoid
import Servant.JQuery

-- | PureScript rendering settings
data PSSettings = PSSettings {
    _baseURL :: String -- ^ Base URL for AJAX requests
}

makeLenses ''PSSettings

-- | Generate a PureScript module containing a list of functions for
-- AJAX requests.
generatePSModule
    :: PSSettings -- ^ PureScript rendering settings
    -> String -- ^ Name of PureScript module
    -> [AjaxReq] -- ^ List of AJAX requests to render in module
    -> String -- ^ Rendered PureScript module
generatePSModule settings mname reqs = unlines
        [ "module " <> mname <> " where"
        , ""
        , "import Data.Foreign"
        , "import Data.Function"
        , "import Data.Maybe"
        , "import Data.Monoid"
        , ""
        , "foreign import encodeURIComponent :: String -> String"
        , ""
        , intercalate "\n" (map (generatePS settings) reqs)
        , ""
        , ajaxImpl
        ]

-- | Generate a single PureScript function for an AJAX request.
generatePS
    :: PSSettings -- ^ PureScript rendering settings
    -> AjaxReq -- ^ AJAX request to render
    -> String -- ^ Rendered PureScript
generatePS settings req = unsafeAjaxRequest
  where
    args = suppliedArgs <> ["onSuccess", "onError"]
    
    suppliedArgs = captures <> queryArgs <> body <> headerArgs

    captures = map captureArg . filter isCapture $ req ^. reqUrl.path
    queryArgs  = map (view argName) queryParams
    headerArgs = map (toValidFunctionName . (<>) "header" . headerArgName) hs
    
    hs = req ^. reqHeaders
    
    queryParams = req ^.. reqUrl.queryStr.traverse
    
    body = ["body" | req ^. reqBody]
    
    fname = req ^. funcName
    
    method = req ^. reqMethod
    
    htname = capitalise (fname <> "Headers")
    
    unsafeAjaxRequest = unlines
        [ typeSig
        , fname <> " " <> argString <> " = do"
        , "    runFn7 ajaxImpl url method headers b isJust onSuccess onError"
        , "  where"
        , "    url = \"" <> urlString
        , "    method = \"" <> method <> "\""
        , "    headers = " <> htname <> " " <> intercalate " " headerArgs
        , "    b = " <> bodyString
        ]
      where
        typeSig = concat
            [ fname
            , " :: forall eff. "
            , intercalate " -> " $ map (\_ -> "String") suppliedArgs
            , " (a -> b) -> (a -> b) -> eff Unit"
            ]
        argString = intercalate " " args
        urlString = concat
            [ "\""
            , settings ^. baseURL
            , "\""
            , psPathSegments $ req ^.. reqUrl.path.traverse
            , if null queryParams then "" else "\"?\" <> " <> psParams queryParams
            ]
        bodyString = if req ^. reqBody then "(Just body)" else "Nothing"

ajaxImpl :: String
ajaxImpl = unlines
    [ "foreign import ajaxImpl"
    , "\"\"\""
    , "function ajaxImpl(url, method, headers, body, isJust, onSuccess, onError){"
    , "return function(){"
    , "$.ajax({"
    , "  url: \" + url + \","
    , ", type: \" + method+ \","
    , ", success: onSuccess,"
    , ", error: onError,"
    , ", headers: headers"
    , ", data: (isJust(body) ? JSON.stringify(body) : null)"
    , "});"
    , "};"
    , "}"
    , "\"\"\" :: forall a eff. Fn7 (String) (String) (a) (Maybe String) (Maybe String -> Bool) (?) (?) (eff Unit}"
    ]

-- | Default PureScript settings: specifies an empty base URL
defaultSettings :: PSSettings
defaultSettings = PSSettings ""

-- | Capitalise a string for use in PureScript variable name
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = [toUpper x] <> xs

-- | Turn a list of path segments into a URL string
psPathSegments :: [Segment] -> String
psPathSegments = (<> "/") . intercalate " <> \"/\" <> " . map psSegmentToStr

-- | Turn an individual path segment into a PureScript variable handler
psSegmentToStr :: Segment -> String
psSegmentToStr (Static s) = "\"" <> s <> "\""
psSegmentToStr (Cap s)    = "encodeURIComponent " <> s

-- | Turn a list of query string params into a URL string
psParams :: [QueryArg] -> String
psParams = intercalate " <> \"&\" <> " . map psParamToStr

-- | Turn an individual query string param into a PureScript variable handler
psParamToStr :: QueryArg -> String
psParamToStr qarg =
  case qarg ^. argType of
    Normal -> "\"" <> name <> "=\" <> encodeURIComponent " <> name
    Flag   -> "\"" <> name <> "=\""
    List   -> "\"" <> name <> "[]=\" <> encodeURIComponent " <> name
  where name = qarg ^. argName

