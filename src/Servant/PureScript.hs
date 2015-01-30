{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Servant.PureScript (
  jquery,
  generatePSModule,
  generatePS,
  PSSettings(..),
  baseURL,
  defaultSettings
) where

import           Control.Lens
import           Data.Char
import           Data.List
import           Data.Monoid
import           Servant.JQuery

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
        , "import Control.Monad.Eff"
        , "import Data.Foreign"
        , "import Data.Function"
        , "import Data.Maybe"
        , "import Data.Monoid"
        , ""
        , "foreign import encodeURIComponent :: String -> String"
        , ""
        , xhrType
        , commonAliases
        , funcTypes
        , ""
        , intercalate "\n" (fmap (generatePS settings) reqs)
        , ""
        , ajaxImpl
        ]

-- | Generate a single PureScript function for an AJAX request.
-- To prevent conflicts, generates a unique function name for every available
-- function name and set of captures.
generatePS
    :: PSSettings -- ^ PureScript rendering settings
    -> AjaxReq -- ^ AJAX request to render
    -> String -- ^ Rendered PureScript
generatePS settings req = concat
    [ unsafeAjaxRequest
    , "\n\n"
    , headerType
    ]
  where
    args = suppliedArgs <> ["onSuccess", "onError"]

    suppliedArgs = captures <> queryArgs <> body <> headerArgs

    captures = fmap captureArg . filter isCapture $ req ^. reqUrl.path
    queryArgs  = fmap (view argName) queryParams
    headerArgs = fmap (toValidFunctionName . (<>) "header" . headerArgName) $ req ^. reqHeaders

    fname = req ^. funcName
         <> if null captures then "" else "With"
         <> intercalate "And" (fmap capitalise captures)

    queryParams = req ^.. reqUrl.queryStr.traverse

    body = ["body" | req ^. reqBody]

    htname = capitalise (fname <> "Headers")

    headerType = concat
        ([ "data "
         , htname
         , " = "
         , htname
         ] <> hfields)
    hfields = if null headerArgs
                then []
                else [" { ", intercalate ", " $ fmap toHField headerArgs, " }"]
    toHField h = h <> " :: String"

    unsafeAjaxRequest = unlines
        [ typeSig
        , fname <> " " <> argString <> " ="
        , "    runFn7 ajaxImpl url method headers b isJust onSuccess onError"
        , "  where"
        , "    url = " <> urlString
        , "    method = \"" <> req ^. reqMethod <> "\""
        , "    headers = " <> htname <> " " <> unwords headerArgs
        , "    b = " <> bodyString
        ]
      where
        typeSig = concat
            [ fname
            , " :: forall eff. "
            , intercalate " -> " $ fmap (const "String") suppliedArgs
            , argLink
            , "(SuccessFn eff) -> (FailureFn eff) -> (Eff (xhr :: XHREff | eff) Unit)"
            ]
        argLink = if null suppliedArgs then "" else " -> "
        argString = unwords args
        urlString = concat
            [ "\""
            , settings ^. baseURL
            , "/"
            , psPathSegments $ req ^.. reqUrl.path.traverse
            , "\""
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
    , "  url: url"
    , ", type: method"
    , ", success: onSuccess"
    , ", error: onError"
    , ", headers: headers"
    , ", data: (isJust(body) ? JSON.stringify(body) : null)"
    , "});"
    , "return {};"
    , "};"
    , "}"
    , "\"\"\" :: forall eff h. Fn7 URL Method h (Maybe Body) (Maybe Body -> Boolean) (SuccessFn eff) (FailureFn eff) (Eff (xhr :: XHREff | eff) Unit)"
    ]

-- | Type aliases for common things
commonAliases :: String
commonAliases = unlines
    [ "type URL = String"
    , "type Method = String"
    , "type Body = String"
    , "type Status = String"
    , "type ResponseData = String"
    ]

-- | Type for XHR
xhrType :: String
xhrType = unlines
    [ "foreign import data XHR :: *"
    , "foreign import data XHREff :: !"
    ]

-- | Type aliases for success & failure functions
funcTypes :: String
funcTypes = unlines
    [ "type SuccessFn eff = (ResponseData -> Status -> XHR -> (Eff (xhr :: XHREff | eff) Unit))"
    , "type FailureFn eff = (XHR -> Status -> ResponseData -> (Eff (xhr :: XHREff | eff) Unit))"
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
psPathSegments = intercalate "/" . fmap psSegmentToStr

-- | Turn an individual path segment into a PureScript variable handler
psSegmentToStr :: Segment -> String
psSegmentToStr (Static s) = s
psSegmentToStr (Cap s)    = "\" <> encodeURIComponent " <> s <> " <> \""

-- | Turn a list of query string params into a URL string
psParams :: [QueryArg] -> String
psParams = intercalate " <> \"&\" <> " . fmap psParamToStr

-- | Turn an individual query string param into a PureScript variable handler
psParamToStr :: QueryArg -> String
psParamToStr qarg =
  case qarg ^. argType of
    Normal -> "\"" <> name <> "=\" <> encodeURIComponent " <> name
    Flag   -> "\"" <> name <> "=\""
    List   -> "\"" <> name <> "[]=\" <> encodeURIComponent " <> name
  where name = qarg ^. argName

