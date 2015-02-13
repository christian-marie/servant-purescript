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

import           Control.Arrow
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
        , "import Data.Foldable"
        , "import Data.Foreign"
        , "import Data.Function"
        , "import Data.Maybe"
        , "import Data.Maybe.Unsafe (fromJust)"
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
    [ headerType
    , "\n\n"
    , unsafeAjaxRequest
    ]
  where
    args = suppliedArgs <> ["onSuccess", "onError"]

    suppliedArgs = captures <> queryArgs <> body <> fmap snd headerArgTuples

    captures = fmap captureArg . filter isCapture $ req ^. reqUrl.path
    queryArgs  = fmap (view argName) queryParams
    headerArgTuples = fmap (decapitalise . headerArgName &&& toValidFunctionName . (<>) "header" . headerArgName) (req ^. reqHeaders)

    fname = req ^. funcName
         <> if null captures then "" else "With"
         <> intercalate "And" (fmap capitalise captures)

    queryParams = req ^.. reqUrl.queryStr.traverse

    body = ["body" | req ^. reqBody]

    htname = capitalise (fname <> "Headers")

    headerType = concat
        ([ "type "
         , htname
         , " = "
         ] <> hfields)
    hfields = [" { ", intercalate ", " hfieldNames, " }"]
    hfieldNames = fmap toHField (htDefaults <> fmap fst headerArgTuples)
    htDefaults = ["content_Type", "accept"]
    toHField h = h <> " :: String"

    unsafeAjaxRequest = unlines
        [ typeSig
        , fname <> " " <> argString <> " ="
        , "    runFn8 ajaxImpl url method headers b isJust fromJust onSuccess onError"
        , "  where"
        , "    url = " <> urlString
        , "    method = \"" <> req ^. reqMethod <> "\""
        , "    headers = " <> " { "
            <> intercalate ", " (fmap wrapDefault htDefaults)
            <> (if null headerArgTuples then " " else ", ")
            <> intercalate ", " (fmap wrapHeader headerArgTuples) <> " }"
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
            , if null queryParams then "\"" else "?\" <> " <> psParams queryParams
            ]
        bodyString = if req ^. reqBody then "(Just body)" else "Nothing"
        wrapDefault h = h <> ": \"application/json\""
        wrapHeader (h, hv)  = h <> ": " <> hv

ajaxImpl :: String
ajaxImpl = unlines
    [ "foreign import ajaxImpl"
    , "\"\"\""
    , "function ajaxImpl(url, method, headers, body, isJust, fromJust, onSuccess, onError){"
    , "return function(){"
    , "var capitalise = function(s) { return s.charAt(0).toUpperCase() + s.slice(1); }"
    , "var filterHeaders = function(obj) {"
    , "var result = {};"
    , "for(var i in obj) if(obj.hasOwnProperty(i)) result[capitalise(i.replace(/_/, '-'))] = obj[i];"
    , "return result;"
    , "};"
    , "$.ajax({"
    , "  url: url"
    , ", type: method"
    , ", success: function(d, s, x){ onSuccess(d)(s)(x)(); }"
    , ", error: function(x, s, d){ onError(x)(s)(d)(); }"
    , ", headers: filterHeaders(headers)"
    , ", data: (isJust(body) ? fromJust(body) : null)"
    , "});"
    , "return {};"
    , "};"
    , "}"
    , "\"\"\" :: forall eff h. Fn8 URL Method h (Maybe Body) (Maybe Body -> Boolean) (Maybe Body -> Body) (SuccessFn eff) (FailureFn eff) (Eff (xhr :: XHREff | eff) Unit)"
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

-- | Decapitalise a string for use as a Purescript variable name
decapitalise :: String -> String
decapitalise [] = []
decapitalise (x:xs) = [toLower x] <> xs

-- | Turn a list of path segments into a URL string
psPathSegments :: [Segment] -> String
psPathSegments = intercalate "/" . fmap psSegmentToStr

-- | Turn an individual path segment into a PureScript variable handler
psSegmentToStr :: Segment -> String
psSegmentToStr (Static s) = s
psSegmentToStr (Cap s)    = "\" <> encodeURIComponent " <> s <> " <> \""

-- | Turn a list of query string params into a URL string
psParams :: [QueryArg] -> String
psParams qa = "intercalate \"&\" [" <> intercalate ", " (fmap psParamToStr qa) <> "]"

-- | Turn an individual query string param into a PureScript variable handler
psParamToStr :: QueryArg -> String
psParamToStr qarg =
  case qarg ^. argType of
    Normal -> "\"" <> name <> "=\" <> encodeURIComponent " <> name
    Flag   -> "\"" <> name <> "=\""
    List   -> "\"" <> name <> "[]=\" <> encodeURIComponent " <> name
  where name = qarg ^. argName

