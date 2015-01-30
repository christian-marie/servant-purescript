module App where

import Control.Monad.Eff
import Data.Foreign
import Data.Function
import Data.Maybe
import Data.Monoid

foreign import encodeURIComponent :: String -> String

foreign import data XHR :: *
foreign import data XHREff :: !

type URL = String
type Method = String
type Body = String
type Status = String
type ResponseData = String

type SuccessFn eff = (ResponseData -> Status -> XHR -> (Eff (xhr :: XHREff | eff) Unit))
type FailureFn eff = (XHR -> Status -> ResponseData -> (Eff (xhr :: XHREff | eff) Unit))


postcounter :: forall eff. (SuccessFn eff) -> (FailureFn eff) -> (Eff (xhr :: XHREff | eff) Unit)
postcounter onSuccess onError =
    runFn7 ajaxImpl url method headers b isJust onSuccess onError
  where
    url = "/counter"
    method = "POST"
    headers = PostcounterHeaders 
    b = Nothing


data PostcounterHeaders = PostcounterHeaders
getcounter :: forall eff. (SuccessFn eff) -> (FailureFn eff) -> (Eff (xhr :: XHREff | eff) Unit)
getcounter onSuccess onError =
    runFn7 ajaxImpl url method headers b isJust onSuccess onError
  where
    url = "/counter"
    method = "GET"
    headers = GetcounterHeaders 
    b = Nothing


data GetcounterHeaders = GetcounterHeaders

foreign import ajaxImpl
"""
function ajaxImpl(url, method, headers, body, isJust, onSuccess, onError){
return function(){
$.ajax({
  url: url
, type: method
, success: onSuccess
, error: onError
, headers: headers
, data: (isJust(body) ? JSON.stringify(body) : null)
});
return {};
};
}
""" :: forall eff h. Fn7 URL Method h (Maybe Body) (Maybe Body -> Boolean) (SuccessFn eff) (FailureFn eff) (Eff (xhr :: XHREff | eff) Unit)

