module App where

import Control.Monad.Eff
import Data.Foreign
import Data.Function
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
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


type PostcounterHeaders =  { content_Type :: String, accept :: String }

postcounter :: forall eff. (SuccessFn eff) -> (FailureFn eff) -> (Eff (xhr :: XHREff | eff) Unit)
postcounter onSuccess onError =
    runFn8 ajaxImpl url method headers b isJust fromJust onSuccess onError
  where
    url = "/counter"
    method = "POST"
    headers =  { content_Type: "application/json", accept: "application/json"  }
    b = Nothing

type GetcounterHeaders =  { content_Type :: String, accept :: String }

getcounter :: forall eff. (SuccessFn eff) -> (FailureFn eff) -> (Eff (xhr :: XHREff | eff) Unit)
getcounter onSuccess onError =
    runFn8 ajaxImpl url method headers b isJust fromJust onSuccess onError
  where
    url = "/counter"
    method = "GET"
    headers =  { content_Type: "application/json", accept: "application/json"  }
    b = Nothing


foreign import ajaxImpl
"""
function ajaxImpl(url, method, headers, body, isJust, fromJust, onSuccess, onError){
return function(){
var capitalise = function(s) { return s.charAt(0).toUpperCase() + s.slice(1); }
var filterHeaders = function(obj) {
var result = {};
for(var i in obj) if(obj.hasOwnProperty(i)) result[capitalise(i.replace(/_/, '-'))] = obj[i];
return result;
};
$.ajax({
  url: url
, type: method
, success: function(d, s, x){ onSuccess(d)(s)(x)(); }
, error: function(x, s, d){ onError(x)(s)(d)(); }
, headers: filterHeaders(headers)
, data: (isJust(body) ? fromJust(body) : null)
});
return {};
};
}
""" :: forall eff h. Fn8 URL Method h (Maybe Body) (Maybe Body -> Boolean) (Maybe Body -> Body) (SuccessFn eff) (FailureFn eff) (Eff (xhr :: XHREff | eff) Unit)

