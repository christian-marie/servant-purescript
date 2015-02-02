module App.Ajax where

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


getitems :: forall eff. (SuccessFn eff) -> (FailureFn eff) -> (Eff (xhr :: XHREff | eff) Unit)
getitems onSuccess onError =
    runFn7 ajaxImpl url method headers b isJust onSuccess onError
  where
    url = "/items"
    method = "GET"
    headers = GetitemsHeaders 
    b = Nothing


data GetitemsHeaders = GetitemsHeaders
postitems :: forall eff. String -> (SuccessFn eff) -> (FailureFn eff) -> (Eff (xhr :: XHREff | eff) Unit)
postitems body onSuccess onError =
    runFn7 ajaxImpl url method headers b isJust onSuccess onError
  where
    url = "/items"
    method = "POST"
    headers = PostitemsHeaders 
    b = (Just body)


data PostitemsHeaders = PostitemsHeaders
getitemsWithUuid :: forall eff. String -> (SuccessFn eff) -> (FailureFn eff) -> (Eff (xhr :: XHREff | eff) Unit)
getitemsWithUuid uuid onSuccess onError =
    runFn7 ajaxImpl url method headers b isJust onSuccess onError
  where
    url = "/items/" <> encodeURIComponent uuid <> ""
    method = "GET"
    headers = GetitemsWithUuidHeaders 
    b = Nothing


data GetitemsWithUuidHeaders = GetitemsWithUuidHeaders
putitemsWithUuid :: forall eff. String -> String -> (SuccessFn eff) -> (FailureFn eff) -> (Eff (xhr :: XHREff | eff) Unit)
putitemsWithUuid uuid body onSuccess onError =
    runFn7 ajaxImpl url method headers b isJust onSuccess onError
  where
    url = "/items/" <> encodeURIComponent uuid <> ""
    method = "PUT"
    headers = PutitemsWithUuidHeaders 
    b = (Just body)


data PutitemsWithUuidHeaders = PutitemsWithUuidHeaders
deleteitemsWithUuid :: forall eff. String -> (SuccessFn eff) -> (FailureFn eff) -> (Eff (xhr :: XHREff | eff) Unit)
deleteitemsWithUuid uuid onSuccess onError =
    runFn7 ajaxImpl url method headers b isJust onSuccess onError
  where
    url = "/items/" <> encodeURIComponent uuid <> ""
    method = "DELETE"
    headers = DeleteitemsWithUuidHeaders 
    b = Nothing


data DeleteitemsWithUuidHeaders = DeleteitemsWithUuidHeaders

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

