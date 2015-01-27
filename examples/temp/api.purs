module App where

import Data.Foreign
import Data.Function


foreign import postcounter
"""
function postcounter (onSuccess,onError) {
return function(){
$.ajax({
url: "/counter'",
type: "POST",
success: onSuccess,
error: onError,


});
};
}
""" :: forall a b eff. Fn2 Foreign (b) (b) (eff Unit)


foreign import getcounter
"""
function getcounter (onSuccess,onError) {
return function(){
$.ajax({
url: "/counter'",
type: "GET",
success: onSuccess,
error: onError,


});
};
}
""" :: forall a b eff. Fn2 Foreign (b) (b) (eff Unit)
