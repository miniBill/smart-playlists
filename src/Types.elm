module Types exposing (BackendModel, BackendMsg(..), Context, FrontendInnerModel(..), FrontendModel, FrontendMsg(..), Path(..), ToBackend(..), ToFrontend(..))

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , inner : FrontendInnerModel
    , context : Context
    }


type alias Context =
    {}


type Path
    = Callback { state : String, code : String }
    | Homepage


type FrontendInnerModel
    = GettingClientId
    | ReadyForAuthentication ClientId
    | GettingToken


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url


type ToBackend
    = TBGetClientId
    | TBGetToken { state : String, code : String }


type BackendMsg
    = BackendNop


type ToFrontend
    = TFGotClientId ClientId
    | TFWrongState
