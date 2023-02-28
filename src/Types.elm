module Types exposing (BackendModel, BackendMsg(..), Context, FrontendInnerModel(..), FrontendModel, FrontendMsg(..), ToBackend(..), ToFrontend(..))

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , inner : FrontendInnerModel
    , context : Context
    }


type alias Context =
    {}


type FrontendInnerModel
    = Authorizing
    | AuthorizationError Http.Error
    | AuthorizationSuccessfull String


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | GotAuthorization (Result Http.Error String)


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
