module Types exposing (AccessToken, BackendModel, BackendMsg(..), Context, FrontendInnerModel(..), FrontendModel, FrontendMsg(..), Path(..), ToBackend(..), ToFrontend(..))

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import Lamdera exposing (ClientId, SessionId)
import Time
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
    = GettingSessionId
    | ReadyForAuthentication SessionId
    | GettingToken
    | GotError String
    | LoggedIn AccessToken


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url


type ToBackend
    = TBGetSessionId
    | TBGetToken { state : String, code : String }


type BackendMsg
    = BackendGotAccessToken ClientId (Result Http.Error AccessToken)


type alias AccessToken =
    { accessToken : String
    , expiresAt : Time.Posix
    , refreshToken : String
    }


type ToFrontend
    = TFGotSessionId SessionId
    | TFWrongState
    | TFGotAccessToken (Result Http.Error AccessToken)
