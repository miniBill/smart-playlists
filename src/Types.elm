module Types exposing (BackendModel, BackendMsg(..), FrontendInnerModel(..), FrontendModel, FrontendMsg(..), Path(..), ToBackend(..), ToFrontend(..))

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import Lamdera exposing (ClientId, SessionId)
import LoggedIn exposing (AccessToken, User)
import Theme exposing (Context)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , inner : FrontendInnerModel
    , context : Context
    , here : Time.Zone
    }


type Path
    = Callback { state : String, code : String }
    | Homepage


type FrontendInnerModel
    = GettingSessionId
    | ReadyForAuthentication SessionId
    | GettingToken
    | GotError String
    | GettingUserId AccessToken
    | LoggedIn LoggedIn.Model


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | Here Time.Zone
    | LoggedInMsg LoggedIn.Msg
    | WithTime LoggedIn.Msg Time.Posix
    | GotCurrentUserProfile (Result Http.Error User)


type ToBackend
    = TBGetSessionId
    | TBGetToken { state : String, code : String }


type BackendMsg
    = BackendGotAccessToken ClientId (Result Http.Error AccessToken)


type ToFrontend
    = TFGotSessionId SessionId
    | TFWrongState
    | TFGotAccessToken (Result Http.Error AccessToken)
