module Types exposing (AccessToken, BackendModel, BackendMsg(..), Context, FrontendInnerModel(..), FrontendModel, FrontendMsg(..), Path(..), TimedMsg(..), ToBackend(..), ToFrontend(..), User)

import Api
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
    , here : Time.Zone
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
    | GettingUserId AccessToken
    | LoggedIn
        { accessToken : AccessToken
        , user : User
        }


type alias AccessToken =
    { accessToken : String
    , expiresAt : Time.Posix
    , refreshToken : String
    }


type alias User =
    { id : String
    , displayName : String
    }


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | Here Time.Zone
    | TimedMsg TimedMsg
    | WithTime TimedMsg Time.Posix
    | Noop
    | GotPlaylists (Result Http.Error Api.PagedPlaylists)
    | GotCurrentUserProfile (Result Http.Error User)


type TimedMsg
    = GetPlaylists


type ToBackend
    = TBGetSessionId
    | TBGetToken { state : String, code : String }


type BackendMsg
    = BackendGotAccessToken ClientId (Result Http.Error AccessToken)


type ToFrontend
    = TFGotSessionId SessionId
    | TFWrongState
    | TFGotAccessToken (Result Http.Error AccessToken)
