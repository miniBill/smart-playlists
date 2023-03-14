module Types exposing (AccessToken, BackendModel, BackendMsg(..), Context, FrontendInnerModel(..), FrontendModel, FrontendMsg(..), Id, LoggedInModel, LoggedInMsg(..), Path(..), ToBackend(..), ToFrontend(..), User)

import Api exposing (SimplifiedPlaylistObject)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import Lamdera exposing (ClientId, SessionId)
import RemoteData exposing (RemoteData)
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
    | LoggedIn LoggedInModel


type alias LoggedInModel =
    { accessToken : AccessToken
    , user : User
    , playlists : RemoteData Http.Error (List SimplifiedPlaylistObject)
    , selectedPlaylist : Maybe Id
    }


type alias Id =
    String


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
    | LoggedInMsg LoggedInMsg
    | WithTime LoggedInMsg Time.Posix
    | GotCurrentUserProfile (Result Http.Error User)


type LoggedInMsg
    = GetPlaylists
    | GotPlaylists (Result Http.Error (List SimplifiedPlaylistObject))
    | SelectPlaylist (Maybe Id)


type ToBackend
    = TBGetSessionId
    | TBGetToken { state : String, code : String }


type BackendMsg
    = BackendGotAccessToken ClientId (Result Http.Error AccessToken)


type ToFrontend
    = TFGotSessionId SessionId
    | TFWrongState
    | TFGotAccessToken (Result Http.Error AccessToken)
