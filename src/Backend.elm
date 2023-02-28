module Backend exposing (app)

import Base64
import Env
import Http
import Http.Tasks
import Json.Decode as JD
import Lamdera exposing (ClientId, SessionId)
import SHA256
import Task
import Time
import Types exposing (BackendModel, BackendMsg(..), ToBackend(..), ToFrontend(..))
import Url


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( {}
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        BackendGotAccessToken clientId data ->
            ( model, Lamdera.sendToFrontend clientId (TFGotAccessToken data) )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        TBGetSessionId ->
            ( model, Lamdera.sendToFrontend clientId (TFGotSessionId sessionId) )

        TBGetToken { state, code } ->
            if state /= SHA256.toHex (SHA256.fromString sessionId) then
                ( model, Lamdera.sendToFrontend clientId TFWrongState )

            else
                ( model, requestAccessToken clientId code )


requestAccessToken : ClientId -> String -> Cmd BackendMsg
requestAccessToken clientId code =
    Http.task
        { method = "POST"
        , headers =
            [ Http.header "Authorization"
                ("Basic "
                    ++ Base64.encode
                        (Env.clientId
                            ++ ":"
                            ++ Env.clientSecret
                        )
                )
            ]
        , url = "https://accounts.spotify.com/api/token"
        , body =
            Http.stringBody "application/x-www-form-urlencoded"
                (encodeFormData
                    [ ( "grant_type", "authorization_code" )
                    , ( "code", code )
                    , ( "redirect_uri", Env.redirectUrl )
                    ]
                )
        , resolver =
            Http.Tasks.resolveJson
                (JD.map3
                    (\access_token expires_in refresh_token ->
                        { accessToken = access_token
                        , expiresIn = expires_in
                        , refreshToken = refresh_token
                        }
                    )
                    (JD.field "access_token" JD.string)
                    (JD.field "expires_in" JD.int)
                    (JD.field "refresh_token" JD.string)
                )
        , timeout = Nothing
        }
        |> Task.andThen
            (\{ accessToken, expiresIn, refreshToken } ->
                Time.now
                    |> Task.map
                        (\now ->
                            { accessToken = accessToken
                            , expiresAt = Time.millisToPosix (Time.posixToMillis now + expiresIn)
                            , refreshToken = refreshToken
                            }
                        )
            )
        |> Task.attempt (BackendGotAccessToken clientId)


encodeFormData : List ( String, String ) -> String
encodeFormData fields =
    fields
        |> List.map
            (\( name, value ) ->
                Url.percentEncode name ++ "=" ++ Url.percentEncode value
            )
        |> String.join "&"
