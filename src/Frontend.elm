module Frontend exposing (app)

import Api
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element.WithContext as Element exposing (centerX, centerY, fill, height, link, paragraph, text, textColumn, width)
import Element.WithContext.Font as Font
import Env
import Lamdera
import SHA256
import Task
import Theme exposing (Element)
import Time
import Types exposing (FrontendInnerModel(..), FrontendModel, FrontendMsg(..), Path(..), TimedMsg(..), ToBackend(..), ToFrontend(..))
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query


app :
    { init : Lamdera.Url -> Lamdera.Key -> ( FrontendModel, Cmd FrontendMsg )
    , view : FrontendModel -> Browser.Document FrontendMsg
    , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , subscriptions : FrontendModel -> Sub FrontendMsg
    , onUrlRequest : UrlRequest -> FrontendMsg
    , onUrlChange : Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


urlParser : Url.Parser.Parser (Path -> a) a
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.s "callback"
            <?> Url.Parser.Query.map2
                    (\code state ->
                        case ( code, state ) of
                            ( Just cd, Just st ) ->
                                Callback
                                    { code = cd
                                    , state = st
                                    }

                            _ ->
                                Homepage
                    )
                    (Url.Parser.Query.string "code")
                    (Url.Parser.Query.string "state")
        , Url.Parser.map Homepage <| Url.Parser.top
        ]


init : Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    let
        toModel : FrontendInnerModel -> FrontendModel
        toModel inner =
            { key = key
            , inner = inner
            , context = {}
            , here = Time.utc
            }
    in
    case Url.Parser.parse urlParser url of
        Just (Callback data) ->
            ( toModel GettingToken
            , [ Task.perform Here Time.here
              , Lamdera.sendToBackend <| TBGetToken data
              ]
                |> Cmd.batch
            )

        Just Homepage ->
            ( toModel GettingSessionId
            , [ Task.perform Here Time.here
              , Lamdera.sendToBackend TBGetSessionId
              ]
                |> Cmd.batch
            )

        Nothing ->
            ( toModel GettingSessionId
            , Nav.load "/"
            )


authenticationUrl : { state : String } -> String
authenticationUrl { state } =
    Url.Builder.crossOrigin "https://accounts.spotify.com"
        [ "authorize" ]
        [ Url.Builder.string "client_id" Env.clientId
        , Url.Builder.string "response_type" "code"
        , Url.Builder.string "redirect_uri" Env.redirectUrl
        , Url.Builder.string "state"
            (SHA256.fromString state |> SHA256.toHex)
        , Url.Builder.string "scopes" <|
            String.join " "
                [ "playlist-read-private"
                , "playlist-read-collaborative"
                , "playlist-modify-public"
                , "playlist-modify-private"
                , "user-library-read"
                ]
        ]


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        Here here ->
            ( { model | here = here }, Cmd.none )

        TimedMsg tmsg ->
            ( model, Task.perform (WithTime tmsg) Time.now )

        WithTime tmsg time ->
            timedUpdate time tmsg model

        Noop ->
            ( model, Cmd.none )

        GotPlaylists (Ok playlists) ->
            let
                _ =
                    Debug.log "playlists" playlists
            in
            Debug.todo "branch 'GotPlaylists (Ok _)' not implemented"

        GotCurrentUserProfile (Ok user) ->
            case model.inner of
                GettingUserId accessToken ->
                    ( { model | inner = LoggedIn { accessToken = accessToken, user = user } }
                    , Cmd.none
                    )

                LoggedIn { accessToken } ->
                    ( { model | inner = LoggedIn { accessToken = accessToken, user = user } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GotCurrentUserProfile (Err e) ->
            let
                _ =
                    Debug.log "GotCurrentUserProfile error" e
            in
            Debug.todo "branch 'GotCurrentUserProfile (Err _)' not implemented"

        GotPlaylists (Err _) ->
            Debug.todo "branch 'GotPlaylists (Err _)' not implemented"


timedUpdate : Time.Posix -> TimedMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
timedUpdate _ msg model =
    case model.inner of
        LoggedIn { accessToken } ->
            case msg of
                GetPlaylists ->
                    ( model
                    , Api.getAListOfCurrentUsersPlaylists
                        { authorization =
                            { bearer = accessToken.accessToken
                            }
                        , params =
                            { limit = Nothing
                            , offset = Nothing
                            }
                        , toMsg = GotPlaylists
                        }
                    )

        _ ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TFGotSessionId sessionId ->
            case model.inner of
                GettingSessionId ->
                    ( { model | inner = ReadyForAuthentication sessionId }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TFWrongState ->
            ( { model | inner = GotError "Something went wrong during authentication: wrong state" }, Cmd.none )

        TFGotAccessToken (Err _) ->
            if Env.mode == Env.Production then
                ( model, Nav.load "/" )

            else
                ( model, Cmd.none )

        TFGotAccessToken (Ok accessToken) ->
            ( { model | inner = GettingUserId accessToken }
            , Api.getCurrentUsersProfile
                { authorization = { bearer = accessToken.accessToken }
                , toMsg =
                    GotCurrentUserProfile
                        << Result.map
                            (\user ->
                                { id = user.id
                                , displayName = user.display_name
                                }
                            )
                }
            )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Smart Playlists for Spotify"
    , body =
        [ Element.layout model.context
            [ width fill
            , height fill
            ]
            (innerView model)
        ]
    }


innerView : FrontendModel -> Element FrontendMsg
innerView model =
    case model.inner of
        GettingSessionId ->
            paragraph [ centerX, centerY ]
                [ text "Getting secure key from the server..."
                ]

        ReadyForAuthentication sessionId ->
            link
                [ Font.underline
                , centerX
                , centerY
                ]
                { url = authenticationUrl { state = sessionId }
                , label = paragraph [] [ text "Login with Spotify" ]
                }

        GettingToken ->
            paragraph
                [ centerX
                , centerY
                ]
                [ text "Getting token from server..."
                ]

        GotError err ->
            paragraph []
                [ text <| "ERROR - " ++ err
                ]

        GettingUserId _ ->
            paragraph [] [ text "Logging in..." ]

        LoggedIn { user } ->
            textColumn []
                [ paragraph []
                    [ text "Logged in!" ]
                , paragraph []
                    [ text <| "Current user: " ++ user.displayName ]
                , Theme.button []
                    { onPress = Just <| TimedMsg GetPlaylists
                    , label = text "Get your playlists"
                    }
                ]
