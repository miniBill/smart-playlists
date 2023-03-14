module Frontend exposing (app)

import Api exposing (SimplifiedPlaylistObject)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element.WithContext as Element exposing (centerX, centerY, column, el, fill, height, link, paddingEach, paragraph, rgb, shrink, text, textColumn, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Env
import Http
import Lamdera
import RemoteData exposing (RemoteData(..))
import SHA256
import Task exposing (Task)
import Theme exposing (Element)
import Time
import Types exposing (Context, FrontendInnerModel(..), FrontendModel, FrontendMsg(..), LoggedInModel, LoggedInMsg(..), Path(..), ToBackend(..), ToFrontend(..))
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

        LoggedInMsg lmsg ->
            ( model, Task.perform (WithTime lmsg) Time.now )

        WithTime lmsg time ->
            timedUpdate time lmsg model

        GotCurrentUserProfile (Ok user) ->
            case model.inner of
                GettingUserId accessToken ->
                    ( { model
                        | inner =
                            LoggedIn
                                { accessToken = accessToken
                                , user = user
                                , playlists = NotAsked
                                , selectedPlaylist = Nothing
                                }
                      }
                    , Nav.pushUrl model.key "/"
                    )

                LoggedIn { accessToken } ->
                    ( { model
                        | inner =
                            LoggedIn
                                { accessToken = accessToken
                                , user = user
                                , playlists = NotAsked
                                , selectedPlaylist = Nothing
                                }
                      }
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


timedUpdate : Time.Posix -> LoggedInMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
timedUpdate now msg model =
    case model.inner of
        LoggedIn loggedInModel ->
            let
                ( newModel, cmd ) =
                    loggedInUpdate now msg loggedInModel
            in
            ( { model | inner = LoggedIn newModel }
            , Cmd.map LoggedInMsg cmd
            )

        _ ->
            ( model, Cmd.none )


loggedInUpdate : Time.Posix -> LoggedInMsg -> LoggedInModel -> ( LoggedInModel, Cmd LoggedInMsg )
loggedInUpdate now msg model =
    case msg of
        GetPlaylists ->
            ( { model
                | playlists = Loading
              }
            , unpaginate Api.getAListOfCurrentUsersPlaylistsTask
                { authorization =
                    { bearer = model.accessToken.accessToken
                    }
                , params =
                    { limit = Nothing
                    , offset = Nothing
                    }
                , toMsg = GotPlaylists
                }
            )

        GotPlaylists result ->
            ( { model | playlists = RemoteData.fromResult result }
            , Cmd.none
            )

        SelectPlaylist selectedPlaylist ->
            ( { model | selectedPlaylist = selectedPlaylist }
            , Cmd.none
            )


unpaginate :
    ({ params : { params | offset : Maybe Int }, authorization : authorization }
     -> Task err { paginated | items : List item, total : Int }
    )
    -> { params : { params | offset : Maybe Int }, authorization : authorization, toMsg : Result err (List item) -> msg }
    -> Cmd msg
unpaginate toTask { params, authorization, toMsg } =
    let
        go : Int -> List (List item) -> Task err (List item)
        go offset acc =
            toTask { params = { params | offset = Just offset }, authorization = authorization }
                |> Task.andThen
                    (\paginated ->
                        if offset + List.length paginated.items < paginated.total then
                            go (offset + List.length paginated.items) (paginated.items :: acc)

                        else
                            (paginated.items :: acc)
                                |> List.reverse
                                |> List.concat
                                |> Task.succeed
                    )
    in
    go (Maybe.withDefault 0 params.offset) []
        |> Task.attempt toMsg


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
            ( model, Nav.load "/" )

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
            , Background.color Theme.color.offWhite
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

        LoggedIn loggedInModel ->
            viewLoggedIn loggedInModel
                |> Element.map LoggedInMsg


viewLoggedIn : LoggedInModel -> Element LoggedInMsg
viewLoggedIn ({ user, playlists } as loggedInModel) =
    column
        [ Theme.spacing
        , Theme.padding
        ]
        [ textColumn []
            [ paragraph []
                [ text "Logged in!" ]
            , paragraph []
                [ text <| "Current user: " ++ user.displayName ]
            ]
        , if playlists == NotAsked then
            Theme.buttonPrimary []
                { onPress = Just GetPlaylists
                , label = text "Get your playlists"
                }

          else
            Theme.buttonSecondary []
                { onPress = Just GetPlaylists
                , label = text "Update playlists' list"
                }
        , viewPlaylists loggedInModel
        ]


viewPlaylists : LoggedInModel -> Element LoggedInMsg
viewPlaylists model =
    case model.playlists of
        NotAsked ->
            Element.none

        Loading ->
            text "Loading..."

        Failure err ->
            let
                ( visible, hidden ) =
                    httpErrorToUserAndHiddenString err
            in
            Element.row []
                [ text visible
                , el [ Font.color <| rgb 1 1 1 ] <| text hidden
                ]

        Success playlists ->
            innerViewPlaylists model playlists


innerViewPlaylists : LoggedInModel -> List SimplifiedPlaylistObject -> Element LoggedInMsg
innerViewPlaylists model playlists =
    let
        header : String -> Element msg
        header label =
            el
                [ Border.widthEach
                    { top = 0
                    , left = 0
                    , right = 0
                    , bottom = 1
                    }
                , paddingEach
                    { top = 0
                    , left = 0
                    , right = Theme.rythm
                    , bottom = 2
                    }
                ]
                (text label)

        isSelected : { a | id : Types.Id } -> Bool
        isSelected playlist =
            Just playlist.id == model.selectedPlaylist

        shrinkColumn :
            String
            -> (SimplifiedPlaylistObject -> String)
            -> Element.Column Context SimplifiedPlaylistObject LoggedInMsg
        shrinkColumn label prop =
            { header = header label
            , width = shrink
            , view =
                \playlist ->
                    if isSelected playlist then
                        el [ Font.bold ] <| text <| prop playlist

                    else
                        Input.button []
                            { label = text <| prop playlist
                            , onPress = Just (SelectPlaylist <| Just playlist.id)
                            }
            }
    in
    column [ Theme.spacing ]
        [ Element.table []
            { data = playlists
            , columns =
                [ shrinkColumn "Name" .name
                ]
            }
        , text <|
            "Showing "
                ++ String.fromInt (List.length playlists)
                ++ " playlists"
        ]


httpErrorToUserAndHiddenString : Http.Error -> ( String, String )
httpErrorToUserAndHiddenString err =
    case err of
        Http.BadStatus code ->
            ( "Unexpected answer from the server"
            , "BadStatus " ++ String.fromInt code
            )

        Http.BadUrl url ->
            ( "Something went wrong"
            , "BadUrl " ++ url
            )

        Http.Timeout ->
            ( "No reply from the server"
            , "Timeout"
            )

        Http.NetworkError ->
            ( "Error connecting to the server"
            , "NetworkError"
            )

        Http.BadBody msg ->
            ( "Unexpected answer from the server"
            , "BadBody " ++ msg
            )
