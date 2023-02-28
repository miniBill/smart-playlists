module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element.WithContext as Element exposing (Element, centerX, centerY, fill, height, link, text, width)
import Element.WithContext.Font as Font
import Env
import Lamdera
import SHA256
import Types exposing (Context, FrontendInnerModel(..), FrontendModel, FrontendMsg(..), Path(..), ToBackend(..), ToFrontend(..))
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
    , onUrlChange : Url.Url -> FrontendMsg
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


init : Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    let
        parser : Url.Parser.Parser (Path -> a) a
        parser =
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
                ]
    in
    case Url.Parser.parse parser url of
        Just (Callback data) ->
            ( { key = key
              , inner = GettingToken
              , context = {}
              }
            , Lamdera.sendToBackend <| TBGetToken data
            )

        Just Homepage ->
            ( { key = key
              , inner = GettingClientId
              , context = {}
              }
            , Lamdera.sendToBackend TBGetClientId
            )

        Nothing ->
            ( { key = key
              , inner = GettingClientId
              , context = {}
              }
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


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TFGotClientId clientId ->
            case model.inner of
                GettingClientId ->
                    ( { model | inner = ReadyForAuthentication clientId }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


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


innerView : FrontendModel -> Element Context msg
innerView model =
    case model.inner of
        GettingClientId ->
            text "Getting secure key from the server..."

        ReadyForAuthentication clientId ->
            link
                [ Font.underline
                , centerX
                , centerY
                ]
                { url = authenticationUrl { state = clientId }
                , label = text "Login with Spotify"
                }

        GettingToken ->
            text "Getting token"
