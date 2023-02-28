module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element.WithContext as Element exposing (Element, fill, height, text, width)
import Env
import Http
import Lamdera
import SHA256
import Types exposing (..)
import Url
import Url.Builder


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


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , inner = Authorizing
      , context = {}
      }
    , Http.get
        { url =
            Url.Builder.crossOrigin "https://api.spotify.com"
                [ "v1", "authorize" ]
                [ Url.Builder.string "client_id" Env.clientId
                , Url.Builder.string "response_type" "code"
                , Url.Builder.string "redirect_url" Env.redirectUrl
                , Url.Builder.string "state"
                    (let
                        _ =
                            Debug.todo
                     in
                     "SOME RANDOM STRING"
                    )
                , Url.Builder.string "scopes" <|
                    String.join " "
                        [ "playlist-read-private"
                        , "playlist-read-collaborative"
                        , "playlist-modify-public"
                        , "playlist-modify-private"
                        , "user-library-read"
                        ]
                , Url.Builder.string "code_challenge_method" "S256"
                , Url.Builder.string "code_challenge"
                    (SHA256.fromString
                        (let
                            _ =
                                Debug.todo
                         in
                         "SOMERANDOMCRAP"
                        )
                        |> SHA256.toHex
                    )
                ]
        , expect = Http.expectString GotAuthorization
        }
    )


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

        GotAuthorization (Err err) ->
            ( { model | inner = AuthorizationError err }, Cmd.none )

        GotAuthorization (Ok res) ->
            ( { model | inner = AuthorizationSuccessfull res }, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
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
        Authorizing ->
            text "Authorization in progress..."

        AuthorizationError err ->
            text <| "AuthorizationError: " ++ Debug.toString err

        AuthorizationSuccessfull res ->
            text <| "AuthorizationSuccessfull: " ++ res
