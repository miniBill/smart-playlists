module LoggedIn exposing (AccessToken, Id, Model, Msg(..), SelectedPlaylist, User, init, update, view)

import Api exposing (SimplifiedPlaylistObject)
import Element.WithContext as Element exposing (column, el, paddingEach, paragraph, rgb, shrink, text, textColumn)
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Html
import Http
import RemoteData exposing (RemoteData(..))
import Task exposing (Task)
import Theme exposing (Context, Element)
import Time


type alias Model =
    { accessToken : AccessToken
    , user : User
    , playlists : RemoteData Http.Error (List SimplifiedPlaylistObject)
    , selectedPlaylist : SelectedPlaylist
    , error : Maybe String
    }


type SelectedPlaylist
    = SelectedPlaylistNone
    | SelectedPlaylistLoading Id
    | SelectedPlaylistLoaded Id (List Api.PlaylistTrackObject)


type alias AccessToken =
    { accessToken : String
    , expiresAt : Time.Posix
    , refreshToken : String
    }


type alias User =
    { id : String
    , displayName : String
    }


type alias Id =
    String


type Msg
    = GetPlaylists
    | GotPlaylists (Result Http.Error (List SimplifiedPlaylistObject))
    | SelectPlaylist Id
    | GotPlaylist Id (Result Http.Error (List Api.PlaylistTrackObject))


init : AccessToken -> User -> Model
init accessToken user =
    { accessToken = accessToken
    , user = user
    , playlists = NotAsked
    , selectedPlaylist = SelectedPlaylistNone
    , error = Nothing
    }


update : Time.Posix -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
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
            ( { model | selectedPlaylist = SelectedPlaylistLoading selectedPlaylist }
            , unpaginate Api.getPlaylistsTracksTask
                { authorization = { bearer = model.accessToken.accessToken }
                , params =
                    { playlist_id = selectedPlaylist
                    , limit = Nothing
                    , offset = Nothing
                    , market = Nothing
                    , fields = Nothing
                    , additional_types = Nothing
                    }
                , toMsg = GotPlaylist selectedPlaylist
                }
            )

        GotPlaylist id (Ok items) ->
            ( { model
                | selectedPlaylist = SelectedPlaylistLoaded id items
                , error = Nothing
              }
            , Cmd.none
            )

        GotPlaylist _ (Err e) ->
            ( { model | error = Just <| httpErrorToString e }, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.Timeout ->
            "Timeout"

        Http.BadUrl badUrl ->
            "Bad url: " ++ badUrl

        Http.NetworkError ->
            "Network error"

        Http.BadStatus badStatus ->
            "Bad status: " ++ String.fromInt badStatus

        Http.BadBody badBody ->
            "Bad body: " ++ badBody


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


view : Model -> Element Msg
view ({ user, playlists, error } as model) =
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
        , viewPlaylists model
        , case error of
            Nothing ->
                Element.none

            Just err ->
                textColumn []
                    [ paragraph [] [ text "ERRRRRROR" ]
                    , Element.html <|
                        Html.pre [] [ Html.text err ]
                    ]
        ]


viewPlaylists : Model -> Element Msg
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


innerViewPlaylists : Model -> List SimplifiedPlaylistObject -> Element Msg
innerViewPlaylists model playlists =
    let
        isSelected : { a | id : Id } -> Bool
        isSelected playlist =
            case model.selectedPlaylist of
                SelectedPlaylistLoading id ->
                    playlist.id == id

                SelectedPlaylistLoaded id _ ->
                    playlist.id == id

                SelectedPlaylistNone ->
                    False

        shrinkColumn :
            String
            -> (SimplifiedPlaylistObject -> String)
            -> Element.Column Context SimplifiedPlaylistObject Msg
        shrinkColumn label prop =
            { header = tableHeader label
            , width = shrink
            , view =
                \playlist ->
                    if isSelected playlist then
                        el [ Font.bold ] <| text <| prop playlist

                    else
                        Input.button []
                            { label = text <| prop playlist
                            , onPress = Just (SelectPlaylist playlist.id)
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
        , case model.selectedPlaylist of
            SelectedPlaylistNone ->
                Element.none

            SelectedPlaylistLoading _ ->
                text "Loading playlist..."

            SelectedPlaylistLoaded _ tracks ->
                viewTracks tracks
        ]


tableHeader : String -> Element msg
tableHeader label =
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


viewTracks : List Api.PlaylistTrackObject -> Element Msg
viewTracks tracks =
    let
        nameColumn : Element.Column Context PlaylistTrackObjectMerged msg
        nameColumn =
            { header = tableHeader "Name"
            , view = \{ name } -> text name
            , width = shrink
            }
    in
    column [ Theme.spacing ]
        [ text "Tracks:"
        , Element.table []
            { data = List.map mergeTrackObject tracks
            , columns = [ nameColumn ]
            }
        ]


mergeTrackObject : Api.PlaylistTrackObject -> PlaylistTrackObjectMerged
mergeTrackObject trackObject =
    case trackObject.track of
        Api.TrackObjectOrEpisodeObject_EpisodeObject ep ->
            { name = ep.name
            , track = trackObject.track
            }

        Api.TrackObjectOrEpisodeObject_TrackObject tr ->
            { name = tr.name
            , track = trackObject.track
            }


type alias PlaylistTrackObjectMerged =
    { name : String
    , track : Api.TrackObjectOrEpisodeObject
    }


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
