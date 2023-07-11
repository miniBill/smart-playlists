module LoggedIn exposing (AccessToken, Id, Model, Msg(..), PlaylistColumn, PlaylistsColumn, SelectedPlaylist, User, init, update, view)

import Api exposing (SimplifiedPlaylistObject)
import Element.WithContext as Element exposing (column, el, fill, height, paragraph, rgb, scrollbarY, text, textColumn, width)
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Html
import Http
import RemoteData exposing (RemoteData(..))
import Task exposing (Task)
import Theme exposing (Element)
import Time


type alias Model =
    { accessToken : AccessToken
    , user : User
    , playlists : RemoteData Http.Error (List SimplifiedPlaylistObject)
    , selectedPlaylist : SelectedPlaylist
    , error : Maybe String
    , sortPlaylistsBy : PlaylistsColumn
    , sortPlaylistBy : PlaylistColumn
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
    | SortPlaylistsBy PlaylistsColumn
    | SortPlaylistBy PlaylistColumn


type PlaylistsColumn
    = PlaylistName


type PlaylistColumn
    = TrackName
    | ArtistsName


init : AccessToken -> User -> Model
init accessToken user =
    { accessToken = accessToken
    , user = user
    , playlists = NotAsked
    , selectedPlaylist = SelectedPlaylistNone
    , error = Nothing
    , sortPlaylistsBy = PlaylistName
    , sortPlaylistBy = TrackName
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

        -- GotPlaylist _ (Err (Http.BadStatus 401)) ->
        --     ( { model | error = Just <| httpErrorToString e }, Cmd.none )
        GotPlaylist _ (Err e) ->
            ( { model | error = Just <| httpErrorToString e }, Cmd.none )

        SortPlaylistsBy column ->
            ( { model | sortPlaylistsBy = column }, Cmd.none )

        SortPlaylistBy column ->
            ( { model | sortPlaylistBy = column }, Cmd.none )


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
        , height fill
        , width fill
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

        cell : ({ a | id : Id } -> String) -> { a | id : Id } -> Element Msg
        cell prop playlist =
            if isSelected playlist then
                el [ Font.bold ] <| text <| prop playlist

            else
                Input.button []
                    { label = text <| prop playlist
                    , onPress = Just (SelectPlaylist playlist.id)
                    }
    in
    column
        [ Theme.spacing
        , height fill
        , width fill
        ]
        [ Theme.table []
            { sortBy = model.sortPlaylistsBy
            , sortToMsg = SortPlaylistsBy
            , data =
                playlists
                    |> List.sortBy
                        (case model.sortPlaylistsBy of
                            PlaylistName ->
                                .name
                        )
            , columns =
                [ { columnName = PlaylistName
                  , label = "Name"
                  , sort = .name
                  , view = cell .name
                  }
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
                viewTracks model tracks
        ]


viewTracks : Model -> List Api.PlaylistTrackObject -> Element Msg
viewTracks model tracks =
    let
        columns :
            List
                { label : String
                , columnName : PlaylistColumn
                , view : PlaylistTrackObjectMerged -> Element Msg
                , sort : PlaylistTrackObjectMerged -> String
                }
        columns =
            [ { label = "Name"
              , columnName = TrackName
              , view = \{ name } -> text name
              , sort = \{ name } -> String.toLower name
              }
            , { label = "Artists"
              , columnName = ArtistsName
              , view = \{ artists } -> text <| String.join ", " artists
              , sort =
                    \{ artists } ->
                        artists
                            |> List.map normalizeArtistName
                            |> List.sort
                            |> String.join ", "
              }
            ]

        data : List PlaylistTrackObjectMerged
        data =
            List.map mergeTrackObject tracks
    in
    column
        [ Theme.spacing
        , height fill
        , width fill
        ]
        [ text "Tracks:"
        , Theme.table
            [ scrollbarY
            , height fill
            , width fill
            ]
            { data = data
            , columns = columns
            , sortBy = model.sortPlaylistBy
            , sortToMsg = SortPlaylistBy
            }
        ]


normalizeArtistName : String -> String
normalizeArtistName artistName =
    let
        lower : String
        lower =
            String.toLower artistName
    in
    case String.split " " lower of
        "the" :: rest ->
            String.join " " rest

        _ ->
            lower


mergeTrackObject : Api.PlaylistTrackObject -> PlaylistTrackObjectMerged
mergeTrackObject trackObject =
    case trackObject.track of
        Api.EpisodeObjectOrTrackObject_EpisodeObject ep ->
            { name = ep.name
            , artists = []
            , track = trackObject.track
            }

        Api.EpisodeObjectOrTrackObject_TrackObject tr ->
            { name = tr.name
            , artists =
                tr.artists
                    |> Maybe.withDefault []
                    |> List.map .name
            , track = trackObject.track
            }


type alias PlaylistTrackObjectMerged =
    { name : String
    , artists : List String
    , track : Api.EpisodeObjectOrTrackObject 
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
