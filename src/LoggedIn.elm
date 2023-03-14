module LoggedIn exposing (update, view)

import Api exposing (SimplifiedPlaylistObject)
import Element.WithContext as Element exposing (column, el, paddingEach, paragraph, rgb, shrink, text, textColumn)
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Http
import RemoteData exposing (RemoteData(..))
import Task exposing (Task)
import Theme exposing (Element)
import Time
import Types exposing (Context, LoggedInModel, LoggedInMsg(..))


update : Time.Posix -> LoggedInMsg -> LoggedInModel -> ( LoggedInModel, Cmd LoggedInMsg )
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


view : LoggedInModel -> Element LoggedInMsg
view ({ user, playlists } as loggedInModel) =
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
