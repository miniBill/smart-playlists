module Theme exposing (Attribute, Context, Element, buttonPrimary, buttonSecondary, color, padding, spacing, table)

import Element.WithContext as Element exposing (Color, fill, height, paragraph, shrink, text, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import List.Extra


type alias Context =
    {}


type alias Element msg =
    Element.Element Context msg


type alias Attribute msg =
    Element.Attribute Context msg


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


rythm : number
rythm =
    8


buttonPrimary :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
buttonPrimary attrs =
    Input.button
        ([ padding
         , Border.width 1
         , Border.rounded rythm
         , Border.color color.primary
         , Font.color color.offWhite
         , Background.color color.primary
         ]
            ++ attrs
        )


buttonSecondary :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
buttonSecondary attrs =
    Input.button
        ([ padding
         , Border.width 1
         , Border.rounded rythm
         , Border.color color.primary
         , Font.color color.primary
         , Background.color color.offWhite
         ]
            ++ attrs
        )


color :
    { primary : Color
    , offWhite : Color
    }
color =
    { primary = Element.rgb255 0x2F 0x9C 0x95
    , offWhite = Element.rgb255 0xF1 0xF7 0xED
    }


table :
    List (Attribute msg)
    ->
        { data : List record
        , sortBy : columnName
        , sortToMsg : columnName -> msg
        , columns :
            List
                { label : String
                , columnName : columnName
                , view : record -> Element msg
                , sort : record -> String
                }
        }
    -> Element msg
table attrs config =
    Element.table attrs
        { data =
            case List.Extra.find (\{ columnName } -> columnName == config.sortBy) config.columns of
                Nothing ->
                    config.data

                Just { sort } ->
                    config.data
                        |> List.sortBy sort
        , columns =
            config.columns
                |> List.map
                    (\column ->
                        { header =
                            tableHeader
                                { onPress = Just (config.sortToMsg column.columnName)
                                , label =
                                    if config.sortBy == column.columnName then
                                        column.label ++ " V"

                                    else
                                        column.label
                                }
                        , view = \element -> cell (column.view element)
                        , width = shrink
                        }
                    )
        }


cell : Element msg -> Element msg
cell content =
    paragraph
        [ width <| Element.maximum 400 fill
        , height fill
        , Border.widthEach
            { bottom = 1
            , left = 0
            , right = 0
            , top = 0
            }
        , Border.color <| Element.rgb255 0xC0 0xC0 0xC0
        ]
        [ content ]


tableHeader : { label : String, onPress : Maybe msg } -> Element msg
tableHeader { label, onPress } =
    Input.button
        [ Border.widthEach
            { top = 0
            , left = 0
            , right = 0
            , bottom = 1
            }
        , Element.paddingEach
            { top = 0
            , left = 0
            , right = rythm
            , bottom = 2
            }
        ]
        { label = text label
        , onPress = onPress
        }
