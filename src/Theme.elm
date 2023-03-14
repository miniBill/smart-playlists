module Theme exposing (Attribute, Context, Element, buttonPrimary, buttonSecondary, color, padding, rythm, spacing)

import Element.WithContext as Element exposing (Color)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input


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
    10


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
