module Theme exposing (Attribute, Element, button, padding, rythm, spacing)

import Element.WithContext as Element
import Element.WithContext.Border as Border
import Element.WithContext.Input as Input
import Types exposing (Context)


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


button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
button attrs =
    Input.button ([ padding, Border.width 1, Border.rounded rythm ] ++ attrs)
