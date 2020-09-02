module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


type alias Model =
    { processedNotes : List (List Tab)
    , enteredNotes : List String
    , tabXpos : Int
    , tabYpos : Int
    }


initialModel : Model
initialModel =
    { processedNotes = []
    , enteredNotes = []
    , tabXpos = 0
    , tabYpos = 0
    }


type alias Tab =
    { fret : Int
    , string : Int
    }


type alias Chord =
    List Tab


type Msg
    = ClearTab
    | NewStaff
    | ReadTab String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewStaff ->
            { model | tabXpos = model.tabXpos + 200 }

        ReadTab notes ->
            let
                readNotes =
                    String.split " " notes

                parsedNotes =
                    parseInput readNotes

                yPos =
                    model.tabYpos + 20
            in
            { model | enteredNotes = readNotes, processedNotes = parsedNotes, tabYpos = yPos }

        ClearTab ->
            { model | processedNotes = [], enteredNotes = [] }


view : Model -> Html Msg
view model =
    div
        [ style "color" "#333"
        , style "textAlign" "center"
        ]
        [ headline model
        , tabInput model 
        , clearButton model
        , tabLines model 
        , tabNotes model.processedNotes model
        , instructions model
        ]


{-| Takes in the list of notes that have been read in and processed and displays them as HTML.
-}
tabNotes :  List (List Tab) -> Model -> Html Msg
tabNotes tabList model =
    let
        tabItem a =
            -- Single note
            if List.length a == 1 then
                let
                    note =
                        List.head a
                            |> Maybe.withDefault { fret = 0, string = 1 }
                in
                if note.fret == 50 then
                    div [ style "color" "rgba(0,0,0,0)" ] [ text "-" ]

                else if note.string == 9 then
                    -- single line / bar end
                    div
                        [ style "height" "85px"
                        , style "border" "1px solid #aaa"
                        , style "zIndex" "1"
                        , style "margin" "5px 5px 0 15px"
                        ]
                        []

                else if note.string == 8 then
                    -- double line
                    div
                        [ style "height" "85px"
                        , style "width" "3px"
                        , style "borderLeft" "2px solid #aaa"
                        , style "borderRight" "2px solid #aaa"
                        , style "zIndex" "1"
                        , style "margin" "5px 5px 0 15px"
                        ]
                        []

                else if note.fret == 51 then
                    -- slur
                    div
                        [ style "transform" "rotate(-90deg) translateX(10px) translateY(6px)"
                        , style "marginTop" (noteXpos note.string)
                        ]
                        [ text ")" ]

                else if note.fret == 52 then
                    -- slide up
                    div
                        [ style "transform" "skewX(-45deg)"
                        , style "margin" (noteXpos note.string ++ " 0 0 10px")
                        ]
                        [ text "|" ]

                else if note.fret == 53 then
                    -- slide down
                    div
                        [ style "transform" "skewX(45deg)"
                        , style "margin" (noteXpos note.string ++ " 0 0 10px")
                        ]
                        [ text "|" ]

                else
                    div
                        [ style "position" "relative"
                        , style "marginTop" (noteXpos note.string)
                        , style "marginLeft" "10px"
                        ]
                        [ text <| String.fromInt note.fret ]
                -- Chord

            else
                let
                    mapper b =
                        if b.fret == 50 then
                            div [] []

                        else
                            div [ style "margin" "-1px 0 0 10px" ] [ text <| String.fromInt b.fret ]

                    finalDiv =
                        List.map mapper a
                in
                div [ style "position" "relative" ] (List.reverse finalDiv)
    in
    div [ style "display" "flex", style "marginTop" "-100px" ]
        (List.map tabItem tabList)


{-| Maps input over splitNotes to format
-}
parseInput : List String -> List (List Tab)
parseInput noteList =
    List.map splitNotes noteList


{-| Determines if note is a fret/string combo, a chord, or a special symbol and formats accordingly
-}
splitNotes : String -> List Tab
splitNotes note =
    if String.length note == 12 then
        let
            e6 =
                String.slice 0 2 note

            a =
                String.slice 2 4 note

            d =
                String.slice 4 6 note

            g =
                String.slice 6 8 note

            b =
                String.slice 8 10 note

            e =
                String.slice 10 12 note

            mapper x =
                Tab (fretNo x) (stringNo x)
        in
        List.map mapper [ e6, a, d, g, b, e ]

    else if String.length note == 1 || String.slice 1 2 note == "#" then
        let
            mapper a =
                Tab (fretNo a) (stringNo a)
        in
        List.map mapper (chordTransform note)

    else
        [ Tab (fretNo note) (stringNo note) ]


{-| gets the second number of a two digit fret/string (so just the string)
-}
fretNo : String -> Int
fretNo a =
    String.dropLeft 1 a
        |> String.toInt
        |> Maybe.withDefault 50


{-| gets just the first number of a two digit fret/string
-}
stringNo : String -> Int
stringNo a =
    String.left 1 a
        |> String.toInt
        |> Maybe.withDefault 0


tabLines : Model -> Html Msg
tabLines model =
    let
        lineStyle =
            [ style "border" "1px solid #aaa"
            , style "marginTop" "15px"
            , style "marginLeft" "5px"
            , style "marginRight" "5px"
            ]
    in
    div
        [ style "marginTop" "50px"
        , style "position" "relative"
        ]
        [ hr lineStyle []
        , hr lineStyle []
        , hr lineStyle []
        , hr lineStyle []
        , hr lineStyle []
        , hr lineStyle []
        ]


tabInput : Model -> Html Msg
tabInput model =
    textarea
        [ placeholder "Enter Tab"
        , onInput ReadTab
        , value (String.join " " model.enteredNotes)
        , style "width" "60%"
        , style "height" "100px"
        , style "textAlign" "center"
        , style "margin" "100px 20% 0"
        , style "backgroundColor" "#fff"
        , style "color" "#03a9f4"
        , style "border" "1px solid #aaa"
        , style "fontSize" "16px"
        ]
        []


headline : Model -> Html Msg
headline model =
    div
        [ style "color" "#555"
        , style "fontSize" "40px"
        , style "marginTop" "20px"
        , style "marginBottom" "-40px"
        ]
        [ text "Easy Tabber 3000" ]


clearButton : Model -> Html Msg
clearButton model =
    button
        [ style "backgroundColor" "#ccc"
        , style "cursor" "pointer"
        , style "color" "red"
        , style "border" "1px solid #aaa"
        , style "marginTop" "10px"
        , style "marginTop" "10px"
        , onClick ClearTab
        ]
        [ text "Clear Tab" ]


noteXpos : Int -> String
noteXpos a =
    case a of
        1 ->
            "-2px"

        2 ->
            "15px"

        3 ->
            "32px"

        4 ->
            "49px"

        5 ->
            "66px"

        6 ->
            "83px"

        _ ->
            "0"


{-| Turns named chords (i.e. "G", "C", into Tab ) capital letter for major chords and vice versa
-}
chordTransform : String -> List String
chordTransform note =
    case note of
        "A" ->
            [ "xx", "50", "42", "32", "22", "10" ]

        "a" ->
            [ "xx", "50", "42", "32", "21", "10" ]

        "A5" ->
            [ "65", "57", "47", "36", "25", "25" ]

        "B7" ->
            [ "xx", "52", "41", "32", "20", "12" ]

        "b" ->
            [ "xx", "xx", "44", "34", "23", "12" ]

        "C" ->
            [ "xx", "53", "42", "30", "21", "10" ]

        "D" ->
            [ "xx", "xx", "40", "32", "23", "12" ]

        "d" ->
            [ "xx", "xx", "40", "32", "23", "11" ]

        "e" ->
            [ "60", "52", "42", "30", "20", "10" ]

        "E" ->
            [ "60", "52", "42", "31", "20", "10" ]

        "F" ->
            [ "xx", "xx", "43", "32", "21", "10" ]

        "f#" ->
            [ "62", "50", "40", "32", "22", "10" ]

        "G" ->
            [ "63", "52", "40", "30", "23", "13" ]

        _ ->
            []


instructions : Model -> Html msg
instructions model =
    ul [ style "position" "absolute", style "bottom" "5%", style "left" "25%", style "width" "60%", style "textAlign" "left", style "color" "#333" ]
        [ li [] [ text "Type string number directly followed by fret number, followed by a space (e.g.: 10 21 33 11)" ]
        , li [] [ text "Type space as many times as desired to add spacing between notes." ]
        , li [] [ text "For chords type all 6 string number/fret sets together (e.g.: G major = 635240302313)" ]
        , li [] [ text "For chords with unused strings use xx for string/fret (e.g.: D major = xxxx40322312)" ]
        , li [] [ text "OR... You can just enter a chords name: G for G major; a for a minor." ]
        , li [] [ text "Type 99 for a single barline, 88 for a double barline." ]
        , li [] [ text "type <STRING> 51 for a slur mark; 52 for / 53 for \\. E.g. if you want a forward slide on string 3, type: 352" ]
        ]



{- 
   TODO: make downloadable,
   extend lines
-}
