module Main exposing (main)

import Array exposing (Array)
import Array.Extra
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)


type alias Model =
    { players : Array Player
    }


type alias Player =
    { name : String
    , score : Score
    }


type alias Score =
    { current : Int
    , previous : List ( Int, Int )
    , changeAmount : String
    }


init : Model
init =
    { players = Array.fromList []
    }


type Msg
    = GotName Int String
    | ScoreChange Int String
    | SubmitScoreChange Int Int
    | AddPlayer


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotName index name ->
            { model
                | players =
                    Array.Extra.update index
                        (\p -> { p | name = name })
                        model.players
            }

        ScoreChange index changeAmount ->
            { model
                | players =
                    Array.Extra.update index
                        (\p ->
                            let
                                score =
                                    p.score
                            in
                            { p
                                | score =
                                    { score
                                        | changeAmount = changeAmount
                                    }
                            }
                        )
                        model.players
            }

        SubmitScoreChange index changeAmount ->
            { model
                | players =
                    Array.Extra.update index
                        (\p ->
                            let
                                score =
                                    p.score
                            in
                            { p
                                | score =
                                    { score
                                        | changeAmount = ""
                                        , previous = ( score.current, changeAmount ) :: score.previous
                                        , current = score.current + changeAmount
                                    }
                            }
                        )
                        model.players
            }

        AddPlayer ->
            { model
                | players =
                    Array.push
                        { name = ""
                        , score =
                            { current = 0
                            , previous = []
                            , changeAmount = ""
                            }
                        }
                        model.players
            }


view : Model -> Html Msg
view model =
    layout []
        (viewPlayers model.players)


viewPlayers : Array Player -> Element Msg
viewPlayers players =
    column
        [ padding 8, spacing 8 ]
        [ viewNewPlayer
        , players
            |> Array.toList
            |> List.indexedMap viewPlayer
            |> row [ width fill, spacing 8 ]
        ]


viewNewPlayer : Element Msg
viewNewPlayer =
    Input.button
        [ paddingXY 16 8
        , Background.color green
        ]
        { onPress = Just AddPlayer
        , label = text "Add Player"
        }


viewPlayer : Int -> Player -> Element Msg
viewPlayer index player =
    column
        [ width <| minimum 100 fill
        , height fill
        , spacing 8
        ]
        [ Input.text
            [ width fill
            , alignTop
            ]
            { onChange = GotName index
            , text = player.name
            , placeholder = Just (Input.placeholder [] (text "name"))
            , label = Input.labelHidden "name"
            }
        , player.score.previous
            |> List.reverse
            |> List.map
                (\( score, diff ) ->
                    row [ spacing 4, alignRight ]
                        [ text (String.fromInt score)
                        , text <|
                            if diff < 0 then
                                "-"

                            else
                                "+"
                        , text (String.fromInt (abs diff))
                        ]
                )
            |> column [ alignRight ]
        , el
            [ width fill
            , Border.widthEach
                { top = 1
                , bottom = 0
                , left = 0
                , right = 0
                }
            ]
            none
        , el [ alignRight ] (text (String.fromInt player.score.current))
        , wrappedRow
            [ spacing 8 ]
            [ Input.text
                [ width <| minimum 50 fill ]
                { onChange = ScoreChange index
                , text = player.score.changeAmount
                , placeholder = Just (Input.placeholder [] (text "point change"))
                , label = Input.labelHidden "score change amount"
                }
            , Input.button
                [ paddingXY 16 8
                , Background.color blue
                , alignRight
                ]
                { onPress =
                    case String.toInt player.score.changeAmount of
                        Nothing ->
                            Nothing

                        Just changeAmount ->
                            Just (SubmitScoreChange index changeAmount)
                , label = text "Submit"
                }
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


green : Color
green =
    rgb 0.6 0.8 0.5


blue : Color
blue =
    rgb 0.7 0.8 1
