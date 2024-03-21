module Main exposing (..)

import Browser
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import OpenApi
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { hover : Bool
    , file : Maybe File
    , contents : Dict String String
    , oas : Maybe OpenApi.OpenApi
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model False Nothing Dict.empty Nothing, Cmd.none )



-- UPDATE


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | ReadFile File String
    | ReadOAS String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick ->
            ( model
            , Select.files [ "application/json" ] GotFiles
            )

        DragEnter ->
            ( { model | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hover = False }
            , Cmd.none
            )

        GotFiles file files ->
            ( { model
                | file = Just file
                , hover = False
              }
            , Task.perform (ReadFile file) (File.toString file)
            )

        ReadFile file contents ->
            ( { model
                | contents = Dict.insert (File.name file) contents model.contents
                , oas = decodeOAS contents
              }
            , Cmd.none
            )

        ReadOAS contents ->
            ( model
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div
            [ style "border"
                (if model.hover then
                    "6px dashed purple"

                 else
                    "6px dashed #ccc"
                )
            , style "border-radius" "20px"
            , style "width" "480px"
            , style "height" "100px"
            , style "margin" "100px auto"
            , style "padding" "20px"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "justify-content" "center"
            , style "align-items" "center"
            , hijackOn "dragenter" (Json.Decode.succeed DragEnter)
            , hijackOn "dragover" (Json.Decode.succeed DragEnter)
            , hijackOn "dragleave" (Json.Decode.succeed DragLeave)
            , hijackOn "drop" dropDecoder
            ]
            [ button [ onClick Pick ] [ text "Upload OpenAPI 3.0 JSON" ]
            , span [ style "color" "#ccc", style "margin-top" "12px" ] [ text (Debug.toString model.file) ]
            ]
        , div [ style "padding" "24px" ]
            [ case model.file of
                Just file ->
                    div []
                        [ h3 [] [ text <| File.name file ]
                        , p [] [ text <| "Size: " ++ String.fromInt (File.size file) ++ " bytes" ]
                        , case model.oas of
                            Just oas ->
                                div []
                                    [ h4 [] [ text "OpenAPI Elm Object:" ]
                                    , p
                                        [ style "max-height" "400px"
                                        , style "overflow-y" "scroll"
                                        , style "background" "gray"
                                        , style "color" "white"
                                        ]
                                        [ text <| Debug.toString oas
                                        ]
                                    , h5 []
                                        [ text "OAS.Info"
                                        ]
                                    , div []
                                        [ h4 [] [ text "Info" ]
                                        , p [] [ text <| Debug.toString <| OpenApi.info oas ]
                                        ]
                                    , h5 []
                                        [ text "OAS.Paths"
                                        ]
                                    , div []
                                        [ h4 [] [ text "Paths" ]
                                        , p [] [ text <| Debug.toString <|   OpenApi.paths oas ]
                                        ]
                                    ]

                            Nothing ->
                                div [] [ text "No OpenAPI 3.0 JSON detected" ]
                        , h4 [] [ text "JSON Contents:" ]
                        , p
                            [ style "max-height" "400px"
                            , style "overflow-y" "scroll"
                            , style "background" "gray"
                            , style "color" "white"
                            ]
                            [ text <| Maybe.withDefault "unable to read file contents" (Dict.get (File.name file) model.contents)
                            ]
                        ]

                Nothing ->
                    text ""
            ]
        ]


decodeOAS : String -> Maybe OpenApi.OpenApi
decodeOAS spec =
    case Json.Decode.decodeString OpenApi.decode spec of
        Result.Ok oas ->
            Just oas

        Result.Err err ->
            Nothing


dropDecoder : Json.Decode.Decoder Msg
dropDecoder =
    Json.Decode.at [ "dataTransfer", "files" ] (Json.Decode.oneOrMore GotFiles File.decoder)


hijackOn : String -> Json.Decode.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (Json.Decode.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
