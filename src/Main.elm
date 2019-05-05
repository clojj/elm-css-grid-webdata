module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Css exposing (Style, backgroundColor, property, px, rgb)
import Css.Media as Media exposing (only, screen, withMedia)
import Debug exposing (toString)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, height, src, width)
import Html.Styled.Events exposing (onClick)
import Http exposing (expectJson)
import Json.Decode exposing (Decoder, field, string)
import RemoteData exposing (..)


type alias Model =
    { dogUrl : WebData DogUrl
    }


type Msg
    = GetNewDog
    | DogResponse (WebData DogUrl)


type alias DogUrl =
    String


init : () -> ( Model, Cmd Msg )
init () =
    ( { dogUrl = NotAsked }, Cmd.none )


getDog : Cmd Msg
getDog =
    Http.get
        { url = "https://dog.ceo/api/breeds/image/random"
        , expect = expectJson (RemoteData.fromResult >> DogResponse) dogDecoder
        }


dogDecoder : Decoder String
dogDecoder =
    field "message" string


update msg model =
    case msg of
        GetNewDog ->
            ( { model | dogUrl = Loading }, getDog )

        DogResponse data ->
            ( { model | dogUrl = data }, Cmd.none )


view model =
    div [ gridContainer ]
        [ redButton [ gridButton, onClick GetNewDog ] [ text "Get dog" ]
        , div [ gridUrl ]
            [ text <|
                case model.dogUrl of
                    Success url ->
                        url

                    _ ->
                        "not yet loaded... press button 'Get dog'!"
            ]
        , div [ gridMain ] [ viewDog model.dogUrl ]
        ]


viewDog : WebData DogUrl -> Html Msg
viewDog webData =
    case webData of
        NotAsked ->
            text "not asked"

        Loading ->
            text "loading..."

        Failure e ->
            text ("error: " ++ toString e)

        Success url ->
            img [ src url, width 300, height 300 ] []


redButton : List (Attribute msg) -> List (Html msg) -> Html msg
redButton =
    styled button
        [ backgroundColor (rgb 255 100 100)
        ]



-- define media queries and grid templates


contentBig : Style
contentBig =
    withMedia [ only screen [ Media.minWidth (px 501) ] ]
        [ gridTemplate [ "button url", "main main" ] [ "1fr 5fr" ] [ "1fr 5fr" ] ]


contentSmall : Style
contentSmall =
    withMedia [ only screen [ Media.maxWidth (px 500) ] ]
        [ gridTemplate [ "button", "main", "url" ] [ "1fr 5fr 1fr" ] [ "1fr" ] ]


gridTemplate : List String -> List String -> List String -> Style
gridTemplate areas rows cols =
    Css.batch
        [ property "display" "grid"
        , property "width" "100%"
        , property "grid-template-areas" <| String.join " " (List.map (\s -> "'" ++ s ++ "'") areas)
        , property "grid-template-rows" <| String.join "" rows
        , property "grid-row-gap" "10px"
        , property "grid-template-columns" <| String.join "" cols
        , property "grid-column-gap" "10px"
        ]


gridContainer =
    css [ contentBig, contentSmall ]



-- grid area references


gridMain =
    css [ property "grid-area" "main" ]


gridButton =
    css [ property "grid-area" "button" ]


gridUrl =
    css [ property "grid-area" "url" ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
