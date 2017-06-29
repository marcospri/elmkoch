module Main exposing (..)

import Html exposing (div, select, option, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Collage
import Element
import Color
import Result


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


type alias Model =
    { passes : Int
    }


type Msg
    = PassesChange String


init : ( Model, Cmd msg )
init =
    ( { passes = 1 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        PassesChange passes ->
            let
                intPasses =
                    String.toInt passes |> Result.withDefault 1
            in
                ( { passes = intPasses }, Cmd.none )


type alias Point =
    { x : Float
    , y : Float
    }


koch : Point -> Point -> Int -> List Point -> List Point
koch a b limit points =
    let
        ( dx, dy ) =
            ( b.x - a.x, b.y - a.y )

        dist =
            dx * dx + dy * dy |> sqrt

        unit =
            dist / 3

        angle =
            atan2 dy dx

        p1 =
            Point (a.x + dx / 3) (a.y + dy / 3)

        p2 =
            Point
                (p1.x
                    + (cos (angle - pi / 3))
                    * unit
                )
                (p1.y
                    + (sin (angle - pi / 3))
                    * unit
                )

        p3 =
            Point (b.x - dx / 3) (b.y - dy / 3)
    in
        if limit > 0 then
            let
                l =
                    limit - 1
            in
                List.concat
                    [ points
                    , koch a p1 l points
                    , koch p1 p2 l points
                    , koch p2 p3 l points
                    , koch p3 b l points
                    ]
        else
            a :: p1 :: p2 :: p3 :: b :: points


startP1 : Point
startP1 =
    Point 0 -150


startP2 : Point
startP2 =
    Point 150 100


startP3 : Point
startP3 =
    Point -150 100


screenWidth : Int
screenWidth =
    420


screenHeight : Int
screenHeight =
    420


toElmCoordinates : Point -> ( Float, Float )
toElmCoordinates { x, y } =
    ( x
    , y * -1
    )


renderOptions : List (Html.Html msg)
renderOptions =
    List.range 1 7
        |> List.map toString
        |> List.map (\x -> option [ value x ] [ text x ])


view : Model -> Html.Html Msg
view model =
    let
        path =
            [ koch startP1 startP2 model.passes []
            , koch startP2 startP3 model.passes []
            , koch startP3 startP1 model.passes []
            ]
                |> List.concat
                |> List.map toElmCoordinates
                |> Collage.path
                |> Collage.traced (Collage.solid Color.black)
    in
        div []
            [ select [ onInput PassesChange ] renderOptions
            , Collage.collage screenWidth screenHeight [ path ] |> Element.toHtml
            ]
