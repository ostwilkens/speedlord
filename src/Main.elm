module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Element exposing (Attribute, Color, Element, alignBottom, alignTop, centerX, centerY, column, el, fill, height, htmlAttribute, inFront, moveDown, moveRight, padding, px, rgb, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Json.Decode exposing (Value)
import Json.Encode as Encode
import List exposing (any, filter, head, map, repeat)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vector2 exposing (vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Shader exposing (Vertex, fragmentShader, vertexShader)
import WebGL exposing (Mesh, Shader)


main : Program (Maybe Encode.Value) Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , subscriptions = \_ -> onAnimationFrameDelta TimeDelta
        , update = update
        }


view : Model -> Html Msg
view model =
    Element.layout [ Background.color (rgb 0.1 0.1 0.1) ] <|
        row [ width (px 1280), height (px 720), centerX, centerY ]
            [ el
                [ width fill
                , height fill
                , Element.behindContent (glView model)
                , htmlAttribute (Mouse.onMove (clientPos >> Move))
                ]
                Element.none
            ]


clientPos : Mouse.Event -> (Float, Float)
clientPos event =
    let
        ( x, y ) =
            event.clientPos
    in
    (x, y)


glView : Model -> Element Msg
glView model =
    Element.html
        (WebGL.toHtml
            [ Html.Attributes.height 720
            , Html.Attributes.width 1280
            ]
            [ WebGL.entity vertexShader
                fragmentShader
                mesh
                { time = model.time / 1000
                , speed = model.speed
                , resolution = vec2 1280 720
                , xPos = model.xPos
                , targetXPos = model.targetXPos
                , distance = model.distance
                }
            ]
        )


type alias Model =
    { time : Float
    , speed : Float
    , distance : Float
    , xPos : Float
    , targetXPos : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { time = 0
      , speed = 0
      , distance = 0
      , xPos = 0
      , targetXPos = 0
      }
    , Cmd.none
    )


type Msg
    = TimeDelta Float
    | Move (Float, Float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeDelta delta ->
            ( { model
                | time = model.time + delta
                , speed = model.speed + delta * 0.05
                , distance = model.distance + model.speed * 0.0001
                , xPos = model.xPos * 0.9 + model.targetXPos * 0.1
              }
            , Cmd.none
            )
        Move (x, y) ->
            ( { model
                | targetXPos = ((x / 1280) - 0.5) * 2
              }
            , Cmd.none
            )


mesh : WebGL.Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 -1 1 0)
          , Vertex (vec3 1 1 0)
          , Vertex (vec3 -1 -1 0)
          )
        , ( Vertex (vec3 -1 -1 0)
          , Vertex (vec3 1 1 0)
          , Vertex (vec3 1 -1 0)
          )
        ]
