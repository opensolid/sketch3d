module Body exposing (..)

import Color
import Html exposing (Html)
import Html.Attributes as Attributes
import Kintail.InputWidget as InputWidget
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Body3d as Body3d
import OpenSolid.Camera as Camera
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Parametric.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Rectangle2d as Rectangle2d
import OpenSolid.Region2d as Region2d
import OpenSolid.Sketch3d as Sketch3d exposing (Sketch3d)
import OpenSolid.SketchPlane3d as SketchPlane3d
import OpenSolid.Surface3d as Surface3d
import OpenSolid.Viewpoint as Viewpoint


roundedRectangle : Region2d
roundedRectangle =
    let
        rectangle =
            Rectangle2d.centeredOn Frame2d.xy ( 6, 4 )
    in
    Region2d.roundedRectangle rectangle 1


body : Body3d
body =
    Body3d.extrusion roundedRectangle SketchPlane3d.xy 2


surface : Surface3d
surface =
    Surface3d.planar roundedRectangle SketchPlane3d.xy


sketch : Sketch3d
sketch =
    Sketch3d.body Color.lightBlue 0.001 body


view : Float -> Html Float
view angleInDegrees =
    let
        width =
            1024

        height =
            768

        eyePoint =
            Point3d.fromCoordinates ( 20, 0, 0 )
                |> Point3d.rotateAround Axis3d.y (degrees -30)
                |> Point3d.rotateAround Axis3d.z (degrees 45)

        viewpoint =
            Viewpoint.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = eyePoint
                , upDirection = Direction3d.z
                }

        camera =
            Camera.perspective
                { viewpoint = viewpoint
                , screenWidth = toFloat width
                , screenHeight = toFloat height
                , verticalFieldOfView = degrees 30
                , nearClipDistance = 0.1
                , farClipDistance = 100
                }

        scene =
            sketch |> Sketch3d.rotateAround Axis3d.z (degrees angleInDegrees)

        sliderAttributes =
            [ Attributes.style [ ( "width", toString width ++ "px" ) ] ]

        sliderConfig =
            { min = -180
            , max = 180
            , step = 1
            }
    in
    Html.div []
        [ Html.div [] [ Sketch3d.render camera scene ]
        , InputWidget.slider sliderAttributes sliderConfig angleInDegrees
        ]


main : Program Never Float Float
main =
    Html.beginnerProgram
        { model = 0.0
        , view = view
        , update = always
        }
