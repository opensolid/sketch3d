module Surfaces exposing (..)

import Color
import Html exposing (Html)
import Html.Attributes as Attributes
import Kintail.InputWidget as InputWidget
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Curve3d as Curve3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Parametric.Types exposing (..)
import OpenSolid.Plane3d as Plane3d
import OpenSolid.Point3d as Point3d
import OpenSolid.Sketch3d as Sketch3d exposing (Sketch3d)
import OpenSolid.Surface3d as Surface3d
import OpenSolid.WebGL.Camera as Camera
import OpenSolid.WebGL.Frame3d as Frame3d
import WebGL


surfaces : List Surface3d
surfaces =
    let
        p0 =
            Point3d ( 0, 0, 3 )

        p1 =
            Point3d ( 3, 0, 3 )

        p2 =
            Point3d ( 4, 0, 3 )

        p3 =
            Point3d ( 5, 0, 2 )

        p4 =
            Point3d ( 5, 0, 0 )

        p5 =
            Point3d ( 6, 0, 0 )

        line1 =
            Curve3d.lineSegment ( p0, p1 )

        line2 =
            Curve3d.lineSegment ( p1, p2 )

        arc =
            let
                axis =
                    Axis3d
                        { originPoint = Point3d ( 4, 0, 2 )
                        , direction = Direction3d.y
                        }
            in
            Curve3d.arc
                { startPoint = p2
                , axis = axis
                , sweptAngle = degrees 90
                }

        spline =
            Curve3d.quadraticSpline ( p3, p4, p5 )

        extrusionVector =
            Vector3d ( 0, 3, 0 )

        extrusionSurfaces1 =
            [ line2, arc, spline ]
                |> List.map (Curve3d.extrudeBy extrusionVector)

        extrusionSurfaces2 =
            let
                rotationAxis =
                    Axis3d
                        { originPoint = Point3d ( 1.5, 1.5, 0 )
                        , direction = Direction3d.z
                        }
            in
            extrusionSurfaces1
                |> List.map (Surface3d.rotateAround rotationAxis (degrees 90))

        squareSurface =
            Curve3d.extrudeBy extrusionVector line1

        revolutionSurfaces =
            let
                revolutionAxis =
                    Axis3d
                        { originPoint = Point3d ( 3, 3, 3 )
                        , direction = Direction3d.z
                        }
            in
            [ line2, arc, spline ]
                |> List.map (Curve3d.translateBy extrusionVector)
                |> List.map (Curve3d.revolveAround revolutionAxis (degrees 90))
    in
    List.concat
        [ [ squareSurface ]
        , extrusionSurfaces1
        , extrusionSurfaces2
        , revolutionSurfaces
        ]


sketch : Sketch3d
sketch =
    surfaces
        |> List.map (Sketch3d.surface Color.lightBlue 0.001)
        |> Sketch3d.group
        |> Sketch3d.relativeTo (Frame3d.at (Point3d ( 2, 2, 0 )))


view : Float -> Html Float
view angleInDegrees =
    let
        width =
            1024

        height =
            768

        eyePoint =
            Point3d ( 20, 0, 0 )
                |> Point3d.rotateAround Axis3d.y (degrees -30)
                |> Point3d.rotateAround Axis3d.z (degrees 45)

        eyeFrame =
            Frame3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = eyePoint
                , upDirection = Direction3d.z
                }

        camera =
            Camera.perspective
                { frame = eyeFrame
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
