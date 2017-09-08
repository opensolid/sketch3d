module Surfaces exposing (..)

import Color
import Html exposing (Html)
import Html.Attributes as Attributes
import Kintail.InputWidget as InputWidget
import OpenSolid.Arc3d as Arc3d
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Camera as Camera
import OpenSolid.Curve3d as Curve3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Parametric.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.QuadraticSpline3d as QuadraticSpline3d
import OpenSolid.Sketch3d as Sketch3d exposing (Sketch3d)
import OpenSolid.Surface3d as Surface3d
import OpenSolid.Vector3d as Vector3d


surfaces : List Surface3d
surfaces =
    let
        p0 =
            Point3d.withCoordinates ( 0, 0, 3 )

        p1 =
            Point3d.withCoordinates ( 3, 0, 3 )

        p2 =
            Point3d.withCoordinates ( 4, 0, 3 )

        p3 =
            Point3d.withCoordinates ( 5, 0, 2 )

        p4 =
            Point3d.withCoordinates ( 5, 0, 0 )

        p5 =
            Point3d.withCoordinates ( 6, 0, 0 )

        line1 =
            Curve3d.lineSegment <| LineSegment3d.from p0 p1

        line2 =
            Curve3d.lineSegment <| LineSegment3d.from p1 p2

        arc =
            let
                axis =
                    Axis3d.with
                        { originPoint = Point3d.withCoordinates ( 4, 0, 2 )
                        , direction = Direction3d.y
                        }
            in
            Curve3d.arc <|
                Arc3d.around axis
                    { startPoint = p2
                    , sweptAngle = degrees 90
                    }

        spline =
            Curve3d.quadraticSpline <|
                QuadraticSpline3d.withControlPoints ( p3, p4, p5 )

        extrusionVector =
            Vector3d.withComponents ( 0, 3, 0 )

        extrusionSurfaces1 =
            [ line2, arc, spline ]
                |> List.map
                    (\curve -> Surface3d.extrusion curve extrusionVector)

        extrusionSurfaces2 =
            let
                rotationAxis =
                    Axis3d.with
                        { originPoint = Point3d.withCoordinates ( 1.5, 1.5, 0 )
                        , direction = Direction3d.z
                        }
            in
            extrusionSurfaces1
                |> List.map (Surface3d.rotateAround rotationAxis (degrees 90))

        squareSurface =
            Surface3d.extrusion line1 extrusionVector

        revolutionSurfaces =
            let
                revolutionAxis =
                    Axis3d.with
                        { originPoint = Point3d.withCoordinates ( 3, 3, 3 )
                        , direction = Direction3d.z
                        }
            in
            [ line2, arc, spline ]
                |> List.map (Curve3d.translateBy extrusionVector)
                |> List.map
                    (\curve ->
                        Surface3d.revolution curve revolutionAxis (degrees 90)
                    )
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
        |> Sketch3d.relativeTo (Frame3d.at (Point3d.withCoordinates ( 2, 2, 0 )))


view : Float -> Html Float
view angleInDegrees =
    let
        width =
            1024

        height =
            768

        eyePoint =
            Point3d.withCoordinates ( 20, 0, 0 )
                |> Point3d.rotateAround Axis3d.y (degrees -30)
                |> Point3d.rotateAround Axis3d.z (degrees 45)

        eyeFrame =
            Camera.lookAt
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
