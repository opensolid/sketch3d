module Cube exposing (..)

import Color exposing (Color)
import Html exposing (Html)
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Sketch3d as Sketch3d exposing (Sketch3d)
import OpenSolid.WebGL.Camera as Camera
import OpenSolid.WebGL.Frame3d as Frame3d


rectangle : Color -> Point3d -> Point3d -> Point3d -> Point3d -> Sketch3d
rectangle color p0 p1 p2 p3 =
    let
        edgeColor =
            Color.rgba 0 0 0 0.5
    in
    Sketch3d.indexedTriangles color
        [ p0, p1, p2, p3 ]
        [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]


main : Html msg
main =
    let
        orange =
            Color.rgb 240 173 0

        green =
            Color.rgb 127 209 59

        lightBlue =
            Color.rgb 96 181 204

        darkBlue =
            Color.rgb 90 99 120

        width =
            1024

        height =
            768

        p0 =
            Point3d ( -1, -1, -1 )

        p1 =
            Point3d ( 1, -1, -1 )

        p2 =
            Point3d ( 1, 1, -1 )

        p3 =
            Point3d ( -1, 1, -1 )

        p4 =
            Point3d ( -1, -1, 1 )

        p5 =
            Point3d ( 1, -1, 1 )

        p6 =
            Point3d ( 1, 1, 1 )

        p7 =
            Point3d ( -1, 1, 1 )

        frontSurface =
            rectangle lightBlue p1 p2 p6 p5

        topSurface =
            rectangle orange p5 p6 p7 p4

        rightSurface =
            rectangle green p2 p3 p7 p6

        cube =
            Sketch3d.group [ frontSurface, topSurface, rightSurface ]

        edges =
            Sketch3d.group

        eyeFrame =
            Frame3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = Point3d ( 10, 6, 6 )
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
    in
    Sketch3d.render camera cube
