module PerspectiveLogos exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Kintail.InputWidget as InputWidget
import Logo
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Illustration as Illustration
import OpenSolid.Plane3d as Plane3d
import OpenSolid.Point3d as Point3d
import OpenSolid.WebGL.Camera as Camera
import OpenSolid.WebGL.Frame3d as Frame3d
import Time
import WebGL


view : Float -> Html Float
view angleInDegrees =
    let
        width =
            800

        height =
            600

        eyePoint =
            Point3d ( 10, 0, 0 )
                |> Point3d.rotateAround Axis3d.y (degrees -22.5)
                |> Point3d.rotateAround Axis3d.z (degrees 60)

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

        angle =
            degrees angleInDegrees

        rotatedLogo =
            Logo.illustration |> Illustration.rotateAround Axis3d.z angle

        rightLogo =
            rotatedLogo |> Illustration.translateBy (Vector3d ( 1, 1, 0.75 ))

        leftLogo =
            rightLogo |> Illustration.mirrorAcross Plane3d.zx

        frontLogos =
            Illustration.group [ rightLogo, leftLogo ]

        backLogos =
            frontLogos |> Illustration.mirrorAcross Plane3d.yz

        topLogos =
            Illustration.group [ frontLogos, backLogos ]

        bottomLogos =
            topLogos |> Illustration.mirrorAcross Plane3d.xy

        scene =
            Illustration.group [ topLogos, bottomLogos ]

        sliderAttributes =
            [ Attributes.style [ ( "width", toString width ++ "px" ) ] ]

        sliderConfig =
            { min = 0
            , max = 360
            , step = 1
            }
    in
    Html.div []
        [ Html.div [] [ Illustration.render camera scene ]
        , InputWidget.slider sliderAttributes sliderConfig angleInDegrees
        ]


main : Program Never Float Float
main =
    Html.beginnerProgram
        { model = 0.0
        , view = view
        , update = always
        }
