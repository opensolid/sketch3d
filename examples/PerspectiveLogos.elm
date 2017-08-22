module PerspectiveLogos exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Kintail.InputWidget as InputWidget
import Logo
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Plane3d as Plane3d
import OpenSolid.Point3d as Point3d
import OpenSolid.Sketch3d as Sketch3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.WebGL.Camera as Camera
import OpenSolid.WebGL.Frame3d as Frame3d


view : Float -> Html Float
view angleInDegrees =
    let
        width =
            1024

        height =
            768

        eyePoint =
            Point3d.withCoordinates ( 10, 0, 0 )
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
            Logo.sketch |> Sketch3d.rotateAround Axis3d.z angle

        rightLogo =
            rotatedLogo
                |> Sketch3d.translateBy (Vector3d.withComponents ( 1, 1, 0.75 ))

        leftLogo =
            rightLogo |> Sketch3d.mirrorAcross Plane3d.zx

        frontLogos =
            Sketch3d.group [ rightLogo, leftLogo ]

        backLogos =
            frontLogos |> Sketch3d.mirrorAcross Plane3d.yz

        topLogos =
            Sketch3d.group [ frontLogos, backLogos ]

        bottomLogos =
            topLogos |> Sketch3d.mirrorAcross Plane3d.xy

        scene =
            Sketch3d.group [ topLogos, bottomLogos ]

        sliderAttributes =
            [ Attributes.style [ ( "width", toString width ++ "px" ) ] ]

        sliderConfig =
            { min = 0
            , max = 360
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
