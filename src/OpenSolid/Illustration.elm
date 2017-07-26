module OpenSolid.Illustration
    exposing
        ( Illustration
        , group
        , indexedTriangles
        , mirrorAcross
        , placeIn
        , relativeTo
        , render
        , rotateAround
        , translateBy
        )

import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.WebGL.Camera as Camera exposing (Camera)
import OpenSolid.WebGL.Frame3d as Frame3d
import OpenSolid.WebGL.Point3d as Point3d
import WebGL
import WebGL.Settings
import WebGL.Settings.DepthTest


type alias Vertex =
    { position : Point3d
    , color : Vec3
    , surfaceIndex : Int
    , surfaceSize : Float
    }


type alias Attributes =
    { position : Vec3
    , color : Vec3
    , surfaceIndex : Float
    , surfaceSize : Float
    }


type Illustration
    = Leaf
        { mesh : WebGL.Mesh Attributes
        , vertices : List Vertex
        , faces : List ( Int, Int, Int )
        , boundingBox : BoundingBox3d
        }
    | Placed
        { placementFrame : Frame3d
        , isMirror : Bool
        , illustration : Illustration
        }
    | Group (List Illustration)
    | Empty


toVec3 : Color -> Vec3
toVec3 color_ =
    let
        { red, green, blue } =
            Color.toRgb color_
    in
    vec3
        (toFloat red / 255.0)
        (toFloat green / 255.0)
        (toFloat blue / 255.0)


diagonalSize : BoundingBox3d -> Float
diagonalSize boundingBox =
    let
        dimensions =
            BoundingBox3d.dimensions boundingBox
    in
    Vector3d.length (Vector3d dimensions)


indexedTriangles : Color -> List Point3d -> List ( Int, Int, Int ) -> Illustration
indexedTriangles color points faces =
    case BoundingBox3d.containing points of
        Just boundingBox ->
            let
                surfaceSize =
                    diagonalSize boundingBox

                colorVec =
                    toVec3 color

                toVertex point =
                    { position = point
                    , color = colorVec
                    , surfaceIndex = 1
                    , surfaceSize = surfaceSize
                    }

                toAttributes point =
                    { position = Point3d.toVec3 point
                    , color = colorVec
                    , surfaceIndex = 1
                    , surfaceSize = surfaceSize
                    }

                vertexAttributes =
                    List.map toAttributes points

                mesh =
                    WebGL.indexedTriangles vertexAttributes faces
            in
            Leaf
                { mesh = mesh
                , vertices = List.map toVertex points
                , faces = faces
                , boundingBox = boundingBox
                }

        Nothing ->
            Empty


group : List Illustration -> Illustration
group =
    Group


transformBy : (Frame3d -> Frame3d) -> Bool -> Illustration -> Illustration
transformBy frameTransformation isMirror illustration =
    case illustration of
        Leaf _ ->
            Placed
                { placementFrame = frameTransformation Frame3d.xyz
                , isMirror = isMirror
                , illustration = illustration
                }

        Group _ ->
            Placed
                { placementFrame = frameTransformation Frame3d.xyz
                , isMirror = isMirror
                , illustration = illustration
                }

        Placed properties ->
            let
                updatedFrame =
                    frameTransformation properties.placementFrame

                updatedIsMirror =
                    isMirror /= properties.isMirror
            in
            Placed
                { placementFrame = updatedFrame
                , isMirror = updatedIsMirror
                , illustration = properties.illustration
                }

        Empty ->
            Empty


translateBy : Vector3d -> Illustration -> Illustration
translateBy displacement illustration =
    transformBy (Frame3d.translateBy displacement) False illustration


rotateAround : Axis3d -> Float -> Illustration -> Illustration
rotateAround axis angle illustration =
    transformBy (Frame3d.rotateAround axis angle) False illustration


mirrorAcross : Plane3d -> Illustration -> Illustration
mirrorAcross plane illustration =
    transformBy (Frame3d.mirrorAcross plane) True illustration


placeIn : Frame3d -> Illustration -> Illustration
placeIn frame illustration =
    let
        isMirror =
            Frame3d.isRightHanded frame
    in
    transformBy (Frame3d.placeIn frame) isMirror illustration


relativeTo : Frame3d -> Illustration -> Illustration
relativeTo frame illustration =
    let
        isMirror =
            Frame3d.isRightHanded frame
    in
    transformBy (Frame3d.relativeTo frame) isMirror illustration


type alias Uniforms =
    { modelViewProjectionMatrix : Mat4
    }


type alias Varyings =
    { interpolatedDepth : Float
    , interpolatedColor : Vec3
    , interpolatedSurfaceIndex : Float
    , interpolatedSurfaceSize : Float
    }


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        attribute float surfaceIndex;
        attribute float surfaceSize;

        uniform mat4 modelViewProjectionMatrix;

        varying float interpolatedDepth;
        varying vec3 interpolatedColor;
        varying float interpolatedSurfaceIndex;
        varying float interpolatedSurfaceSize;

        void main () {
            gl_Position = modelViewProjectionMatrix * vec4(position, 1.0);
            interpolatedDepth = gl_Position.w;
            interpolatedColor = color;
            interpolatedSurfaceIndex = surfaceIndex;
            interpolatedSurfaceSize = surfaceSize;
        }
    |]


colorShader : WebGL.Shader {} Uniforms Varyings
colorShader =
    [glsl|
        precision mediump float;

        varying float interpolatedDepth;
        varying vec3 interpolatedColor;
        varying float interpolatedSurfaceIndex;
        varying float interpolatedSurfaceSize;

        void main() {
            gl_FragColor = vec4(interpolatedColor, 1.0);
        }
    |]


toEntity : Camera -> Frame3d -> Bool -> WebGL.Mesh Attributes -> WebGL.Entity
toEntity camera placementFrame isMirror mesh =
    let
        modelViewMatrix =
            Frame3d.modelViewMatrix (Camera.frame camera) placementFrame

        projectionMatrix =
            Camera.projectionMatrix camera

        modelViewProjectionMatrix =
            Math.Matrix4.mul projectionMatrix modelViewMatrix

        cullSetting =
            if isMirror then
                WebGL.Settings.front
            else
                WebGL.Settings.back

        settings =
            [ WebGL.Settings.DepthTest.default
            , WebGL.Settings.cullFace cullSetting
            ]

        uniforms =
            { modelViewProjectionMatrix = modelViewProjectionMatrix
            }
    in
    WebGL.entityWith settings vertexShader colorShader mesh uniforms


collectEntities : Camera -> Frame3d -> Bool -> Illustration -> List WebGL.Entity -> List WebGL.Entity
collectEntities camera currentFrame currentMirror illustration accumulated =
    case illustration of
        Leaf { mesh } ->
            toEntity camera currentFrame currentMirror mesh :: accumulated

        Placed properties ->
            collectEntities camera
                (Frame3d.placeIn currentFrame properties.placementFrame)
                (currentMirror /= properties.isMirror)
                properties.illustration
                accumulated

        Group illustrations ->
            List.foldl (collectEntities camera currentFrame currentMirror)
                accumulated
                illustrations

        Empty ->
            accumulated


render : Camera -> Illustration -> Html msg
render camera illustration =
    let
        width =
            Camera.screenWidth camera

        height =
            Camera.screenHeight camera
    in
    WebGL.toHtml
        [ Html.Attributes.width (2 * round width)
        , Html.Attributes.height (2 * round height)
        , Html.Attributes.style
            [ ( "width", toString width ++ "px" )
            , ( "height", toString height ++ "px" )
            ]
        ]
        (collectEntities camera Frame3d.xyz False illustration [])
