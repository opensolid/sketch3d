module OpenSolid.Sketch3d
    exposing
        ( Sketch3d
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
import Math.Vector2 exposing (Vec2, vec2)
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
import WebGL.Texture as Texture exposing (Texture)


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


type Sketch3d
    = Leaf
        { mesh : WebGL.Mesh Attributes
        , vertices : List Vertex
        , faces : List ( Int, Int, Int )
        , boundingBox : BoundingBox3d
        }
    | Placed
        { placementFrame : Frame3d
        , isMirror : Bool
        , sketch : Sketch3d
        }
    | Group (List Sketch3d)
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


indexedTriangles : Color -> List Point3d -> List ( Int, Int, Int ) -> Sketch3d
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


group : List Sketch3d -> Sketch3d
group =
    Group


transformBy : (Frame3d -> Frame3d) -> Bool -> Sketch3d -> Sketch3d
transformBy frameTransformation isMirror sketch =
    case sketch of
        Leaf _ ->
            Placed
                { placementFrame = frameTransformation Frame3d.xyz
                , isMirror = isMirror
                , sketch = sketch
                }

        Group _ ->
            Placed
                { placementFrame = frameTransformation Frame3d.xyz
                , isMirror = isMirror
                , sketch = sketch
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
                , sketch = properties.sketch
                }

        Empty ->
            Empty


translateBy : Vector3d -> Sketch3d -> Sketch3d
translateBy displacement sketch =
    transformBy (Frame3d.translateBy displacement) False sketch


rotateAround : Axis3d -> Float -> Sketch3d -> Sketch3d
rotateAround axis angle sketch =
    transformBy (Frame3d.rotateAround axis angle) False sketch


mirrorAcross : Plane3d -> Sketch3d -> Sketch3d
mirrorAcross plane sketch =
    transformBy (Frame3d.mirrorAcross plane) True sketch


placeIn : Frame3d -> Sketch3d -> Sketch3d
placeIn frame sketch =
    let
        isMirror =
            Frame3d.isRightHanded frame
    in
    transformBy (Frame3d.placeIn frame) isMirror sketch


relativeTo : Frame3d -> Sketch3d -> Sketch3d
relativeTo frame sketch =
    let
        isMirror =
            Frame3d.isRightHanded frame
    in
    transformBy (Frame3d.relativeTo frame) isMirror sketch


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


collectEntities : Camera -> Frame3d -> Bool -> Sketch3d -> List WebGL.Entity -> List WebGL.Entity
collectEntities camera currentFrame currentMirror sketch accumulated =
    case sketch of
        Leaf { mesh } ->
            toEntity camera currentFrame currentMirror mesh :: accumulated

        Placed properties ->
            collectEntities camera
                (Frame3d.placeIn currentFrame properties.placementFrame)
                (currentMirror /= properties.isMirror)
                properties.sketch
                accumulated

        Group sketches ->
            List.foldl (collectEntities camera currentFrame currentMirror)
                accumulated
                sketches

        Empty ->
            accumulated


type alias PostProcessAttributes =
    { position : Vec2
    , textureCoordinates : Vec2
    }


type alias PostProcessUniforms =
    { texture : Texture
    , dx : Float
    , dy : Float
    }


type alias PostProcessVaryings =
    { interpolatedTextureCoordinates : Vec2
    }


postProcessVertexShader : WebGL.Shader PostProcessAttributes PostProcessUniforms PostProcessVaryings
postProcessVertexShader =
    [glsl|
        attribute vec2 position;
        attribute vec2 textureCoordinates;

        varying vec2 interpolatedTextureCoordinates;

        void main() {
            gl_Position = vec4(position, 0, 1);
            interpolatedTextureCoordinates = textureCoordinates;
        }
    |]


postProcessFragmentShader : WebGL.Shader {} PostProcessUniforms PostProcessVaryings
postProcessFragmentShader =
    [glsl|
        precision mediump float;

        uniform sampler2D texture;
        uniform float dx;
        uniform float dy;

        varying vec2 interpolatedTextureCoordinates;

        void main() {
            float x1 = interpolatedTextureCoordinates.x;
            float y1 = interpolatedTextureCoordinates.y;
            float x0 = x1 - dx;
            float x2 = x1 + dx;
            float y0 = y1 - dx;
            float y2 = y1 + dx;

            vec4 col00 = texture2D(texture, vec2(x0, y0));
            vec4 col10 = texture2D(texture, vec2(x1, y0));
            vec4 col20 = texture2D(texture, vec2(x2, y0));
            vec4 col01 = texture2D(texture, vec2(x0, y1));
            vec4 col11 = texture2D(texture, vec2(x1, y1));
            vec4 col21 = texture2D(texture, vec2(x2, y1));
            vec4 col02 = texture2D(texture, vec2(x0, y2));
            vec4 col12 = texture2D(texture, vec2(x1, y2));
            vec4 col22 = texture2D(texture, vec2(x2, y2));

            float weight = 0.0;
            weight += float(col11 != col12);
            weight += float(col11 != col21);
            weight += float(col11 != col01);
            weight += float(col11 != col10);
            weight += 0.5 * float(col11 != col00);
            weight += 0.5 * float(col11 != col22);
            weight += 0.5 * float(col11 != col02);
            weight += 0.5 * float(col11 != col20);
            weight = clamp(weight, 0.0, 0.5);

            gl_FragColor = (1.0 - weight) * col00 + vec4(0, 0, 0, weight);


            //gl_FragColor = texture2D(texture, interpolatedTextureCoordinates);
            //gl_FragColor = vec4(0, 0, 1, 1);
        }
    |]


screenSpaceQuad : WebGL.Mesh PostProcessAttributes
screenSpaceQuad =
    WebGL.indexedTriangles
        [ { position = vec2 -1 -1, textureCoordinates = vec2 0 0 }
        , { position = vec2 1 -1, textureCoordinates = vec2 1 0 }
        , { position = vec2 1 1, textureCoordinates = vec2 1 1 }
        , { position = vec2 -1 1, textureCoordinates = vec2 0 1 }
        ]
        [ ( 0, 1, 2 )
        , ( 0, 2, 3 )
        ]


render : Camera -> Sketch3d -> Html msg
render camera sketch =
    let
        width =
            round (Camera.screenWidth camera)

        height =
            round (Camera.screenHeight camera)

        entities =
            collectEntities camera Frame3d.xyz False sketch []

        textureOptions =
            { magnify = Texture.linear
            , minify = Texture.nearestMipmapLinear
            , horizontalWrap = Texture.mirroredRepeat
            , verticalWrap = Texture.mirroredRepeat
            , flipY = False
            }

        textureDimensions =
            ( 2 * width, 2 * height )

        textureResult =
            Texture.fromEntities textureOptions textureDimensions entities
    in
    case textureResult of
        Ok texture ->
            let
                postProcessed =
                    WebGL.entity
                        postProcessVertexShader
                        postProcessFragmentShader
                        screenSpaceQuad
                        { texture = texture
                        , dx = 1 / (2 * toFloat width)
                        , dy = 1 / (2 * toFloat height)
                        }
            in
            WebGL.toHtml
                [ Html.Attributes.width (2 * width)
                , Html.Attributes.height (2 * height)
                , Html.Attributes.style
                    [ ( "width", toString width ++ "px" )
                    , ( "height", toString height ++ "px" )
                    ]
                ]
                [ postProcessed ]

        Err _ ->
            Html.text "Error rendering to texture"
