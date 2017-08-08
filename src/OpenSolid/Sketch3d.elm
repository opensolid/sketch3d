module OpenSolid.Sketch3d
    exposing
        ( Sketch3d
        , curve
        , group
        , indexedTriangles
        , mirrorAcross
        , placeIn
        , points
        , relativeTo
        , render
        , rotateAround
        , translateBy
        )

import Array.Hamt as Array
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Sketch3d.EdgeSet as EdgeSet exposing (EdgeSet)
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.WebGL.Camera as Camera exposing (Camera)
import OpenSolid.WebGL.Frame3d as Frame3d
import OpenSolid.WebGL.Point3d as Point3d
import WebGL
import WebGL.Settings
import WebGL.Settings.DepthTest
import WebGL.Texture as Texture exposing (Texture)


type alias SurfaceVertex =
    { position : Point3d
    }


type alias CachedSurfaceVertex =
    { position : Point3d
    , color : Vec3
    }


type alias SurfaceVertexAttributes =
    { position : Vec3
    , color : Vec3
    }


type alias Face =
    ( SurfaceVertex, SurfaceVertex, SurfaceVertex )


type alias CachedFace =
    ( CachedSurfaceVertex, CachedSurfaceVertex, CachedSurfaceVertex )


type alias FaceAttributes =
    ( SurfaceVertexAttributes, SurfaceVertexAttributes, SurfaceVertexAttributes )


type alias CachedCurveVertex =
    { position : Point3d
    , color : Vec4
    }


type alias CurveVertexAttributes =
    { position : Vec3
    , color : Vec4
    }


type alias Edge =
    ( Point3d, Point3d )


type alias CachedEdge =
    ( CachedCurveVertex, CachedCurveVertex )


type alias EdgeAttributes =
    ( CurveVertexAttributes, CurveVertexAttributes )


type alias CachedPoint =
    { position : Point3d
    , color : Vec4
    , radius : Float
    }


type alias PointAttributes =
    { position : Vec3
    , color : Vec4
    , radius : Float
    }


type Sketch3d
    = Surface
        { mesh : WebGL.Mesh SurfaceVertexAttributes
        , cachedFaces : List CachedFace
        , boundingBox : BoundingBox3d
        }
    | Curve
        { mesh : WebGL.Mesh CurveVertexAttributes
        , cachedEdges : List CachedEdge
        , boundingBox : BoundingBox3d
        }
    | Points
        { mesh : WebGL.Mesh PointAttributes
        , cachedPoints : List CachedPoint
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


toVec4 : Color -> Vec4
toVec4 color_ =
    let
        { red, green, blue, alpha } =
            Color.toRgb color_
    in
    vec4
        (toFloat red / 255.0)
        (toFloat green / 255.0)
        (toFloat blue / 255.0)
        alpha


toCachedSurfaceVertex : Vec3 -> SurfaceVertex -> CachedSurfaceVertex
toCachedSurfaceVertex color surfaceVertex =
    { position = surfaceVertex.position
    , color = color
    }


toSurfaceVertexAttributes : CachedSurfaceVertex -> SurfaceVertexAttributes
toSurfaceVertexAttributes cachedSurfaceVertex =
    { position = Point3d.toVec3 cachedSurfaceVertex.position
    , color = cachedSurfaceVertex.color
    }


toCachedFace : Vec3 -> Face -> CachedFace
toCachedFace color ( firstSurfaceVertex, secondSurfaceVertex, thirdSurfaceVertex ) =
    ( toCachedSurfaceVertex color firstSurfaceVertex
    , toCachedSurfaceVertex color secondSurfaceVertex
    , toCachedSurfaceVertex color thirdSurfaceVertex
    )


toFaceAttributes : CachedFace -> FaceAttributes
toFaceAttributes ( cachedSurfaceVertex1, cachedSurfaceVertex2, cachedSurfaceVertex3 ) =
    ( toSurfaceVertexAttributes cachedSurfaceVertex1
    , toSurfaceVertexAttributes cachedSurfaceVertex2
    , toSurfaceVertexAttributes cachedSurfaceVertex3
    )


toCachedCurveVertex : Vec4 -> Point3d -> CachedCurveVertex
toCachedCurveVertex color position =
    { position = position
    , color = color
    }


toCurveVertexAttributes : CachedCurveVertex -> CurveVertexAttributes
toCurveVertexAttributes cachedCurveVertex =
    { position = Point3d.toVec3 cachedCurveVertex.position
    , color = cachedCurveVertex.color
    }


toCachedEdge : Vec4 -> Edge -> CachedEdge
toCachedEdge color ( startPosition, endPosition ) =
    ( toCachedCurveVertex color startPosition
    , toCachedCurveVertex color endPosition
    )


toEdgeAttributes : CachedEdge -> EdgeAttributes
toEdgeAttributes ( cachedStartVertex, cachedEndVertex ) =
    ( toCurveVertexAttributes cachedStartVertex
    , toCurveVertexAttributes cachedEndVertex
    )


toCachedPoint : Float -> Vec4 -> Point3d -> CachedPoint
toCachedPoint radius color point =
    { position = point
    , color = color
    , radius = radius
    }


toPointAttributes : CachedPoint -> PointAttributes
toPointAttributes cachedPoint =
    { position = Point3d.toVec3 cachedPoint.position
    , color = cachedPoint.color
    , radius = cachedPoint.radius
    }


faceBounds : Face -> BoundingBox3d
faceBounds ( firstVertex, secondVertex, thirdVertex ) =
    Triangle3d.boundingBox <|
        Triangle3d
            ( firstVertex.position
            , secondVertex.position
            , thirdVertex.position
            )


edgeBounds : Edge -> BoundingBox3d
edgeBounds ( startPosition, endPosition ) =
    Point3d.hull startPosition endPosition


indexedTriangles : Color -> List Point3d -> List ( Int, Int, Int ) -> Sketch3d
indexedTriangles color points faceIndices =
    let
        pointArray =
            Array.fromList points

        edgeSet =
            EdgeSet.build faceIndices

        toFace ( i1, i2, i3 ) =
            Maybe.map3
                (\p1 p2 p3 ->
                    ( { position = p1 }
                    , { position = p2 }
                    , { position = p3 }
                    )
                )
                (Array.get i1 pointArray)
                (Array.get i2 pointArray)
                (Array.get i3 pointArray)

        faces =
            List.filterMap toFace faceIndices

        toEdge ( i, j ) =
            Maybe.map2 (,) (Array.get i pointArray) (Array.get j pointArray)

        edges =
            List.filterMap toEdge (EdgeSet.openEdges edgeSet)
    in
    group [ surface color faces, curve (Color.rgba 127 127 127 1) edges ]


surface : Color -> List Face -> Sketch3d
surface color faces =
    case BoundingBox3d.hullOf (List.map faceBounds faces) of
        Just boundingBox ->
            let
                cachedFaces =
                    List.map (toCachedFace (toVec3 color)) faces

                faceAttributes =
                    List.map toFaceAttributes cachedFaces

                mesh =
                    WebGL.triangles faceAttributes
            in
            Surface
                { mesh = mesh
                , cachedFaces = cachedFaces
                , boundingBox = boundingBox
                }

        Nothing ->
            Empty


curve : Color -> List Edge -> Sketch3d
curve color edges =
    case BoundingBox3d.hullOf (List.map edgeBounds edges) of
        Just boundingBox ->
            let
                cachedEdges =
                    List.map (toCachedEdge (toVec4 color)) edges

                edgeAttributes =
                    List.map toEdgeAttributes cachedEdges

                mesh =
                    WebGL.lines edgeAttributes
            in
            Curve
                { mesh = mesh
                , cachedEdges = cachedEdges
                , boundingBox = boundingBox
                }

        Nothing ->
            Empty


points : Float -> Color -> List Point3d -> Sketch3d
points radius color points_ =
    case BoundingBox3d.containing points_ of
        Just boundingBox ->
            let
                cachedPoints =
                    List.map (toCachedPoint radius (toVec4 color)) points_

                pointAttributes =
                    List.map toPointAttributes cachedPoints

                mesh =
                    WebGL.points pointAttributes
            in
            Points
                { mesh = mesh
                , cachedPoints = cachedPoints
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
        Surface _ ->
            Placed
                { placementFrame = frameTransformation Frame3d.xyz
                , isMirror = isMirror
                , sketch = sketch
                }

        Curve _ ->
            Placed
                { placementFrame = frameTransformation Frame3d.xyz
                , isMirror = isMirror
                , sketch = sketch
                }

        Points _ ->
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


type alias SurfaceUniforms =
    { modelViewProjectionMatrix : Mat4
    }


type alias SurfaceVaryings =
    { interpolatedColor : Vec3
    }


surfaceVertexShader : WebGL.Shader SurfaceVertexAttributes SurfaceUniforms SurfaceVaryings
surfaceVertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;

        uniform mat4 modelViewProjectionMatrix;

        varying vec3 interpolatedColor;

        void main () {
            gl_Position = modelViewProjectionMatrix * vec4(position, 1.0);
            interpolatedColor = color;
        }
    |]


surfaceFragmentShader : WebGL.Shader {} SurfaceUniforms SurfaceVaryings
surfaceFragmentShader =
    [glsl|
        precision highp float;

        varying vec3 interpolatedColor;

        void main() {
            gl_FragColor = vec4(interpolatedColor, 1.0);
        }
    |]


surfaceToEntity : Camera -> Float -> Frame3d -> Bool -> WebGL.Mesh SurfaceVertexAttributes -> WebGL.Entity
surfaceToEntity camera pixelScale placementFrame isMirror mesh =
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
            , WebGL.Settings.polygonOffset 1 1
            ]

        uniforms =
            { modelViewProjectionMatrix = modelViewProjectionMatrix
            }
    in
    WebGL.entityWith settings
        surfaceVertexShader
        surfaceFragmentShader
        mesh
        uniforms


type alias CurveUniforms =
    { modelViewProjectionMatrix : Mat4
    }


type alias CurveVaryings =
    { interpolatedColor : Vec4
    }


curveVertexShader : WebGL.Shader CurveVertexAttributes CurveUniforms CurveVaryings
curveVertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec4 color;

        uniform mat4 modelViewProjectionMatrix;

        varying vec4 interpolatedColor;

        void main () {
            gl_Position = modelViewProjectionMatrix * vec4(position, 1.0);
            interpolatedColor = color;
        }
    |]


curveFragmentShader : WebGL.Shader {} CurveUniforms CurveVaryings
curveFragmentShader =
    [glsl|
        precision mediump float;

        varying vec4 interpolatedColor;

        void main() {
            gl_FragColor = interpolatedColor;
        }
    |]


curveToEntity : Camera -> Frame3d -> WebGL.Mesh CurveVertexAttributes -> WebGL.Entity
curveToEntity camera placementFrame mesh =
    let
        modelViewMatrix =
            Frame3d.modelViewMatrix (Camera.frame camera) placementFrame

        projectionMatrix =
            Camera.projectionMatrix camera

        modelViewProjectionMatrix =
            Math.Matrix4.mul projectionMatrix modelViewMatrix

        uniforms =
            { modelViewProjectionMatrix = modelViewProjectionMatrix
            }
    in
    WebGL.entity curveVertexShader curveFragmentShader mesh uniforms


type alias PointUniforms =
    { modelViewProjectionMatrix : Mat4
    }


type alias PointVaryings =
    { interpolatedColor : Vec4
    }


pointVertexShader : WebGL.Shader PointAttributes PointUniforms PointVaryings
pointVertexShader =
    [glsl|
        attribute vec3 position;
        attribute float radius;
        attribute vec4 color;

        uniform mat4 modelViewProjectionMatrix;

        varying vec4 interpolatedColor;

        void main() {
            gl_Position = modelViewProjectionMatrix * vec4(position, 1.0);
            gl_PointSize = 2.0 * radius;
            interpolatedColor = color;
        }
    |]


pointFragmentShader : WebGL.Shader {} PointUniforms PointVaryings
pointFragmentShader =
    [glsl|
        precision mediump float;

        varying vec4 interpolatedColor;

        void main() {
            float x = (2.0 * gl_PointCoord.x - 1.0);
            float y = (1.0 - 2.0 * gl_PointCoord.y);
            if (x * x + y * y > 1.0) {
                discard;
            }
            gl_FragColor = interpolatedColor;
        }
    |]


pointsToEntity : Camera -> Frame3d -> WebGL.Mesh PointAttributes -> WebGL.Entity
pointsToEntity camera placementFrame mesh =
    let
        modelViewMatrix =
            Frame3d.modelViewMatrix (Camera.frame camera) placementFrame

        projectionMatrix =
            Camera.projectionMatrix camera

        modelViewProjectionMatrix =
            Math.Matrix4.mul projectionMatrix modelViewMatrix

        uniforms =
            { modelViewProjectionMatrix = modelViewProjectionMatrix
            }
    in
    WebGL.entity pointVertexShader pointFragmentShader mesh uniforms


collectEntities : Camera -> Float -> Frame3d -> Bool -> Sketch3d -> List WebGL.Entity -> List WebGL.Entity
collectEntities camera pixelScale currentFrame currentMirror sketch accumulated =
    case sketch of
        Surface { mesh } ->
            surfaceToEntity camera pixelScale currentFrame currentMirror mesh :: accumulated

        Curve { mesh } ->
            curveToEntity camera currentFrame mesh :: accumulated

        Points { mesh } ->
            pointsToEntity camera currentFrame mesh :: accumulated

        Placed properties ->
            collectEntities
                camera
                pixelScale
                (Frame3d.placeIn currentFrame properties.placementFrame)
                (currentMirror /= properties.isMirror)
                properties.sketch
                accumulated

        Group sketches ->
            List.foldl
                (collectEntities camera pixelScale currentFrame currentMirror)
                accumulated
                sketches

        Empty ->
            accumulated


render : Camera -> Sketch3d -> Html msg
render camera sketch =
    let
        width =
            Camera.screenWidth camera

        height =
            Camera.screenHeight camera

        projectionMatrix =
            Camera.projectionMatrix camera

        pixelScale =
            (Math.Matrix4.toRecord projectionMatrix).m11 * width / 2

        entities =
            collectEntities camera pixelScale Frame3d.xyz False sketch []
    in
    WebGL.toHtmlWith
        [ WebGL.alpha True
        , WebGL.antialias
        , WebGL.depth 1
        ]
        [ Html.Attributes.width (2 * round width)
        , Html.Attributes.height (2 * round height)
        , Html.Attributes.style
            [ ( "width", toString width ++ "px" )
            , ( "height", toString height ++ "px" )
            ]
        ]
        entities
