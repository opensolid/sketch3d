module OpenSolid.Illustration.Geometry
    exposing
        ( Geometry
        , indexedTriangles
        , triangles
        )

import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Illustration.Placement as Placement
import OpenSolid.Illustration.Types as Types
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Triangle3d as Triangle3d
import WebGL


type alias Geometry =
    Types.Geometry


diagonalSize : BoundingBox3d -> Float
diagonalSize boundingBox =
    let
        dimensions =
            BoundingBox3d.dimensions boundingBox
    in
    Vector3d.length (Vector3d dimensions)


triangles : List Triangle3d -> Geometry
triangles triangles_ =
    case BoundingBox3d.hullOf (List.map Triangle3d.boundingBox triangles_) of
        Just boundingBox ->
            let
                size =
                    diagonalSize boundingBox

                vertexAttributes position =
                    { position = position
                    , surfaceIndex = 1
                    , surfaceSize = size
                    }

                toAttributes triangle =
                    let
                        ( p1, p2, p3 ) =
                            Triangle3d.vertexPositions triangle
                    in
                    ( vertexAttributes p1
                    , vertexAttributes p2
                    , vertexAttributes p3
                    )

                mesh =
                    WebGL.triangles (List.map toAttributes triangles_)

                rawGeometry =
                    Types.Triangles triangles_
            in
            Types.Geometry Placement.identity boundingBox mesh rawGeometry

        Nothing ->
            Types.EmptyGeometry


indexedTriangles : List Point3d -> List ( Int, Int, Int ) -> Geometry
indexedTriangles vertices faces =
    case BoundingBox3d.containing vertices of
        Just boundingBox ->
            let
                size =
                    diagonalSize boundingBox

                toAttributes vertex =
                    { position = Point3d.toVec3 vertex
                    , surfaceIndex = 1
                    , surfaceSize = size
                    }

                vertexAttributes =
                    List.map toAttributes vertices

                mesh =
                    WebGL.indexedTriangles vertexAttributes faces

                rawGeometry =
                    Types.IndexedTriangles vertices faces
            in
            Types.Geometry Placement.identity boundingBox mesh rawGeometry

        Nothing ->
            Types.EmptyGeometry
