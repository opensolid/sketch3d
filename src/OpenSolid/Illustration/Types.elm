module OpenSolid.Illustration.Types exposing (..)

import Math.Vector3 exposing (Vec3)
import OpenSolid.Geometry.Types exposing (..)
import WebGL


type RawGeometry
    = Triangles (List Triangle3d)
    | IndexedTriangles (List Point3d) (List ( Int, Int, Int ))


type alias Mesh =
    WebGL.Mesh
        { position : Vec3
        , surfaceIndex : Int
        , surfaceSize : Float
        }


type Placement
    = Placement
        { frame : Frame3d
        , scale : Float
        , isRightHanded : Bool
        }


type Geometry
    = Geometry Placement BoundingBox3d Mesh RawGeometry
    | EmptyGeometry
