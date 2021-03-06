module Logo
    exposing
        ( edges
        , sketch
        , vertices
        )

import Array.Hamt as Array
import Color
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.LineSegment3d as LineSegment3d exposing (LineSegment3d)
import OpenSolid.Mesh as Mesh exposing (Mesh)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Sketch3d as Sketch3d exposing (Sketch3d)
import OpenSolid.Vector3d as Vector3d


height : Float
height =
    0.9


xOffset : Float
xOffset =
    0.6


yOffset : Float
yOffset =
    0.6


zOffset : Float
zOffset =
    0.6


p0 : Point3d
p0 =
    Point3d.origin


p1 : Point3d
p1 =
    Point3d.fromCoordinates ( 1, 0, 0 )


p2 : Point3d
p2 =
    Point3d.fromCoordinates ( 1, 1, 0 )


p3 : Point3d
p3 =
    Point3d.fromCoordinates ( 0, 1, 0 )


p4 : Point3d
p4 =
    Point3d.fromCoordinates ( 0, 1, height )


p5 : Point3d
p5 =
    Point3d.fromCoordinates ( 0, 0, height )


p6 : Point3d
p6 =
    Point3d.fromCoordinates ( 1, 0, height )


p7 : Point3d
p7 =
    Point3d.fromCoordinates ( 1, 1 - yOffset, height )


p8 : Point3d
p8 =
    Point3d.fromCoordinates ( 1, 1, height - zOffset )


p9 : Point3d
p9 =
    Point3d.fromCoordinates ( 1 - xOffset, 1, height )


centerFrame : Frame3d
centerFrame =
    Frame3d.atPoint (Point3d.fromCoordinates ( 0.5, 0.5, height / 2 ))


vertices : List Point3d
vertices =
    [ p0, p1, p2, p3, p4, p5, p6, p7, p8, p9 ]
        |> List.map (Point3d.relativeTo centerFrame)


edges : List LineSegment3d
edges =
    [ LineSegment3d.from p0 p1
    , LineSegment3d.from p1 p2
    , LineSegment3d.from p2 p3
    , LineSegment3d.from p3 p0
    , LineSegment3d.from p0 p5
    , LineSegment3d.from p1 p6
    , LineSegment3d.from p2 p8
    , LineSegment3d.from p3 p4
    , LineSegment3d.from p5 p6
    , LineSegment3d.from p6 p7
    , LineSegment3d.from p7 p8
    , LineSegment3d.from p8 p9
    , LineSegment3d.from p7 p9
    , LineSegment3d.from p9 p4
    , LineSegment3d.from p4 p5
    ]
        |> List.map (LineSegment3d.relativeTo centerFrame)


sketch : Sketch3d
sketch =
    let
        orange =
            Color.rgb 240 173 0

        green =
            Color.rgb 127 209 59

        lightBlue =
            Color.rgb 96 181 204

        darkBlue =
            Color.rgb 90 99 120

        verticesWithDummyNormals =
            vertices
                |> List.map (\point -> ( point, Vector3d.zero ))
                |> Array.fromList

        leftFace =
            Sketch3d.mesh orange <|
                Mesh.with
                    { vertices = verticesWithDummyNormals
                    , faceIndices =
                        [ ( 1, 2, 8 )
                        , ( 1, 8, 7 )
                        , ( 1, 7, 6 )
                        ]
                    }

        rightFace =
            Sketch3d.mesh lightBlue <|
                Mesh.with
                    { vertices = verticesWithDummyNormals
                    , faceIndices =
                        [ ( 2, 3, 4 )
                        , ( 2, 4, 9 )
                        , ( 2, 9, 8 )
                        ]
                    }

        topFace =
            Sketch3d.mesh green <|
                Mesh.with
                    { vertices = verticesWithDummyNormals
                    , faceIndices =
                        [ ( 6, 7, 9 )
                        , ( 6, 9, 4 )
                        , ( 6, 4, 5 )
                        ]
                    }

        triangleFace =
            Sketch3d.mesh darkBlue <|
                Mesh.with
                    { vertices = verticesWithDummyNormals
                    , faceIndices =
                        [ ( 7, 8, 9 ) ]
                    }

        bottomFace =
            Sketch3d.mesh green <|
                Mesh.with
                    { vertices = verticesWithDummyNormals
                    , faceIndices =
                        [ ( 0, 3, 2 )
                        , ( 0, 2, 1 )
                        ]
                    }

        backLeftFace =
            Sketch3d.mesh lightBlue <|
                Mesh.with
                    { vertices = verticesWithDummyNormals
                    , faceIndices =
                        [ ( 6, 5, 0 )
                        , ( 6, 0, 1 )
                        ]
                    }

        backRightFace =
            Sketch3d.mesh orange <|
                Mesh.with
                    { vertices = verticesWithDummyNormals
                    , faceIndices =
                        [ ( 3, 0, 5 )
                        , ( 3, 5, 4 )
                        ]
                    }
    in
    Sketch3d.group
        [ leftFace
        , rightFace
        , topFace
        , triangleFace
        , backLeftFace
        , backRightFace
        , bottomFace
        ]
