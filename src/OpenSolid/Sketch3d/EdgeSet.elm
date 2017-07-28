module OpenSolid.Sketch3d.EdgeSet
    exposing
        ( EdgeSet
        , build
        , isEdgeVertex
        , isOpenEdge
        , openEdges
        )

import Dict exposing (Dict)
import Set exposing (Set)


type alias EdgeDict =
    Dict ( Int, Int ) Int


type EdgeSet
    = EdgeSet EdgeDict (Set Int)


sort : Int -> Int -> ( Int, Int )
sort i j =
    if i <= j then
        ( i, j )
    else
        ( j, i )


increment : Maybe Int -> Maybe Int
increment entry =
    case entry of
        Just value ->
            Just (value + 1)

        Nothing ->
            Just 1


add : ( Int, Int ) -> EdgeDict -> EdgeDict
add edge edgeDict =
    Dict.update edge increment edgeDict


build : List ( Int, Int, Int ) -> EdgeSet
build faces =
    let
        addEdges ( i, j, k ) edgeDict =
            let
                edge1 =
                    sort i j

                edge2 =
                    sort j k

                edge3 =
                    sort k i
            in
            edgeDict |> add edge1 |> add edge2 |> add edge3

        edgeDict =
            List.foldl addEdges Dict.empty faces

        processEdge ( i, j ) count vertexSet =
            if count == 1 then
                vertexSet |> Set.insert i |> Set.insert j
            else
                vertexSet

        vertexSet =
            Dict.foldl processEdge Set.empty edgeDict
    in
    EdgeSet edgeDict vertexSet


isOpenEdge : Int -> Int -> EdgeSet -> Bool
isOpenEdge i j (EdgeSet edgeDict vertexSet) =
    Dict.get (sort i j) edgeDict == Just 1


isEdgeVertex : Int -> EdgeSet -> Bool
isEdgeVertex i (EdgeSet edgeDict vertexSet) =
    Set.member i vertexSet


openEdges : EdgeSet -> List ( Int, Int )
openEdges (EdgeSet edgeDict vertexSet) =
    Dict.toList edgeDict
        |> List.filter (\( indices, count ) -> count == 1)
        |> List.map Tuple.first
