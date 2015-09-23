module State (State(NewState, GameOfLife), Grid, neighborhoods, Neighborhood, Cell, cells, transpose) where

import List as L
import Maybe as M
import Trampoline as T

type State = NewState | GameOfLife { size: Size
                                   , grid: Grid
                                   }

type alias Size = (Int, Int)
type alias Grid = List (List Int)
type alias Cell = (Int, Int, Bool)
type alias Neighborhood = ((Int, Int, Int)
                          ,(Int, Int, Int)
                          ,(Int, Int, Int)
                          )

neighborhoods : Grid -> List Neighborhood
neighborhoods = rowsToHoods << neighboringRows

neighboringRows : Grid -> List (List Int, List Int, List Int)
neighboringRows list = L.reverse <| T.trampoline (neighboringRows' [] ([]::list))

neighboringRows' : List (List Int, List Int, List Int) -> List (List Int) -> T.Trampoline (List (List Int, List Int, List Int))
neighboringRows' rows list =
    case list of
        [] -> T.Done rows
        [hd] -> T.Done rows
        [hd1, hd2] -> T.Done ((hd1, hd2, (L.repeat (L.length hd1) 0))::rows)
        []::hd1::hd2::tl -> T.Continue (\() -> neighboringRows' (((L.repeat (L.length hd1) 0), hd1, hd2)::rows) (hd1::hd2::tl))
        hd1::hd2::hd3::tl -> T.Continue (\() -> neighboringRows' ((hd1, hd2, hd3)::rows) (hd2::hd3::tl))

rowsToHoods : List (List Int, List Int, List Int) -> List Neighborhood
rowsToHoods rows = L.concat <| L.map (L.reverse << (\(a, b, c) -> T.trampoline (rowsToHoods' (0, 0, 0) a b c []))) rows

rowsToHoods' : (Int, Int, Int) -> List Int -> List Int -> List Int -> List Neighborhood -> T.Trampoline (List Neighborhood)
rowsToHoods' (a1, b1, c1) a b c ns =
    case (a, b, c) of
        ([], [], []) -> T.Done ns
        ([], _, _) -> T.Done ns
        (_, [], _) -> T.Done ns
        (_, _, []) -> T.Done ns
        ([a2], [b2], [c2]) ->
            let
                n = ((a1, a2, 0), (b1, b2, 0), (c1, c2, 0))
            in
                T.Done (n::ns)
        (a2::a3::at, b2::b3::bt, c2::c3::ct) ->
            let
                n = ((a1, a2, a3), (b1, b2, b3), (c1, c2, c3))
            in
                T.Continue (\() -> rowsToHoods' (a2, b2, c2) (a3::at) (b3::bt) (c3::ct) (n::ns))

-- neighborshoods : Grid -> List (List (List Int))
-- neighborshoods grid =
--     L.map neighboringRows (>> neighboringRows grid)

-- neighborCounts : Grid -> Grid
-- neighborCounts grid =
--     let
--         sumZip = L.map (uncurry (L.map2 (+)))
--         rowNeighbors = sumZip (neighboringRows grid)
--         colNeighbors = transpose (sumZip (neighboringRows (transpose grid)))
--         cornerNeighbors = L.map rotl grid
--     in
--         sumZip ((L.map2 (,)) colNeighbors rowNeighbors)

transpose : Grid -> Grid
transpose grid =
    let
        first = L.head grid
    in
        case first of
            M.Just row ->
                let
                    n = L.length row
                    init = L.repeat n []
                in
                    L.foldr (L.map2 (::)) init grid
            M.Nothing -> grid

-- Take the first row of length n. create n columns, append the i=0..n-1th item to the ith column.

concatIndexedMap : ((Int, List a) -> List b) -> List (List a) -> List b
concatIndexedMap fn list = L.concatMap fn <| L.indexedMap (,) list

cells : Grid -> List Cell
cells grid = concatIndexedMap
    (\(y, row) -> L.indexedMap (\x cell -> (x, y, 1 == cell)) row)
    grid
