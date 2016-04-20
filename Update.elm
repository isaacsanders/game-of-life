module Update (update) where

import State as S
import Input as I
import Render as R
import List as L
import Array as A

update : I.Input -> S.State -> S.State
update input state =
    case state of
        S.GameOfLife {size, grid, running} ->
            case input of
                I.Toggle (x, y, isAlive) ->
                    let
                        nextState = if isAlive then 0 else 1
                    in
                        S.GameOfLife { grid = putCell (x, y, isAlive) nextState grid
                                     , size = size
                                     , running = running
                                     }
                I.Tick ->
                    let
                        (width, height) = size
                        neighborhoods = S.neighborhoods grid
                        cells = S.cells grid
                        newGrid = L.foldl (uncurry nextGrid) (L.repeat height (L.repeat width 0)) <| L.map2 (,) neighborhoods cells
                    in
                        S.GameOfLife { grid = newGrid
                                     , size = size
                                     , running = running
                                     }

nextGrid : S.Neighborhood -> S.Cell -> S.Grid -> S.Grid
nextGrid neighborhood (x, y, isAlive) grid =
    let
        ((a1, a2, a3)
        ,(b1, _ , b3)
        ,(c1, c2, c3)
        ) = neighborhood
        neighborCount = a1 + a2 + a3 + b1 + b3 + c1 + c2 + c3
        nextState = if updateCell isAlive neighborCount then 1 else 0
    in
        putCell (x, y, isAlive) nextState grid

putCell : S.Cell -> Int -> S.Grid -> S.Grid
putCell (x, y, isAlive) nextState grid =
    let
        mutableGrid = (A.fromList grid)
        row = case A.get y mutableGrid of
            Just r -> A.fromList r
            Nothing -> A.empty
        updatedRow = A.toList (A.set x nextState row)
    in
        A.toList (A.set y updatedRow mutableGrid)

updateCell : Bool -> Int -> Bool
updateCell isAlive neighborCount =
    case neighborCount of
        2 -> isAlive
        3 -> True
        _ -> False
