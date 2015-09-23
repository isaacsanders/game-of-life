module Render (render, unitLength) where

import Graphics.Element as GE
import Graphics.Input as GI
import Graphics.Collage as GC
import Color as C
import State as St
import Signal as S
import List as L
import Input as I

render : St.State -> GE.Element
render state =
    case state of
        St.NewState -> GE.show state
        St.GameOfLife {grid} ->
            let
                groupMap = (\fn l -> GC.group <| L.map fn l)
            in
                GC.collage 500 500 [ groupMap renderCell (St.cells grid) ]

unitLength : Int
unitLength = 5

renderCell : (Int, Int, Bool) -> GC.Form
renderCell (x, y, isAlive) =
    let
        form = if isAlive then liveCell unitLength else deadCell unitLength
        element = GC.collage unitLength unitLength [ form ]
        clickMessage = S.message I.toggleAddress (x, y, isAlive)
        clickableElement = GI.clickable clickMessage element
        clickableForm = GC.toForm clickableElement
        dx = x * unitLength
        dy = y * unitLength
    in
        GC.move (toFloat dx, toFloat dy) clickableForm

liveCell : Int -> GC.Form
liveCell unitLength = GC.filled C.black (GC.square <| toFloat unitLength)

deadCellLineStyle : GC.LineStyle
deadCellLineStyle = GC.solid C.black

deadCell : Int -> GC.Form
deadCell unitLength = GC.outlined deadCellLineStyle (GC.square <| toFloat unitLength)
