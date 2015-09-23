import Graphics.Element as GE
import Signal as S
import State as St
import Render as R
import Update as U
import Input as I
import List as L

-- main = R.render (St.GameOfLife { grid = [ [True, False, True]
--                                         , [False, True, False]
--                                         , [True, False, True]
--                                         , [False, True, False]
--                                         , [True, False, True]
--                                         , [False, True, False]
--                                         , [True, False, True]
--                                         ]
--                                , size = (2, 2)
--                                })

-- main = GE.show (U.rowPairs [[True, True] , [False, True], [True, False]])

main =
    let
        grid = L.repeat 200
            <| L.repeat 200 0
        state = St.GameOfLife { grid = grid
                              , size = (200, 200)
                              }
    in
        S.map R.render (S.foldp U.update state I.inputs)
