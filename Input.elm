module Input (Input(Toggle, Tick, Noop), inputs, toggleAddress) where

import Signal as S
import State as St
import Time as T
import Window as W

type Input = Toggle St.Cell
           | Tick
           | Noop

inputs : S.Signal Input
inputs = S.mergeMany [toggleSignal, tickSignal]

-- Toggle
toggleMailbox : S.Mailbox St.Cell
toggleMailbox = S.mailbox (0, 0, False)

toggleSignal : S.Signal Input
toggleSignal = S.map Toggle toggleMailbox.signal

toggleAddress = toggleMailbox.address

-- Tick
tickSignal : S.Signal Input
tickSignal = S.map (always Tick) (T.fps 2)

-- Resize
-- resizeSignal : S.Signal Input
-- resizeSignal = S.map Resize W.dimensions
