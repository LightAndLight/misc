module Animation where

import FRP (Behaviour, FRP, behaviour)

type Animation m i = Behaviour m i

oscillate ::
  FRP m =>
  -- | Frequency
  Double ->
  Behaviour m Double
oscillate = oscillateFrom 0.0

oscillateFrom ::
  FRP m =>
  -- | Start
  Double ->
  -- | Frequency
  Double ->
  Behaviour m Double
oscillateFrom start freq = behaviour $ \t -> sin (2 * pi * freq * (start + t))
