module Util

import Network.Socket
import Data.Primitives.Views
import System

%access public export

randHelper : (seed' : Int) -> Int
randHelper seed' with (divides seed' 9999)
  randHelper ((9999 * div) + rem) | (DivBy prf) = rem

randPort : (seed : Nat) -> Port
randPort seed = let seed' = toIntNat (1664525 * seed + 1013904223) in 
                randHelper (seed' `shiftR` 2)