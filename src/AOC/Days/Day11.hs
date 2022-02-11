-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day11 (solution) where

import           AOC.Solution
import           Foreign.C.String (CString)

foreign import ccall "day11" c_day11 :: CString -> IO ()

solution :: () :=> ()
solution = ForeignC c_day11
