{-# OPTIONS --without-K #-}

module Ffi.Hs.Numeric.Natural where

postulate
    Natural : Set

{-# FOREIGN GHC import qualified Numeric.Natural #-}
{-# COMPILE GHC Natural = type Natural #-}
