{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Int where

postulate
    Int   : Set
    Int8  : Set
    Int16 : Set
    Int32 : Set
    Int64 : Set

{-# FOREIGN GHC import qualified Data.Int   #-}
{-# COMPILE GHC Int   = type Data.Int.Int   #-}
{-# COMPILE GHC Int8  = type Data.Int.Int8  #-}
{-# COMPILE GHC Int16 = type Data.Int.Int16 #-}
{-# COMPILE GHC Int32 = type Data.Int.Int32 #-}
{-# COMPILE GHC Int64 = type Data.Int.Int64 #-}
