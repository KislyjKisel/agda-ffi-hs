{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Info where

open import Agda.Builtin.Char   using (Char)
open import Agda.Builtin.List   using (List)
open import Ffi.Hs.Data.Version using (Version)

{-# FOREIGN GHC
import qualified System.Info
#-}

postulate
    os                  : List Char
    arch                : List Char
    compilerName        : List Char
    compilerVersion     : Version
    fullCompilerVersion : Version

{-# COMPILE GHC os                  = System.Info.os                  #-}
{-# COMPILE GHC arch                = System.Info.arch                #-}
{-# COMPILE GHC compilerName        = System.Info.compilerName        #-}
{-# COMPILE GHC compilerVersion     = System.Info.compilerVersion     #-}
{-# COMPILE GHC fullCompilerVersion = System.Info.fullCompilerVersion #-}
