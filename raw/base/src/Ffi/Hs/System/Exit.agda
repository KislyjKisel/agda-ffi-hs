{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Exit where

open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Show; Read; Eq; Ord; Exception)
open import Ffi.Hs.Data.Int    using (Int)

{-# FOREIGN GHC
import qualified System.Exit
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaShow, AgdaRead, AgdaEq, AgdaOrd, AgdaException
    )
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

data ExitCode : Set where
    ExitSuccess : ExitCode
    ExitFailure : Int → ExitCode

{-# COMPILE GHC ExitCode = data System.Exit.ExitCode (System.Exit.ExitSuccess | System.Exit.ExitFailure) #-}

postulate
    exitWith    : ExitCode → IO A
    exitFailure : IO A
    exitSuccess : IO A
    die         : List Char → IO A

{-# COMPILE GHC exitWith    = \ aℓ a -> System.Exit.exitWith    #-}
{-# COMPILE GHC exitFailure = \ aℓ a -> System.Exit.exitFailure #-}
{-# COMPILE GHC exitSuccess = \ aℓ a -> System.Exit.exitSuccess #-}
{-# COMPILE GHC die         = \ aℓ a -> System.Exit.die         #-}

postulate
    Show[ExitCode]      : Show ExitCode
    Read[ExitCode]      : Read ExitCode
    Exception[ExitCode] : Exception ExitCode
    Eq[ExitCode]        : Eq ExitCode
    Ord[ExitCode]       : Ord ExitCode

{-# COMPILE GHC Show[ExitCode]      = AgdaShow      #-}
{-# COMPILE GHC Read[ExitCode]      = AgdaRead      #-}
{-# COMPILE GHC Exception[ExitCode] = AgdaException #-}
{-# COMPILE GHC Eq[ExitCode]        = AgdaEq        #-}
{-# COMPILE GHC Ord[ExitCode]       = AgdaOrd       #-}
