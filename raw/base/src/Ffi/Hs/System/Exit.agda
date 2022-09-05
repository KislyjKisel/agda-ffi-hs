{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Exit where

open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.List using (List)
open import Agda.Builtin.IO  using (IO)
open import Agda.Primitive
open import Ffi.Hs.Data.Int using (Int)
open import Ffi.Hs.-base.Class using (Show; Read; Eq; Ord; Exception)

{-# FOREIGN GHC
import qualified System.Exit
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaShow, AgdaRead, AgdaEq, AgdaOrd, AgdaException)
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

data ExitCode : Set where
    ExitSuccess : ExitCode
    ExitFailure : Int → ExitCode

{-# COMPILE GHC ExitCode = data System.Exit.ExitCode (ExitSuccess | ExitFailure) #-}

postulate
    exitWith    : ExitCode → IO A
    exitFailure : IO A
    exitSuccess : IO A
    die         : List Char → IO A

{-# COMPILE GHC exitWith    = \ aℓ a -> System.Exit.exitWith    #-}
{-# COMPILE GHC exitFailure = \ aℓ a -> System.Exit.exitFailure #-}
{-# COMPILE GHC exitSuccess = \ aℓ a -> System.Exit.exitSuccess #-}
{-# COMPILE GHC die         = \ aℓ a -> System.Exit.die         #-}

module Instances where
    postulate
        Show[ExitCode]      : Show ExitCode
        Read[ExitCode]      : Read ExitCode
        Exception[ExitCode] : Exception ExitCode
        Eq[ExitCode]        : Eq ExitCode
        Ord[ExitCode]       : Ord ExitCode
        -- todo: Rep, Generic

{-# COMPILE GHC Instances.Show[ExitCode]      = AgdaShow      #-}
{-# COMPILE GHC Instances.Read[ExitCode]      = AgdaRead      #-}
{-# COMPILE GHC Instances.Exception[ExitCode] = AgdaException #-}
{-# COMPILE GHC Instances.Eq[ExitCode]        = AgdaEq        #-}
{-# COMPILE GHC Instances.Ord[ExitCode]       = AgdaOrd       #-}
