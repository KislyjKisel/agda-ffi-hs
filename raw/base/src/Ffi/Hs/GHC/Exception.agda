{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.Exception where

open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Show; Eq; Ord)

open import Ffi.Hs.GHC.Exception.Type public

open import Ffi.Hs.GHC.Stack public
    using
    ( CallStack; IsList[CallStack]; Show[CallStack]
    ; fromCallSiteList; getCallStack; prettyCallStack
    ; SrcLoc; mkSrcLoc; module SrcLoc
    ; Show[SrcLoc]; Eq[SrcLoc]; prettySrcLoc
    )

{-# FOREIGN GHC
import qualified GHC.Exception
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    (AgdaShow, AgdaEq, AgdaOrd, AgdaException)
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ

postulate
    throw : ⦃ Exception A ⦄ → B
    -- todo: throw for other representations

{-# COMPILE GHC throw = \ aℓ a bℓ b AgdaException -> GHC.Exception.throw #-}

data ErrorCall : Set where
    ErrorCallWithLocation : List Char → List Char → ErrorCall

-- todo: pattern `(mk?)ErrorCall`

{-# COMPILE GHC ErrorCall = data GHC.Exception.ErrorCall (GHC.Exception.ErrorCallWithLocation) #-}

postulate
    Exception[ErrorCall] : Exception ErrorCall
    Show[ErrorCall]      : Show ErrorCall
    Eq[ErrorCall]        : Eq ErrorCall
    Ord[ErrorCall]       : Ord ErrorCall

{-# COMPILE GHC Exception[ErrorCall] = AgdaException #-}
{-# COMPILE GHC Show[ErrorCall]      = AgdaShow      #-}
{-# COMPILE GHC Eq[ErrorCall]        = AgdaEq        #-}
{-# COMPILE GHC Ord[ErrorCall]       = AgdaOrd       #-}

postulate
    errorCallException              : List Char → SomeException
    errorCallWithCallStackException : List Char → CallStack → SomeException

    prettyCallStackLines : CallStack → List (List Char)
    showCCSStack         : List (List Char) → List (List Char)

{-# COMPILE GHC errorCallException              = GHC.Exception.errorCallException              #-}
{-# COMPILE GHC errorCallWithCallStackException = GHC.Exception.errorCallWithCallStackException #-}

{-# COMPILE GHC prettyCallStackLines = GHC.Exception.prettyCallStackLines #-}
{-# COMPILE GHC showCCSStack         = GHC.Exception.showCCSStack         #-}
