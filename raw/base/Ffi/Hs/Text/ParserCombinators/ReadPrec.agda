{-# OPTIONS --without-K #-}

module Ffi.Hs.Text.ParserCombinators.ReadPrec where

open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Functor; Applicative; Alternative; Monad; MonadFail; MonadPlus)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Text.ParserCombinators.ReadP using (ReadP; ReadS)

{-# FOREIGN GHC
import qualified Text.ParserCombinators.ReadPrec as AgdaHsReadPrec
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    ReadPrec : Set aℓ → Set aℓ

{-# FOREIGN GHC type AgdaReadPrec aℓ = AgdaHsReadPrec.ReadPrec #-}
{-# COMPILE GHC ReadPrec = type(1) AgdaReadPrec #-}

Prec : Set
Prec = ReadPrec Int

postulate
    minPrec : Prec

    lift  : ReadP A → ReadPrec A
    prec  : Prec → ReadPrec A → ReadPrec A
    step  : ReadPrec A → ReadPrec A
    reset : ReadPrec A → ReadPrec A

    get    : ReadPrec Char
    look   : ReadPrec (List Char)
    _+++_  : ReadPrec A → ReadPrec A → ReadPrec A
    _<++_  : ReadPrec A → ReadPrec A → ReadPrec A
    pfail  : ReadPrec A
    choice : List (ReadPrec A) → ReadPrec A

    readPrec_to_P : ReadPrec A → Int → ReadP A
    readP_to_Prec : (Int → ReadP A) → ReadPrec A
    readPrec_to_S : ReadPrec A → Int → ReadS A
    readS_to_Prec : (Int → ReadS A) → ReadPrec A

{-# COMPILE GHC minPrec = AgdaHsReadPrec.minPrec #-}

{-# COMPILE GHC lift  = \ aℓ a -> AgdaHsReadPrec.lift  #-}
{-# COMPILE GHC prec  = \ aℓ a -> AgdaHsReadPrec.prec  #-}
{-# COMPILE GHC step  = \ aℓ a -> AgdaHsReadPrec.step  #-}
{-# COMPILE GHC reset = \ aℓ a -> AgdaHsReadPrec.reset #-}

{-# COMPILE GHC get    =           AgdaHsReadPrec.get    #-}
{-# COMPILE GHC look   =           AgdaHsReadPrec.look   #-}
{-# COMPILE GHC _+++_  = \ aℓ a -> (AgdaHsReadPrec.+++)  #-}
{-# COMPILE GHC _<++_  = \ aℓ a -> (AgdaHsReadPrec.<++)  #-}
{-# COMPILE GHC pfail  = \ aℓ a -> AgdaHsReadPrec.pfail  #-}
{-# COMPILE GHC choice = \ aℓ a -> AgdaHsReadPrec.choice #-}

{-# COMPILE GHC readPrec_to_P = \ aℓ a -> AgdaHsReadPrec.readPrec_to_P #-}
{-# COMPILE GHC readP_to_Prec = \ aℓ a -> AgdaHsReadPrec.readP_to_Prec #-}
{-# COMPILE GHC readPrec_to_S = \ aℓ a -> AgdaHsReadPrec.readPrec_to_S #-}
{-# COMPILE GHC readS_to_Prec = \ aℓ a -> AgdaHsReadPrec.readS_to_Prec #-}

postulate
    Functor[ReadPrec]     : Functor {aℓ} ReadPrec
    Applicative[ReadPrec] : Applicative {aℓ} ReadPrec
    Alternative[ReadPrec] : Alternative {aℓ} ReadPrec
    Monad[ReadPrec]       : Monad {aℓ} ReadPrec
    MonadFail[ReadPrec]   : MonadFail {aℓ} ReadPrec
    MonadPlus[ReadPrec]   : MonadPlus {aℓ} ReadPrec

{-# COMPILE GHC Functor[ReadPrec]     = \ aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC Applicative[ReadPrec] = \ aℓ -> AgdaApplicative #-}
{-# COMPILE GHC Alternative[ReadPrec] = \ aℓ -> AgdaAlternative #-}
{-# COMPILE GHC Monad[ReadPrec]       = \ aℓ -> AgdaMonad       #-}
{-# COMPILE GHC MonadFail[ReadPrec]   = \ aℓ -> AgdaMonadFail   #-}
{-# COMPILE GHC MonadPlus[ReadPrec]   = \ aℓ -> AgdaMonadPlus   #-}
