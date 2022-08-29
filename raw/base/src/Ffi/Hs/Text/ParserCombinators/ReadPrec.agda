{-# OPTIONS --without-K #-}

module Ffi.Hs.Text.ParserCombinators.ReadPrec where

open import Agda.Builtin.Char                   using (Char)
open import Agda.Builtin.List                   using (List)
open import Agda.Primitive
open import Ffi.Hs.Control.Applicative          using (Applicative; Alternative)
open import Ffi.Hs.Control.Monad                using (Monad; MonadPlus)
open import Ffi.Hs.Control.Monad.Fail           using (MonadFail)
open import Ffi.Hs.Data.Functor                 using (Functor)
open import Ffi.Hs.Data.Int                     using (Int)
open import Ffi.Hs.Text.ParserCombinators.ReadP using (ReadP; ReadS)

{-# FOREIGN GHC import qualified Text.ParserCombinators.ReadPrec as AgdaHsReadPrec #-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    ReadPrec : Set aℓ → Set aℓ

module Instances where
    postulate
        Functor[ReadPrec]     : Functor     {aℓ} ReadPrec
        Applicative[ReadPrec] : Applicative {aℓ} ReadPrec
        Alternative[ReadPrec] : Alternative {aℓ} ReadPrec
        Monad[ReadPrec]       : Monad       {aℓ} ReadPrec
        MonadPlus[ReadPrec]   : MonadPlus   {aℓ} ReadPrec
        MonadFail[ReadPrec]   : MonadFail   {aℓ} ReadPrec

{-# FOREIGN GHC type AgdaReadPrec aℓ = AgdaHsReadPrec.ReadPrec #-}
{-# COMPILE GHC ReadPrec = type(1) AgdaReadPrec #-}

{-# FOREIGN GHC import qualified MAlonzo.Code.Ffi.Hs.Data.Functor        as AgdaHsDataFun     #-}
{-# FOREIGN GHC import qualified MAlonzo.Code.Ffi.Hs.Control.Applicative as AgdaHsControlApp  #-}
{-# FOREIGN GHC import qualified MAlonzo.Code.Ffi.Hs.Control.Monad       as AgdaHsControlMon  #-}
{-# FOREIGN GHC import qualified MAlonzo.Code.Ffi.Hs.Control.Monad.Fail  as AgdaHsControlMonF #-}
{-# COMPILE GHC Instances.Functor[ReadPrec]     = AgdaHsDataFun.AgdaFunctor     #-}
{-# COMPILE GHC Instances.Applicative[ReadPrec] = AgdaHsDataApp.AgdaApplicative #-}
{-# COMPILE GHC Instances.Alternative[ReadPrec] = AgdaHsDataApp.AgdaAlternative #-}
{-# COMPILE GHC Instances.Monad[ReadPrec]       = AgdaHsDataMon.AgdaMonad       #-}
{-# COMPILE GHC Instances.MonadPlus[ReadPrec]   = AgdaHsDataMon.AgdaMonadPlus   #-}
{-# COMPILE GHC Instances.MonadFail[ReadPrec]   = AgdaHsDataMonF.AgdaMonadFail  #-}

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
