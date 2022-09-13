{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Console.GetOpt where

open import Agda.Builtin.Char   using (Char)
open import Agda.Builtin.List   using (List)
open import Agda.Builtin.Maybe  using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class  using (Functor)
open import Ffi.Hs.Data.Tuple   using (Tuple3; Tuple4)

{-# FOREIGN GHC
import qualified System.Console.GetOpt
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

data ArgOrder (A : Set aℓ) : Set aℓ where
    RequireOrder  : ArgOrder A
    Permute       : ArgOrder A
    ReturnInOrder : (List Char → A) → ArgOrder A

{-# FOREIGN GHC type AgdaArgOrder aℓ = System.Console.GetOpt.ArgOrder #-}
{-# COMPILE GHC ArgOrder = data(1) AgdaArgOrder (System.Console.GetOpt.RequireOrder | System.Console.GetOpt.Permute | System.Console.GetOpt.ReturnInOrder) #-}

data ArgDescr (A : Set aℓ) : Set aℓ where
    NoArg : A → ArgDescr A
    ReqArg : (List Char → A) → List Char → ArgDescr A
    OptArg : (Maybe (List Char) → A) → List Char → ArgDescr A

{-# FOREIGN GHC type AgdaArgDescr aℓ = System.Console.GetOpt.ArgDescr #-}
{-# COMPILE GHC ArgDescr = data(1) AgdaArgDescr (System.Console.GetOpt.NoArg | System.Console.GetOpt.ReqArg | System.Console.GetOpt.OptArg) #-}

data OptDescr (A : Set aℓ) : Set aℓ where
    Option : List Char → List (List Char) → ArgDescr A → List Char → OptDescr A

{-# FOREIGN GHC type AgdaOptDescr aℓ = System.Console.GetOpt.OptDescr #-}
{-# COMPILE GHC OptDescr = data(1) AgdaOptDescr (System.Console.GetOpt.Option) #-}

postulate
    getOpt    : ArgOrder A → List (OptDescr A) → List (List Char) → Tuple3 (List A) (List (List Char)) (List (List Char))
    getOpt'   : ArgOrder A → List (OptDescr A) → List (List Char) → Tuple4 (List A) (List (List Char)) (List (List Char)) (List (List Char))
    usageInfo : List Char → List (OptDescr A) → List Char

{-# COMPILE GHC getOpt    = \ aℓ a -> System.Console.GetOpt.getOpt    #-}
{-# COMPILE GHC getOpt'   = \ aℓ a -> System.Console.GetOpt.getOpt'   #-}
{-# COMPILE GHC usageInfo = \ aℓ a -> System.Console.GetOpt.usageInfo #-}

postulate
    Functor[ArgOrder] : Functor {aℓ} ArgOrder
    Functor[OptDescr] : Functor {aℓ} OptDescr
    Functor[ArgDescr] : Functor {aℓ} ArgDescr

{-# COMPILE GHC Functor[ArgOrder] = \ aℓ -> AgdaFunctor #-}
{-# COMPILE GHC Functor[OptDescr] = \ aℓ -> AgdaFunctor #-}
{-# COMPILE GHC Functor[ArgDescr] = \ aℓ -> AgdaFunctor #-}
