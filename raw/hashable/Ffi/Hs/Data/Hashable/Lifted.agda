{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Hashable.Lifted where

open import Agda.Builtin.List            using (List)
open import Agda.Builtin.Maybe           using (Maybe)
open import Agda.Primitive               using (Level; _⊔_)
open import Ffi.Hs.Data.Complex          using (Complex)
open import Ffi.Hs.Data.Either           using (Either)
open import Ffi.Hs.Data.Fixed            using (Fixed)
open import Ffi.Hs.Data.Functor.Compose  using (Compose)
open import Ffi.Hs.Data.Functor.Const    using (Const)
open import Ffi.Hs.Data.Functor.Identity using (Identity)
open import Ffi.Hs.Data.Functor.Product  using (Product)
open import Ffi.Hs.Data.Functor.Sum      using (Sum)
open import Ffi.Hs.Data.Hashable         using (Hashed; Hashable)
open import Ffi.Hs.Data.Int              using (Int)
open import Ffi.Hs.Data.Proxy            using (Proxy)
open import Ffi.Hs.Data.Tuple            using (Tuple2; Tuple3; Tuple4; Tuple5)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Hashable.Lifted
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ cℓ dℓ eℓ : Level
        A B C D : Set aℓ
        F G : Set aℓ → Set bℓ


postulate
    Hashable1 : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)

    liftHashWithSalt : ⦃ Hashable1 F ⦄ → (Int → A → Int) → Int → F A → Int

    Hashable1[List]            : Hashable1 {aℓ} List
    Hashable1[Maybe]           : Hashable1 {aℓ} Maybe
    Hashable1[Complex]         : Hashable1 {aℓ} Complex
    Hashable1[Fixed]           : Hashable1 {aℓ} Fixed
    Hashable1[Identity]        : Hashable1 {aℓ} Identity
    Hashable1[Hashed]          : Hashable1 {aℓ} Hashed
    Hashable1[Either[A]]       : Hashable1 (Either {bℓ = bℓ} A)
    Hashable1[Tuple2[A]]       : Hashable1 (Tuple2 {bℓ = bℓ} A)
    Hashable1[Proxy]           : Hashable1 {aℓ} Proxy
    Hashable1[Tuple3[A,B]]     : Hashable1 (Tuple3 {cℓ = cℓ} A B)
    Hashable1[Tuple4[A,B,C]]   : Hashable1 (Tuple4 {dℓ = dℓ} A B C)
    Hashable1[Tuple5[A,B,C,D]] : Hashable1 (Tuple5 {eℓ = eℓ} A B C D)
    Hashable[Product[F,G,A]]   : ⦃ Hashable1 F ⦄ → ⦃ Hashable1 G ⦄ → ⦃ Hashable A ⦄ → Hashable (Product F G A)
    Hashable[Sum[F,G,A]]       : ⦃ Hashable1 F ⦄ → ⦃ Hashable1 G ⦄ → ⦃ Hashable A ⦄ → Hashable (Sum F G A)
    Hashable[Compose[F,G,A]]   : ⦃ Hashable1 F ⦄ → ⦃ Hashable1 G ⦄ → ⦃ Hashable A ⦄ → Hashable (Compose F G A)

{-# FOREIGN GHC data AgdaHashable1 aℓ bℓ t = Data.Hashable.Lifted.Hashable1 t => AgdaHashable1 #-}
{-# COMPILE GHC Hashable1 = type(0) AgdaHashable1 #-}

{-# COMPILE GHC liftHashWithSalt = \ aℓ bℓ t a AgdaHashable1 -> Data.Hashable.Lifted.liftHashWithSalt #-}

{-# COMPILE GHC Hashable1[List]              = \ aℓ                                                      -> AgdaHashable1 #-}
{-# COMPILE GHC Hashable1[Maybe]             = \ aℓ                                                      -> AgdaHashable1 #-}
{-# COMPILE GHC Hashable1[Complex]           = \ aℓ                                                      -> AgdaHashable1 #-}
{-# COMPILE GHC Hashable1[Fixed]             = \ aℓ                                                      -> AgdaHashable1 #-}
{-# COMPILE GHC Hashable1[Identity]          = \ aℓ                                                      -> AgdaHashable1 #-}
{-# COMPILE GHC Hashable1[Hashed]            = \ aℓ                                                      -> AgdaHashable1 #-}
{-# COMPILE GHC Hashable1[Either[A]]         = \ aℓ                                                      -> AgdaHashable1 #-}
{-# COMPILE GHC Hashable1[Tuple2[A]]         = \ aℓ a bℓ                                                 -> AgdaHashable1 #-}
{-# COMPILE GHC Hashable1[Proxy]             = \ aℓ                                                      -> AgdaHashable1 #-}
{-# COMPILE GHC Hashable1[Tuple3[A,B]]       = \ aℓ a bℓ b cℓ                                            -> AgdaHashable1 #-}
{-# COMPILE GHC Hashable1[Tuple4[A,B,C]]     = \ aℓ a bℓ b cℓ c dℓ                                       -> AgdaHashable1 #-}
{-# COMPILE GHC Hashable1[Tuple5[A,B,C,D]]   = \ aℓ a bℓ b cℓ c dℓ d e                                   -> AgdaHashable1 #-}
{-# COMPILE GHC Hashable[Product[F,G,A]]     = \ fℓ gℓ aℓ f g a AgdaHashable1 AgdaHashable1 AgdaHashable -> AgdaHashable  #-}
{-# COMPILE GHC Hashable[Sum[F,G,A]]         = \ fℓ gℓ aℓ f g a AgdaHashable1 AgdaHashable1 AgdaHashable -> AgdaHashable  #-}
{-# COMPILE GHC Hashable[Compose[F,G,A]]     = \ fℓ gℓ aℓ f g a AgdaHashable1 AgdaHashable1 AgdaHashable -> AgdaHashable  #-}


postulate
    Hashable2 : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)

    liftHashWithSalt2 : ∀{T : Set aℓ → Set bℓ → Set cℓ} → ⦃ Hashable2 T ⦄ → (Int → A → Int) → (Int → B → Int) → Int → T A B → Int

    Hashable2[Either]        : Hashable2 (Either {aℓ} {bℓ})
    Hashable2[Tuple2]        : Hashable2 (Tuple2 {aℓ} {bℓ})
    Hashable2[Const]         : Hashable2 (Const {aℓ})
    Hashable2[Tuple3[A]]     : Hashable2 (Tuple3 {bℓ = bℓ} {cℓ = cℓ} A)
    Hashable2[Tuple4[A,B]]   : Hashable2 (Tuple4 {cℓ = cℓ} {dℓ = dℓ} A B)
    Hashable2[Tuple5[A,B,C]] : Hashable2 (Tuple5 {dℓ = dℓ} {eℓ = eℓ} A B C)

{-# FOREIGN GHC data AgdaHashable2 aℓ bℓ cℓ t = Data.Hashable.Lifted.Hashable2 t => AgdaHashable2 #-}
{-# COMPILE GHC Hashable2 = type(0) AgdaHashable2 #-}

{-# COMPILE GHC liftHashWithSalt2 = \ aℓ bℓ cℓ t a b AgdaHashable2 -> Data.Hashable.Lifted.liftHashWithSalt2 #-}

{-# COMPILE GHC Hashable2[Either]        = \ aℓ bℓ                -> AgdaHashable2 #-}
{-# COMPILE GHC Hashable2[Tuple2]        = \ aℓ bℓ                -> AgdaHashable2 #-}
{-# COMPILE GHC Hashable2[Const]         = \ aℓ                   -> AgdaHashable2 #-}
{-# COMPILE GHC Hashable2[Tuple3[A]]     = \ bℓ cℓ aℓ a           -> AgdaHashable2 #-}
{-# COMPILE GHC Hashable2[Tuple4[A,B]]   = \ cℓ dℓ aℓ a bℓ b      -> AgdaHashable2 #-}
{-# COMPILE GHC Hashable2[Tuple5[A,B,C]] = \ dℓ eℓ aℓ a bℓ b cℓ c -> AgdaHashable2 #-}
