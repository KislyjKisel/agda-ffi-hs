{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Tuple where

open import Agda.Primitive

{-# FOREIGN GHC import qualified Data.Tuple #-}

private
    variable
        aℓ bℓ cℓ dℓ eℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ
        D : Set dℓ
        E : Set eℓ

record Tuple2 (A : Set aℓ) (B : Set bℓ) : Set (aℓ ⊔ bℓ) where
    constructor tuple2
    field
        fst : A
        snd : B

open Tuple2 public

data Tuple3 (A : Set aℓ) (B : Set bℓ) (C : Set cℓ) : Set (aℓ ⊔ bℓ ⊔ cℓ) where
    tuple3 : A → B → C → Tuple3 A B C

data Tuple4 (A : Set aℓ) (B : Set bℓ) (C : Set cℓ) (D : Set dℓ) : Set (aℓ ⊔ bℓ ⊔ cℓ ⊔ dℓ) where
    tuple4 : A → B → C → D → Tuple4 A B C D

data Tuple5 (A : Set aℓ) (B : Set bℓ) (C : Set cℓ) (D : Set dℓ) (E : Set eℓ) : Set (aℓ ⊔ bℓ ⊔ cℓ ⊔ dℓ ⊔ eℓ) where
    tuple5 : A → B → C → D → E → Tuple5 A B C D E

{-# FOREIGN GHC type AgdaTuple2 aℓ bℓ          = (,)    #-}
{-# FOREIGN GHC type AgdaTuple3 aℓ bℓ cℓ       = (,,)   #-}
{-# FOREIGN GHC type AgdaTuple4 aℓ bℓ cℓ dℓ    = (,,,)  #-}
{-# FOREIGN GHC type AgdaTuple5 aℓ bℓ cℓ dℓ eℓ = (,,,,) #-}

{-# COMPILE GHC Tuple2 = data(2) AgdaTuple2 ((,))    #-}
{-# COMPILE GHC Tuple3 = data(3) AgdaTuple3 ((,,))   #-}
{-# COMPILE GHC Tuple4 = data(4) AgdaTuple4 ((,,,))  #-}
{-# COMPILE GHC Tuple5 = data(5) AgdaTuple5 ((,,,,)) #-}

data Solo (A : Set aℓ) : Set aℓ where
    mkSolo : A → Solo A

{-# FOREIGN GHC type AgdaSolo aℓ = Data.Tuple.Solo #-}
{-# COMPILE GHC Solo = data(1) AgdaSolo (Data.Tuple.Solo) #-}

postulate
    curry   : (Tuple2 A B → C) → A → B → C
    uncurry : (A → B → C) → Tuple2 A B → C
    swap    : Tuple2 A B → Tuple2 B A

{-# COMPILE GHC curry   = \ aℓ bℓ cℓ a b c -> Data.Tuple.curry   #-}
{-# COMPILE GHC uncurry = \ aℓ bℓ cℓ a b c -> Data.Tuple.uncurry #-}
{-# COMPILE GHC swap    = \ aℓ bℓ a b      -> Data.Tuple.swap    #-}
