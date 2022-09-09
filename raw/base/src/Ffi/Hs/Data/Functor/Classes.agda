{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor.Classes where

open import Agda.Builtin.Bool                      using (Bool)
open import Agda.Builtin.Char                      using (Char)
open import Agda.Builtin.List                      using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class                     using (Eq; Ord; Read; Show)
open import Ffi.Hs.Data.Int                        using (Int)
open import Ffi.Hs.Data.Ord                        using (Ordering)
open import Ffi.Hs.Text.ParserCombinators.ReadP    using (ReadS)
open import Ffi.Hs.Text.ParserCombinators.ReadPrec using (ReadPrec)
open import Ffi.Hs.Text.Show                       using (ShowS)

private
    variable
        aℓ bℓ cℓ : Level
        A B C D : Set aℓ

module _ where

    private
        variable
            F : Set aℓ → Set bℓ

    postulate
        Eq1 : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
        liftEq : ⦃ Eq1 F ⦄ → (A → B → Bool) → F A → F B → Bool
        eq1 : ⦃ Eq1 F ⦄ → ⦃ Eq A ⦄ → F A → F A → Bool

        Ord1 : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
        liftCompare : ⦃ Ord1 F ⦄ → (A → B → Ordering) → F A → F B → Ordering
        compare1 : ⦃ Ord1 F ⦄ → ⦃ Ord A ⦄ → F A → F A → Ordering
        Ord1[F]⇒Eq1[F] : ⦃ Ord1 F ⦄ → Eq1 F

        Read1 : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
        liftReadsPrec    : ⦃ Read1 F ⦄ → (Int → ReadS A) → ReadS (List A) → Int → ReadS (F A)
        liftReadList     : ⦃ Read1 F ⦄ → (Int → ReadS A) → ReadS (List A) → ReadS (List (F A))
        liftReadPrec     : ⦃ Read1 F ⦄ → ReadPrec A → ReadPrec (List A) → ReadPrec (F A)
        liftReadListPrec : ⦃ Read1 F ⦄ → ReadPrec A → ReadPrec (List A) → ReadPrec (List (F A))
        readsPrec1              : ⦃ Read1 F ⦄ → ⦃ Read A ⦄ → Int → ReadS (F A)
        readPrec1               : ⦃ Read1 F ⦄ → ⦃ Read A ⦄ → ReadPrec (F A)
        liftReadListDefault     : ⦃ Read1 F ⦄ → (Int → ReadS A) → ReadS (List A) → ReadS (List (F A))
        liftReadListPrecDefault : ⦃ Read1 F ⦄ → ReadPrec A → ReadPrec (List A) → ReadPrec (List (F A))

        Show1 : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
        liftShowsPrec : ⦃ Show1 F ⦄ → (Int → A → ShowS) → (List A → ShowS) → Int → F A → ShowS
        liftShowList  : ⦃ Show1 F ⦄ → (Int → A → ShowS) → (List A → ShowS) → List (F A) → ShowS
        showsPrec1 : ⦃ Show1 F ⦄ → ⦃ Show A ⦄ → Int → F A → ShowS

module _ where

    private
        variable
            F : Set aℓ → Set bℓ → Set cℓ

    postulate
        Eq2 : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
        liftEq2 : ⦃ Eq2 F ⦄ → (A → B → Bool) → (C → D → Bool) → F A C → F B D → Bool
        eq2 : ⦃ Eq2 F ⦄ → ⦃ Eq A ⦄ → ⦃ Eq B ⦄ → F A B → F A B → Bool

        Ord2 : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
        liftCompare2 : ⦃ Ord2 F ⦄ → (A → B → Ordering) → (C → D → Ordering) → F A C → F B D → Ordering
        compare2 : ⦃ Ord2 F ⦄ → ⦃ Ord A ⦄ → ⦃ Ord B ⦄ → F A B → F A B → Ordering
        Ord2[F]⇒Eq2[F] : ⦃ Ord2 F ⦄ → Eq2 F

        Read2 : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
        liftReadsPrec2    : ⦃ Read2 F ⦄ → (Int → ReadS A) → ReadS (List A) → (Int → ReadS B) → ReadS (List B) → Int → ReadS (F A B)
        liftReadList2     : ⦃ Read2 F ⦄ → (Int → ReadS A) → ReadS (List A) → (Int → ReadS B) → ReadS (List B) → ReadS (List (F A B))
        liftReadPrec2     : ⦃ Read2 F ⦄ → ReadPrec A → ReadPrec (List A) → ReadPrec B → ReadPrec (List B) → ReadPrec (F A B)
        liftReadListPrec2 : ⦃ Read2 F ⦄ → ReadPrec A → ReadPrec (List A) → ReadPrec B → ReadPrec (List B) → ReadPrec (List (F A B))
        readsPrec2               : ⦃ Read2 F ⦄ → ⦃ Read A ⦄ → ⦃ Read B ⦄ → Int → ReadS (F A B)
        readPrec2                : ⦃ Read2 F ⦄ → ⦃ Read A ⦄ → ⦃ Read B ⦄ → ReadPrec (F A B)
        liftReadList2Default     : ⦃ Read2 F ⦄ → (Int → ReadS A) → ReadS (List A) → (Int → ReadS B) → ReadS (List B) → ReadS (List (F A B))
        liftReadListPrec2Default : ⦃ Read2 F ⦄ → ReadPrec A → ReadPrec (List A) → ReadPrec B → ReadPrec (List B) → ReadPrec (List (F A B))

        Show2 : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
        liftShowsPrec2 : ⦃ Show2 F ⦄ → (Int → A → ShowS) → (List A → ShowS) → (Int → B → ShowS) → (List B → ShowS) → Int → F A B → ShowS
        liftShowList2  : ⦃ Show2 F ⦄ → (Int → A → ShowS) → (List A → ShowS) → (Int → B → ShowS) → (List B → ShowS) → List (F A B) → ShowS
        showsPrec2 : ⦃ Show2 F ⦄ → ⦃ Show A ⦄ → ⦃ Show B ⦄ → Int → F A B → ShowS

postulate
    readsData       : (List Char → ReadS A) → Int → ReadS A
    readData        : ReadPrec A → ReadPrec A
    readsUnaryWith  : (Int → ReadS A) → List Char → (A → B) → List Char → ReadS B
    readUnaryWith   : ReadPrec A → List Char → (A → B) → ReadPrec B
    readsBinaryWith : (Int → ReadS A) → (Int → ReadS B) → List Char → (A → B → C) → List Char → ReadS C
    readBinaryWith  : ReadPrec A → ReadPrec B → List Char → (A → B → C) → ReadPrec C
    showsUnaryWith  : (Int → A → ShowS) → List Char → Int → A → ShowS
    showsBinaryWith : (Int → A → ShowS) → (Int → B → ShowS) → List Char → Int → A → B → ShowS

-- todo: compile
-- todo: instances