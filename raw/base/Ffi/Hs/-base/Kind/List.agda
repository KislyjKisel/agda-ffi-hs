{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Kind.List where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Primitive
open import Ffi.Hs.-base.Kind using (IsKind)

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

private
    postulate
        P`List : (A : Set aℓ) → Set aℓ

{-# POLARITY P`List _ ++ #-}

`List : (A : Set aℓ) → ⦃ IsKind A ⦄ → Set aℓ
`List A = P`List A

postulate
    `[] : ⦃ _ : IsKind A ⦄ → `List A
    _`∷_ : ⦃ _ : IsKind A ⦄ → A → `List A → `List A

    IsKind[`List[A]] : ⦃ _ : IsKind A ⦄ → IsKind (`List A)

{-# FOREIGN GHC {-# LANGUAGE DataKinds #-} #-}

{-# FOREIGN GHC type AgdaP'96'List aℓ = [] #-}
{-# COMPILE GHC P`List = type(1) AgdaP'96'List #-}

{-# FOREIGN GHC type Agda'96'_Nil i = '[] #-}
{-# COMPILE GHC `[] = type(1) Agda'96'_Nil #-}

{-# FOREIGN GHC type Agda'96'_Cons i = '(:) #-}
{-# COMPILE GHC _`∷_ = type(1) Agda'96'_Cons #-}

{-# COMPILE GHC IsKind[`List[A]] = \ aℓ a i -> MAlonzo.Code.Ffi.Hs.QZ45Zbase.Kind.AgdaIsKind #-}

lift`List : ⦃ _ : IsKind B ⦄ → (A → B) → List A → `List B
lift`List f []       = `[]
lift`List f (x ∷ xs) = f x `∷ lift`List f xs
