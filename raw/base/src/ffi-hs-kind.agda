{-# OPTIONS --without-K #-}

module ffi-hs-kind where

open import Agda.Primitive
open import Agda.Builtin.Int using () renaming (Int to Integer)

private
    variable
        aℓ bℓ : Level
        A : Set aℓ

postulate
    `List : (A : Set aℓ) → Set aℓ
    `[] : `List A
    _`∷_ : A → `List A → `List A

{-# POLARITY `List _ ++ #-}

{-# FOREIGN GHC {-# LANGUAGE DataKinds #-} #-}

{-# FOREIGN GHC type AgdaP'96'List aℓ = [] #-}
{-# COMPILE GHC `List = type(1) AgdaP'96'List #-}

{-# FOREIGN GHC type Agda'96'_Nil = '[] #-}
{-# COMPILE GHC `[] = type(0) Agda'96'_Nil #-}

{-# FOREIGN GHC type Agda'96'_Cons = '(:) #-}
{-# COMPILE GHC _`∷_ = type(0) Agda'96'_Cons #-}

{-# FOREIGN GHC {-# LANGUAGE KindSignatures, PolyKinds #-} #-}
{-# FOREIGN GHC import qualified GHC.Exts #-}

-- data VecCount : Set where
--     Vec2 Vec4 Vec8 Vec16 Vec32 Vec64 : VecCount

-- {-# COMPILE GHC VecCount = data GHC.Exts.VecCount (Vec2 | Vec4 | Vec8 | Vec16 | Vec32 | Vec64) #-}

-- data VecElem : Set where
--     Int8ElemRep   : VecElem
--     Int16ElemRep  : VecElem
--     Int32ElemRep  : VecElem
--     Int64ElemRep  : VecElem
--     Word8ElemRep  : VecElem
--     Word16ElemRep : VecElem
--     Word32ElemRep : VecElem
--     Word64ElemRep : VecElem
--     FloatElemRep  : VecElem
--     DoubleElemRep : VecElem

-- {-# COMPILE GHC VecElem = data GHC.Exts.VecElem (Int8ElemRep | Int16ElemRep | Int32ElemRep | Int64ElemRep | Word8ElemRep | Word8ElemRep | Word8ElemRep | Word8ElemRep | FloatElemRep | DoubleElemRep) #-}

-- data Levity : Set where
--     Lifted Unlifted : Levity

-- {-# COMPILE GHC Levity = data GHC.Exts.Levity (Lifted | Unlifted)  #-}

-- data RuntimeRep : Set where
--     VecRep    : VecCount → VecElem → RuntimeRep
--     TupleRep  : `List RuntimeRep → RuntimeRep
--     SumRep    : `List RuntimeRep → RuntimeRep
--     BoxedRep  : Levity → RuntimeRep
--     IntRep    : RuntimeRep
--     Int8Rep   : RuntimeRep
--     Int16Rep  : RuntimeRep
--     Int32Rep  : RuntimeRep
--     Int64Rep  : RuntimeRep
--     WordRep   : RuntimeRep
--     Word8Rep  : RuntimeRep
--     Word16Rep : RuntimeRep
--     Word32Rep : RuntimeRep
--     Word64Rep : RuntimeRep
--     AddrRep   : RuntimeRep
--     FloatRep  : RuntimeRep
--     DoubleRep : RuntimeRep

-- {-# COMPILE GHC RuntimeRep = data GHC.Exts.RuntimeRep (VecRep | TupleRep | SumRep | BoxedRep | IntRep | Int8Rep | Int16Rep | Int32Rep | Int64Rep | WordRep | Word8Rep | Word16Rep | Word32Rep | Word64Rep | AddrRep | FloatRep | DoubleRep)  #-}

postulate
    `VecCount : Set₁
    `Vec2 `Vec4 `Vec8 `Vec16 `Vec32 `Vec64 : `VecCount

    `VecElem : Set₁
    `Int8ElemRep   : `VecElem
    `Int16ElemRep  : `VecElem
    `Int32ElemRep  : `VecElem
    `Int64ElemRep  : `VecElem
    `Word8ElemRep  : `VecElem
    `Word16ElemRep : `VecElem
    `Word32ElemRep : `VecElem
    `Word64ElemRep : `VecElem
    `FloatElemRep  : `VecElem
    `DoubleElemRep : `VecElem

    `Levity : Set₁
    `Lifted : `Levity
    `Unlifted : `Levity

    `RuntimeRep : Set₁
    `VecRep    : `VecCount → `VecElem → `RuntimeRep
    `TupleRep  : `List `RuntimeRep → `RuntimeRep
    `SumRep    : `List `RuntimeRep → `RuntimeRep
    `BoxedRep  : `Levity → `RuntimeRep
    `IntRep    : `RuntimeRep
    `Int8Rep   : `RuntimeRep
    `Int16Rep  : `RuntimeRep
    `Int32Rep  : `RuntimeRep
    `Int64Rep  : `RuntimeRep
    `WordRep   : `RuntimeRep
    `Word8Rep  : `RuntimeRep
    `Word16Rep : `RuntimeRep
    `Word32Rep : `RuntimeRep
    `Word64Rep : `RuntimeRep
    `AddrRep   : `RuntimeRep
    `FloatRep  : `RuntimeRep
    `DoubleRep : `RuntimeRep

    TYPE : ∀{aℓ} → `RuntimeRep → Set aℓ → Set aℓ

Type : ∀{aℓ} → Set aℓ → Set aℓ
Type = TYPE (`BoxedRep `Lifted)

Hs-Type-syntax : ∀ aℓ {kℓ rℓ} (K : Set aℓ → Set kℓ) → (Set aℓ → Set rℓ) → Set (lsuc aℓ ⊔ kℓ ⊔ rℓ)
Hs-Type-syntax aℓ K R = {A : Set aℓ} → ⦃ K A ⦄ → R A
{-# STATIC Hs-Type-syntax #-}

infixr 0.1 _⟶_
_⟶_ : ∀{aℓ bℓ} → Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)
A ⟶ B = A → B

Hs-Type-syntax-0ℓ = Hs-Type-syntax lzero

infixr 0 Hs-Type-syntax Hs-Type-syntax-0ℓ
syntax Hs-Type-syntax aℓ K (λ A → r) = A :: K ^ aℓ ∙ r
syntax Hs-Type-syntax-0ℓ K (λ A → r) = A :: K ∙ r

postulate
    instance Type[Integer] : Type Integer

{-# COMPILE GHC `BoxedRep = type(0) 'GHC.Exts.BoxedRep #-}
{-# COMPILE GHC `Lifted = type(0) 'GHC.Exts.Lifted  #-}

postulate
    id : B :: Type ^ aℓ ∙ B ⟶ B
    const : A :: Type ^ aℓ ∙
            B :: Type ^ bℓ ∙
            A ⟶ B ⟶ A

{-# FOREIGN GHC data AgdaTYPE aℓ (r :: GHC.Exts.RuntimeRep) (x :: GHC.Exts.TYPE r) = AgdaTYPE #-}
{-# COMPILE GHC TYPE = type(2) AgdaTYPE #-} -- ! RuntimeRep arg must be expanded

{-# COMPILE GHC id = \ aℓ a AgdaTYPE -> id #-}
{-# COMPILE GHC const = \ aℓ bℓ a AgdaTYPE b AgdaTYPE -> const #-}
