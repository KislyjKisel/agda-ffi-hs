{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.Exts where

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Builtin.Char      using (Char)
open import Agda.Builtin.IO        using (IO)
open import Agda.Builtin.List      using (List; []; _∷_)
open import Agda.Primitive
open import Ffi.Hs.-base.Kind      using (IsKind)
open import Ffi.Hs.-base.Kind.List using (`List; `[]; lift`List)
open import Ffi.Hs.Data.Int        using (Int)

{-# FOREIGN GHC {-# LANGUAGE KindSignatures, PolyKinds, DataKinds #-} #-}
{-# FOREIGN GHC import qualified GHC.Exts #-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

data VecCount : Set where
    Vec2 Vec4 Vec8 Vec16 Vec32 Vec64 : VecCount

{-# COMPILE GHC VecCount = data GHC.Exts.VecCount (GHC.Exts.Vec2 | GHC.Exts.Vec4 | GHC.Exts.Vec8 | GHC.Exts.Vec16 | GHC.Exts.Vec32 | GHC.Exts.Vec64) #-}

postulate
    `VecCount : Set₁
    IsKind[`VecCount] : IsKind `VecCount
    `Vec2 `Vec4 `Vec8 `Vec16 `Vec32 `Vec64 : `VecCount

lift`VecCount : VecCount → `VecCount
lift`VecCount Vec2  = `Vec2
lift`VecCount Vec4  = `Vec4
lift`VecCount Vec8  = `Vec8
lift`VecCount Vec16 = `Vec16
lift`VecCount Vec32 = `Vec32
lift`VecCount Vec64 = `Vec64

{-# COMPILE GHC `VecCount = type(0) GHC.Exts.VecElem  #-}
{-# COMPILE GHC `Vec2     = type(0) 'GHC.Exts.Vec2    #-}
{-# COMPILE GHC `Vec4     = type(0) 'GHC.Exts.Vec4    #-}
{-# COMPILE GHC `Vec8     = type(0) 'GHC.Exts.Vec8    #-}
{-# COMPILE GHC `Vec16    = type(0) 'GHC.Exts.Vec16   #-}
{-# COMPILE GHC `Vec32    = type(0) 'GHC.Exts.Vec32   #-}
{-# COMPILE GHC `Vec64    = type(0) 'GHC.Exts.Vec64   #-}

data VecElem : Set where
    Int8ElemRep   : VecElem
    Int16ElemRep  : VecElem
    Int32ElemRep  : VecElem
    Int64ElemRep  : VecElem
    Word8ElemRep  : VecElem
    Word16ElemRep : VecElem
    Word32ElemRep : VecElem
    Word64ElemRep : VecElem
    FloatElemRep  : VecElem
    DoubleElemRep : VecElem

{-# COMPILE GHC VecElem = data GHC.Exts.VecElem
    ( GHC.Exts.Int8ElemRep  | GHC.Exts.Int16ElemRep  | GHC.Exts.Int32ElemRep  | GHC.Exts.Int64ElemRep
    | GHC.Exts.Word8ElemRep | GHC.Exts.Word16ElemRep | GHC.Exts.Word32ElemRep | GHC.Exts.Word64ElemRep
    | GHC.Exts.FloatElemRep | GHC.Exts.DoubleElemRep
    ) #-}

postulate
    `VecElem : Set₁
    IsKind[`VecElem] : IsKind `VecElem
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

lift`VecElem : VecElem → `VecElem
lift`VecElem Int8ElemRep   = `Int8ElemRep
lift`VecElem Int16ElemRep  = `Int16ElemRep
lift`VecElem Int32ElemRep  = `Int32ElemRep
lift`VecElem Int64ElemRep  = `Int64ElemRep
lift`VecElem Word8ElemRep  = `Word8ElemRep
lift`VecElem Word16ElemRep = `Word16ElemRep
lift`VecElem Word32ElemRep = `Word32ElemRep
lift`VecElem Word64ElemRep = `Word64ElemRep
lift`VecElem FloatElemRep  = `FloatElemRep
lift`VecElem DoubleElemRep = `DoubleElemRep

{-# COMPILE GHC `VecElem       = type(0) GHC.Exts.VecElem         #-}
{-# COMPILE GHC `Int8ElemRep   = type(0) 'GHC.Exts.Int8ElemRep    #-}
{-# COMPILE GHC `Int16ElemRep  = type(0) 'GHC.Exts.Int16ElemRep   #-}
{-# COMPILE GHC `Int32ElemRep  = type(0) 'GHC.Exts.Int32ElemRep   #-}
{-# COMPILE GHC `Int64ElemRep  = type(0) 'GHC.Exts.Int64ElemRep   #-}
{-# COMPILE GHC `Word8ElemRep  = type(0) 'GHC.Exts.Word8ElemRep   #-}
{-# COMPILE GHC `Word16ElemRep = type(0) 'GHC.Exts.Word16ElemRep  #-}
{-# COMPILE GHC `Word32ElemRep = type(0) 'GHC.Exts.Word32ElemRep  #-}
{-# COMPILE GHC `Word64ElemRep = type(0) 'GHC.Exts.Word64ElemRep  #-}
{-# COMPILE GHC `FloatElemRep  = type(0) 'GHC.Exts.FloatElemRep   #-}
{-# COMPILE GHC `DoubleElemRep = type(0) 'GHC.Exts.DoubleElemRep  #-}

data Levity : Set where
    Lifted Unlifted : Levity

{-# COMPILE GHC Levity = data GHC.Exts.Levity (GHC.Exts.Lifted | GHC.Exts.Unlifted)  #-}

postulate
    `Levity : Set₁
    IsKind[`Levity] : IsKind `Levity
    `Lifted : `Levity
    `Unlifted : `Levity

lift`Levity : Levity → `Levity
lift`Levity Lifted   = `Lifted
lift`Levity Unlifted = `Unlifted

{-# COMPILE GHC `Levity   = type(0) GHC.Exts.Levity     #-}
{-# COMPILE GHC `Lifted   = type(0) 'GHC.Exts.Lifted    #-}
{-# COMPILE GHC `Unlifted = type(0) 'GHC.Exts.Unlifted  #-}

data RuntimeRep : Set where
    VecRep    : VecCount → VecElem → RuntimeRep
    TupleRep  : List RuntimeRep → RuntimeRep
    SumRep    : List RuntimeRep → RuntimeRep
    BoxedRep  : Levity → RuntimeRep
    IntRep    : RuntimeRep
    Int8Rep   : RuntimeRep
    Int16Rep  : RuntimeRep
    Int32Rep  : RuntimeRep
    Int64Rep  : RuntimeRep
    WordRep   : RuntimeRep
    Word8Rep  : RuntimeRep
    Word16Rep : RuntimeRep
    Word32Rep : RuntimeRep
    Word64Rep : RuntimeRep
    AddrRep   : RuntimeRep
    FloatRep  : RuntimeRep
    DoubleRep : RuntimeRep

{-# COMPILE GHC RuntimeRep = data GHC.Exts.RuntimeRep
    ( GHC.Exts.VecRep    | GHC.Exts.TupleRep  | GHC.Exts.SumRep
    | GHC.Exts.BoxedRep  | GHC.Exts.IntRep    | GHC.Exts.Int8Rep
    | GHC.Exts.Int16Rep  | GHC.Exts.Int32Rep  | GHC.Exts.Int64Rep
    | GHC.Exts.WordRep   | GHC.Exts.Word8Rep  | GHC.Exts.Word16Rep
    | GHC.Exts.Word32Rep | GHC.Exts.Word64Rep | GHC.Exts.AddrRep
    | GHC.Exts.FloatRep  | GHC.Exts.DoubleRep
    )  #-}

postulate
    `RuntimeRep : Set₁
    IsKind[`RuntimeRep] : IsKind `RuntimeRep
    `VecRep    : `VecCount → `VecElem → `RuntimeRep
    `TupleRep  : `List `RuntimeRep ⦃ IsKind[`RuntimeRep] ⦄ → `RuntimeRep
    `SumRep    : `List `RuntimeRep ⦃ IsKind[`RuntimeRep] ⦄ → `RuntimeRep
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

{-# TERMINATING #-}
lift`RuntimeRep : RuntimeRep → `RuntimeRep
lift`RuntimeRep (VecRep cnt el) = `VecRep (lift`VecCount cnt) (lift`VecElem el)
lift`RuntimeRep (TupleRep fs)   = `TupleRep (lift`List ⦃ IsKind[`RuntimeRep] ⦄ lift`RuntimeRep fs)
lift`RuntimeRep (SumRep fs)     = `SumRep (lift`List ⦃ IsKind[`RuntimeRep] ⦄ lift`RuntimeRep fs)
lift`RuntimeRep (BoxedRep lev)  = `BoxedRep (lift`Levity lev)
lift`RuntimeRep IntRep          = `IntRep
lift`RuntimeRep Int8Rep         = `Int8Rep
lift`RuntimeRep Int16Rep        = `Int16Rep
lift`RuntimeRep Int32Rep        = `Int32Rep
lift`RuntimeRep Int64Rep        = `Int64Rep
lift`RuntimeRep WordRep         = `WordRep
lift`RuntimeRep Word8Rep        = `Word8Rep
lift`RuntimeRep Word16Rep       = `Word16Rep
lift`RuntimeRep Word32Rep       = `Word32Rep
lift`RuntimeRep Word64Rep       = `Word64Rep
lift`RuntimeRep AddrRep         = `AddrRep
lift`RuntimeRep FloatRep        = `FloatRep
lift`RuntimeRep DoubleRep       = `DoubleRep

{-# COMPILE GHC `RuntimeRep = type(0) GHC.Exts.RuntimeRep #-}
{-# COMPILE GHC `VecRep     = type(0) 'GHC.Exts.VecRep    #-}
{-# COMPILE GHC `SumRep     = type(0) 'GHC.Exts.SumRep    #-}
{-# COMPILE GHC `TupleRep   = type(0) 'GHC.Exts.TupleRep  #-}
{-# COMPILE GHC `BoxedRep   = type(0) 'GHC.Exts.BoxedRep  #-}
{-# COMPILE GHC `IntRep     = type(0) 'GHC.Exts.IntRep    #-}
{-# COMPILE GHC `Int8Rep    = type(0) 'GHC.Exts.Int8Rep   #-}
{-# COMPILE GHC `Int16Rep   = type(0) 'GHC.Exts.Int16Rep  #-}
{-# COMPILE GHC `Int32Rep   = type(0) 'GHC.Exts.Int32Rep  #-}
{-# COMPILE GHC `Int64Rep   = type(0) 'GHC.Exts.Int64Rep  #-}
{-# COMPILE GHC `WordRep    = type(0) 'GHC.Exts.WordRep   #-}
{-# COMPILE GHC `Word8Rep   = type(0) 'GHC.Exts.Word8Rep  #-}
{-# COMPILE GHC `Word16Rep  = type(0) 'GHC.Exts.Word16Rep #-}
{-# COMPILE GHC `Word32Rep  = type(0) 'GHC.Exts.Word32Rep #-}
{-# COMPILE GHC `Word64Rep  = type(0) 'GHC.Exts.Word64Rep #-}
{-# COMPILE GHC `AddrRep    = type(0) 'GHC.Exts.AddrRep   #-}
{-# COMPILE GHC `FloatRep   = type(0) 'GHC.Exts.FloatRep  #-}
{-# COMPILE GHC `DoubleRep  = type(0) 'GHC.Exts.DoubleRep #-}

postulate
    TYPE : `RuntimeRep → Set aℓ → Set aℓ

{-# FOREIGN GHC data AgdaTYPE aℓ (rr :: GHC.Exts.RuntimeRep) (a :: GHC.Exts.TYPE rr) = AgdaTYPE #-}
{-# COMPILE GHC TYPE = type(2) AgdaTYPE #-} -- ! RuntimeRep arg must be present (why?)

`ZeroBitRep : `RuntimeRep
`ZeroBitRep = `TupleRep (`[] ⦃ IsKind[`RuntimeRep] ⦄)

`ZeroBitType : Set aℓ → Set aℓ
`ZeroBitType = TYPE `ZeroBitRep

`UnliftedRep : `RuntimeRep
`UnliftedRep = `BoxedRep `Unlifted

UnliftedType : Set aℓ → Set aℓ
UnliftedType = TYPE `UnliftedRep

`LiftedRep : `RuntimeRep
`LiftedRep = `BoxedRep `Lifted

LiftedType : Set aℓ → Set aℓ
LiftedType = TYPE `LiftedRep

postulate
    RealWorld : Set

{-# COMPILE GHC RealWorld = GHC.Exts.RealWorld #-}

--     Addr# Void# Char# Double# Float# : Set
--     ByteArray# ArrayArray# ThreadId# BCO Compact# : Set
--     Int# Int8# Int16# Int32# Int64# : Set
--     Word# Word8# Word16# Word32# Word64# : Set
--     Array# Weak# MutableByteArray# StablePtr# : Set aℓ → Set aℓ
--     StableName# MutableArrayArray# SmallArray# : Set aℓ → Set aℓ
--     TVar# MVar# IOPort# MutVar# : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)
--     SmallMutableArray# MutableArray# : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)
--     -- todo Proxy#, rep insts, Vec types, primops

--     Ptr : Set aℓ → Set aℓ
--     FunPtr : Set aℓ → Set aℓ
--     seq : A → B → B


postulate
    breakpoint         : A → A
    breakpointCond     : Bool → A → A
    currentCallStack   : IO (List (List Char))
    inline             : A → A
    noinline           : A → A
    lazy               : A → A
    considerAccessible : Bool
    maxTupleSize       : Int

    oneShot-ll : ⦃ LiftedType A   ⦄ → ⦃ LiftedType B   ⦄ → (A → B) → A → B
    oneShot-lu : ⦃ LiftedType A   ⦄ → ⦃ UnliftedType B ⦄ → (A → B) → A → B
    oneShot-ul : ⦃ UnliftedType A ⦄ → ⦃ LiftedType B   ⦄ → (A → B) → A → B
    oneShot-uu : ⦃ UnliftedType A ⦄ → ⦃ UnliftedType B ⦄ → (A → B) → A → B

{-# COMPILE GHC breakpoint         = \ aℓ a -> GHC.Exts.breakpoint         #-}
{-# COMPILE GHC breakpointCond     = \ aℓ a -> GHC.Exts.breakpointCond     #-}
{-# COMPILE GHC currentCallStack   =           GHC.Exts.currentCallStack   #-}
{-# COMPILE GHC inline             = \ aℓ a -> GHC.Exts.inline             #-}
{-# COMPILE GHC noinline           = \ aℓ a -> GHC.Exts.noinline           #-}
{-# COMPILE GHC lazy               = \ aℓ a -> GHC.Exts.lazy               #-}
{-# COMPILE GHC considerAccessible =           GHC.Exts.considerAccessible #-}
{-# COMPILE GHC maxTupleSize       =           GHC.Exts.maxTupleSize       #-}

{-# COMPILE GHC oneShot-ll = \ aℓ bℓ a b AgdaTYPE AgdaTYPE -> GHC.Exts.oneShot #-}
{-# COMPILE GHC oneShot-lu = \ aℓ bℓ a b AgdaTYPE AgdaTYPE -> GHC.Exts.oneShot #-}
{-# COMPILE GHC oneShot-ul = \ aℓ bℓ a b AgdaTYPE AgdaTYPE -> GHC.Exts.oneShot #-}
{-# COMPILE GHC oneShot-uu = \ aℓ bℓ a b AgdaTYPE AgdaTYPE -> GHC.Exts.oneShot #-}

data SpecConstrAnnotation : Set where
    NoSpecConstr : SpecConstrAnnotation
    ForceSpecConstr : SpecConstrAnnotation

{-# COMPILE GHC SpecConstrAnnotation = data GHC.Exts.SpecConstrAnnotation (GHC.Exts.NoSpecConstr | GHC.Exts.ForceSpecConstr) #-}

data SPEC : Set where
    mkSPEC SPEC2 : SPEC

{-# COMPILE GHC SPEC = data GHC.Exts.SPEC (GHC.Exts.SPEC | GHC.Exts.SPEC2) #-}

-- WithDict - нема понимания
-- todo: equalities? etc


