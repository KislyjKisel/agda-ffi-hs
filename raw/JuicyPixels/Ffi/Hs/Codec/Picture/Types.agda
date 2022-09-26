{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Types where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List)
open import Agda.Primitive
open import Ffi.Hs.Control.DeepSeq using (NFData)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.Type.Equality using (_~_)
open import Agda.Builtin.Equality using (_≡_)
open import Ffi.Hs.Data.Word using (Word8; Word16; Word32)
open import Ffi.Hs.-base.Float using (Float)
open import Ffi.Hs.Data.Int using (Int)
open import Ffi.Hs.Control.Monad.Primitive using (PrimMonad; PrimState)
open import Ffi.Hs.-base.Unit using (⊤; ⊤′)
open import Ffi.Hs.Data.Vector.Storable using (Vector)
open import Ffi.Hs.Data.Vector.Storable.Mutable using (STVector)
open import Ffi.Hs.Data.Tuple using (Tuple2)

private
    variable
        aℓ bℓ mℓ : Level
        A : Set aℓ
        B : Set bℓ
        M : Set mℓ → Set mℓ
        S : Set

-- Image Types
record Image (A : Set aℓ) : Set aℓ
record MutableImage (S : Set) (A : Set aℓ) : Set aℓ
data DynamicImage : Set

-- Type classes

postulate
    Pixel : Set aℓ → Set aℓ
    PixelBaseComponent : (A : Set aℓ) → Set aℓ

-- todo: Image Lenses

-- Pixel types

Pixel8 : Set
Pixel8 = Word8

Pixel16 : Set
Pixel16 = Word16

Pixel32 : Set
Pixel32 = Word32

PixelF : Set
PixelF = Float

data PixelYA8 : Set where
    mkPixelYA8 : Pixel8 → Pixel8 → PixelYA8

postulate
    Eq[PixelYA8]   : Eq PixelYA8
    Ord[PixelYA8]  : Ord PixelYA8
    Show[PixelYA8] : Show PixelYA8
    Pixel[PixelYA8] : Pixel PixelYA8
    PixelBaseComponent[PixelYA8] : PixelBaseComponent PixelYA8 ≡ Word8

data PixelYA16 : Set where
    mkPixelYA16 : Pixel16 → Pixel16 → PixelYA16

postulate
    Eq[PixelYA16]   : Eq PixelYA16
    Ord[PixelYA16]  : Ord PixelYA16
    Show[PixelYA16] : Show PixelYA16

data PixelRGB8 : Set where
    mkPixelRGB8 : Pixel8 → Pixel8 → Pixel8 → PixelRGB8

postulate
    Eq[PixelRGB8]   : Eq PixelRGB8
    Ord[PixelRGB8]  : Ord PixelRGB8
    Show[PixelRGB8] : Show PixelRGB8

data PixelRGB16 : Set where
    mkPixelRGB16 : Pixel16 → Pixel16 → Pixel16 → PixelRGB16

postulate
    Eq[PixelRGB16]   : Eq PixelRGB16
    Ord[PixelRGB16]  : Ord PixelRGB16
    Show[PixelRGB16] : Show PixelRGB16

data PixelRGBF : Set where
    mkPixelRGBF : PixelF → PixelF → PixelF → PixelRGBF

postulate
    Eq[PixelRGBF]   : Eq PixelRGBF
    Ord[PixelRGBF]  : Ord PixelRGBF
    Show[PixelRGBF] : Show PixelRGBF

data PixelRGBA8 : Set where
    mkPixelRGBA8 : Pixel8 → Pixel8 → Pixel8 → Pixel8 → PixelRGBA8

postulate
    Eq[PixelRGBA8]   : Eq PixelRGBA8
    Ord[PixelRGBA8]  : Ord PixelRGBA8
    Show[PixelRGBA8] : Show PixelRGBA8

data PixelRGBA16 : Set where
    mkPixelRGBA16 : Pixel16 → Pixel16 → Pixel16 → Pixel16 → PixelRGBA16

postulate
    Eq[PixelRGBA16]   : Eq PixelRGBA16
    Ord[PixelRGBA16]  : Ord PixelRGBA16
    Show[PixelRGBA16] : Show PixelRGBA16

data PixelCMYK8 : Set where
    mkPixelCMYK8 : Pixel8 → Pixel8 → Pixel8 → Pixel8 → PixelCMYK8

postulate
    Eq[PixelCMYK8]   : Eq PixelCMYK8
    Ord[PixelCMYK8]  : Ord PixelCMYK8
    Show[PixelCMYK8] : Show PixelCMYK8

data PixelCMYK16 : Set where
    mkPixelCMYK16 : Pixel16 → Pixel16 → Pixel16 → Pixel16 → PixelCMYK16

postulate
    Eq[PixelCMYK16]   : Eq PixelCMYK16
    Ord[PixelCMYK16]  : Ord PixelCMYK16
    Show[PixelCMYK16] : Show PixelCMYK16

data PixelYCbCr8 : Set where
    mkPixelYCbCr8 : Pixel8 → Pixel8 → Pixel8 → PixelYCbCr8

postulate
    Eq[PixelYCbCr8]   : Eq PixelYCbCr8
    Ord[PixelYCbCr8]  : Ord PixelYCbCr8
    Show[PixelYCbCr8] : Show PixelYCbCr8

data PixelYCbCrK8 : Set where
    mkPixelYCbCrK8 : Pixel8 → Pixel8 → Pixel8 → Pixel8 → PixelYCbCrK8

postulate
    Eq[PixelYCbCrK8]   : Eq PixelYCbCrK8
    Ord[PixelYCbCrK8]  : Ord PixelYCbCrK8
    Show[PixelYCbCrK8] : Show PixelYCbCrK8

-- Type classes

postulate
    Pixel[A]⇒Storable[PixelBaseComponent[A]] : ⦃ _ : Pixel A ⦄ → Storable (PixelBaseComponent A)
    Pixel[A]⇒Num[PixelBaseComponent[A]]      : ⦃ _ : Pixel A ⦄ → Num (PixelBaseComponent A)
    Pixel[A]⇒Eq[A]                           : ⦃ Pixel A ⦄ → Eq A

    mixWith               : ⦃ _ : Pixel A ⦄ → (Int → PixelBaseComponent A → PixelBaseComponent A → PixelBaseComponent A) → A → A → A
    mixWithAlpha          : ⦃ _ : Pixel A ⦄ → (Int → PixelBaseComponent A → PixelBaseComponent A → PixelBaseComponent A) → (PixelBaseComponent A → PixelBaseComponent A → PixelBaseComponent A) → A → A → A
    pixelOpacity          : ⦃ _ : Pixel A ⦄ → A → PixelBaseComponent A
    componentCount        : ⦃ Pixel A ⦄ → A → Int
    colorMap              : ⦃ _ : Pixel A ⦄ → (PixelBaseComponent A → PixelBaseComponent A) → A → A
    pixelBaseIndex        : ⦃ Pixel A ⦄ → Image A → Int → Int → Int
    mutablePixelBaseIndex : ⦃ Pixel A ⦄ → MutableImage S A → Int → Int → Int
    pixelAt               : ⦃ Pixel A ⦄ → Image A → Int → Int → A
    readPixel             : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → MutableImage (PrimState M) A → Int → Int → M A
    writePixel            : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → MutableImage (PrimState M) A → Int → Int → A → M ⊤′
    unsafePixelAt         : ⦃ _ : Pixel A ⦄ → Vector (PixelBaseComponent A) → Int → A
    unsafeReadPixel       : ⦃ _ : Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → STVector (PrimState M) (PixelBaseComponent A) → Int → M A
    unsafeWritePixel      : ⦃ _ : Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → STVector (PrimState M) (PixelBaseComponent A) → Int → A → M ⊤′


postulate
    ColorConvertible : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)

    ColorConvertible[A,B]⇒Pixel[A] : ⦃ ColorConvertible A B ⦄ → Pixel A
    ColorConvertible[A,B]⇒Pixel[B] : ⦃ ColorConvertible A B ⦄ → Pixel B
    ColorConvertible[A,A] : ⦃ Pixel A ⦄ → ColorConvertible A A

    promotePixel : ⦃ ColorConvertible A B ⦄ → A → B
    promoteImage : ⦃ ColorConvertible A B ⦄ → Image A → Image B

    ColorConvertible[PixelRGBA8,PixelRGBA16] : ColorConvertible PixelRGBA8 PixelRGBA16
    ColorConvertible[PixelRGB16,PixelRGBA16] : ColorConvertible PixelRGB16 PixelRGBA16
    ColorConvertible[PixelRGB8,PixelRGBA16] : ColorConvertible PixelRGB8 PixelRGBA16
    ColorConvertible[PixelRGB8,PixelRGBA8] : ColorConvertible PixelRGB8 PixelRGBA8
    ColorConvertible[PixelRGB8,PixelRGBF] : ColorConvertible PixelRGB8 PixelRGBF
    ColorConvertible[PixelRGB8,PixelRGB16] : ColorConvertible PixelRGB8 PixelRGB16
    ColorConvertible[PixelYA16,PixelRGBA16] : ColorConvertible PixelYA16 PixelRGBA16
    ColorConvertible[PixelYA16,PixelRGB16] : ColorConvertible PixelYA16 PixelRGB16
    ColorConvertible[PixelYA8,PixelRGBA8] : ColorConvertible PixelYA8 PixelRGBA8
    ColorConvertible[PixelYA8,PixelRGB16] : ColorConvertible PixelYA8 PixelRGB16
    ColorConvertible[PixelYA8,PixelRGB8] : ColorConvertible PixelYA8 PixelRGB8
    ColorConvertible[PixelF,PixelRGBF] : ColorConvertible PixelF PixelRGBF
    ColorConvertible[Pixel16,PixelRGBA16] : ColorConvertible Pixel16 PixelRGBA16
    ColorConvertible[Pixel16,PixelRGB16] : ColorConvertible Pixel16 PixelRGB16
    ColorConvertible[Pixel16,PixelYA16] : ColorConvertible Pixel16 PixelYA16
    ColorConvertible[Pixel8,PixelRGBA8] : ColorConvertible Pixel8 PixelRGBA8
    ColorConvertible[Pixel8,PixelRGB16] : ColorConvertible Pixel8 PixelRGB16
    ColorConvertible[Pixel8,PixelRGB8] : ColorConvertible Pixel8 PixelRGB8
    ColorConvertible[Pixel8,PixelYA8] : ColorConvertible Pixel8 PixelYA8
    ColorConvertible[Pixel8,PixelF] : ColorConvertible Pixel8 PixelF
    ColorConvertible[Pixel8,Pixel16] : ColorConvertible Pixel8 Pixel16

postulate
    ColorSpaceConvertible : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)

    ColorSpaceConvertible[A,B]⇒Pixel[A] : ⦃ ColorSpaceConvertible A B ⦄ → Pixel A
    ColorSpaceConvertible[A,B]⇒Pixel[B] : ⦃ ColorSpaceConvertible A B ⦄ → Pixel B
    ColorSpaceConvertible[A,A] : ⦃ Pixel A ⦄ → ColorSpaceConvertible A A

    convertPixel : ⦃ ColorSpaceConvertible A B ⦄ → A → B
    convertImage : ⦃ ColorSpaceConvertible A B ⦄ → Image A → Image B

    ColorSpaceConvertible[PixelCMYK16,PixelRGB16] : ColorSpaceConvertible PixelCMYK16 PixelRGB16
    ColorSpaceConvertible[PixelCMYK8,PixelRGB8] : ColorSpaceConvertible PixelCMYK8 PixelRGB8
    ColorSpaceConvertible[PixelYCbCr8,PixelRGB8] : ColorSpaceConvertible PixelYCbCr8 PixelRGB8
    ColorSpaceConvertible[PixelRGB16,PixelCMYK16] : ColorSpaceConvertible PixelRGB16 PixelCMYK16
    ColorSpaceConvertible[PixelYCbCrK8,PixelCMYK8] : ColorSpaceConvertible PixelYCbCrK8 PixelCMYK8
    ColorSpaceConvertible[PixelYCbCrK8,PixelRGB8] : ColorSpaceConvertible PixelYCbCrK8 PixelRGB8
    ColorSpaceConvertible[PixelRGB8,PixelCMYK8] : ColorSpaceConvertible PixelRGB8 PixelCMYK8
    ColorSpaceConvertible[PixelRGB8,PixelYCbCr8] : ColorSpaceConvertible PixelRGB8 PixelYCbCr8


postulate
    LumaPlaneExtractable : Set aℓ → Set aℓ

    LumaPlaneExtractable[A]⇒Pixel[A] : ⦃ LumaPlaneExtractable A ⦄ → Pixel A
    LumaPlaneExtractable[A]⇒Pixel[B] : ⦃ _ : LumaPlaneExtractable A ⦄ → Pixel (PixelBaseComponent A)

    computeLuma      : ⦃ _ : LumaPlaneExtractable A ⦄ → A → PixelBaseComponent A
    extractLumaPlane : ⦃ _ : LumaPlaneExtractable A ⦄ → Image A → Image (PixelBaseComponent A)

    LumaPlaneExtractable[PixelRGBA8]  : LumaPlaneExtractable PixelRGBA8
    LumaPlaneExtractable[PixelYCbCr8] : LumaPlaneExtractable PixelYCbCr8
    LumaPlaneExtractable[PixelRGBF]   : LumaPlaneExtractable PixelRGBF
    LumaPlaneExtractable[PixelRGB16]  : LumaPlaneExtractable PixelRGB16
    LumaPlaneExtractable[PixelRGB8]   : LumaPlaneExtractable PixelRGB8
    LumaPlaneExtractable[PixelYA8]    : LumaPlaneExtractable PixelYA8
    LumaPlaneExtractable[PixelF]      : LumaPlaneExtractable PixelF
    LumaPlaneExtractable[Pixel32]     : LumaPlaneExtractable Pixel32
    LumaPlaneExtractable[Pixel16]     : LumaPlaneExtractable Pixel16
    LumaPlaneExtractable[Pixel8]      : LumaPlaneExtractable Pixel8


postulate
    TransparentPixel : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)

    TransparentPixel[A,B]⇒Pixel[A] : ⦃ TransparentPixel A B ⦄ → Pixel A
    TransparentPixel[A,B]⇒Pixel[B] : ⦃ TransparentPixel A B ⦄ → Pixel B

    dropTransparency : ⦃ TransparentPixel A B ⦄ → A → B

    TransparentPixel[PixelRGBA16,PixelRGB16] : TransparentPixel PixelRGBA16 PixelRGB16
    TransparentPixel[PixelRGBA8,PixelRGB8] : TransparentPixel PixelRGBA8 PixelRGB8
    TransparentPixel[PixelYA16,Pixel16] : TransparentPixel PixelYA16 Pixel16
    TransparentPixel[PixelYA8,Pixel8] : TransparentPixel PixelYA8 Pixel8


-- Helper functions

postulate
    pixelMap            : ⦃ Pixel A ⦄ → ⦃ Pixel B ⦄ → (A → B) → Image A → Image B
    pixelMapXY          : ⦃ Pixel A ⦄ → ⦃ Pixel B ⦄ → (Int → Int → A → B) → Image A → Image B
    pixelFold           : ⦃ Pixel A ⦄ → (B → Int → Int → A → B) → B → Image A → B
    pixelFoldM          : ⦃ Pixel A ⦄ → ⦃ Monad M ⦄ → (B → Int → Int → A → M B) → B → Image A → M B
    pixelFoldMap        : ⦃ Pixel A ⦄ → ⦃ Monoid B ⦄ → (A → B) → Image A → B
    dynamicMap          : (∀{aℓ}{A : Set aℓ} → ⦃ Pixel A ⦄ → Image A → B) → DynamicImage → B
    dynamicPixelMap     : (∀{aℓ}{A : Set aℓ} → ⦃ Pixel A ⦄ → Image A → Image A) → DynamicImage → DynamicImage
    dynSquare           : DynamicImage → DynamicImage
    squareImage         : ⦃ Pixel A ⦄ → Image A → Image A
    -- todo: palettedToTrueColor : PalettedImage → DynamicImage
    -- todo: palettedAsImage     : Palette' A → Image A
    dropAlphaLayer      : ⦃ TransparentPixel A B ⦄ → Image A → Image B
    withImage           : ⦃ Pixel A ⦄ → ⦃ PrimMonad M ⦄ → Int → Int → (Int → Int → M A) → M (Image A)
    zipPixelComponent3  : ⦃ _ : Pixel A ⦄ → (PixelBaseComponent A → PixelBaseComponent A → PixelBaseComponent A → PixelBaseComponent A) → Image A → Image A → Image A → Image A
    generateImage       : ⦃ Pixel A ⦄ → (Int → Int → A) → Int → Int → Image A
    imageCreator        : List Char → IO ⊤
    generateFoldImage   : ⦃ Pixel A ⦄ → (B → Int → Int → Tuple2 B A) → B → Int → Int → Tuple2 B (Image A)
    gammaCorrection     : PixelF → Image PixelRGBF → Image PixelRGBF
    toneMapping         : PixelF → Image PixelRGBF → Image PixelRGBF

-- Color plane extraction

data PlaneRed : Set where
    mkPlaneRed : PlaneRed

{-# COMPILE GHC PlaneRed = data Codec.Picture.Types.PlaneRed (Codec.Picture.Types.PlaneRed) #-}

data PlaneGreen : Set where
    mkPlaneGreen : PlaneGreen

{-# COMPILE GHC PlaneGreen = data Codec.Picture.Types.PlaneGreen (Codec.Picture.Types.PlaneGreen) #-}

data PlaneBlue : Set where
    mkPlaneBlue : PlaneBlue

{-# COMPILE GHC PlaneBlue = data Codec.Picture.Types.PlaneBlue (Codec.Picture.Types.PlaneBlue) #-}

data PlaneAlpha : Set where
    mkPlaneAlpha : PlaneAlpha

{-# COMPILE GHC PlaneAlpha = data Codec.Picture.Types.PlaneAlpha (Codec.Picture.Types.PlaneAlpha) #-}

data PlaneLuma : Set where
    mkPlaneLuma : PlaneLuma

{-# COMPILE GHC PlaneLuma = data Codec.Picture.Types.PlaneLuma (Codec.Picture.Types.PlaneLuma) #-}

data PlaneCr : Set where
    mkPlaneCr : PlaneCr

{-# COMPILE GHC PlaneCr = data Codec.Picture.Types.PlaneCr (Codec.Picture.Types.PlaneCr) #-}

data PlaneCb : Set where
    mkPlaneCb : PlaneCb

{-# COMPILE GHC PlaneCb = data Codec.Picture.Types.PlaneCb (Codec.Picture.Types.PlaneCb) #-}

data PlaneCyan : Set where
    mkPlaneCyan : PlaneCyan

{-# COMPILE GHC PlaneCyan = data Codec.Picture.Types.PlaneCyan (Codec.Picture.Types.PlaneCyan) #-}

data PlaneMagenta : Set where
    mkPlaneMagenta : PlaneMagenta

{-# COMPILE GHC PlaneMagenta = data Codec.Picture.Types.PlaneMagenta (Codec.Picture.Types.PlaneMagenta) #-}

data PlaneYellow : Set where
    mkPlaneYellow : PlaneYellow

{-# COMPILE GHC PlaneYellow = data Codec.Picture.Types.PlaneYellow (Codec.Picture.Types.PlaneYellow) #-}

data PlaneBlack : Set where
    mkPlaneBlack : PlaneBlack

{-# COMPILE GHC PlaneBlack = data Codec.Picture.Types.PlaneBlack (Codec.Picture.Types.PlaneBlack) #-}

postulate
    ColorPlane : Set aℓ → Set → Set aℓ

    ColorPlane[PixelRGBA16,PlaneAlpha]   : ColorPlane PixelRGBA16 PlaneAlpha
    ColorPlane[PixelRGBA16,PlaneBlue]    : ColorPlane PixelRGBA16 PlaneBlue
    ColorPlane[PixelRGBA16,PlaneGreen]   : ColorPlane PixelRGBA16 PlaneGreen
    ColorPlane[PixelRGBA16,PlaneRed]     : ColorPlane PixelRGBA16 PlaneRed
    ColorPlane[PixelRGBA8,PlaneAlpha]    : ColorPlane PixelRGBA8 PlaneAlpha
    ColorPlane[PixelRGBA8,PlaneBlue]     : ColorPlane PixelRGBA8 PlaneBlue
    ColorPlane[PixelRGBA8,PlaneGreen]    : ColorPlane PixelRGBA8 PlaneGreen
    ColorPlane[PixelRGBA8,PlaneRed]      : ColorPlane PixelRGBA8 PlaneRed
    ColorPlane[PixelCMYK16,PlaneBlack]   : ColorPlane PixelCMYK16 PlaneBlack
    ColorPlane[PixelCMYK16,PlaneYellow]  : ColorPlane PixelCMYK16 PlaneYellow
    ColorPlane[PixelCMYK16,PlaneMagenta] : ColorPlane PixelCMYK16 PlaneMagenta
    ColorPlane[PixelCMYK16,PlaneCyan]    : ColorPlane PixelCMYK16 PlaneCyan
    ColorPlane[PixelCMYK8,PlaneBlack]    : ColorPlane PixelCMYK8 PlaneBlack
    ColorPlane[PixelCMYK8,PlaneYellow]   : ColorPlane PixelCMYK8 PlaneYellow
    ColorPlane[PixelCMYK8,PlaneMagenta]  : ColorPlane PixelCMYK8 PlaneMagenta
    ColorPlane[PixelCMYK8,PlaneCyan]     : ColorPlane PixelCMYK8 PlaneCyan
    ColorPlane[PixelYCbCr8,PlaneCb]      : ColorPlane PixelYCbCr8 PlaneCb
    ColorPlane[PixelYCbCr8,PlaneCr]      : ColorPlane PixelYCbCr8 PlaneCr
    ColorPlane[PixelYCbCr8,PlaneLuma]    : ColorPlane PixelYCbCr8 PlaneLuma
    ColorPlane[PixelRGBF,PlaneBlue]      : ColorPlane PixelRGBF PlaneBlue
    ColorPlane[PixelRGBF,PlaneGreen]     : ColorPlane PixelRGBF PlaneGreen
    ColorPlane[PixelRGBF,PlaneRed]       : ColorPlane PixelRGBF PlaneRed
    ColorPlane[PixelRGB16,PlaneBlue]     : ColorPlane PixelRGB16 PlaneBlue
    ColorPlane[PixelRGB16,PlaneGreen]    : ColorPlane PixelRGB16 PlaneGreen
    ColorPlane[PixelRGB16,PlaneRed]      : ColorPlane PixelRGB16 PlaneRed
    ColorPlane[PixelRGB8,PlaneBlue]      : ColorPlane PixelRGB8 PlaneBlue
    ColorPlane[PixelRGB8,PlaneGreen]     : ColorPlane PixelRGB8 PlaneGreen
    ColorPlane[PixelRGB8,PlaneRed]       : ColorPlane PixelRGB8 PlaneRed
    ColorPlane[PixelYA16,PlaneLuma]      : ColorPlane PixelYA16 PlaneLuma
    ColorPlane[PixelYA16,PlaneAlpha]     : ColorPlane PixelYA16 PlaneAlpha
    ColorPlane[PixelYA8,PlaneLuma]       : ColorPlane PixelYA8 PlaneLuma
    ColorPlane[PixelYA8,PlaneAlpha]      : ColorPlane PixelYA8 PlaneAlpha

postulate
    extractComponent       : ∀{Plane} → ⦃ _ : Pixel A ⦄ → ⦃ _ : Pixel (PixelBaseComponent A) ⦄ → ⦃ PixelBaseComponent (PixelBaseComponent A) ~ PixelBaseComponent A ⦄ → ⦃ ColorPlane A Plane ⦄ → Plane → Image A → Image (PixelBaseComponent A)
    unsafeExtractComponent : ⦃ _ : Pixel A ⦄ → ⦃ _ : Pixel (PixelBaseComponent A) ⦄ → ⦃ PixelBaseComponent (PixelBaseComponent A) ~ PixelBaseComponent A ⦄ → Int → Image A → Image (PixelBaseComponent A)


-- todo: packeable writing

-- Image Types

record Image A where
    constructor mkImage
    field
        imageWidth : Int
        imageHeight : Int
        imageData : Vector (PixelBaseComponent A)

postulate
    Eq[Image[A]]     : ⦃ Eq (PixelBaseComponent A) ⦄ → ⦃ Storable (PixelBaseComponent A) ⦄ → Eq (Image A)
    NFData[Image[A]] : NFData (Image A)

record MutableImage S A where
    constructor mkMutableImage
    field
        mutableImageWidth  : Int
        mutableImageHeight : Int
        mutableImageData   : STVector S (PixelBaseComponent A)

postulate
    NFData[MutableImage[S,A]] : NFData (MutableImage S A)

data DynamicImage where
    ImageY8     : Image Pixel8      → DynamicImage
    ImageY16    : Image Pixel16     → DynamicImage
    ImageY32    : Image Pixel32     → DynamicImage
    ImageYF     : Image PixelF      → DynamicImage
    ImageYA8    : Image PixelYA8    → DynamicImage
    ImageYA16   : Image PixelYA16   → DynamicImage
    ImageRGB8   : Image PixelRGB8   → DynamicImage
    ImageRGB16  : Image PixelRGB16  → DynamicImage
    ImageRGBF   : Image PixelRGBF   → DynamicImage
    ImageRGBA8  : Image PixelRGBA8  → DynamicImage
    ImageRGBA16 : Image PixelRGBA16 → DynamicImage
    ImageYCbCr8 : Image PixelYCbCr8 → DynamicImage
    ImageCMYK8  : Image PixelCMYK8  → DynamicImage
    ImageCMYK16 : Image PixelCMYK16 → DynamicImage

postulate
    Eq[DynamicImage] : Eq DynamicImage
    NFData[DynamicImage] : NFData DynamicImage

Palette : Set
Palette = Image PixelRGB8

-- todo: PalettedImage, Palette'

-- Image functions
postulate
    createMutableImage : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → Int → Int → A → M (MutableImage (PrimState M) A)
    newMutableImage    : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → Int → Int → M (MutableImage (PrimState M) A)
    freezeImage        : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → MutableImage (PrimState M) A → M (Image A)
    unsafeFreezeImage  : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → MutableImage (PrimState M) A → M (Image A)
    thawImage          : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → Image A → M (MutableImage (PrimState M) A)
    unsafeThawImage    : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → Image A → M (MutableImage (PrimState M) A)
