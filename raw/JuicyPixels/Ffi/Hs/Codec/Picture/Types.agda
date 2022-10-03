{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Types where

open import Agda.Builtin.Char                   using (Char)
open import Agda.Builtin.Equality               using (_≡_)
open import Agda.Builtin.IO                     using (IO)
open import Agda.Builtin.List                   using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Unit                   using (⊤; ⊤′)
open import Ffi.Hs.Control.DeepSeq              using (NFData)
open import Ffi.Hs.Control.Monad.Primitive      using (PrimMonad; PrimState)
open import Ffi.Hs.Data.Int                     using (Int)
open import Ffi.Hs.Data.Tuple                   using (Tuple2)
open import Ffi.Hs.Data.Type.Equality           using (_~_)
open import Ffi.Hs.Data.Vector.Storable         using (Vector)
open import Ffi.Hs.Data.Vector.Storable.Mutable using (STVector)
open import Ffi.Hs.Data.Word                    using (Word8; Word16; Word32)
open import Ffi.Hs.GHC.Float                    using (Float)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Codec.Picture.Types
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.Monad.Primitive (AgdaPrimMonad(AgdaPrimMonad))
import MAlonzo.Code.Ffi.Hs.Data.Type.Equality (AgdaTypeEq(AgdaTypeEq))
import MAlonzo.Code.Ffi.Hs.Control.DeepSeq (AgdaNFData(AgdaNFData))
#-}

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

record Palette' (A : Set aℓ) : Set aℓ
data PalettedImage : Set

-- Type classes

postulate
    Pixel : Set aℓ → Set aℓ
    PixelBaseComponent : (A : Set aℓ) → Set aℓ

{-# FOREIGN GHC data AgdaPixel aℓ a = Codec.Picture.Types.Pixel a => AgdaPixel #-}
{-# COMPILE GHC Pixel = type(0) AgdaPixel #-}

{-# FOREIGN GHC type AgdaPixelBaseComponent aℓ a = Codec.Picture.Types.PixelBaseComponent a #-}
{-# COMPILE GHC PixelBaseComponent = type(2) AgdaPixelBaseComponent #-}

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

{-# COMPILE GHC PixelYA8 = data Codec.Picture.Types.PixelYA8 (Codec.Picture.Types.PixelYA8) #-}

postulate
    Eq[PixelYA8]    : Eq PixelYA8
    Ord[PixelYA8]   : Ord PixelYA8
    Show[PixelYA8]  : Show PixelYA8
    Pixel[PixelYA8] : Pixel PixelYA8
    PixelBaseComponent[PixelYA8] : PixelBaseComponent PixelYA8 ≡ Word8

{-# COMPILE GHC Eq[PixelYA8]    = AgdaEq    #-}
{-# COMPILE GHC Ord[PixelYA8]   = AgdaOrd   #-}
{-# COMPILE GHC Show[PixelYA8]  = AgdaShow  #-}
{-# COMPILE GHC Pixel[PixelYA8] = AgdaPixel #-}

data PixelYA16 : Set where
    mkPixelYA16 : Pixel16 → Pixel16 → PixelYA16

{-# COMPILE GHC PixelYA16 = data Codec.Picture.Types.PixelYA16 (Codec.Picture.Types.PixelYA16) #-}

postulate
    Eq[PixelYA16]   : Eq PixelYA16
    Ord[PixelYA16]  : Ord PixelYA16
    Show[PixelYA16] : Show PixelYA16
    Pixel[PixelYA16] : Pixel PixelYA16
    PixelBaseComponent[PixelYA16] : PixelBaseComponent PixelYA16 ≡ Word16

{-# COMPILE GHC Eq[PixelYA16]   = AgdaEq   #-}
{-# COMPILE GHC Ord[PixelYA16]  = AgdaOrd  #-}
{-# COMPILE GHC Show[PixelYA16] = AgdaShow #-}
{-# COMPILE GHC Pixel[PixelYA16] = AgdaPixel #-}

data PixelRGB8 : Set where
    mkPixelRGB8 : Pixel8 → Pixel8 → Pixel8 → PixelRGB8

{-# COMPILE GHC PixelRGB8 = data Codec.Picture.Types.PixelRGB8 (Codec.Picture.Types.PixelRGB8) #-}

postulate
    Eq[PixelRGB8]    : Eq PixelRGB8
    Ord[PixelRGB8]   : Ord PixelRGB8
    Show[PixelRGB8]  : Show PixelRGB8
    Pixel[PixelRGB8] : Pixel PixelRGB8
    PixelBaseComponent[PixelRGB8] : PixelBaseComponent PixelRGB8 ≡ Word8

{-# COMPILE GHC Eq[PixelRGB8]    = AgdaEq    #-}
{-# COMPILE GHC Ord[PixelRGB8]   = AgdaOrd   #-}
{-# COMPILE GHC Show[PixelRGB8]  = AgdaShow  #-}
{-# COMPILE GHC Pixel[PixelRGB8] = AgdaPixel #-}

data PixelRGB16 : Set where
    mkPixelRGB16 : Pixel16 → Pixel16 → Pixel16 → PixelRGB16

{-# COMPILE GHC PixelRGB16 = data Codec.Picture.Types.PixelRGB16 (Codec.Picture.Types.PixelRGB16) #-}

postulate
    Eq[PixelRGB16]    : Eq PixelRGB16
    Ord[PixelRGB16]   : Ord PixelRGB16
    Show[PixelRGB16]  : Show PixelRGB16
    Pixel[PixelRGB16] : Pixel PixelRGB16
    PixelBaseComponent[PixelRGB16] : PixelBaseComponent PixelRGB16 ≡ Word16

{-# COMPILE GHC Eq[PixelRGB16]    = AgdaEq    #-}
{-# COMPILE GHC Ord[PixelRGB16]   = AgdaOrd   #-}
{-# COMPILE GHC Show[PixelRGB16]  = AgdaShow  #-}
{-# COMPILE GHC Pixel[PixelRGB16] = AgdaPixel #-}

data PixelRGBF : Set where
    mkPixelRGBF : PixelF → PixelF → PixelF → PixelRGBF

{-# COMPILE GHC PixelRGBF = data Codec.Picture.Types.PixelRGBF (Codec.Picture.Types.PixelRGBF) #-}

postulate
    Eq[PixelRGBF]    : Eq PixelRGBF
    Ord[PixelRGBF]   : Ord PixelRGBF
    Show[PixelRGBF]  : Show PixelRGBF
    Pixel[PixelRGBF] : Pixel PixelRGBF
    PixelBaseComponent[PixelRGBF] : PixelBaseComponent PixelRGBF ≡ PixelF

{-# COMPILE GHC Eq[PixelRGBF]    = AgdaEq    #-}
{-# COMPILE GHC Ord[PixelRGBF]   = AgdaOrd   #-}
{-# COMPILE GHC Show[PixelRGBF]  = AgdaShow  #-}
{-# COMPILE GHC Pixel[PixelRGBF] = AgdaPixel #-}

data PixelRGBA8 : Set where
    mkPixelRGBA8 : Pixel8 → Pixel8 → Pixel8 → Pixel8 → PixelRGBA8

{-# COMPILE GHC PixelRGBA8 = data Codec.Picture.Types.PixelRGBA8 (Codec.Picture.Types.PixelRGBA8) #-}

postulate
    Eq[PixelRGBA8]    : Eq PixelRGBA8
    Ord[PixelRGBA8]   : Ord PixelRGBA8
    Show[PixelRGBA8]  : Show PixelRGBA8
    Pixel[PixelRGBA8] : Pixel PixelRGBA8
    PixelBaseComponent[PixelRGBA8] : PixelBaseComponent PixelRGBA8 ≡ Word8

{-# COMPILE GHC Eq[PixelRGBA8]    = AgdaEq    #-}
{-# COMPILE GHC Ord[PixelRGBA8]   = AgdaOrd   #-}
{-# COMPILE GHC Show[PixelRGBA8]  = AgdaShow  #-}
{-# COMPILE GHC Pixel[PixelRGBA8] = AgdaPixel #-}

data PixelRGBA16 : Set where
    mkPixelRGBA16 : Pixel16 → Pixel16 → Pixel16 → Pixel16 → PixelRGBA16

{-# COMPILE GHC PixelRGBA16 = data Codec.Picture.Types.PixelRGBA16 (Codec.Picture.Types.PixelRGBA16) #-}

postulate
    Eq[PixelRGBA16]    : Eq PixelRGBA16
    Ord[PixelRGBA16]   : Ord PixelRGBA16
    Show[PixelRGBA16]  : Show PixelRGBA16
    Pixel[PixelRGBA16] : Pixel PixelRGBA16
    PixelBaseComponent[PixelRGBA16] : PixelBaseComponent PixelRGBA16 ≡ Word16

{-# COMPILE GHC Eq[PixelRGBA16]    = AgdaEq    #-}
{-# COMPILE GHC Ord[PixelRGBA16]   = AgdaOrd   #-}
{-# COMPILE GHC Show[PixelRGBA16]  = AgdaShow  #-}
{-# COMPILE GHC Pixel[PixelRGBA16] = AgdaPixel #-}

data PixelCMYK8 : Set where
    mkPixelCMYK8 : Pixel8 → Pixel8 → Pixel8 → Pixel8 → PixelCMYK8

{-# COMPILE GHC PixelCMYK8 = data Codec.Picture.Types.PixelCMYK8 (Codec.Picture.Types.PixelCMYK8) #-}

postulate
    Eq[PixelCMYK8]    : Eq PixelCMYK8
    Ord[PixelCMYK8]   : Ord PixelCMYK8
    Show[PixelCMYK8]  : Show PixelCMYK8
    Pixel[PixelCMYK8] : Pixel PixelCMYK8
    PixelBaseComponent[PixelCMYK8] : PixelBaseComponent PixelCMYK8 ≡ Word8

{-# COMPILE GHC Eq[PixelCMYK8]    = AgdaEq    #-}
{-# COMPILE GHC Ord[PixelCMYK8]   = AgdaOrd   #-}
{-# COMPILE GHC Show[PixelCMYK8]  = AgdaShow  #-}
{-# COMPILE GHC Pixel[PixelCMYK8] = AgdaPixel #-}

data PixelCMYK16 : Set where
    mkPixelCMYK16 : Pixel16 → Pixel16 → Pixel16 → Pixel16 → PixelCMYK16

{-# COMPILE GHC PixelCMYK16 = data Codec.Picture.Types.PixelCMYK16 (Codec.Picture.Types.PixelCMYK16) #-}

postulate
    Eq[PixelCMYK16]    : Eq PixelCMYK16
    Ord[PixelCMYK16]   : Ord PixelCMYK16
    Show[PixelCMYK16]  : Show PixelCMYK16
    Pixel[PixelCMYK16] : Pixel PixelCMYK16
    PixelBaseComponent[PixelCMYK16] : PixelBaseComponent PixelCMYK16 ≡ Word16

{-# COMPILE GHC Eq[PixelCMYK16]    = AgdaEq    #-}
{-# COMPILE GHC Ord[PixelCMYK16]   = AgdaOrd   #-}
{-# COMPILE GHC Show[PixelCMYK16]  = AgdaShow  #-}
{-# COMPILE GHC Pixel[PixelCMYK16] = AgdaPixel #-}

data PixelYCbCr8 : Set where
    mkPixelYCbCr8 : Pixel8 → Pixel8 → Pixel8 → PixelYCbCr8

{-# COMPILE GHC PixelYCbCr8 = data Codec.Picture.Types.PixelYCbCr8 (Codec.Picture.Types.PixelYCbCr8) #-}

postulate
    Eq[PixelYCbCr8]    : Eq PixelYCbCr8
    Ord[PixelYCbCr8]   : Ord PixelYCbCr8
    Show[PixelYCbCr8]  : Show PixelYCbCr8
    Pixel[PixelYCbCr8] : Pixel PixelYCbCr8
    PixelBaseComponent[PixelYCbCr8] : PixelBaseComponent PixelYCbCr8 ≡ Word8

{-# COMPILE GHC Eq[PixelYCbCr8]    = AgdaEq    #-}
{-# COMPILE GHC Ord[PixelYCbCr8]   = AgdaOrd   #-}
{-# COMPILE GHC Show[PixelYCbCr8]  = AgdaShow  #-}
{-# COMPILE GHC Pixel[PixelYCbCr8] = AgdaPixel #-}

data PixelYCbCrK8 : Set where
    mkPixelYCbCrK8 : Pixel8 → Pixel8 → Pixel8 → Pixel8 → PixelYCbCrK8

{-# COMPILE GHC PixelYCbCrK8 = data Codec.Picture.Types.PixelYCbCrK8 (Codec.Picture.Types.PixelYCbCrK8) #-}

postulate
    Eq[PixelYCbCrK8]    : Eq PixelYCbCrK8
    Ord[PixelYCbCrK8]   : Ord PixelYCbCrK8
    Show[PixelYCbCrK8]  : Show PixelYCbCrK8
    Pixel[PixelYCbCrK8] : Pixel PixelYCbCrK8
    PixelBaseComponent[PixelYCbCrK8] : PixelBaseComponent PixelYCbCrK8 ≡ Word8

{-# COMPILE GHC Eq[PixelYCbCrK8]    = AgdaEq    #-}
{-# COMPILE GHC Ord[PixelYCbCrK8]   = AgdaOrd   #-}
{-# COMPILE GHC Show[PixelYCbCrK8]  = AgdaShow  #-}
{-# COMPILE GHC Pixel[PixelYCbCrK8] = AgdaPixel #-}

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

{-# COMPILE GHC Pixel[A]⇒Storable[PixelBaseComponent[A]] = \ aℓ a AgdaPixel -> AgdaStorable #-}
{-# COMPILE GHC Pixel[A]⇒Num[PixelBaseComponent[A]]      = \ aℓ a AgdaPixel -> AgdaNum      #-}
{-# COMPILE GHC Pixel[A]⇒Eq[A]                           = \ aℓ a AgdaPixel -> AgdaEq       #-}

{-# COMPILE GHC mixWith               = \ aℓ a AgdaPixel             -> Codec.Picture.Types.mixWith               #-}
{-# COMPILE GHC mixWithAlpha          = \ aℓ a AgdaPixel             -> Codec.Picture.Types.mixWithAlpha          #-}
{-# COMPILE GHC pixelOpacity          = \ aℓ a AgdaPixel             -> Codec.Picture.Types.pixelOpacity          #-}
{-# COMPILE GHC componentCount        = \ aℓ a AgdaPixel             -> Codec.Picture.Types.componentCount        #-}
{-# COMPILE GHC colorMap              = \ aℓ a AgdaPixel             -> Codec.Picture.Types.colorMap              #-}
{-# COMPILE GHC pixelBaseIndex        = \ aℓ a AgdaPixel             -> Codec.Picture.Types.pixelBaseIndex        #-}
{-# COMPILE GHC mutablePixelBaseIndex = \ aℓ a s AgdaPixel           -> Codec.Picture.Types.mutablePixelBaseIndex #-}
{-# COMPILE GHC pixelAt               = \ aℓ a AgdaPixel             -> Codec.Picture.Types.pixelAt               #-}
{-# COMPILE GHC readPixel             = \ aℓ a m AgdaPixel AgdaPrimMonad -> Codec.Picture.Types.readPixel             #-}
{-# COMPILE GHC writePixel            = \ aℓ a mℓ m AgdaPixel AgdaPrimMonad -> Codec.Picture.Types.writePixel            #-}
{-# COMPILE GHC unsafePixelAt         = \ aℓ a AgdaPixel             -> Codec.Picture.Types.unsafePixelAt         #-}
{-# COMPILE GHC unsafeReadPixel       = \ aℓ a m AgdaPixel AgdaPrimMonad -> Codec.Picture.Types.unsafeReadPixel       #-}
{-# COMPILE GHC unsafeWritePixel      = \ aℓ a mℓ m AgdaPixel AgdaPrimMonad -> Codec.Picture.Types.unsafeWritePixel      #-}


postulate
    ColorConvertible : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)

    ColorConvertible[A,B]⇒Pixel[A] : ⦃ ColorConvertible A B ⦄ → Pixel A
    ColorConvertible[A,B]⇒Pixel[B] : ⦃ ColorConvertible A B ⦄ → Pixel B
    ColorConvertible[A,A] : ⦃ Pixel A ⦄ → ColorConvertible A A

    promotePixel : ⦃ ColorConvertible A B ⦄ → A → B
    promoteImage : ⦃ ColorConvertible A B ⦄ → Image A → Image B

    ColorConvertible[PixelRGBA8,PixelRGBA16] : ColorConvertible PixelRGBA8 PixelRGBA16
    ColorConvertible[PixelRGB16,PixelRGBA16] : ColorConvertible PixelRGB16 PixelRGBA16
    ColorConvertible[PixelRGB8,PixelRGBA16]  : ColorConvertible PixelRGB8 PixelRGBA16
    ColorConvertible[PixelRGB8,PixelRGBA8]   : ColorConvertible PixelRGB8 PixelRGBA8
    ColorConvertible[PixelRGB8,PixelRGBF]    : ColorConvertible PixelRGB8 PixelRGBF
    ColorConvertible[PixelRGB8,PixelRGB16]   : ColorConvertible PixelRGB8 PixelRGB16
    ColorConvertible[PixelYA16,PixelRGBA16]  : ColorConvertible PixelYA16 PixelRGBA16
    ColorConvertible[PixelYA16,PixelRGB16]   : ColorConvertible PixelYA16 PixelRGB16
    ColorConvertible[PixelYA8,PixelRGBA8]    : ColorConvertible PixelYA8 PixelRGBA8
    ColorConvertible[PixelYA8,PixelRGB16]    : ColorConvertible PixelYA8 PixelRGB16
    ColorConvertible[PixelYA8,PixelRGB8]     : ColorConvertible PixelYA8 PixelRGB8
    ColorConvertible[PixelF,PixelRGBF]       : ColorConvertible PixelF PixelRGBF
    ColorConvertible[Pixel16,PixelRGBA16]    : ColorConvertible Pixel16 PixelRGBA16
    ColorConvertible[Pixel16,PixelRGB16]     : ColorConvertible Pixel16 PixelRGB16
    ColorConvertible[Pixel16,PixelYA16]      : ColorConvertible Pixel16 PixelYA16
    ColorConvertible[Pixel8,PixelRGBA8]      : ColorConvertible Pixel8 PixelRGBA8
    ColorConvertible[Pixel8,PixelRGB16]      : ColorConvertible Pixel8 PixelRGB16
    ColorConvertible[Pixel8,PixelRGB8]       : ColorConvertible Pixel8 PixelRGB8
    ColorConvertible[Pixel8,PixelYA8]        : ColorConvertible Pixel8 PixelYA8
    ColorConvertible[Pixel8,PixelF]          : ColorConvertible Pixel8 PixelF
    ColorConvertible[Pixel8,Pixel16]         : ColorConvertible Pixel8 Pixel16

{-# FOREIGN GHC data AgdaColorConvertible aℓ bℓ a b = Codec.Picture.Types.ColorConvertible a b => AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible = type(0) AgdaColorConvertible #-}

{-# COMPILE GHC ColorConvertible[A,B]⇒Pixel[A] = \ aℓ bℓ a b AgdaColorConvertible -> AgdaPixel            #-}
{-# COMPILE GHC ColorConvertible[A,B]⇒Pixel[B] = \ aℓ bℓ a b AgdaColorConvertible -> AgdaPixel            #-}
{-# COMPILE GHC ColorConvertible[A,A]          = \ aℓ a AgdaPixel                 -> AgdaColorConvertible #-}

{-# COMPILE GHC promotePixel = \ aℓ bℓ a b AgdaColorConvertible -> Codec.Picture.Types.promotePixel #-}
{-# COMPILE GHC promoteImage = \ aℓ bℓ a b AgdaColorConvertible -> Codec.Picture.Types.promoteImage #-}

{-# COMPILE GHC ColorConvertible[PixelRGBA8,PixelRGBA16] = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[PixelRGB16,PixelRGBA16] = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[PixelRGB8,PixelRGBA16]  = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[PixelRGB8,PixelRGBA8]   = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[PixelRGB8,PixelRGBF]    = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[PixelRGB8,PixelRGB16]   = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[PixelYA16,PixelRGBA16]  = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[PixelYA16,PixelRGB16]   = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[PixelYA8,PixelRGBA8]    = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[PixelYA8,PixelRGB16]    = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[PixelYA8,PixelRGB8]     = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[PixelF,PixelRGBF]       = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[Pixel16,PixelRGBA16]    = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[Pixel16,PixelRGB16]     = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[Pixel16,PixelYA16]      = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[Pixel8,PixelRGBA8]      = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[Pixel8,PixelRGB16]      = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[Pixel8,PixelRGB8]       = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[Pixel8,PixelYA8]        = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[Pixel8,PixelF]          = AgdaColorConvertible #-}
{-# COMPILE GHC ColorConvertible[Pixel8,Pixel16]         = AgdaColorConvertible #-}


postulate
    ColorSpaceConvertible : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)

    ColorSpaceConvertible[A,B]⇒Pixel[A] : ⦃ ColorSpaceConvertible A B ⦄ → Pixel A
    ColorSpaceConvertible[A,B]⇒Pixel[B] : ⦃ ColorSpaceConvertible A B ⦄ → Pixel B
    ColorSpaceConvertible[A,A] : ⦃ Pixel A ⦄ → ColorSpaceConvertible A A

    convertPixel : ⦃ ColorSpaceConvertible A B ⦄ → A → B
    convertImage : ⦃ ColorSpaceConvertible A B ⦄ → Image A → Image B

    ColorSpaceConvertible[PixelCMYK16,PixelRGB16]  : ColorSpaceConvertible PixelCMYK16 PixelRGB16
    ColorSpaceConvertible[PixelCMYK8,PixelRGB8]    : ColorSpaceConvertible PixelCMYK8 PixelRGB8
    ColorSpaceConvertible[PixelYCbCr8,PixelRGB8]   : ColorSpaceConvertible PixelYCbCr8 PixelRGB8
    ColorSpaceConvertible[PixelRGB16,PixelCMYK16]  : ColorSpaceConvertible PixelRGB16 PixelCMYK16
    ColorSpaceConvertible[PixelYCbCrK8,PixelCMYK8] : ColorSpaceConvertible PixelYCbCrK8 PixelCMYK8
    ColorSpaceConvertible[PixelYCbCrK8,PixelRGB8]  : ColorSpaceConvertible PixelYCbCrK8 PixelRGB8
    ColorSpaceConvertible[PixelRGB8,PixelCMYK8]    : ColorSpaceConvertible PixelRGB8 PixelCMYK8
    ColorSpaceConvertible[PixelRGB8,PixelYCbCr8]   : ColorSpaceConvertible PixelRGB8 PixelYCbCr8

{-# FOREIGN GHC data AgdaColorSpaceConvertible aℓ bℓ a b = Codec.Picture.Types.ColorSpaceConvertible a b => AgdaColorSpaceConvertible #-}
{-# COMPILE GHC ColorSpaceConvertible = type(0) AgdaColorSpaceConvertible #-}

{-# COMPILE GHC ColorSpaceConvertible[A,B]⇒Pixel[A] = \ aℓ bℓ a b AgdaColorSpaceConvertible -> AgdaPixel                 #-}
{-# COMPILE GHC ColorSpaceConvertible[A,B]⇒Pixel[B] = \ aℓ bℓ a b AgdaColorSpaceConvertible -> AgdaPixel                 #-}
{-# COMPILE GHC ColorSpaceConvertible[A,A]          = \ aℓ a AgdaPixel                      -> AgdaColorSpaceConvertible #-}

{-# COMPILE GHC convertPixel = \ aℓ bℓ a b AgdaColorSpaceConvertible -> Codec.Picture.Types.convertPixel #-}
{-# COMPILE GHC convertImage = \ aℓ bℓ a b AgdaColorSpaceConvertible -> Codec.Picture.Types.convertImage #-}

{-# COMPILE GHC ColorSpaceConvertible[PixelCMYK16,PixelRGB16]  = AgdaColorSpaceConvertible #-}
{-# COMPILE GHC ColorSpaceConvertible[PixelCMYK8,PixelRGB8]    = AgdaColorSpaceConvertible #-}
{-# COMPILE GHC ColorSpaceConvertible[PixelYCbCr8,PixelRGB8]   = AgdaColorSpaceConvertible #-}
{-# COMPILE GHC ColorSpaceConvertible[PixelRGB16,PixelCMYK16]  = AgdaColorSpaceConvertible #-}
{-# COMPILE GHC ColorSpaceConvertible[PixelYCbCrK8,PixelCMYK8] = AgdaColorSpaceConvertible #-}
{-# COMPILE GHC ColorSpaceConvertible[PixelYCbCrK8,PixelRGB8]  = AgdaColorSpaceConvertible #-}
{-# COMPILE GHC ColorSpaceConvertible[PixelRGB8,PixelCMYK8]    = AgdaColorSpaceConvertible #-}
{-# COMPILE GHC ColorSpaceConvertible[PixelRGB8,PixelYCbCr8]   = AgdaColorSpaceConvertible #-}


postulate
    LumaPlaneExtractable : Set aℓ → Set aℓ

    LumaPlaneExtractable[A]⇒Pixel[A]                     : ⦃ LumaPlaneExtractable A ⦄ → Pixel A
    LumaPlaneExtractable[A]⇒Pixel[PixelBaseComponent[A]] : ⦃ _ : LumaPlaneExtractable A ⦄ → Pixel (PixelBaseComponent A)

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

{-# FOREIGN GHC data AgdaLumaPlaneExtractable aℓ a = Codec.Picture.Types.LumaPlaneExtractable a => AgdaLumaPlaneExtractable #-}
{-# COMPILE GHC LumaPlaneExtractable = type(0) AgdaLumaPlaneExtractable #-}

{-# COMPILE GHC LumaPlaneExtractable[A]⇒Pixel[A]                     = \ aℓ a AgdaLumaPlaneExtractable -> AgdaPixel #-}
{-# COMPILE GHC LumaPlaneExtractable[A]⇒Pixel[PixelBaseComponent[A]] = \ aℓ a AgdaLumaPlaneExtractable -> AgdaPixel #-}

{-# COMPILE GHC computeLuma      = \ aℓ a AgdaLumaPlaneExtractable -> Codec.Picture.Types.computeLuma      #-}
{-# COMPILE GHC extractLumaPlane = \ aℓ a AgdaLumaPlaneExtractable -> Codec.Picture.Types.extractLumaPlane #-}

{-# COMPILE GHC LumaPlaneExtractable[PixelRGBA8]  = AgdaLumaPlaneExtractable #-}
{-# COMPILE GHC LumaPlaneExtractable[PixelYCbCr8] = AgdaLumaPlaneExtractable #-}
{-# COMPILE GHC LumaPlaneExtractable[PixelRGBF]   = AgdaLumaPlaneExtractable #-}
{-# COMPILE GHC LumaPlaneExtractable[PixelRGB16]  = AgdaLumaPlaneExtractable #-}
{-# COMPILE GHC LumaPlaneExtractable[PixelRGB8]   = AgdaLumaPlaneExtractable #-}
{-# COMPILE GHC LumaPlaneExtractable[PixelYA8]    = AgdaLumaPlaneExtractable #-}
{-# COMPILE GHC LumaPlaneExtractable[PixelF]      = AgdaLumaPlaneExtractable #-}
{-# COMPILE GHC LumaPlaneExtractable[Pixel32]     = AgdaLumaPlaneExtractable #-}
{-# COMPILE GHC LumaPlaneExtractable[Pixel16]     = AgdaLumaPlaneExtractable #-}
{-# COMPILE GHC LumaPlaneExtractable[Pixel8]      = AgdaLumaPlaneExtractable #-}


postulate
    TransparentPixel : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)

    TransparentPixel[A,B]⇒Pixel[A] : ⦃ TransparentPixel A B ⦄ → Pixel A
    TransparentPixel[A,B]⇒Pixel[B] : ⦃ TransparentPixel A B ⦄ → Pixel B

    dropTransparency : ⦃ TransparentPixel A B ⦄ → A → B

    TransparentPixel[PixelRGBA16,PixelRGB16] : TransparentPixel PixelRGBA16 PixelRGB16
    TransparentPixel[PixelRGBA8,PixelRGB8]   : TransparentPixel PixelRGBA8 PixelRGB8
    TransparentPixel[PixelYA16,Pixel16]      : TransparentPixel PixelYA16 Pixel16
    TransparentPixel[PixelYA8,Pixel8]        : TransparentPixel PixelYA8 Pixel8

{-# FOREIGN GHC data AgdaTransparentPixel aℓ bℓ a b = Codec.Picture.Types.TransparentPixel a b => AgdaTransparentPixel #-}
{-# COMPILE GHC TransparentPixel = type(0) AgdaTransparentPixel #-}

{-# COMPILE GHC TransparentPixel[A,B]⇒Pixel[A] = \ aℓ a bℓ b AgdaTransparentPixel -> AgdaPixel #-}
{-# COMPILE GHC TransparentPixel[A,B]⇒Pixel[B] = \ aℓ a bℓ b AgdaTransparentPixel -> AgdaPixel #-}

{-# COMPILE GHC dropTransparency = \ aℓ a bℓ b AgdaTransparentPixel -> Codec.Picture.Types.dropTransparency #-}

{-# COMPILE GHC TransparentPixel[PixelRGBA16,PixelRGB16] = AgdaTransparentPixel #-}
{-# COMPILE GHC TransparentPixel[PixelRGBA8,PixelRGB8]   = AgdaTransparentPixel #-}
{-# COMPILE GHC TransparentPixel[PixelYA16,Pixel16]      = AgdaTransparentPixel #-}
{-# COMPILE GHC TransparentPixel[PixelYA8,Pixel8]        = AgdaTransparentPixel #-}


-- Helper functions

postulate
    pixelMap            : ⦃ Pixel A ⦄ → ⦃ Pixel B ⦄ → (A → B) → Image A → Image B
    pixelMapXY          : ⦃ Pixel A ⦄ → ⦃ Pixel B ⦄ → (Int → Int → A → B) → Image A → Image B
    pixelFold           : ⦃ Pixel A ⦄ → (B → Int → Int → A → B) → B → Image A → B
    pixelFoldM          : ⦃ Pixel A ⦄ → ⦃ Monad M ⦄ → (B → Int → Int → A → M B) → B → Image A → M B
    pixelFoldMap        : ⦃ Pixel A ⦄ → ⦃ Monoid B ⦄ → (A → B) → Image A → B
    dynamicMap          : (∀{aℓ}{A : Set aℓ} → ⦃ Pixel A ⦄ → Image A → B) → DynamicImage → B
    dynamicPixelMap     : (∀{aℓ}{A : Set aℓ} → ⦃ Pixel A ⦄ → Image A → Image A) → DynamicImage → DynamicImage
    palettedToTrueColor : PalettedImage → DynamicImage
    palettedAsImage     : Palette' A → Image A
    dropAlphaLayer      : ⦃ TransparentPixel A B ⦄ → Image A → Image B
    withImage           : ⦃ Pixel A ⦄ → ⦃ PrimMonad M ⦄ → Int → Int → (Int → Int → M A) → M (Image A)
    zipPixelComponent3  : ⦃ _ : Pixel A ⦄ → (PixelBaseComponent A → PixelBaseComponent A → PixelBaseComponent A → PixelBaseComponent A) → Image A → Image A → Image A → Image A
    generateImage       : ⦃ Pixel A ⦄ → (Int → Int → A) → Int → Int → Image A
    generateFoldImage   : ⦃ Pixel A ⦄ → (B → Int → Int → Tuple2 B A) → B → Int → Int → Tuple2 B (Image A)
    gammaCorrection     : PixelF → Image PixelRGBF → Image PixelRGBF
    toneMapping         : PixelF → Image PixelRGBF → Image PixelRGBF

{-# COMPILE GHC pixelMap            = \ aℓ a bℓ b AgdaPixel AgdaPixel   -> Codec.Picture.Types.pixelMap                            #-}
{-# COMPILE GHC pixelMapXY          = \ aℓ a bℓ b AgdaPixel AgdaPixel   -> Codec.Picture.Types.pixelMapXY                          #-}
{-# COMPILE GHC pixelFold           = \ aℓ a bℓ b AgdaPixel             -> Codec.Picture.Types.pixelFold                           #-}
{-# COMPILE GHC pixelFoldM          = \ aℓ a mℓ m b AgdaPixel AgdaMonad -> Codec.Picture.Types.pixelFoldM                          #-}
{-# COMPILE GHC pixelFoldMap        = \ aℓ a bℓ b AgdaPixel AgdaMonoid  -> Codec.Picture.Types.pixelFoldMap                        #-}
{-# COMPILE GHC dynamicMap          = \ bℓ b f                          -> Codec.Picture.Types.dynamicMap (f () () AgdaPixel)      #-}
{-# COMPILE GHC dynamicPixelMap     = \ f                               -> Codec.Picture.Types.dynamicPixelMap (f () () AgdaPixel) #-}
{-# COMPILE GHC palettedToTrueColor =                                      Codec.Picture.Types.palettedToTrueColor                 #-}
{-# COMPILE GHC palettedAsImage     = \ aℓ a                            -> Codec.Picture.Types.palettedAsImage                     #-}
{-# COMPILE GHC dropAlphaLayer      = \ aℓ a bℓ b AgdaTransparentPixel  -> Codec.Picture.Types.dropAlphaLayer                      #-}
{-# COMPILE GHC withImage           = \ aℓ a m AgdaPixel AgdaPrimMonad  -> Codec.Picture.Types.withImage                           #-}
{-# COMPILE GHC zipPixelComponent3  = \ aℓ a AgdaPixel                  -> Codec.Picture.Types.zipPixelComponent3                  #-}
{-# COMPILE GHC generateImage       = \ aℓ a AgdaPixel                  -> Codec.Picture.Types.generateImage                       #-}
{-# COMPILE GHC generateFoldImage   = \ aℓ a bℓ b AgdaPixel             -> Codec.Picture.Types.generateFoldImage                   #-}
{-# COMPILE GHC gammaCorrection     =                                      Codec.Picture.Types.gammaCorrection                     #-}
{-# COMPILE GHC toneMapping         =                                      Codec.Picture.Types.toneMapping                         #-}


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

{-# FOREIGN GHC data AgdaColorPlane aℓ a p = Codec.Picture.Types.ColorPlane a p => AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane = type(0) AgdaColorPlane #-}

{-# COMPILE GHC ColorPlane[PixelRGBA16,PlaneAlpha]   = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGBA16,PlaneBlue]    = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGBA16,PlaneGreen]   = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGBA16,PlaneRed]     = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGBA8,PlaneAlpha]    = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGBA8,PlaneBlue]     = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGBA8,PlaneGreen]    = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGBA8,PlaneRed]      = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelCMYK16,PlaneBlack]   = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelCMYK16,PlaneYellow]  = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelCMYK16,PlaneMagenta] = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelCMYK16,PlaneCyan]    = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelCMYK8,PlaneBlack]    = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelCMYK8,PlaneYellow]   = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelCMYK8,PlaneMagenta]  = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelCMYK8,PlaneCyan]     = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelYCbCr8,PlaneCb]      = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelYCbCr8,PlaneCr]      = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelYCbCr8,PlaneLuma]    = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGBF,PlaneBlue]      = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGBF,PlaneGreen]     = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGBF,PlaneRed]       = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGB16,PlaneBlue]     = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGB16,PlaneGreen]    = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGB16,PlaneRed]      = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGB8,PlaneBlue]      = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGB8,PlaneGreen]     = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelRGB8,PlaneRed]       = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelYA16,PlaneLuma]      = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelYA16,PlaneAlpha]     = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelYA8,PlaneLuma]       = AgdaColorPlane #-}
{-# COMPILE GHC ColorPlane[PixelYA8,PlaneAlpha]      = AgdaColorPlane #-}


-- todo: kinds
-- postulate
--     extractComponent       : ∀{Plane} → ⦃ _ : Pixel A ⦄ → ⦃ _ : Pixel (PixelBaseComponent A) ⦄ → ⦃ PixelBaseComponent (PixelBaseComponent A) ~ PixelBaseComponent A ⦄ → ⦃ ColorPlane A Plane ⦄ → Plane → Image A → Image (PixelBaseComponent A)
--     unsafeExtractComponent : ⦃ _ : Pixel A ⦄ → ⦃ _ : Pixel (PixelBaseComponent A) ⦄ → ⦃ PixelBaseComponent (PixelBaseComponent A) ~ PixelBaseComponent A ⦄ → Int → Image A → Image (PixelBaseComponent A)

-- {-# COMPILE GHC extractComponent       = \ aℓ a plane AgdaPixel AgdaPixel AgdaTypeEq AgdaColorPlane -> Codec.Picture.Types.extractComponent       #-}
-- {-# COMPILE GHC unsafeExtractComponent = \ aℓ a AgdaPixel AgdaPixel AgdaTypeEq                      -> Codec.Picture.Types.unsafeExtractComponent #-}

-- todo: packeable writing

-- Image Types

record Image A where
    constructor mkImage
    field
        imageWidth  : Int
        imageHeight : Int
        imageData   : Vector (PixelBaseComponent A)

{-# FOREIGN GHC type AgdaImage aℓ = Codec.Picture.Types.Image #-}
{-# COMPILE GHC Image = data(1) AgdaImage (Codec.Picture.Types.Image) #-}

postulate
    Eq[Image[A]]     : ⦃ Eq (PixelBaseComponent A) ⦄ → ⦃ Storable (PixelBaseComponent A) ⦄ → Eq (Image A)
    NFData[Image[A]] : NFData (Image A)

{-# COMPILE GHC Eq[Image[A]]     = \ aℓ a AgdaEq AgdaStorable -> AgdaEq     #-}
{-# COMPILE GHC NFData[Image[A]] = \ aℓ a                     -> AgdaNFData #-}


record MutableImage S A where
    constructor mkMutableImage
    field
        mutableImageWidth  : Int
        mutableImageHeight : Int
        mutableImageData   : STVector S (PixelBaseComponent A)

{-# FOREIGN GHC type AgdaMutableImage aℓ = Codec.Picture.Types.MutableImage #-}
{-# COMPILE GHC MutableImage = data(1) AgdaMutableImage (Codec.Picture.Types.MutableImage) #-}

postulate
    NFData[MutableImage[S,A]] : NFData (MutableImage S A)

{-# COMPILE GHC NFData[MutableImage[S,A]] = \ s aℓ a -> AgdaNFData #-}


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

{-# COMPILE GHC DynamicImage = data Codec.Picture.Types.DynamicImage
    ( Codec.Picture.Types.ImageY8
    | Codec.Picture.Types.ImageY16
    | Codec.Picture.Types.ImageY32
    | Codec.Picture.Types.ImageYF
    | Codec.Picture.Types.ImageYA8
    | Codec.Picture.Types.ImageYA16
    | Codec.Picture.Types.ImageRGB8
    | Codec.Picture.Types.ImageRGB16
    | Codec.Picture.Types.ImageRGBF
    | Codec.Picture.Types.ImageRGBA8
    | Codec.Picture.Types.ImageRGBA16
    | Codec.Picture.Types.ImageYCbCr8
    | Codec.Picture.Types.ImageCMYK8
    | Codec.Picture.Types.ImageCMYK16
    ) #-}

postulate
    Eq[DynamicImage]     : Eq DynamicImage
    NFData[DynamicImage] : NFData DynamicImage

{-# COMPILE GHC Eq[DynamicImage]     = AgdaEq     #-}
{-# COMPILE GHC NFData[DynamicImage] = AgdaNFData #-}


Palette : Set
Palette = Image PixelRGB8


record Palette' A where
    constructor mkPalette'
    field
        _paletteSize : Int
        _paletteData : Vector (PixelBaseComponent A)

{-# FOREIGN GHC type AgdaPalette' aℓ = Codec.Picture.Types.Palette' #-}
{-# COMPILE GHC Palette' = data(1) AgdaPalette' (Codec.Picture.Types.Palette') #-}


data PalettedImage where
    TrueColorImage : DynamicImage → PalettedImage
    PalettedY8     : Image Pixel8 → Palette' Pixel8 → PalettedImage
    PalettedRGB8   : Image Pixel8 → Palette' PixelRGB8 → PalettedImage
    PalettedRGBA8  : Image Pixel8 → Palette' PixelRGBA8 → PalettedImage
    PalettedRGB16  : Image Pixel8 → Palette' PixelRGB16 → PalettedImage

{-# COMPILE GHC PalettedImage = data Codec.Picture.Types.PalettedImage
    ( Codec.Picture.Types.TrueColorImage
    | Codec.Picture.Types.PalettedY8
    | Codec.Picture.Types.PalettedRGB8
    | Codec.Picture.Types.PalettedRGBA8
    | Codec.Picture.Types.PalettedRGB16
    ) #-}


-- Image functions
postulate
    createMutableImage : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → Int → Int → A → M (MutableImage (PrimState M) A)
    newMutableImage    : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → Int → Int → M (MutableImage (PrimState M) A)
    freezeImage        : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → MutableImage (PrimState M) A → M (Image A)
    unsafeFreezeImage  : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → MutableImage (PrimState M) A → M (Image A)
    thawImage          : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → Image A → M (MutableImage (PrimState M) A)
    unsafeThawImage    : ⦃ Pixel A ⦄ → ⦃ _ : PrimMonad M ⦄ → Image A → M (MutableImage (PrimState M) A)

{-# COMPILE GHC createMutableImage = \ aℓ a m AgdaPixel AgdaPrimMonad -> Codec.Picture.Types.createMutableImage #-}
{-# COMPILE GHC newMutableImage    = \ aℓ a m AgdaPixel AgdaPrimMonad -> Codec.Picture.Types.newMutableImage    #-}
{-# COMPILE GHC freezeImage        = \ aℓ a m AgdaPixel AgdaPrimMonad -> Codec.Picture.Types.freezeImage        #-}
{-# COMPILE GHC unsafeFreezeImage  = \ aℓ a m AgdaPixel AgdaPrimMonad -> Codec.Picture.Types.unsafeFreezeImage  #-}
{-# COMPILE GHC thawImage          = \ aℓ a m AgdaPixel AgdaPrimMonad -> Codec.Picture.Types.thawImage          #-}
{-# COMPILE GHC unsafeThawImage    = \ aℓ a m AgdaPixel AgdaPrimMonad -> Codec.Picture.Types.unsafeThawImage    #-}
