{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.Marshal.Utils where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.List  using (List)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Storable; Num; Eq)
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Word   using (Word8)
open import Ffi.Hs.Foreign.Ptr using (Ptr)

{-# FOREIGN GHC
import qualified Foreign.Marshal.Utils
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaStorable, AgdaNum, AgdaEq
    )
#-}

private
    variable
        aℓ : Level
        A B C : Set aℓ

postulate
    with' : ⦃ Storable A ⦄ → A → (Ptr A → IO B) → IO B
    new : ⦃ Storable A ⦄ → A → IO (Ptr A)

    fromBool : ⦃ Num A ⦄ → Bool → A
    toBool : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A → Bool

    maybeNew  : (A → IO (Ptr B)) → Maybe A → IO (Ptr A)
    maybeWith : (A → (Ptr B → IO C) → IO C) → Maybe A → (Ptr B → IO C) → IO C
    maybePeek : (Ptr A → IO B) → Ptr A → IO (Maybe B)

    withMany : (A → (B → C) → C) → List A → (List B → C) → C

    copyBytes : Ptr A → Ptr A → Int → IO (⊤ {lzero})
    moveBytes : Ptr A → Ptr A → Int → IO (⊤ {lzero})
    fillBytes : Ptr A → Word8 → Int → IO (⊤ {lzero})

{-# COMPILE GHC with' = \ aℓ a bℓ b AgdaStorable -> Foreign.Marshal.Utils.with #-}
{-# COMPILE GHC new   = \ aℓ a      AgdaStorable -> Foreign.Marshal.Utils.new  #-}

{-# COMPILE GHC fromBool = \ aℓ a AgdaNum        -> Foreign.Marshal.Utils.fromBool #-}
{-# COMPILE GHC toBool   = \ aℓ a AgdaEq AgdaNum -> Foreign.Marshal.Utils.toBool   #-}

{-# COMPILE GHC maybeNew  = \ aℓ a bℓ b      -> Foreign.Marshal.Utils.maybeNew  #-}
{-# COMPILE GHC maybeWith = \ aℓ a bℓ b cℓ c -> Foreign.Marshal.Utils.maybeWith #-}
{-# COMPILE GHC maybePeek = \ aℓ a bℓ b      -> Foreign.Marshal.Utils.maybePeek #-}

{-# COMPILE GHC withMany = \ aℓ a bℓ b cℓ c -> Foreign.Marshal.Utils.withMany #-}

{-# COMPILE GHC copyBytes = \ aℓ a -> Foreign.Marshal.Utils.copyBytes #-}
{-# COMPILE GHC moveBytes = \ aℓ a -> Foreign.Marshal.Utils.moveBytes #-}
{-# COMPILE GHC fillBytes = \ aℓ a -> Foreign.Marshal.Utils.fillBytes #-}
