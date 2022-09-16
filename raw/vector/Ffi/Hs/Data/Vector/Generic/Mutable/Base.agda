{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Vector.Generic.Mutable.Base where

open import Agda.Builtin.Bool       using (Bool)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit       using (⊤)
open import Ffi.Hs.Control.Monad.ST using (ST)
open import Ffi.Hs.Data.Int         using (Int)

{-# FOREIGN GHC
import qualified Data.Vector.Generic.Mutable.Base
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        V : Set → Set aℓ → Set aℓ
        S : Set

postulate
    MVector : (Set → Set aℓ → Set aℓ) → Set aℓ → Set aℓ
    basicLength          : ⦃ MVector V A ⦄ → V S A → Int
    basicUnsafeSlice     : ⦃ MVector V A ⦄ → Int → Int → V S A → V S A
    basicOverlaps        : ⦃ MVector V A ⦄ → V S A → V S A → Bool
    basicUnsafeNew       : ⦃ MVector V A ⦄ → Int → ST S (V S A)
    basicInitialize      : ⦃ MVector V A ⦄ → V S A → ST S (⊤ {lzero})
    basicUnsafeReplicate : ⦃ MVector V A ⦄ → Int → A → ST S (V S A)
    basicUnsafeRead      : ⦃ MVector V A ⦄ → V S A → Int → ST S A
    basicUnsafeWrite     : ⦃ MVector V A ⦄ → V S A → Int → A → ST S (⊤ {lzero})
    basicClear           : ⦃ MVector V A ⦄ → V S A → ST S (⊤ {lzero})
    basicSet             : ⦃ MVector V A ⦄ → V S A → A → ST S (⊤ {lzero})
    basicUnsafeCopy      : ⦃ MVector V A ⦄ → V S A → V S A → ST S (⊤ {lzero})
    basicUnsafeMove      : ⦃ MVector V A ⦄ → V S A → V S A → ST S (⊤ {lzero})
    basicUnsafeGrow      : ⦃ MVector V A ⦄ → V S A → Int → ST S (V S A)

{-# FOREIGN GHC data AgdaMVector aℓ v a = Data.Vector.Generic.Mutable.Base.MVector v a => AgdaMVector #-}
{-# COMPILE GHC MVector = type(0) AgdaMVector #-}

{-# COMPILE GHC basicLength          = \ aℓ v a s AgdaMVector -> Data.Vector.Generic.Mutable.Base.basicLength          #-}
{-# COMPILE GHC basicUnsafeSlice     = \ aℓ v a s AgdaMVector -> Data.Vector.Generic.Mutable.Base.basicUnsafeSlice     #-}
{-# COMPILE GHC basicOverlaps        = \ aℓ v a s AgdaMVector -> Data.Vector.Generic.Mutable.Base.basicOverlaps        #-}
{-# COMPILE GHC basicUnsafeNew       = \ aℓ v a s AgdaMVector -> Data.Vector.Generic.Mutable.Base.basicUnsafeNew       #-}
{-# COMPILE GHC basicInitialize      = \ aℓ v a s AgdaMVector -> Data.Vector.Generic.Mutable.Base.basicInitialize      #-}
{-# COMPILE GHC basicUnsafeReplicate = \ aℓ v a s AgdaMVector -> Data.Vector.Generic.Mutable.Base.basicUnsafeReplicate #-}
{-# COMPILE GHC basicUnsafeRead      = \ aℓ v a s AgdaMVector -> Data.Vector.Generic.Mutable.Base.basicUnsafeRead      #-}
{-# COMPILE GHC basicUnsafeWrite     = \ aℓ v a s AgdaMVector -> Data.Vector.Generic.Mutable.Base.basicUnsafeWrite     #-}
{-# COMPILE GHC basicClear           = \ aℓ v a s AgdaMVector -> Data.Vector.Generic.Mutable.Base.basicClear           #-}
{-# COMPILE GHC basicSet             = \ aℓ v a s AgdaMVector -> Data.Vector.Generic.Mutable.Base.basicSet             #-}
{-# COMPILE GHC basicUnsafeCopy      = \ aℓ v a s AgdaMVector -> Data.Vector.Generic.Mutable.Base.basicUnsafeCopy      #-}
{-# COMPILE GHC basicUnsafeMove      = \ aℓ v a s AgdaMVector -> Data.Vector.Generic.Mutable.Base.basicUnsafeMove      #-}
{-# COMPILE GHC basicUnsafeGrow      = \ aℓ v a s AgdaMVector -> Data.Vector.Generic.Mutable.Base.basicUnsafeGrow      #-}
