{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Vector.Storable.Mutable where

open import Agda.Builtin.Bool              using (Bool)
open import Agda.Builtin.IO                using (IO)
open import Agda.Builtin.Maybe             using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class             using (Storable; Ord; Coercible)
open import Ffi.Hs.-base.Unit              using (⊤; ⊤′)
open import Ffi.Hs.Control.DeepSeq         using (NFData; NFData1)
open import Ffi.Hs.Control.Monad.Primitive using (PrimMonad; PrimState)
open import Ffi.Hs.Data.Int                using (Int)
open import Ffi.Hs.Data.Tuple              using (Tuple2; Tuple3)
open import Ffi.Hs.Foreign.ForeignPtr      using (ForeignPtr)
open import Ffi.Hs.Foreign.Ptr             using (Ptr)
open import Ffi.Hs.GHC.Exts                using (RealWorld)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Vector.Storable.Mutable
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.DeepSeq (AgdaNFData(AgdaNFData), AgdaNFData1(AgdaNFData1))
import MAlonzo.Code.Ffi.Hs.Control.Monad.Primitive (AgdaPrimMonad(AgdaPrimMonad))
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ
        M : Set aℓ → Set aℓ
        S : Set

data MVector (S : Set) (A : Set aℓ) : Set aℓ where
    mkMVector : Int → ForeignPtr A → MVector S A

{-# FOREIGN GHC type AgdaMVector aℓ = Data.Vector.Storable.Mutable.MVector #-}
{-# COMPILE GHC MVector = data(1) AgdaMVector (Data.Vector.Storable.Mutable.MVector) #-}

postulate
    NFData1[MVector[S]]  : NFData1 (MVector {aℓ} S)
    NFData[MVector[S,A]] : NFData (MVector S A)

{-# COMPILE GHC NFData1[MVector[S]]  = \ aℓ s   -> AgdaNFData1 #-}
{-# COMPILE GHC NFData[MVector[S,A]] = \ s aℓ a -> AgdaNFData  #-}

IOVector : Set aℓ → Set aℓ
IOVector = MVector RealWorld

STVector : Set → Set aℓ → Set aℓ
STVector = MVector

postulate
    length                : ⦃ Storable A ⦄ → MVector S A → Int
    null                  : ⦃ Storable A ⦄ → MVector S A → Bool
    slice                 : ⦃ Storable A ⦄ → Int → Int → MVector S A → MVector S A
    init                  : ⦃ Storable A ⦄ → MVector S A → MVector S A
    tail                  : ⦃ Storable A ⦄ → MVector S A → MVector S A
    take                  : ⦃ Storable A ⦄ → Int → MVector S A → MVector S A
    drop                  : ⦃ Storable A ⦄ → Int → MVector S A → MVector S A
    splitAt               : ⦃ Storable A ⦄ → Int → MVector S A → Tuple2 (MVector S A) (MVector S A)
    unsafeSlice           : ⦃ Storable A ⦄ → Int → Int → MVector S A → MVector S A
    unsafeInit            : ⦃ Storable A ⦄ → MVector S A → MVector S A
    unsafeTail            : ⦃ Storable A ⦄ → MVector S A → MVector S A
    unsafeTake            : ⦃ Storable A ⦄ → Int → MVector S A → MVector S A
    unsafeDrop            : ⦃ Storable A ⦄ → Int → MVector S A → MVector S A
    overlaps              : ⦃ Storable A ⦄ → MVector S A → MVector S A → Bool
    new                   : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → Int → M (MVector (PrimState M) A)
    unsafeNew             : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → Int → M (MVector (PrimState M) A)
    replicate             : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → Int → A → M (MVector (PrimState M) A)
    replicateM            : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → Int → M A → M (MVector (PrimState M) A)
    generate              : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → Int → (Int → A) → M (MVector (PrimState M) A)
    generateM             : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → Int → (Int → M A) → M (MVector (PrimState M) A)
    clone                 : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → M (MVector (PrimState M) A)
    grow                  : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → Int → M (MVector (PrimState M) A)
    unsafeGrow            : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → Int → M (MVector (PrimState M) A)
    clear                 : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → M ⊤′
    read                  : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → Int → M A
    -- todo: (req v13.0.0, incompat w/ sdl2) readMaybe             : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → Int → M (Maybe A)
    write                 : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → Int → A → M ⊤′
    modify                : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → (A → A) → Int → M ⊤′
    modifyM               : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → (A → M A) → Int → M ⊤′
    swap                  : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → Int → Int → M ⊤′
    exchange              : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → Int → A → M A
    unsafeRead            : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → Int → M A
    unsafeWrite           : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → Int → A → M ⊤′
    unsafeModify          : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → (A → A) → Int → M ⊤′
    unsafeModifyM         : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → (A → M A) → Int → M ⊤′
    unsafeSwap            : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → Int → Int → M ⊤′
    unsafeExchange        : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → Int → A → M A
    mapM-                 : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (A → M B) → MVector (PrimState M) A → M ⊤′
    imapM-                : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (Int → A → M B) → MVector (PrimState M) A → M ⊤′
    forM-                 : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → (A → M B) → M ⊤′
    iforM-                : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → (Int → A → M B) → M ⊤′
    foldl                 : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (B → A → B) → B → MVector (PrimState M) A → M B
    foldl'                : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (B → A → B) → B → MVector (PrimState M) A → M B
    foldM                 : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (B → A → M B) → B → MVector (PrimState M) A → M B
    foldM'                : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (B → A → M B) → B → MVector (PrimState M) A → M B
    foldr                 : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (A → B → B) → B → MVector (PrimState M) A → M B
    foldr'                : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (A → B → B) → B → MVector (PrimState M) A → M B
    foldrM                : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (A → B → M B) → B → MVector (PrimState M) A → M B
    foldrM'               : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (A → B → M B) → B → MVector (PrimState M) A → M B
    ifoldl                : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (B → Int → A → B) → B → MVector (PrimState M) A → M B
    ifoldl'               : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (B → Int → A → B) → B → MVector (PrimState M) A → M B
    ifoldM                : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (B → Int → A → M B) → B → MVector (PrimState M) A → M B
    ifoldM'               : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (B → Int → A → M B) → B → MVector (PrimState M) A → M B
    ifoldr                : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (Int → A → B → B) → B → MVector (PrimState M) A → M B
    ifoldr'               : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (Int → A → B → B) → B → MVector (PrimState M) A → M B
    ifoldrM               : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (Int → A → B → M B) → B → MVector (PrimState M) A → M B
    ifoldrM'              : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → (Int → A → B → M B) → B → MVector (PrimState M) A → M B
    nextPermutation       : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → ⦃ Ord A ⦄ → MVector (PrimState M) A → M Bool
    set                   : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → A → M ⊤′
    copy                  : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → MVector (PrimState M) A → M ⊤′
    move                  : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → MVector (PrimState M) A → M ⊤′
    unsafeCopy            : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → MVector (PrimState M) A → M ⊤′
    unsafeMove            : ⦃ _ : PrimMonad M ⦄ → ⦃ Storable A ⦄ → MVector (PrimState M) A → MVector (PrimState M) A → M ⊤′
    unsafeCast            : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → MVector S A → MVector S B
    -- todo: (req v13.0.0, incompat w/ sdl2) unsafeCoerceMVector   : ⦃ Coercible A B ⦄ → MVector S A → MVector S B
    unsafeFromForeignPtr  : ⦃ Storable A ⦄ → ForeignPtr A → Int → Int → MVector S A
    -- todo: (v13/or Storable) unsafeFromForeignPtr0 : ForeignPtr A → Int → MVector S A
    -- todo: (v13/or Storable) unsafeToForeignPtr    : MVector S A → Tuple3 (ForeignPtr A) Int Int
    -- todo: (v13/or Storable) unsafeToForeignPtr0   : MVector S A → Tuple2 (ForeignPtr A) Int
    unsafeWith            : ⦃ Storable A ⦄ → IOVector A → (Ptr A → IO B) → IO B

{-# COMPILE GHC length                = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.length                #-}
{-# COMPILE GHC null                  = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.null                  #-}
{-# COMPILE GHC slice                 = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.slice                 #-}
{-# COMPILE GHC init                  = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.init                  #-}
{-# COMPILE GHC tail                  = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.tail                  #-}
{-# COMPILE GHC take                  = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.take                  #-}
{-# COMPILE GHC drop                  = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.drop                  #-}
{-# COMPILE GHC splitAt               = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.splitAt               #-}
{-# COMPILE GHC unsafeSlice           = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.unsafeSlice           #-}
{-# COMPILE GHC unsafeInit            = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.unsafeInit            #-}
{-# COMPILE GHC unsafeTail            = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.unsafeTail            #-}
{-# COMPILE GHC unsafeTake            = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.unsafeTake            #-}
{-# COMPILE GHC unsafeDrop            = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.unsafeDrop            #-}
{-# COMPILE GHC overlaps              = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.overlaps              #-}
{-# COMPILE GHC new                   = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.new                   #-}
{-# COMPILE GHC unsafeNew             = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.unsafeNew             #-}
{-# COMPILE GHC replicate             = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.replicate             #-}
{-# COMPILE GHC replicateM            = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.replicateM            #-}
{-# COMPILE GHC generate              = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.generate              #-}
{-# COMPILE GHC generateM             = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.generateM             #-}
{-# COMPILE GHC clone                 = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.clone                 #-}
{-# COMPILE GHC grow                  = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.grow                  #-}
{-# COMPILE GHC unsafeGrow            = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.unsafeGrow            #-}
{-# COMPILE GHC clear                 = \ mℓ m aℓ a AgdaPrimMonad AgdaStorable      -> Data.Vector.Storable.Mutable.clear                 #-}
{-# COMPILE GHC read                  = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.read                  #-}
-- {-# COMPILE GHC readMaybe             = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.readMaybe             #-}
{-# COMPILE GHC write                 = \ mℓ m aℓ a AgdaPrimMonad AgdaStorable      -> Data.Vector.Storable.Mutable.write                 #-}
{-# COMPILE GHC modify                = \ mℓ m aℓ a AgdaPrimMonad AgdaStorable      -> Data.Vector.Storable.Mutable.modify                #-}
{-# COMPILE GHC modifyM               = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.modifyM               #-}
{-# COMPILE GHC swap                  = \ mℓ m aℓ a AgdaPrimMonad AgdaStorable      -> Data.Vector.Storable.Mutable.swap                  #-}
{-# COMPILE GHC exchange              = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.exchange              #-}
{-# COMPILE GHC unsafeRead            = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.unsafeRead            #-}
{-# COMPILE GHC unsafeWrite           = \ mℓ m aℓ a AgdaPrimMonad AgdaStorable      -> Data.Vector.Storable.Mutable.unsafeWrite           #-}
{-# COMPILE GHC unsafeModify          = \ mℓ m aℓ a AgdaPrimMonad AgdaStorable      -> Data.Vector.Storable.Mutable.unsafeModify          #-}
{-# COMPILE GHC unsafeModifyM         = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.unsafeModifyM         #-}
{-# COMPILE GHC unsafeSwap            = \ mℓ m aℓ a AgdaPrimMonad AgdaStorable      -> Data.Vector.Storable.Mutable.unsafeSwap            #-}
{-# COMPILE GHC unsafeExchange        = \ mℓ m a AgdaPrimMonad AgdaStorable         -> Data.Vector.Storable.Mutable.unsafeExchange        #-}
{-# COMPILE GHC mapM-                 = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.mapM_                 #-}
{-# COMPILE GHC imapM-                = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.imapM_                #-}
{-# COMPILE GHC forM-                 = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.forM_                 #-}
{-# COMPILE GHC iforM-                = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.iforM_                #-}
{-# COMPILE GHC foldl                 = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.foldl                 #-}
{-# COMPILE GHC foldl'                = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.foldl'                #-}
{-# COMPILE GHC foldM                 = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.foldM                 #-}
{-# COMPILE GHC foldM'                = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.foldM'                #-}
{-# COMPILE GHC foldr                 = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.foldr                 #-}
{-# COMPILE GHC foldr'                = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.foldr'                #-}
{-# COMPILE GHC foldrM                = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.foldrM                #-}
{-# COMPILE GHC foldrM'               = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.foldrM'               #-}
{-# COMPILE GHC ifoldl                = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.ifoldl                #-}
{-# COMPILE GHC ifoldl'               = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.ifoldl'               #-}
{-# COMPILE GHC ifoldM                = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.ifoldM                #-}
{-# COMPILE GHC ifoldM'               = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.ifoldM'               #-}
{-# COMPILE GHC ifoldr                = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.ifoldr                #-}
{-# COMPILE GHC ifoldr'               = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.ifoldr'               #-}
{-# COMPILE GHC ifoldrM               = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.ifoldrM               #-}
{-# COMPILE GHC ifoldrM'              = \ mℓ m aℓ a b AgdaPrimMonad AgdaStorable    -> Data.Vector.Storable.Mutable.ifoldrM'              #-}
{-# COMPILE GHC nextPermutation       = \ m aℓ a AgdaPrimMonad AgdaStorable AgdaOrd -> Data.Vector.Storable.Mutable.nextPermutation       #-}
{-# COMPILE GHC set                   = \ mℓ m aℓ a AgdaPrimMonad AgdaStorable      -> Data.Vector.Storable.Mutable.set                   #-}
{-# COMPILE GHC copy                  = \ mℓ m aℓ a AgdaPrimMonad AgdaStorable      -> Data.Vector.Storable.Mutable.copy                  #-}
{-# COMPILE GHC move                  = \ mℓ m aℓ a AgdaPrimMonad AgdaStorable      -> Data.Vector.Storable.Mutable.move                  #-}
{-# COMPILE GHC unsafeCopy            = \ mℓ m aℓ a AgdaPrimMonad AgdaStorable      -> Data.Vector.Storable.Mutable.unsafeCopy            #-}
{-# COMPILE GHC unsafeMove            = \ mℓ m aℓ a AgdaPrimMonad AgdaStorable      -> Data.Vector.Storable.Mutable.unsafeMove            #-}
{-# COMPILE GHC unsafeCast            = \ aℓ a bℓ b s AgdaStorable AgdaStorable     -> Data.Vector.Storable.Mutable.unsafeCast            #-}
-- {-# COMPILE GHC unsafeCoerceMVector   = \ aℓ a bℓ b s AgdaCoercible                 -> Data.Vector.Storable.Mutable.unsafeCoerceMVector   #-}
{-# COMPILE GHC unsafeFromForeignPtr  = \ aℓ a s AgdaStorable                       -> Data.Vector.Storable.Mutable.unsafeFromForeignPtr  #-}
-- {-# COMPILE GHC unsafeFromForeignPtr0 = \ aℓ a s                                    -> Data.Vector.Storable.Mutable.unsafeFromForeignPtr0 #-}
-- {-# COMPILE GHC unsafeToForeignPtr    = \ aℓ a s                                    -> Data.Vector.Storable.Mutable.unsafeToForeignPtr    #-}
-- {-# COMPILE GHC unsafeToForeignPtr0   = \ aℓ a s                                    -> Data.Vector.Storable.Mutable.unsafeToForeignPtr0   #-}
{-# COMPILE GHC unsafeWith            = \ aℓ a bℓ b AgdaStorable                    -> Data.Vector.Storable.Mutable.unsafeWith            #-}
