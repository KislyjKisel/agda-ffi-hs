{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Bool where

open import Agda.Builtin.Bool public
    using    (Bool)
    renaming (true to True; false to False)

infixr 6 _&&_
infixr 5 _||_

postulate
    _&&_      : Bool → Bool → Bool
    _||_      : Bool → Bool → Bool
    not       : Bool → Bool
    otherwise : Bool
    bool      : ∀{aℓ} {A : Set aℓ} → A → A → Bool → A

{-# FOREIGN GHC import qualified Data.Bool #-}

{-# COMPILE GHC _&&_      = (Data.Bool.&&)          #-}
{-# COMPILE GHC _||_      = (Data.Bool.||)          #-}
{-# COMPILE GHC not       = Data.Bool.not           #-}
{-# COMPILE GHC otherwise = Data.Bool.otherwise     #-}
{-# COMPILE GHC bool      = \ ℓ a -> Data.Bool.bool #-}
