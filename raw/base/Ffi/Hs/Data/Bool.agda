{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Bool where

open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Kind using (IsKind)

open import Agda.Builtin.Bool public
    using (Bool) renaming (true to True; false to False)

{-# FOREIGN GHC {-# LANGUAGE DataKinds #-} #-}
{-# FOREIGN GHC
import qualified Data.Bool
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Kind (AgdaIsKind(AgdaIsKind))
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

infixr 3 _&&_
infixr 2 _||_

_&&_ : Bool → Bool → Bool
True  && x = x
False && x = False

_||_ : Bool → Bool → Bool
True  || x = True
False || x = x

bool : ∀{aℓ} {A : Set aℓ} → A → A → Bool → A
bool x y False = x
bool x y True  = y

not : Bool → Bool
not True  = False
not False = True

otherwise = True

postulate
    Data[Bool]       : Data Bool
    Storable[Bool]   : Storable Bool
    Bits[Bool]       : Bits Bool
    FiniteBits[Bool] : FiniteBits Bool
    Bounded[Bool]    : Bounded Bool
    Enum[Bool]       : Enum Bool
    Ix[Bool]         : Ix Bool
    Read[Bool]       : Read Bool
    Show[Bool]       : Show Bool
    Eq[Bool]         : Eq Bool
    Ord[Bool]        : Ord Bool

{-# COMPILE GHC Data[Bool]       = AgdaData       #-}
{-# COMPILE GHC Storable[Bool]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[Bool]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Bool] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Bool]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[Bool]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[Bool]         = AgdaIx         #-}
{-# COMPILE GHC Read[Bool]       = AgdaRead       #-}
{-# COMPILE GHC Show[Bool]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Bool]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Bool]        = AgdaOrd        #-}

postulate
    `Bool  : Set₁
    `True  : `Bool
    `False : `Bool

{-# COMPILE GHC `Bool  = type Data.Bool.Bool #-}
{-# COMPILE GHC `True  = type 'True          #-}
{-# COMPILE GHC `False = type 'False         #-}

lift`Bool : Bool → `Bool
lift`Bool True  = `True
lift`Bool False = `False

postulate
    IsKind[`Bool] : IsKind `Bool

{-# COMPILE GHC IsKind[`Bool] = AgdaIsKind #-}
