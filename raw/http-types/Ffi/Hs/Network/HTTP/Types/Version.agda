{-# OPTIONS --without-K #-}

module Ffi.Hs.Network.HTTP.Types.Version where

open import Agda.Builtin.Unit     using () -- instanced constructor
open import Ffi.Hs.-base.Class    using (Eq; Ord; Show)
open import Ffi.Hs.-base.Literals
open import Ffi.Hs.Data.Int       using (Int; Lit-FromNat[Int])

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Network.HTTP.Types.Version
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    instance
        _ = Lit-FromNat[Int]


record HttpVersion : Set where
    constructor mkHttpVersion
    field
        httpMajor : Int
        httpMinor : Int

{-# COMPILE GHC HttpVersion = data Network.HTTP.Types.Version.HttpVersion
    ( Network.HTTP.Types.Version.HttpVersion
    ) #-}

postulate
    Eq[HttpVersion]   : Eq HttpVersion
    Ord[HttpVersion]  : Ord HttpVersion
    Show[HttpVersion] : Show HttpVersion

{-# COMPILE GHC Eq[HttpVersion]   = AgdaEq   #-}
{-# COMPILE GHC Ord[HttpVersion]  = AgdaOrd  #-}
{-# COMPILE GHC Show[HttpVersion] = AgdaShow #-}


http09 : HttpVersion
http09 = mkHttpVersion 0 9
{-# COMPILE GHC http09 = Network.HTTP.Types.Version.http09 #-}

http10 : HttpVersion
http10 = mkHttpVersion 1 0
{-# COMPILE GHC http10 = Network.HTTP.Types.Version.http10 #-}

http11 : HttpVersion
http11 = mkHttpVersion 1 1
{-# COMPILE GHC http11 = Network.HTTP.Types.Version.http11 #-}

http20 : HttpVersion
http20 = mkHttpVersion 2 0
{-# COMPILE GHC http20 = Network.HTTP.Types.Version.http20 #-}
