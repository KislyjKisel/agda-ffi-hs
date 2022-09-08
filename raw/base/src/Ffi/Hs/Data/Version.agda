{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Version where

open import Agda.Builtin.Char                   using (Char)
open import Agda.Builtin.List                   using (List)
open import Ffi.Hs.-base.Class                  using (Data; Read; Show; Eq; Ord)
open import Ffi.Hs.Data.Int                     using (Int)
open import Ffi.Hs.GHC.IsList                   using (IsList)
open import Ffi.Hs.Text.ParserCombinators.ReadP using (ReadP)

{-# FOREIGN GHC
import qualified Data.Version
import MAlonzo.Code.Ffi.Hs.GHC.IsList (AgdaIsList)
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaData, AgdaRead, AgdaShow, AgdaEq, AgdaOrd
    )
#-}

record Version : Set where
    constructor mkVersion
    field
        versionBranch : List Int
        versionTags   : List (List Char)

{-# COMPILE GHC Version = data Data.Version.Version (Data.Version.Version) #-}

postulate
    showVersion  : Version → List Char
    parseVersion : ReadP Version
    makeVersion  : List Int → Version

{-# COMPILE GHC showVersion  = Data.Version.showVersion  #-}
{-# COMPILE GHC parseVersion = Data.Version.parseVersion #-}
{-# COMPILE GHC makeVersion  = Data.Version.makeVersion  #-}

postulate
    Data[Version]   : Data Version
    IsList[Version] : IsList Version
    Read[Version]   : Read Version
    Show[Version]   : Show Version
    Eq[Version]     : Eq Version
    Ord[Version]    : Ord Version

{-# COMPILE GHC Data[Version]   = AgdaData   #-}
{-# COMPILE GHC IsList[Version] = AgdaIsList #-}
{-# COMPILE GHC Read[Version]   = AgdaRead   #-}
{-# COMPILE GHC Show[Version]   = AgdaShow   #-}
{-# COMPILE GHC Eq[Version]     = AgdaEq     #-}
{-# COMPILE GHC Ord[Version]    = AgdaOrd    #-}
