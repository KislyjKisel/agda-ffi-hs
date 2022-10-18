{-# OPTIONS --without-K #-}

module Ffi.Hs.Network.HTTP.Types.Method where

open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.ByteString using (ByteString)
open import Ffi.Hs.Data.Either     using (Either)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Network.HTTP.Types.Method
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}


Method : Set
Method = ByteString

postulate
    methodGet     : Method
    methodPost    : Method
    methodHead    : Method
    methodPut     : Method
    methodDelete  : Method
    methodTrace   : Method
    methodConnect : Method
    methodOptions : Method
    methodPatch   : Method

{-# COMPILE GHC methodGet     = Network.HTTP.Types.Method.methodGet     #-}
{-# COMPILE GHC methodPost    = Network.HTTP.Types.Method.methodPost    #-}
{-# COMPILE GHC methodHead    = Network.HTTP.Types.Method.methodHead    #-}
{-# COMPILE GHC methodPut     = Network.HTTP.Types.Method.methodPut     #-}
{-# COMPILE GHC methodDelete  = Network.HTTP.Types.Method.methodDelete  #-}
{-# COMPILE GHC methodTrace   = Network.HTTP.Types.Method.methodTrace   #-}
{-# COMPILE GHC methodConnect = Network.HTTP.Types.Method.methodConnect #-}
{-# COMPILE GHC methodOptions = Network.HTTP.Types.Method.methodOptions #-}
{-# COMPILE GHC methodPatch   = Network.HTTP.Types.Method.methodPatch   #-}


data StdMethod : Set where
    GET     : StdMethod
    POST    : StdMethod
    HEAD    : StdMethod
    PUT     : StdMethod
    DELETE  : StdMethod
    TRACE   : StdMethod
    CONNECT : StdMethod
    OPTIONS : StdMethod
    PATCH   : StdMethod

{-# COMPILE GHC StdMethod = data Network.HTTP.Types.Method.StdMethod
    ( Network.HTTP.Types.Method.GET
    | Network.HTTP.Types.Method.POST
    | Network.HTTP.Types.Method.HEAD
    | Network.HTTP.Types.Method.PUT
    | Network.HTTP.Types.Method.DELETE
    | Network.HTTP.Types.Method.TRACE
    | Network.HTTP.Types.Method.CONNECT
    | Network.HTTP.Types.Method.OPTIONS
    | Network.HTTP.Types.Method.PATCH
    ) #-}

postulate
    Bounded[StdMethod] : Bounded StdMethod
    Enum[StdMethod]    : Enum StdMethod
    Eq[StdMethod]      : Eq StdMethod
    Ord[StdMethod]     : Ord StdMethod
    Read[StdMethod]    : Read StdMethod
    Show[StdMethod]    : Show StdMethod
    Ix[StdMethod]      : Ix StdMethod

{-# COMPILE GHC Bounded[StdMethod] = AgdaBounded #-}
{-# COMPILE GHC Enum[StdMethod]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[StdMethod]      = AgdaEq      #-}
{-# COMPILE GHC Ord[StdMethod]     = AgdaOrd     #-}
{-# COMPILE GHC Read[StdMethod]    = AgdaRead    #-}
{-# COMPILE GHC Show[StdMethod]    = AgdaShow    #-}
{-# COMPILE GHC Ix[StdMethod]      = AgdaIx      #-}

postulate
    parseMethod     : Method → Either ByteString StdMethod
    renderMethod    : Either ByteString StdMethod -> Method
    renderStdMethod : StdMethod → Method

{-# COMPILE GHC parseMethod     = Network.HTTP.Types.Method.parseMethod     #-}
{-# COMPILE GHC renderMethod    = Network.HTTP.Types.Method.renderMethod    #-}
{-# COMPILE GHC renderStdMethod = Network.HTTP.Types.Method.renderStdMethod #-}
