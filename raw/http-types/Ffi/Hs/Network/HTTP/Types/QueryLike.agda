{-# OPTIONS --without-K #-}

module Ffi.Hs.Network.HTTP.Types.QueryLike where

open import Agda.Builtin.Char             using (Char)
open import Agda.Builtin.List             using (List)
open import Agda.Builtin.Maybe            using (Maybe)
open import Agda.Builtin.String           using () renaming (String to Text)
open import Agda.Primitive                using (Level)
open import Ffi.Hs.Data.ByteString        using (StrictByteString)
open import Ffi.Hs.Data.ByteString.Lazy   using (LazyByteString)
open import Ffi.Hs.Data.Tuple             using (Tuple2)
open import Ffi.Hs.Network.HTTP.Types.URI using (Query)

{-# FOREIGN GHC
import qualified Network.HTTP.Types.QueryLike
#-}

private
    variable
        aℓ : Level
        A K V : Set aℓ


postulate
    QueryLike      : Set aℓ → Set aℓ
    QueryKeyLike   : Set aℓ → Set aℓ
    QueryValueLike : Set aℓ → Set aℓ

    toQuery      : ⦃ QueryLike A ⦄ → A → Query
    toQueryKey   : ⦃ QueryKeyLike A ⦄ → A → StrictByteString
    toQueryValue : ⦃ QueryValueLike A ⦄ → A → Maybe StrictByteString

    QueryLike[List[Maybe[Tuple2[K,V]]]] : ⦃ QueryKeyLike K ⦄ → ⦃ QueryValueLike V ⦄ → QueryLike (List (Maybe (Tuple2 K V)))
    QueryLike[List[Tuple2[K,V]]]        : ⦃ QueryKeyLike K ⦄ → ⦃ QueryValueLike V ⦄ → QueryLike (List (Tuple2 K V))

    QueryKeyLike[StrictByteString] : QueryKeyLike StrictByteString
    QueryKeyLike[LazyByteString]   : QueryKeyLike LazyByteString
    QueryKeyLike[Text]             : QueryKeyLike Text
    QueryKeyLike[String]           : QueryKeyLike (List Char)

    QueryValueLike[StrictByteString] : QueryValueLike StrictByteString
    QueryValueLike[LazyByteString]   : QueryValueLike LazyByteString
    QueryValueLike[Text]             : QueryValueLike Text
    QueryValueLike[String]           : QueryValueLike (List Char)
    QueryValueLike[Maybe[A]]         : ⦃ QueryValueLike A ⦄ → QueryValueLike (Maybe A)

{-# FOREIGN GHC
data AgdaQueryLike aℓ a = Network.HTTP.Types.QueryLike.QueryLike a => AgdaQueryLike
data AgdaQueryKeyLike aℓ a = Network.HTTP.Types.QueryLike.QueryKeyLike a => AgdaQueryKeyLike
data AgdaQueryValueLike aℓ a = Network.HTTP.Types.QueryLike.QueryValueLike a => AgdaQueryValueLike
#-}
{-# COMPILE GHC QueryLike      = type(0) AgdaQueryLike      #-}
{-# COMPILE GHC QueryKeyLike   = type(0) AgdaQueryKeyLike   #-}
{-# COMPILE GHC QueryValueLike = type(0) AgdaQueryValueLike #-}

{-# COMPILE GHC toQuery      = \ aℓ a AgdaQueryLike      -> Network.HTTP.Types.QueryLike.toQuery      #-}
{-# COMPILE GHC toQueryKey   = \ aℓ a AgdaQueryKeyLike   -> Network.HTTP.Types.QueryLike.toQueryKey   #-}
{-# COMPILE GHC toQueryValue = \ aℓ a AgdaQueryValueLike -> Network.HTTP.Types.QueryLike.toQueryValue #-}

{-# COMPILE GHC QueryLike[List[Maybe[Tuple2[K,V]]]] = \ kℓ k vℓ v AgdaQueryKeyLike AgdaQueryValueLike -> AgdaQueryLike #-}
{-# COMPILE GHC QueryLike[List[Tuple2[K,V]]]        = \ kℓ k vℓ v AgdaQueryKeyLike AgdaQueryValueLike -> AgdaQueryLike #-}

{-# COMPILE GHC QueryKeyLike[StrictByteString] = AgdaQueryKeyLike #-}
{-# COMPILE GHC QueryKeyLike[LazyByteString]   = AgdaQueryKeyLike #-}
{-# COMPILE GHC QueryKeyLike[Text]             = AgdaQueryKeyLike #-}
{-# COMPILE GHC QueryKeyLike[String]           = AgdaQueryKeyLike #-}

{-# COMPILE GHC QueryValueLike[StrictByteString] =                              AgdaQueryValueLike #-}
{-# COMPILE GHC QueryValueLike[LazyByteString]   =                              AgdaQueryValueLike #-}
{-# COMPILE GHC QueryValueLike[Text]             =                              AgdaQueryValueLike #-}
{-# COMPILE GHC QueryValueLike[String]           =                              AgdaQueryValueLike #-}
{-# COMPILE GHC QueryValueLike[Maybe[A]]         = \ aℓ a AgdaQueryValueLike -> AgdaQueryValueLike #-}
