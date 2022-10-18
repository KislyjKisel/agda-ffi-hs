{-# OPTIONS --without-K #-}

module Ffi.Hs.Network.HTTP.Types.URI where

open import Agda.Builtin.Bool          using (Bool)
open import Agda.Builtin.Maybe         using (Maybe; just)
open import Agda.Builtin.String        using () renaming (String to Text)
open import Ffi.Hs.-base.Class         using (Eq; Ord; Show)
open import Ffi.Hs.Data.Bifunctor      using (second)
open import Ffi.Hs.Data.Binary.Builder using (Builder)
open import Ffi.Hs.Data.ByteString     using (ByteString)
open import Ffi.Hs.Data.List           using (List; map)
open import Ffi.Hs.Data.Tuple          using (Tuple2; Bifunctor[Tuple2])

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Network.HTTP.Types.URI
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    instance
        _ = Bifunctor[Tuple2]


QueryItem : Set
QueryItem = Tuple2 ByteString (Maybe ByteString)

Query : Set
Query = List QueryItem

SimpleQueryItem : Set
SimpleQueryItem = Tuple2 ByteString ByteString

SimpleQuery : Set
SimpleQuery = List SimpleQueryItem

simpleQueryToQuery : SimpleQuery → Query
simpleQueryToQuery = map (second just)
{-# COMPILE GHC simpleQueryToQuery = Network.HTTP.Types.URI.simpleQueryToQuery #-}


postulate
    renderQuery           : Bool → Query → ByteString
    renderSimpleQuery     : Bool → SimpleQuery → ByteString
    parseQuery            : ByteString → Query
    parseQueryReplacePlus : Bool → ByteString → Query
    parseSimpleQuery      : ByteString → SimpleQuery
    renderQueryBuilder    : Bool → Query → Builder

{-# COMPILE GHC renderQuery           = Network.HTTP.Types.URI.renderQuery           #-}
{-# COMPILE GHC renderSimpleQuery     = Network.HTTP.Types.URI.renderSimpleQuery     #-}
{-# COMPILE GHC parseQuery            = Network.HTTP.Types.URI.parseQuery            #-}
{-# COMPILE GHC parseQueryReplacePlus = Network.HTTP.Types.URI.parseQueryReplacePlus #-}
{-# COMPILE GHC parseSimpleQuery      = Network.HTTP.Types.URI.parseSimpleQuery      #-}
{-# COMPILE GHC renderQueryBuilder    = Network.HTTP.Types.URI.renderQueryBuilder    #-}

data EscapeItem : Set where
    QE : ByteString → EscapeItem
    QN : ByteString → EscapeItem

{-# COMPILE GHC EscapeItem = data Network.HTTP.Types.URI.EscapeItem
    ( Network.HTTP.Types.URI.QE
    | Network.HTTP.Types.URI.QN
    ) #-}

postulate
    Eq[EscapeItem]   : Eq EscapeItem
    Ord[EscapeItem]  : Ord EscapeItem
    Show[EscapeItem] : Show EscapeItem

{-# COMPILE GHC Eq[EscapeItem]   = AgdaEq   #-}
{-# COMPILE GHC Ord[EscapeItem]  = AgdaOrd  #-}
{-# COMPILE GHC Show[EscapeItem] = AgdaShow #-}

PartialEscapeQueryItem : Set
PartialEscapeQueryItem = Tuple2 ByteString (List EscapeItem)

PartialEscapeQuery : Set
PartialEscapeQuery = List PartialEscapeQueryItem

postulate
    renderQueryPartialEscape        : Bool → PartialEscapeQuery → ByteString
    renderQueryBuilderPartialEscape : Bool → PartialEscapeQuery → Builder

{-# COMPILE GHC renderQueryPartialEscape        = Network.HTTP.Types.URI.renderQueryPartialEscape        #-}
{-# COMPILE GHC renderQueryBuilderPartialEscape = Network.HTTP.Types.URI.renderQueryBuilderPartialEscape #-}


QueryText : Set
QueryText = List (Tuple2 Text (Maybe Text))

postulate
    queryTextToQuery : QueryText → Query
    queryToQueryText : Query → QueryText
    renderQueryText : Bool → QueryText → Builder
    parseQueryText : ByteString → QueryText

{-# COMPILE GHC queryTextToQuery = Network.HTTP.Types.URI.queryTextToQuery #-}
{-# COMPILE GHC queryToQueryText = Network.HTTP.Types.URI.queryToQueryText #-}
{-# COMPILE GHC renderQueryText  = Network.HTTP.Types.URI.renderQueryText  #-}
{-# COMPILE GHC parseQueryText   = Network.HTTP.Types.URI.parseQueryText   #-}


postulate
    encodePathSegments : List Text → Builder
    decodePathSegments : ByteString → List Text
    encodePathSegmentsRelative : List Text → Builder

    extractPath : ByteString → ByteString
    encodePath : List Text → Query → Builder
    decodePath : ByteString → Tuple2 (List Text) Query

    urlEncodeBuilder : Bool → ByteString → Builder
    urlEncode        : Bool → ByteString → ByteString
    urlDecode        : Bool → ByteString → ByteString

{-# COMPILE GHC encodePathSegments         = Network.HTTP.Types.URI.encodePathSegments         #-}
{-# COMPILE GHC decodePathSegments         = Network.HTTP.Types.URI.decodePathSegments         #-}
{-# COMPILE GHC encodePathSegmentsRelative = Network.HTTP.Types.URI.encodePathSegmentsRelative #-}

{-# COMPILE GHC extractPath = Network.HTTP.Types.URI.extractPath #-}
{-# COMPILE GHC encodePath  = Network.HTTP.Types.URI.encodePath  #-}
{-# COMPILE GHC decodePath  = Network.HTTP.Types.URI.decodePath  #-}

{-# COMPILE GHC urlEncodeBuilder = Network.HTTP.Types.URI.urlEncodeBuilder #-}
{-# COMPILE GHC urlEncode        = Network.HTTP.Types.URI.urlEncode        #-}
{-# COMPILE GHC urlDecode        = Network.HTTP.Types.URI.urlDecode        #-}
