{-# OPTIONS --without-K #-}

module Ffi.Hs.Network.HTTP.Types.Header where

open import Agda.Builtin.Int                      using () renaming (Int to Integer)
open import Agda.Builtin.List                     using (List)
open import Agda.Builtin.Maybe                    using (Maybe)
open import Agda.Builtin.Unit                     using ()
open import Ffi.Hs.-base.Class                    using (Eq; Data; Ord; Show)
open import Ffi.Hs.-base.Class                    using (Eq; Data; Ord; Show)
open import Ffi.Hs.-base.Literals
open import Ffi.Hs.Data.Binary.Builder            using (Builder)
open import Ffi.Hs.Data.ByteString.Internal as BS using (ByteString)
open import Ffi.Hs.Data.CaseInsensitive     as CI using (CI; FoldCase[ByteString])
open import Ffi.Hs.Data.String                    using (Lit-FromText[String])
open import Ffi.Hs.Data.Tuple                     using (Tuple2)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Network.HTTP.Types.Header
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    instance
        _ = FoldCase[ByteString]
        _ = Lit-FromText[String]


HeaderName : Set
HeaderName = CI ByteString

Header : Set
Header = Tuple2 HeaderName ByteString

RequestHeaders : Set
RequestHeaders = List Header

ResponseHeaders : Set
ResponseHeaders = List Header


hAccept : HeaderName
hAccept = CI.mk (BS.packChars "Accept")

hAcceptCharset : HeaderName
hAcceptCharset = CI.mk (BS.packChars "Accept-Charset")

hAcceptEncoding : HeaderName
hAcceptEncoding = CI.mk (BS.packChars "Accept-Encoding")

hAcceptLanguage : HeaderName
hAcceptLanguage = CI.mk (BS.packChars "Accept-Language")

hAcceptRanges : HeaderName
hAcceptRanges = CI.mk (BS.packChars "Accept-Ranges")

hAge : HeaderName
hAge = CI.mk (BS.packChars "Age")

hAllow : HeaderName
hAllow = CI.mk (BS.packChars "Allow")

hAuthorization : HeaderName
hAuthorization = CI.mk (BS.packChars "Authorization")

hCacheControl : HeaderName
hCacheControl = CI.mk (BS.packChars "Cache-Control")

hConnection : HeaderName
hConnection = CI.mk (BS.packChars "Connection")

hContentEncoding : HeaderName
hContentEncoding = CI.mk (BS.packChars "Content-Encoding")

hContentLanguage : HeaderName
hContentLanguage = CI.mk (BS.packChars "Content-Language")

hContentLength : HeaderName
hContentLength = CI.mk (BS.packChars "Content-Length")

hContentLocation : HeaderName
hContentLocation = CI.mk (BS.packChars "Content-Location")

hContentMD5 : HeaderName
hContentMD5 = CI.mk (BS.packChars "Content-MD5")

hContentRange : HeaderName
hContentRange = CI.mk (BS.packChars "Content-Range")

hContentType : HeaderName
hContentType = CI.mk (BS.packChars "Content-Type")

hDate : HeaderName
hDate = CI.mk (BS.packChars "Date")

hETag : HeaderName
hETag = CI.mk (BS.packChars "ETag")

hExpect : HeaderName
hExpect = CI.mk (BS.packChars "Expect")

hExpires : HeaderName
hExpires = CI.mk (BS.packChars "Expires")

hFrom : HeaderName
hFrom = CI.mk (BS.packChars "From")

hHost : HeaderName
hHost = CI.mk (BS.packChars "Host")

hIfMatch : HeaderName
hIfMatch = CI.mk (BS.packChars "If-Match")

hIfModifiedSince : HeaderName
hIfModifiedSince = CI.mk (BS.packChars "If-Modified-Since")

hIfNoneMatch : HeaderName
hIfNoneMatch = CI.mk (BS.packChars "If-None-Match")

hIfRange : HeaderName
hIfRange = CI.mk (BS.packChars "If-Range")

hIfUnmodifiedSince : HeaderName
hIfUnmodifiedSince = CI.mk (BS.packChars "If-Unmodified-Since")

hLastModified : HeaderName
hLastModified = CI.mk (BS.packChars "Last-Modified")

hLocation : HeaderName
hLocation = CI.mk (BS.packChars "Location")

hMaxForwards : HeaderName
hMaxForwards = CI.mk (BS.packChars "Max-Forwards")

hPragma : HeaderName
hPragma = CI.mk (BS.packChars "Pragma")

hProxyAuthenticate : HeaderName
hProxyAuthenticate = CI.mk (BS.packChars "Proxy-Authenticate")

hProxyAuthorization : HeaderName
hProxyAuthorization = CI.mk (BS.packChars "Proxy-Authorization")

hRange : HeaderName
hRange = CI.mk (BS.packChars "Range")

hReferer : HeaderName
hReferer = CI.mk (BS.packChars "Referer")

hRetryAfter : HeaderName
hRetryAfter = CI.mk (BS.packChars "Retry-After")

hServer : HeaderName
hServer = CI.mk (BS.packChars "Server")

hTE : HeaderName
hTE = CI.mk (BS.packChars "TE")

hTrailer : HeaderName
hTrailer = CI.mk (BS.packChars "Trailer")

hTransferEncoding : HeaderName
hTransferEncoding = CI.mk (BS.packChars "Transfer-Encoding")

hUpgrade : HeaderName
hUpgrade = CI.mk (BS.packChars "Upgrade")

hUserAgent : HeaderName
hUserAgent = CI.mk (BS.packChars "User-Agent")

hVary : HeaderName
hVary = CI.mk (BS.packChars "Vary")

hVia : HeaderName
hVia = CI.mk (BS.packChars "Via")

hWWWAuthenticate : HeaderName
hWWWAuthenticate = CI.mk (BS.packChars "WWW-Authenticate")

hWarning : HeaderName
hWarning = CI.mk (BS.packChars "Warning")

hContentDisposition : HeaderName
hContentDisposition = CI.mk (BS.packChars "Content-Disposition")

hMIMEVersion : HeaderName
hMIMEVersion = CI.mk (BS.packChars "MIME-Version")

hCookie : HeaderName
hCookie = CI.mk (BS.packChars "Cookie")

hSetCookie : HeaderName
hSetCookie = CI.mk (BS.packChars "Set-Cookie")

hOrigin : HeaderName
hOrigin = CI.mk (BS.packChars "Origin")

hPrefer : HeaderName
hPrefer = CI.mk (BS.packChars "Prefer")

hPreferenceApplied : HeaderName
hPreferenceApplied = CI.mk (BS.packChars "Preference-Applied")


data ByteRange : Set where
    ByteRangeFrom   : Integer → ByteRange
    ByteRangeFromTo : Integer → Integer → ByteRange
    ByteRangeSuffix : Integer → ByteRange

{-# COMPILE GHC ByteRange = data Network.HTTP.Types.Header.ByteRange
    ( Network.HTTP.Types.Header.ByteRangeFrom
    | Network.HTTP.Types.Header.ByteRangeFromTo
    | Network.HTTP.Types.Header.ByteRangeSuffix
    ) #-}

postulate
    Eq[ByteRange]   : Eq ByteRange
    Data[ByteRange] : Data ByteRange
    Ord[ByteRange]  : Ord ByteRange
    Show[ByteRange] : Show ByteRange

{-# COMPILE GHC Eq[ByteRange]   = AgdaEq   #-}
{-# COMPILE GHC Data[ByteRange] = AgdaData #-}
{-# COMPILE GHC Ord[ByteRange]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ByteRange] = AgdaShow #-}

ByteRanges : Set
ByteRanges = List ByteRange

postulate
    renderByteRangeBuilder  : ByteRange → Builder
    renderByteRange         : ByteRange → ByteString
    renderByteRangesBuilder : ByteRanges → Builder
    renderByteRanges        : ByteRanges → ByteString
    parseByteRanges         : ByteString → Maybe ByteRanges

{-# COMPILE GHC renderByteRangeBuilder  = Network.HTTP.Types.Header.renderByteRangeBuilder  #-}
{-# COMPILE GHC renderByteRange         = Network.HTTP.Types.Header.renderByteRange         #-}
{-# COMPILE GHC renderByteRangesBuilder = Network.HTTP.Types.Header.renderByteRangesBuilder #-}
{-# COMPILE GHC renderByteRanges        = Network.HTTP.Types.Header.renderByteRanges        #-}
{-# COMPILE GHC parseByteRanges         = Network.HTTP.Types.Header.parseByteRanges         #-}
