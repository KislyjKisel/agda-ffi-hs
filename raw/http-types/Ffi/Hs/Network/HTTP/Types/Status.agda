{-# OPTIONS --without-K #-}

module Ffi.Hs.Network.HTTP.Types.Status where

open import Agda.Builtin.Unit                     using () -- instanced constructor
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Literals
open import Ffi.Hs.Data.Bool                      using (Bool; _&&_)
open import Ffi.Hs.Data.ByteString.Internal as BS using (ByteString)
open import Ffi.Hs.Data.Int                       using (Int; Ord[Int]; Lit-FromNat[Int])
open import Ffi.Hs.Data.Ord                       using (_>=_; _<_)
open import Ffi.Hs.Data.String                    using (Lit-FromText[String])

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Network.HTTP.Types.Status
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    instance
        _ = Ord[Int]
        _ = Lit-FromNat[Int]
        _ = Lit-FromText[String]


record Status : Set where
    constructor mkStatus
    field
        statusCode    : Int
        statusMessage : ByteString

open Status public

{-# COMPILE GHC Status = data Network.HTTP.Types.Status.Status (Network.HTTP.Types.Status.Status) #-}

postulate
    Bounded[Status] : Bounded Status
    Enum[Status]    : Enum Status
    Eq[Status]      : Eq Status
    Ord[Status]     : Ord Status
    Show[Status]    : Show Status

{-# COMPILE GHC Bounded[Status] = AgdaBounded #-}
{-# COMPILE GHC Enum[Status]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[Status]      = AgdaEq      #-}
{-# COMPILE GHC Ord[Status]     = AgdaOrd     #-}
{-# COMPILE GHC Show[Status]    = AgdaShow    #-}


status100 : Status
status100 = mkStatus 100 (BS.packChars "Continue")
{-# COMPILE GHC status100 = Network.HTTP.Types.Status.status100 #-}

continue100 : Status
continue100 = status100
{-# COMPILE GHC continue100 = Network.HTTP.Types.Status.continue100 #-}

status101 : Status
status101 = mkStatus 101 (BS.packChars "Switching Protocols")
{-# COMPILE GHC status101 = Network.HTTP.Types.Status.status101 #-}

switchingProtocols101 : Status
switchingProtocols101 = status101
{-# COMPILE GHC switchingProtocols101 = Network.HTTP.Types.Status.switchingProtocols101 #-}

status200 : Status
status200 = mkStatus 200 (BS.packChars "OK")
{-# COMPILE GHC status200 = Network.HTTP.Types.Status.status200 #-}

ok200 : Status
ok200 = status200
{-# COMPILE GHC ok200 = Network.HTTP.Types.Status.ok200 #-}

status201 : Status
status201 = mkStatus 201 (BS.packChars "Created")
{-# COMPILE GHC status201 = Network.HTTP.Types.Status.status201 #-}

created201 : Status
created201 = status201
{-# COMPILE GHC created201 = Network.HTTP.Types.Status.created201 #-}

status202 : Status
status202 = mkStatus 202 (BS.packChars "Accepted")
{-# COMPILE GHC status202 = Network.HTTP.Types.Status.status202 #-}

accepted202 : Status
accepted202 = status202
{-# COMPILE GHC accepted202 = Network.HTTP.Types.Status.accepted202 #-}

status203 : Status
status203 = mkStatus 203 (BS.packChars "Non-Authoritative Information")
{-# COMPILE GHC status203 = Network.HTTP.Types.Status.status203 #-}

nonAuthoritative203 : Status
nonAuthoritative203 = status203
{-# COMPILE GHC nonAuthoritative203 = Network.HTTP.Types.Status.nonAuthoritative203 #-}

status204 : Status
status204 = mkStatus 204 (BS.packChars "No Content")
{-# COMPILE GHC status204 = Network.HTTP.Types.Status.status204 #-}

noContent204 : Status
noContent204 = status204
{-# COMPILE GHC noContent204 = Network.HTTP.Types.Status.noContent204 #-}

status205 : Status
status205 = mkStatus 205 (BS.packChars "Reset Content")
{-# COMPILE GHC status205 = Network.HTTP.Types.Status.status205 #-}

resetContent205 : Status
resetContent205 = status205
{-# COMPILE GHC resetContent205 = Network.HTTP.Types.Status.resetContent205 #-}

status206 : Status
status206 = mkStatus 206 (BS.packChars "Partial Content")
{-# COMPILE GHC status206 = Network.HTTP.Types.Status.status206 #-}

partialContent206 : Status
partialContent206 = status206
{-# COMPILE GHC partialContent206 = Network.HTTP.Types.Status.partialContent206 #-}

status300 : Status
status300 = mkStatus 300 (BS.packChars "Multiple Choices")
{-# COMPILE GHC status300 = Network.HTTP.Types.Status.status300 #-}

multipleChoices300 : Status
multipleChoices300 = status300
{-# COMPILE GHC multipleChoices300 = Network.HTTP.Types.Status.multipleChoices300 #-}

status301 : Status
status301 = mkStatus 301 (BS.packChars "Moved Permanently")
{-# COMPILE GHC status301 = Network.HTTP.Types.Status.status301 #-}

movedPermanently301 : Status
movedPermanently301 = status301
{-# COMPILE GHC movedPermanently301 = Network.HTTP.Types.Status.movedPermanently301 #-}

status302 : Status
status302 = mkStatus 302 (BS.packChars "Found")
{-# COMPILE GHC status302 = Network.HTTP.Types.Status.status302 #-}

found302 : Status
found302 = status302
{-# COMPILE GHC found302 = Network.HTTP.Types.Status.found302 #-}

status303 : Status
status303 = mkStatus 303 (BS.packChars "See Other")
{-# COMPILE GHC status303 = Network.HTTP.Types.Status.status303 #-}

seeOther303 : Status
seeOther303 = status303
{-# COMPILE GHC seeOther303 = Network.HTTP.Types.Status.seeOther303 #-}

status304 : Status
status304 = mkStatus 304 (BS.packChars "Not Modified")
{-# COMPILE GHC status304 = Network.HTTP.Types.Status.status304 #-}

notModified304 : Status
notModified304 = status304
{-# COMPILE GHC notModified304 = Network.HTTP.Types.Status.notModified304 #-}

status305 : Status
status305 = mkStatus 305 (BS.packChars "Use Proxy")
{-# COMPILE GHC status305 = Network.HTTP.Types.Status.status305 #-}

useProxy305 : Status
useProxy305 = status305
{-# COMPILE GHC useProxy305 = Network.HTTP.Types.Status.useProxy305 #-}

status307 : Status
status307 = mkStatus 307 (BS.packChars "Temporary Redirect")
{-# COMPILE GHC status307 = Network.HTTP.Types.Status.status307 #-}

temporaryRedirect307 : Status
temporaryRedirect307 = status307
{-# COMPILE GHC temporaryRedirect307 = Network.HTTP.Types.Status.temporaryRedirect307 #-}

status308 : Status
status308 = mkStatus 308 (BS.packChars "Permanent Redirect")
{-# COMPILE GHC status308 = Network.HTTP.Types.Status.status308 #-}

permanentRedirect308 : Status
permanentRedirect308 = status308
{-# COMPILE GHC permanentRedirect308 = Network.HTTP.Types.Status.permanentRedirect308 #-}

status400 : Status
status400 = mkStatus 400 (BS.packChars "Bad Request")
{-# COMPILE GHC status400 = Network.HTTP.Types.Status.status400 #-}

badRequest400 : Status
badRequest400 = status400
{-# COMPILE GHC badRequest400 = Network.HTTP.Types.Status.badRequest400 #-}

status401 : Status
status401 = mkStatus 401 (BS.packChars "Unauthorized")
{-# COMPILE GHC status401 = Network.HTTP.Types.Status.status401 #-}

unauthorized401 : Status
unauthorized401 = status401
{-# COMPILE GHC unauthorized401 = Network.HTTP.Types.Status.unauthorized401 #-}

status402 : Status
status402 = mkStatus 402 (BS.packChars "Payment Required")
{-# COMPILE GHC status402 = Network.HTTP.Types.Status.status402 #-}

paymentRequired402 : Status
paymentRequired402 = status402
{-# COMPILE GHC paymentRequired402 = Network.HTTP.Types.Status.paymentRequired402 #-}

status403 : Status
status403 = mkStatus 403 (BS.packChars "Forbidden")
{-# COMPILE GHC status403 = Network.HTTP.Types.Status.status403 #-}

forbidden403 : Status
forbidden403 = status403
{-# COMPILE GHC forbidden403 = Network.HTTP.Types.Status.forbidden403 #-}

status404 : Status
status404 = mkStatus 404 (BS.packChars "Not Found")
{-# COMPILE GHC status404 = Network.HTTP.Types.Status.status404 #-}

notFound404 : Status
notFound404 = status404
{-# COMPILE GHC notFound404 = Network.HTTP.Types.Status.notFound404 #-}

status405 : Status
status405 = mkStatus 405 (BS.packChars "Method Not Allowed")
{-# COMPILE GHC status405 = Network.HTTP.Types.Status.status405 #-}

methodNotAllowed405 : Status
methodNotAllowed405 = status405
{-# COMPILE GHC methodNotAllowed405 = Network.HTTP.Types.Status.methodNotAllowed405 #-}

status406 : Status
status406 = mkStatus 406 (BS.packChars "Not Acceptable")
{-# COMPILE GHC status406 = Network.HTTP.Types.Status.status406 #-}

notAcceptable406 : Status
notAcceptable406 = status406
{-# COMPILE GHC notAcceptable406 = Network.HTTP.Types.Status.notAcceptable406 #-}

status407 : Status
status407 = mkStatus 407 (BS.packChars "Proxy Authentication Required")
{-# COMPILE GHC status407 = Network.HTTP.Types.Status.status407 #-}

proxyAuthenticationRequired407 : Status
proxyAuthenticationRequired407 = status407
{-# COMPILE GHC proxyAuthenticationRequired407 = Network.HTTP.Types.Status.proxyAuthenticationRequired407 #-}

status408 : Status
status408 = mkStatus 408 (BS.packChars "Request Timeout")
{-# COMPILE GHC status408 = Network.HTTP.Types.Status.status408 #-}

requestTimeout408 : Status
requestTimeout408 = status408
{-# COMPILE GHC requestTimeout408 = Network.HTTP.Types.Status.requestTimeout408 #-}

status409 : Status
status409 = mkStatus 409 (BS.packChars "Conflict")
{-# COMPILE GHC status409 = Network.HTTP.Types.Status.status409 #-}

conflict409 : Status
conflict409 = status409
{-# COMPILE GHC conflict409 = Network.HTTP.Types.Status.conflict409 #-}

status410 : Status
status410 = mkStatus 410 (BS.packChars "Gone")
{-# COMPILE GHC status410 = Network.HTTP.Types.Status.status410 #-}

gone410 : Status
gone410 = status410
{-# COMPILE GHC gone410 = Network.HTTP.Types.Status.gone410 #-}

status411 : Status
status411 = mkStatus 411 (BS.packChars "Length Required")
{-# COMPILE GHC status411 = Network.HTTP.Types.Status.status411 #-}

lengthRequired411 : Status
lengthRequired411 = status411
{-# COMPILE GHC lengthRequired411 = Network.HTTP.Types.Status.lengthRequired411 #-}

status412 : Status
status412 = mkStatus 412 (BS.packChars "Precondition Failed")
{-# COMPILE GHC status412 = Network.HTTP.Types.Status.status412 #-}

preconditionFailed412 : Status
preconditionFailed412 = status412
{-# COMPILE GHC preconditionFailed412 = Network.HTTP.Types.Status.preconditionFailed412 #-}

status413 : Status
status413 = mkStatus 413 (BS.packChars "Request Entity Too Large")
{-# COMPILE GHC status413 = Network.HTTP.Types.Status.status413 #-}

requestEntityTooLarge413 : Status
requestEntityTooLarge413 = status413
{-# COMPILE GHC requestEntityTooLarge413 = Network.HTTP.Types.Status.requestEntityTooLarge413 #-}

status414 : Status
status414 = mkStatus 414 (BS.packChars "Request-URI Too Long")
{-# COMPILE GHC status414 = Network.HTTP.Types.Status.status414 #-}

requestURITooLong414 : Status
requestURITooLong414 = status414
{-# COMPILE GHC requestURITooLong414 = Network.HTTP.Types.Status.requestURITooLong414 #-}

status415 : Status
status415 = mkStatus 415 (BS.packChars "Unsupported Media Type")
{-# COMPILE GHC status415 = Network.HTTP.Types.Status.status415 #-}

unsupportedMediaType415 : Status
unsupportedMediaType415 = status415
{-# COMPILE GHC unsupportedMediaType415 = Network.HTTP.Types.Status.unsupportedMediaType415 #-}

status416 : Status
status416 = mkStatus 416 (BS.packChars "Requested Range Not Satisfiable")
{-# COMPILE GHC status416 = Network.HTTP.Types.Status.status416 #-}

requestedRangeNotSatisfiable416 : Status
requestedRangeNotSatisfiable416 = status416
{-# COMPILE GHC requestedRangeNotSatisfiable416 = Network.HTTP.Types.Status.requestedRangeNotSatisfiable416 #-}

status417 : Status
status417 = mkStatus 417 (BS.packChars "Expectation Failed")
{-# COMPILE GHC status417 = Network.HTTP.Types.Status.status417 #-}

expectationFailed417 : Status
expectationFailed417 = status417
{-# COMPILE GHC expectationFailed417 = Network.HTTP.Types.Status.expectationFailed417 #-}

status418 : Status
status418 = mkStatus 418 (BS.packChars "I'm a teapot")
{-# COMPILE GHC status418 = Network.HTTP.Types.Status.status418 #-}

imATeapot418 : Status
imATeapot418 = status418
{-# COMPILE GHC imATeapot418 = Network.HTTP.Types.Status.imATeapot418 #-}

status422 : Status
status422 = mkStatus 422 (BS.packChars "Unprocessable Entity")
{-# COMPILE GHC status422 = Network.HTTP.Types.Status.status422 #-}

unprocessableEntity422 : Status
unprocessableEntity422 = status422
{-# COMPILE GHC unprocessableEntity422 = Network.HTTP.Types.Status.unprocessableEntity422 #-}

status426 : Status
status426 = mkStatus 426 (BS.packChars "Upgrade Required")
{-# COMPILE GHC status426 = Network.HTTP.Types.Status.status426 #-}

upgradeRequired426 : Status
upgradeRequired426 = status426
{-# COMPILE GHC upgradeRequired426 = Network.HTTP.Types.Status.upgradeRequired426 #-}

status428 : Status
status428 = mkStatus 428 (BS.packChars "Precondition Required")
{-# COMPILE GHC status428 = Network.HTTP.Types.Status.status428 #-}

preconditionRequired428 : Status
preconditionRequired428 = status428
{-# COMPILE GHC preconditionRequired428 = Network.HTTP.Types.Status.preconditionRequired428 #-}

status429 : Status
status429 = mkStatus 429 (BS.packChars "Too Many Requests")
{-# COMPILE GHC status429 = Network.HTTP.Types.Status.status429 #-}

tooManyRequests429 : Status
tooManyRequests429 = status429
{-# COMPILE GHC tooManyRequests429 = Network.HTTP.Types.Status.tooManyRequests429 #-}

status431 : Status
status431 = mkStatus 431 (BS.packChars "Request Header Fields Too Large")
{-# COMPILE GHC status431 = Network.HTTP.Types.Status.status431 #-}

requestHeaderFieldsTooLarge431 : Status
requestHeaderFieldsTooLarge431 = status431
{-# COMPILE GHC requestHeaderFieldsTooLarge431 = Network.HTTP.Types.Status.requestHeaderFieldsTooLarge431 #-}

status500 : Status
status500 = mkStatus 500 (BS.packChars "Internal Server Error")
{-# COMPILE GHC status500 = Network.HTTP.Types.Status.status500 #-}

internalServerError500 : Status
internalServerError500 = status500
{-# COMPILE GHC internalServerError500 = Network.HTTP.Types.Status.internalServerError500 #-}

status501 : Status
status501 = mkStatus 501 (BS.packChars "Not Implemented")
{-# COMPILE GHC status501 = Network.HTTP.Types.Status.status501 #-}

notImplemented501 : Status
notImplemented501 = status501
{-# COMPILE GHC notImplemented501 = Network.HTTP.Types.Status.notImplemented501 #-}

status502 : Status
status502 = mkStatus 502 (BS.packChars "Bad Gateway")
{-# COMPILE GHC status502 = Network.HTTP.Types.Status.status502 #-}

badGateway502 : Status
badGateway502 = status502
{-# COMPILE GHC badGateway502 = Network.HTTP.Types.Status.badGateway502 #-}

status503 : Status
status503 = mkStatus 503 (BS.packChars "Service Unavailable")
{-# COMPILE GHC status503 = Network.HTTP.Types.Status.status503 #-}

serviceUnavailable503 : Status
serviceUnavailable503 = status503
{-# COMPILE GHC serviceUnavailable503 = Network.HTTP.Types.Status.serviceUnavailable503 #-}

status504 : Status
status504 = mkStatus 504 (BS.packChars "Gateway Timeout")
{-# COMPILE GHC status504 = Network.HTTP.Types.Status.status504 #-}

gatewayTimeout504 : Status
gatewayTimeout504 = status504
{-# COMPILE GHC gatewayTimeout504 = Network.HTTP.Types.Status.gatewayTimeout504 #-}

status505 : Status
status505 = mkStatus 505 (BS.packChars "HTTP Version Not Supported")
{-# COMPILE GHC status505 = Network.HTTP.Types.Status.status505 #-}

httpVersionNotSupported505 : Status
httpVersionNotSupported505 = status505
{-# COMPILE GHC httpVersionNotSupported505 = Network.HTTP.Types.Status.httpVersionNotSupported505 #-}

status511 : Status
status511 = mkStatus 511 (BS.packChars "Network Authentication Required")
{-# COMPILE GHC status511 = Network.HTTP.Types.Status.status511 #-}

networkAuthenticationRequired511 : Status
networkAuthenticationRequired511 = status511
{-# COMPILE GHC networkAuthenticationRequired511 = Network.HTTP.Types.Status.networkAuthenticationRequired511 #-}

statusIsInformational : Status -> Bool
statusIsInformational record { statusCode = code } = code >= 100 && code < 200
{-# COMPILE GHC statusIsInformational = Network.HTTP.Types.Status.statusIsInformational #-}

statusIsSuccessful : Status -> Bool
statusIsSuccessful record { statusCode = code } = code >= 200 && code < 300
{-# COMPILE GHC statusIsSuccessful = Network.HTTP.Types.Status.statusIsSuccessful #-}

statusIsRedirection : Status -> Bool
statusIsRedirection record { statusCode = code } = code >= 300 && code < 400
{-# COMPILE GHC statusIsRedirection = Network.HTTP.Types.Status.statusIsRedirection #-}

statusIsClientError : Status -> Bool
statusIsClientError record { statusCode = code } = code >= 400 && code < 500
{-# COMPILE GHC statusIsClientError = Network.HTTP.Types.Status.statusIsClientError #-}

statusIsServerError : Status -> Bool
statusIsServerError record { statusCode = code } = code >= 500 && code < 600
{-# COMPILE GHC statusIsServerError = Network.HTTP.Types.Status.statusIsServerError #-}
