{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.ByteString.Short where

open import Ffi.Hs.Data.ByteString.Short.Internal public
    using
    ( ShortByteString
    ; SBS
    ; IsList[ShortByteString]
    ; Eq[ShortByteString]
    ; Data[ShortByteString]
    ; Ord[ShortByteString]
    ; Read[ShortByteString]
    ; Show[ShortByteString]
    ; IsString[ShortByteString]
    ; Semigroup[ShortByteString]
    ; Monoid[ShortByteString]
    ; NFData[ShortByteString]
    ; Item[ShortByteString]

    ; empty
    ; singleton
    ; pack
    ; unpack
    ; fromShort
    ; toShort

    ; snoc
    ; cons
    ; append
    ; last
    ; tail
    ; uncons
    ; head
    ; init
    ; unsnoc
    ; null
    ; length

    ; isValidUtf8

    ; map
    ; reverse
    ; intercalate

    ; foldl
    ; foldl'
    ; foldl1
    ; foldl1'

    ; foldr
    ; foldr'
    ; foldr1
    ; foldr1'

    ; all
    ; any
    ; concat

    ; replicate
    ; unfoldr
    ; unfoldrN

    ; take
    ; takeEnd
    ; takeWhileEnd
    ; takeWhile
    ; drop
    ; dropEnd
    ; dropWhile
    ; dropWhileEnd
    ; breakEnd
    ; break
    ; span
    ; spanEnd
    ; splitAt
    ; split
    ; splitWith
    ; stripSuffix
    ; stripPrefix

    ; isInfixOf
    ; isPrefixOf
    ; isSuffixOf

    ; breakSubstring

    ; elem
    ; find
    ; filter
    ; partition

    ; index
    ; indexMaybe
    ; _!?_
    ; elemIndex
    ; elemIndices
    ; count
    ; findIndex
    ; findIndices

    ; packCString
    ; packCStringLen

    ; useAsCString
    ; useAsCStringLen
    )
