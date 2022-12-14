{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Binary-Instanced where

open import Ffi.Hs.Data.Binary

instance
    inst:Binary[Bool]            = Binary[Bool]
    inst:Binary[Char]            = Binary[Char]
    inst:Binary[Double]          = Binary[Double]
    inst:Binary[Float]           = Binary[Float]
    inst:Binary[Int]             = Binary[Int]
    inst:Binary[Int8]            = Binary[Int8]
    inst:Binary[Int16]           = Binary[Int16]
    inst:Binary[Int32]           = Binary[Int32]
    inst:Binary[Int64]           = Binary[Int64]
    inst:Binary[Integer]         = Binary[Integer]
    inst:Binary[Natural]         = Binary[Natural]
    inst:Binary[Ordering]        = Binary[Ordering]
    inst:Binary[Word]            = Binary[Word]
    inst:Binary[Word8]           = Binary[Word8]
    inst:Binary[Word16]          = Binary[Word16]
    inst:Binary[Word32]          = Binary[Word32]
    inst:Binary[Word64]          = Binary[Word64]
    inst:Binary[⊤]               = Binary[⊤]
    inst:Binary[Void]            = Binary[Void]
    inst:Binary[Version]         = Binary[Version]
    inst:Binary[All]             = Binary[All]
    inst:Binary[Any]             = Binary[Any]
    inst:Binary[Fingerprint]     = Binary[Fingerprint]
    inst:Binary[ShortByteString] = Binary[ShortByteString]
    inst:Binary[LazyByteString]  = Binary[LazyByteString]
    inst:Binary[ByteString]      = Binary[ByteString]

    inst:Binary[List[A]]     = Binary[List[A]]
    inst:Binary[Maybe[A]]    = Binary[Maybe[A]]
    inst:Binary[Ratio[A]]    = Binary[Ratio[A]]
    inst:Binary[Complex[A]]  = Binary[Complex[A]]
    inst:Binary[Fixed[A]]    = Binary[Fixed[A]]
    inst:Binary[Min[A]]      = Binary[Min[A]]
    inst:Binary[Max[A]]      = Binary[Max[A]]
    inst:Binary[SFirst[A]]   = Binary[SFirst[A]]
    inst:Binary[SLast[A]]    = Binary[SLast[A]]
    inst:Binary[Identity[A]] = Binary[Identity[A]]
    inst:Binary[MFirst[A]]   = Binary[MFirst[A]]
    inst:Binary[MLast[A]]    = Binary[MLast[A]]
    inst:Binary[Dual[A]]     = Binary[Dual[A]]
    inst:Binary[Sum[A]]      = Binary[Sum[A]]
    inst:Binary[Product[A]]  = Binary[Product[A]]
    inst:Binary[NonEmpty[A]] = Binary[NonEmpty[A]]

    inst:Binary[Either[A,B]] = Binary[Either[A,B]]
    inst:Binary[Tuple2[A,B]] = Binary[Tuple2[A,B]]
    inst:Binary[Arg[A,B]]    = Binary[Arg[A,B]]
    inst:Binary[Alt[F,A]]    = Binary[Alt[F,A]]

    inst:Binary[Tuple3[A,B,C]]     = Binary[Tuple3[A,B,C]]
    inst:Binary[Tuple4[A,B,C,D]]   = Binary[Tuple4[A,B,C,D]]
    inst:Binary[Tuple5[A,B,C,D,E]] = Binary[Tuple5[A,B,C,D,E]]
