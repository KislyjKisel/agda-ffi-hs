{-# OPTIONS --without-K #-}

module Ffi.Hs.Prelude where

open import Ffi.Hs.-base.Level public
    using (Liftℓ; liftℓ; unliftℓ)

open import Ffi.Hs.-base.Unit public
    using (⊤; tt; ⊤′; tt′)

open import Ffi.Hs.Data.Bool public
    using (Bool; True; False; not; otherwise; _&&_; _||_)

instance
    inst:Data[Bool]       = Ffi.Hs.Data.Bool.Data[Bool]
    inst:Storable[Bool]   = Ffi.Hs.Data.Bool.Storable[Bool]
    inst:Bits[Bool]       = Ffi.Hs.Data.Bool.Bits[Bool]
    inst:FiniteBits[Bool] = Ffi.Hs.Data.Bool.FiniteBits[Bool]
    inst:Bounded[Bool]    = Ffi.Hs.Data.Bool.Bounded[Bool]
    inst:Enum[Bool]       = Ffi.Hs.Data.Bool.Enum[Bool]
    inst:Ix[Bool]         = Ffi.Hs.Data.Bool.Ix[Bool]
    inst:Read[Bool]       = Ffi.Hs.Data.Bool.Read[Bool]
    inst:Show[Bool]       = Ffi.Hs.Data.Bool.Show[Bool]
    inst:Eq[Bool]         = Ffi.Hs.Data.Bool.Eq[Bool]
    inst:Ord[Bool]        = Ffi.Hs.Data.Bool.Ord[Bool]

open import Ffi.Hs.Data.Maybe public
    using (Maybe; Just; Nothing; maybe)

instance
    inst:MonadFail[Maybe]    = Ffi.Hs.Data.Maybe.MonadFail[Maybe]
    inst:MonadFix[Maybe]     = Ffi.Hs.Data.Maybe.MonadFix[Maybe]
    inst:MonadZip[Maybe]     = Ffi.Hs.Data.Maybe.MonadZip[Maybe]
    inst:Foldable[Maybe]     = Ffi.Hs.Data.Maybe.Foldable[Maybe]
    inst:Eq1[Maybe]          = Ffi.Hs.Data.Maybe.Eq1[Maybe]
    inst:Ord1[Maybe]         = Ffi.Hs.Data.Maybe.Ord1[Maybe]
    inst:Read1[Maybe]        = Ffi.Hs.Data.Maybe.Read1[Maybe]
    inst:Show1[Maybe]        = Ffi.Hs.Data.Maybe.Show1[Maybe]
    inst:Traversable[Maybe]  = Ffi.Hs.Data.Maybe.Traversable[Maybe]
    inst:Alternative[Maybe]  = Ffi.Hs.Data.Maybe.Alternative[Maybe]
    inst:Applicative[Maybe]  = Ffi.Hs.Data.Maybe.Applicative[Maybe]
    inst:Functor[Maybe]      = Ffi.Hs.Data.Maybe.Functor[Maybe]
    inst:Monad[Maybe]        = Ffi.Hs.Data.Maybe.Monad[Maybe]
    inst:MonadPlus[Maybe]    = Ffi.Hs.Data.Maybe.MonadPlus[Maybe]
    inst:Data[Maybe[A]]      = Ffi.Hs.Data.Maybe.Data[Maybe[A]]
    inst:Monoid[Maybe[A]]    = Ffi.Hs.Data.Maybe.Monoid[Maybe[A]]
    inst:Semigroup[Maybe[A]] = Ffi.Hs.Data.Maybe.Semigroup[Maybe[A]]
    inst:Read[Maybe[A]]      = Ffi.Hs.Data.Maybe.Read[Maybe[A]]
    inst:Show[Maybe[A]]      = Ffi.Hs.Data.Maybe.Show[Maybe[A]]
    inst:Eq[Maybe[A]]        = Ffi.Hs.Data.Maybe.Eq[Maybe[A]]
    inst:Ord[Maybe[A]]       = Ffi.Hs.Data.Maybe.Ord[Maybe[A]]

open import Ffi.Hs.Data.Either public
    using (Either; Left; Right; either)

instance
    inst:Bifoldable[Either]     = Ffi.Hs.Data.Either.Bifoldable[Either]
    inst:Bifunctor[Either]      = Ffi.Hs.Data.Either.Bifunctor[Either]
    inst:Bitraversable[Either]  = Ffi.Hs.Data.Either.Bitraversable[Either]
    inst:Eq2[Either]            = Ffi.Hs.Data.Either.Eq2[Either]
    inst:Ord2[Either]           = Ffi.Hs.Data.Either.Ord2[Either]
    inst:Read2[Either]          = Ffi.Hs.Data.Either.Read2[Either]
    inst:Show2[Either]          = Ffi.Hs.Data.Either.Show2[Either]
    inst:MonadFix[Either[A]]    = Ffi.Hs.Data.Either.MonadFix[Either[A]]
    inst:Foldable[Either[A]]    = Ffi.Hs.Data.Either.Foldable[Either[A]]
    inst:Eq1[Either[A]]         = Ffi.Hs.Data.Either.Eq1[Either[A]]
    inst:Ord1[Either[A]]        = Ffi.Hs.Data.Either.Ord1[Either[A]]
    inst:Read1[Either[A]]       = Ffi.Hs.Data.Either.Read1[Either[A]]
    inst:Show1[Either[A]]       = Ffi.Hs.Data.Either.Show1[Either[A]]
    inst:Traversable[Either[A]] = Ffi.Hs.Data.Either.Traversable[Either[A]]
    inst:Applicative[Either[A]] = Ffi.Hs.Data.Either.Applicative[Either[A]]
    inst:Functor[Either[A]]     = Ffi.Hs.Data.Either.Functor[Either[A]]
    inst:Monad[Either[A]]       = Ffi.Hs.Data.Either.Monad[Either[A]]
    inst:Data[Either[A,B]]      = Ffi.Hs.Data.Either.Data[Either[A,B]]
    inst:Semigroup[Either[A,B]] = Ffi.Hs.Data.Either.Semigroup[Either[A,B]]
    inst:Read[Either[A,B]]      = Ffi.Hs.Data.Either.Read[Either[A,B]]
    inst:Show[Either[A,B]]      = Ffi.Hs.Data.Either.Show[Either[A,B]]
    inst:Eq[Either[A,B]]        = Ffi.Hs.Data.Either.Eq[Either[A,B]]
    inst:Ord[Either[A,B]]       = Ffi.Hs.Data.Either.Ord[Either[A,B]]

open import Ffi.Hs.Data.Ord public
    using
    ( Ordering; LT; EQ; GT
    ; compare; _<_; _<=_; _>_; _>=_; max; min
    )

instance
    inst:Data[Ordering]      = Ffi.Hs.Data.Ord.Data[Ordering]
    inst:Monoid[Ordering]    = Ffi.Hs.Data.Ord.Monoid[Ordering]
    inst:Semigroup[Ordering] = Ffi.Hs.Data.Ord.Semigroup[Ordering]
    inst:Bounded[Ordering]   = Ffi.Hs.Data.Ord.Bounded[Ordering]
    inst:Enum[Ordering]      = Ffi.Hs.Data.Ord.Enum[Ordering]
    inst:Ix[Ordering]        = Ffi.Hs.Data.Ord.Ix[Ordering]
    inst:Read[Ordering]      = Ffi.Hs.Data.Ord.Read[Ordering]
    inst:Show[Ordering]      = Ffi.Hs.Data.Ord.Show[Ordering]
    inst:Eq[Ordering]        = Ffi.Hs.Data.Ord.Eq[Ordering]
    inst:Ord[Ordering]       = Ffi.Hs.Data.Ord.Ord[Ordering]

open import Ffi.Hs.Data.Char using (Char)

instance
    inst:Data[Char]     = Ffi.Hs.Data.Char.Data[Char]
    inst:Storable[Char] = Ffi.Hs.Data.Char.Storable[Char]
    inst:Bounded[Char]  = Ffi.Hs.Data.Char.Bounded[Char]
    inst:Enum[Char]     = Ffi.Hs.Data.Char.Enum[Char]
    inst:Ix[Char]       = Ffi.Hs.Data.Char.Ix[Char]
    inst:Read[Char]     = Ffi.Hs.Data.Char.Read[Char]
    inst:Show[Char]     = Ffi.Hs.Data.Char.Show[Char]
    inst:Eq[Char]       = Ffi.Hs.Data.Char.Eq[Char]
    inst:Ord[Char]      = Ffi.Hs.Data.Char.Ord[Char]

open import Ffi.Hs.Data.String public
    using
    ( String
    ; lines
    ; words
    ; unlines
    ; unwords
    )

instance
    inst:IsString[String] = Ffi.Hs.Data.String.IsString[String]

open import Ffi.Hs.Data.Tuple public
    using (Tuple2; Tuple3; Tuple4; Tuple5; fst; snd; curry; uncurry)

open import Ffi.Hs.Data.Eq public
    using (Eq; _==_; _/=_)

open import Ffi.Hs.GHC.Enum public
    using
    ( Enum; succ; pred; toEnum; fromEnum
    ; enumFrom; enumFromThen; enumFromTo; enumFromThenTo
    ; Bounded; minBound; maxBound
    )

open import Ffi.Hs.Data.Int using (Int)

instance
    inst:Data[Int]       = Ffi.Hs.Data.Int.Data[Int]
    inst:Storable[Int]   = Ffi.Hs.Data.Int.Storable[Int]
    inst:Bits[Int]       = Ffi.Hs.Data.Int.Bits[Int]
    inst:FiniteBits[Int] = Ffi.Hs.Data.Int.FiniteBits[Int]
    inst:Bounded[Int]    = Ffi.Hs.Data.Int.Bounded[Int]
    inst:Enum[Int]       = Ffi.Hs.Data.Int.Enum[Int]
    inst:Ix[Int]         = Ffi.Hs.Data.Int.Ix[Int]
    inst:Num[Int]        = Ffi.Hs.Data.Int.Num[Int]
    inst:Read[Int]       = Ffi.Hs.Data.Int.Read[Int]
    inst:Integral[Int]   = Ffi.Hs.Data.Int.Integral[Int]
    inst:Real[Int]       = Ffi.Hs.Data.Int.Real[Int]
    inst:Show[Int]       = Ffi.Hs.Data.Int.Show[Int]
    inst:Eq[Int]         = Ffi.Hs.Data.Int.Eq[Int]
    inst:Ord[Int]        = Ffi.Hs.Data.Int.Ord[Int]

open import Ffi.Hs.GHC.Num public
    using
    ( Integer

    ; Num
    ; _+_
    ; _-_
    ; _*_
    ; abs
    ; signum
    ; fromInteger
    ; negate

    ; subtract
    )

instance
    inst:Data[Integer]     = Ffi.Hs.GHC.Num.Data[Integer]
    inst:Bits[Integer]     = Ffi.Hs.GHC.Num.Bits[Integer]
    inst:Enum[Integer]     = Ffi.Hs.GHC.Num.Enum[Integer]
    inst:Ix[Integer]       = Ffi.Hs.GHC.Num.Ix[Integer]
    inst:Num[Integer]      = Ffi.Hs.GHC.Num.Num[Integer]
    inst:Read[Integer]     = Ffi.Hs.GHC.Num.Read[Integer]
    inst:Integral[Integer] = Ffi.Hs.GHC.Num.Integral[Integer]
    inst:Real[Integer]     = Ffi.Hs.GHC.Num.Real[Integer]
    inst:Show[Integer]     = Ffi.Hs.GHC.Num.Show[Integer]
    inst:Eq[Integer]       = Ffi.Hs.GHC.Num.Eq[Integer]
    inst:Ord[Integer]      = Ffi.Hs.GHC.Num.Ord[Integer]

open import Ffi.Hs.GHC.Float public
    using
    ( Float
    ; Double

    ; Floating
    ; pi
    ; exp
    ; log
    ; sqrt
    ; _**_
    ; logBase
    ; sin
    ; cos
    ; tan
    ; asin
    ; acos
    ; atan
    ; sinh
    ; cosh
    ; tanh
    ; asinh
    ; acosh
    ; atanh

    ; RealFloat
    ; floatRadix
    ; floatDigits
    ; floatRange
    ; decodeFloat
    ; encodeFloat
    ; exponent
    ; significand
    ; scaleFloat
    ; isNaN
    ; isInfinite
    ; isDenormalized
    ; isNegativeZero
    ; isIEEE
    ; atan2
    )

instance
    inst:Data[Float]       = Ffi.Hs.GHC.Float.Data[Float]
    inst:Storable[Float]   = Ffi.Hs.GHC.Float.Storable[Float]
    inst:Enum[Float]       = Ffi.Hs.GHC.Float.Enum[Float]
    inst:Floating[Float]   = Ffi.Hs.GHC.Float.Floating[Float]
    inst:RealFloat[Float]  = Ffi.Hs.GHC.Float.RealFloat[Float]
    inst:Num[Float]        = Ffi.Hs.GHC.Float.Num[Float]
    inst:Read[Float]       = Ffi.Hs.GHC.Float.Read[Float]
    inst:Fractional[Float] = Ffi.Hs.GHC.Float.Fractional[Float]
    inst:Real[Float]       = Ffi.Hs.GHC.Float.Real[Float]
    inst:RealFrac[Float]   = Ffi.Hs.GHC.Float.RealFrac[Float]
    inst:Show[Float]       = Ffi.Hs.GHC.Float.Show[Float]
    inst:Eq[Float]         = Ffi.Hs.GHC.Float.Eq[Float]
    inst:Ord[Float]        = Ffi.Hs.GHC.Float.Ord[Float]

    inst:Data[Double]       = Ffi.Hs.GHC.Float.Data[Double]
    inst:Storable[Double]   = Ffi.Hs.GHC.Float.Storable[Double]
    inst:Enum[Double]       = Ffi.Hs.GHC.Float.Enum[Double]
    inst:Floating[Double]   = Ffi.Hs.GHC.Float.Floating[Double]
    inst:RealFloat[Double]  = Ffi.Hs.GHC.Float.RealFloat[Double]
    inst:Num[Double]        = Ffi.Hs.GHC.Float.Num[Double]
    inst:Read[Double]       = Ffi.Hs.GHC.Float.Read[Double]
    inst:Fractional[Double] = Ffi.Hs.GHC.Float.Fractional[Double]
    inst:Real[Double]       = Ffi.Hs.GHC.Float.Real[Double]
    inst:RealFrac[Double]   = Ffi.Hs.GHC.Float.RealFrac[Double]
    inst:Show[Double]       = Ffi.Hs.GHC.Float.Show[Double]
    inst:Eq[Double]         = Ffi.Hs.GHC.Float.Eq[Double]
    inst:Ord[Double]        = Ffi.Hs.GHC.Float.Ord[Double]

open import Ffi.Hs.GHC.Real public
    using
    ( Rational

    ; Real
    ; toRational

    ; Integral
    ; quot
    ; rem
    ; div
    ; mod
    ; quotRem
    ; divMod
    ; toInteger

    ; Fractional
    ; _/_
    ; recip
    ; fromRational

    ; RealFrac
    ; properFraction
    ; truncate
    ; round
    ; ceiling
    ; floor

    ; even
    ; odd
    ; gcd
    ; lcm
    ; _^_
    ; _^^_
    ; fromIntegral
    ; realToFrac
    )

instance
    inst:Data[Ratio[A]]       = Ffi.Hs.GHC.Real.Data[Ratio[A]]
    inst:Storable[Ratio[A]]   = Ffi.Hs.GHC.Real.Storable[Ratio[A]]
    inst:Enum[Ratio[A]]       = Ffi.Hs.GHC.Real.Enum[Ratio[A]]
    inst:Num[Ratio[A]]        = Ffi.Hs.GHC.Real.Num[Ratio[A]]
    inst:Read[Ratio[A]]       = Ffi.Hs.GHC.Real.Read[Ratio[A]]
    inst:Fractional[Ratio[A]] = Ffi.Hs.GHC.Real.Fractional[Ratio[A]]
    inst:Real[Ratio[A]]       = Ffi.Hs.GHC.Real.Real[Ratio[A]]
    inst:RealFrac[Ratio[A]]   = Ffi.Hs.GHC.Real.RealFrac[Ratio[A]]
    inst:Show[Ratio[A]]       = Ffi.Hs.GHC.Real.Show[Ratio[A]]
    inst:Eq[Ratio[A]]         = Ffi.Hs.GHC.Real.Eq[Ratio[A]]
    inst:Ord[Ratio[A]]        = Ffi.Hs.GHC.Real.Ord[Ratio[A]]

open import Ffi.Hs.Data.Word public
    using (Word)

instance
    inst:Data[Word]       = Ffi.Hs.Data.Word.Data[Word]
    inst:Storable[Word]   = Ffi.Hs.Data.Word.Storable[Word]
    inst:Bits[Word]       = Ffi.Hs.Data.Word.Bits[Word]
    inst:FiniteBits[Word] = Ffi.Hs.Data.Word.FiniteBits[Word]
    inst:Bounded[Word]    = Ffi.Hs.Data.Word.Bounded[Word]
    inst:Enum[Word]       = Ffi.Hs.Data.Word.Enum[Word]
    inst:Ix[Word]         = Ffi.Hs.Data.Word.Ix[Word]
    inst:Num[Word]        = Ffi.Hs.Data.Word.Num[Word]
    inst:Read[Word]       = Ffi.Hs.Data.Word.Read[Word]
    inst:Integral[Word]   = Ffi.Hs.Data.Word.Integral[Word]
    inst:Real[Word]       = Ffi.Hs.Data.Word.Real[Word]
    inst:Show[Word]       = Ffi.Hs.Data.Word.Show[Word]
    inst:Eq[Word]         = Ffi.Hs.Data.Word.Eq[Word]
    inst:Ord[Word]        = Ffi.Hs.Data.Word.Ord[Word]

open import Ffi.Hs.Data.Semigroup public
    using
    ( Semigroup
    ; _<>_
    )

open import Ffi.Hs.Data.Monoid public
    using
    ( Monoid
    ; mempty
    ; mconcat
    )

open import Ffi.Hs.Data.Functor public
    using
    ( Functor
    ; fmap
    ; _<$_
    ; _<$>_
    )

open import Ffi.Hs.Control.Applicative public
    using
    ( pure
    ; _<*>_
    ; _*>_
    ; _<*_
    )

open import Ffi.Hs.Control.Monad public
    using
    ( Monad
    ; _>>=_
    ; _>>_
    ; return

    ; _=<<_
    )

open import Ffi.Hs.Control.Monad.Fail public
    using
    ( MonadFail
    ; fail
    )

open import Ffi.Hs.Data.Foldable
    using
    ( mapM-
    ; sequence-

    ; Foldable
    ; foldMap
    ; foldr
    ; foldl
    ; foldr1
    ; foldl1
    ; elem
    ; maximum
    ; minimum
    ; sum
    ; product

    ; null
    ; length
    ; and
    ; or
    ; any
    ; all
    ; concat
    ; concatMap
    ; notElem
    )

-- todo: Traversable, traverse, sequenceA, mapM, sequence

open import Ffi.Hs.Data.Function public
    using
    ( id
    ; const
    ; _∘_
    ; flip
    ; _$_
    )

-- todo: until, asTypeOf, error, errorWithoutStackTrace, undefined, seq

import Agda.Builtin.Strict

_$!_ : ∀{aℓ bℓ} {A : Set aℓ} {B : A → Set bℓ} → (∀ x → B x) → (x : A) → B x
f $! x = Agda.Builtin.Strict.primForce x f

open import Ffi.Hs.Data.List public
    using
    ( List
    ; map
    ; _++_
    ; filter
    ; head
    ; last
    ; tail
    ; init
    ; _!!_
    ; reverse
    ; scanl
    ; scanl1
    ; scanr
    ; scanr1
    ; iterate
    ; repeat
    ; replicate
    ; cycle
    ; take
    ; drop
    ; takeWhile
    ; dropWhile
    ; span
    ; break
    ; splitAt
    ; lookup
    ; zip
    ; zip3
    ; zipWith
    ; zipWith3
    ; unzip
    ; unzip3
    )

instance
    inst:Functor[List]     = Ffi.Hs.Data.List.Functor[List]
    inst:Applicative[List] = Ffi.Hs.Data.List.Applicative[List]
    inst:Alternative[List] = Ffi.Hs.Data.List.Alternative[List]
    inst:Monad[List]       = Ffi.Hs.Data.List.Monad[List]
    inst:MonadPlus[List]   = Ffi.Hs.Data.List.MonadPlus[List]
    inst:Foldable[List]    = Ffi.Hs.Data.List.Foldable[List]
    inst:Traversable[List] = Ffi.Hs.Data.List.Traversable[List]
    inst:Semigroup[List]   = Ffi.Hs.Data.List.Semigroup[List]
    inst:Monoid[List]      = Ffi.Hs.Data.List.Monoid[List]
    inst:Data[List[A]]     = Ffi.Hs.Data.List.Data[List[A]]
    inst:Eq[List[A]]       = Ffi.Hs.Data.List.Eq[List[A]]
    inst:Ord[List[A]]      = Ffi.Hs.Data.List.Ord[List[A]]
    inst:Show[List[A]]     = Ffi.Hs.Data.List.Show[List[A]]
    inst:Read[List[A]]     = Ffi.Hs.Data.List.Read[List[A]]
    inst:Eq1[List]         = Ffi.Hs.Data.List.Eq1[List]
    inst:Ord1[List]        = Ffi.Hs.Data.List.Ord1[List]
    inst:Read1[List]       = Ffi.Hs.Data.List.Read1[List]
    inst:Show1[List]       = Ffi.Hs.Data.List.Show1[List]

open import Ffi.Hs.Text.Show public
    using
    ( ShowS

    ; Show
    ; showsPrec
    ; show
    ; showList

    ; shows
    ; showChar
    ; showString
    ; showParen
    )

open import Ffi.Hs.Text.Read public
    using
    ( ReadS

    ; Read
    ; readsPrec
    ; readList
    ; reads
    ; readParen
    ; read
    ; lex
    )

open import Ffi.Hs.System.IO public
    using
    ( IO
    ; putChar
    ; putStr
    ; putStrLn
    ; print
    ; getChar
    ; getLine
    ; getContents
    ; interact
    ; FilePath
    ; readFile
    ; writeFile
    ; appendFile
    ; readIO
    ; readLn
    )

instance
    Functor[IO]      = Ffi.Hs.System.IO.Functor[IO]
    Applicative[IO]  = Ffi.Hs.System.IO.Applicative[IO]
    Alternative[IO]  = Ffi.Hs.System.IO.Alternative[IO]
    Monad[IO]        = Ffi.Hs.System.IO.Monad[IO]
    MonadFail[IO]    = Ffi.Hs.System.IO.MonadFail[IO]
    MonadFix[IO]     = Ffi.Hs.System.IO.MonadFix[IO]
    MonadIO[IO]      = Ffi.Hs.System.IO.MonadIO[IO]
    MonadPlus[IO]    = Ffi.Hs.System.IO.MonadPlus[IO]
    Semigroup[IO[A]] = Ffi.Hs.System.IO.Semigroup[IO[A]]
    Monoid[IO[A]]    = Ffi.Hs.System.IO.Monoid[IO[A]]

open import Ffi.Hs.System.IO.Error public
    using
    ( IOError
    ; ioError
    ; userError
    )

open import Ffi.Hs.Data.Type.Equality public
    using
    ( _~_
    )
