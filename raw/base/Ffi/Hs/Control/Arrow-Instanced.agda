{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Arrow-Instanced where

open import Ffi.Hs.Control.Arrow

instance
    inst:Arrow[Kleisli[M]]         = Arrow[Kleisli[M]]
    inst:Category[Kleisli[M]]      = Category[Kleisli[M]]
    inst:ArrowApply[Kleisli[M]]    = ArrowApply[Kleisli[M]]
    inst:ArrowChoice[Kleisli[M]]   = ArrowChoice[Kleisli[M]]
    inst:ArrowLoop[Kleisli[M]]     = ArrowLoop[Kleisli[M]]
    inst:ArrowPlus[Kleisli[M]]     = ArrowPlus[Kleisli[M]]
    inst:ArrowZero[Kleisli[M]]     = ArrowZero[Kleisli[M]]
    inst:Alternative[Kleisli[M,A]] = Alternative[Kleisli[M,A]]
    inst:Applicative[Kleisli[M,A]] = Applicative[Kleisli[M,A]]
    inst:Functor[Kleisli[M,A]]     = Functor[Kleisli[M,A]]
    inst:Monad[Kleisli[M,A]]       = Monad[Kleisli[M,A]]
    inst:MonadPlus[Kleisli[M,A]]   = MonadPlus[Kleisli[M,A]]

    inst:Functor[ArrowMonad[Cat]]     = Functor[ArrowMonad[Cat]]
    inst:Applicative[ArrowMonad[Cat]] = Applicative[ArrowMonad[Cat]]
    inst:Alternative[ArrowMonad[Cat]] = Alternative[ArrowMonad[Cat]]
    inst:Monad[ArrowMonad[Cat]]       = Monad[ArrowMonad[Cat]]
    inst:MonadPlus[ArrowMonad[Cat]]   = MonadPlus[ArrowMonad[Cat]]

    inst:Arrow[Cat]⇒Category[Cat]      = Arrow[Cat]⇒Category[Cat]
    inst:ArrowZero[Cat]⇒Arrow[Cat]     = ArrowZero[Cat]⇒Arrow[Cat]
    inst:ArrowPlus[Cat]⇒ArrowZero[Cat] = ArrowPlus[Cat]⇒ArrowZero[Cat]
    inst:ArrowChoice[Cat]⇒Arrow[Cat]   = ArrowChoice[Cat]⇒Arrow[Cat]
    inst:ArrowApply[Cat]⇒Arrow[Cat]    = ArrowApply[Cat]⇒Arrow[Cat]
    inst:ArrowLoop[Cat]⇒Arrow[Cat]     = ArrowLoop[Cat]⇒Arrow[Cat]
