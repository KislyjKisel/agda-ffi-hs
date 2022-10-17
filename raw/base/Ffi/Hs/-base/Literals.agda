{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Literals where

open import Agda.Builtin.Nat  using (Nat)
open import Agda.Builtin.Unit using (⊤)

open import Agda.Builtin.FromString public
    using ()
    renaming
    ( IsString to Lit-FromText
    ; fromString to Lit-fromText-i
    )

open Lit-FromText public
    using ()
    renaming
    ( Constraint to Lit-ConstrainText
    ; fromString to Lit-fromText
    )

open import Agda.Builtin.FromNat public
    using ()
    renaming
    ( Number to Lit-FromNat
    ; fromNat to Lit-fromNat-i
    )

open Lit-FromNat public
    using ()
    renaming
    ( Constraint to Lit-ConstrainNat
    ; fromNat to Lit-fromNat
    )

open import Agda.Builtin.FromNeg public
    using ()
    renaming
    ( Negative to Lit-FromNeg
    ; fromNeg to Lit-fromNeg-i
    )

open Lit-FromNeg public
    using ()
    renaming
    ( Constraint to Lit-ConstrainNeg
    ; fromNeg to Lit-fromNeg
    )


instance
    Lit-FromNat[ℕ] : Lit-FromNat Nat
    Lit-FromNat[ℕ] .Lit-ConstrainNat n = ⊤
    Lit-FromNat[ℕ] .Lit-fromNat      n = n
