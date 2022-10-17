{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Literals where

open import Agda.Builtin.FromString public
    using ()
    renaming
    ( IsString to Lit-FromText
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
    )

open Lit-FromNat public
    using ()
    renaming
    ( Constraint to Lit-ConstrainNat
    ; fromNat to Lit-fromNat
    )
