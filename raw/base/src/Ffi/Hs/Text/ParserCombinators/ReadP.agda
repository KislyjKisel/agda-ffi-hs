{-# OPTIONS --without-K #-}

module Ffi.Hs.Text.ParserCombinators.ReadP where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Functor; Applicative; Alternative; Monad; MonadFail; MonadPlus)
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Tuple  using (Tuple2)

{-# FOREIGN GHC
import qualified Text.ParserCombinators.ReadP as AgdaHsReadP
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaFunctor, AgdaApplicative, AgdaAlternative
    , AgdaMonad, AgdaMonadFail, AgdaMonadPlus
    )
#-}

private
    variable
        aℓ bℓ cℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ

ReadS : Set aℓ → Set aℓ
ReadS A = List Char → List (Tuple2 A (List Char))

postulate
    ReadP : Set aℓ → Set aℓ

    get    : ReadP Char
    look   : ReadP (List Char)
    _+++_  : ReadP A → ReadP A → ReadP A
    _<++_  : ReadP A → ReadP A → ReadP A
    gather : ReadP A → ReadP (Tuple2 (List Char) A)

    pfail      : ReadP A
    eof        : ∀{ℓ} → ReadP (⊤ {ℓ})
    satisfy    : (Char → Bool) → ReadP Char
    char       : Char → ReadP Char
    string     : List Char → ReadP (List Char)
    munch      : (Char → Bool) → ReadP (List Char)
    munch1     : (Char → Bool) → ReadP (List Char)
    skipSpaces : ReadP (⊤ {lzero})
    choice     : List (ReadP A) → ReadP A
    count      : Int → ReadP A → ReadP (List A)
    between    : ReadP A → ReadP B → ReadP C → ReadP C
    option     : A → ReadP A → ReadP A
    optional   : ∀{ℓ} → ReadP A → ReadP (⊤ {ℓ})
    many       : ReadP A → ReadP (List A)
    many1      : ReadP A → ReadP (List A)
    skipMany   : ∀{ℓ} → ReadP A → ReadP (⊤ {ℓ})
    skipMany1  : ∀{ℓ} → ReadP A → ReadP (⊤ {ℓ})
    sepBy      : ReadP A → ReadP B → ReadP (List A)
    sepBy1     : ReadP A → ReadP B → ReadP (List A)
    endBy      : ReadP A → ReadP B → ReadP (List A)
    endBy1     : ReadP A → ReadP B → ReadP (List A)
    chainr     : ReadP A → ReadP (A → A → A) → A → ReadP A
    chainl     : ReadP A → ReadP (A → A → A) → A → ReadP A
    chainl1    : ReadP A → ReadP (A → A → A) → ReadP A
    chainr1    : ReadP A → ReadP (A → A → A) → ReadP A
    manyTill   : ReadP A → ReadP B → ReadP (List A)

    readP-to-S : ReadP A → ReadS A
    readS-to-P : ReadS A → ReadP A

    -- todo: properties?

{-# FOREIGN GHC type AgdaReadP aℓ = AgdaHsReadP.ReadP #-}
{-# COMPILE GHC ReadP = type(1) AgdaReadP #-}

{-# COMPILE GHC get    = AgdaHsReadP.get              #-}
{-# COMPILE GHC look   = AgdaHsReadP.look             #-}
{-# COMPILE GHC _+++_  = \ aℓ a -> (AgdaHsReadP.+++)  #-}
{-# COMPILE GHC _<++_  = \ aℓ a -> (AgdaHsReadP.<++)  #-}
{-# COMPILE GHC gather = \ aℓ a -> AgdaHsReadP.gather #-}

{-# COMPILE GHC pfail      = \ aℓ a           -> AgdaHsReadP.pfail      #-}
{-# COMPILE GHC eof        = \ ℓ              -> AgdaHsReadP.eof        #-}
{-# COMPILE GHC satisfy    =                     AgdaHsReadP.satisfy    #-}
{-# COMPILE GHC char       =                     AgdaHsReadP.char       #-}
{-# COMPILE GHC string     =                     AgdaHsReadP.string     #-}
{-# COMPILE GHC munch      =                     AgdaHsReadP.munch      #-}
{-# COMPILE GHC munch1     =                     AgdaHsReadP.munch1     #-}
{-# COMPILE GHC skipSpaces =                     AgdaHsReadP.skipSpaces #-}
{-# COMPILE GHC choice     = \ aℓ a           -> AgdaHsReadP.choice     #-}
{-# COMPILE GHC count      = \ aℓ a           -> AgdaHsReadP.count      #-}
{-# COMPILE GHC between    = \ aℓ a bℓ b cℓ c -> AgdaHsReadP.between    #-}
{-# COMPILE GHC option     = \ aℓ a           -> AgdaHsReadP.option     #-}
{-# COMPILE GHC optional   = \ ℓ aℓ a         -> AgdaHsReadP.optional   #-}
{-# COMPILE GHC many       = \ aℓ a           -> AgdaHsReadP.many       #-}
{-# COMPILE GHC many1      = \ aℓ a           -> AgdaHsReadP.many1      #-}
{-# COMPILE GHC skipMany   = \ ℓ aℓ a         -> AgdaHsReadP.skipMany   #-}
{-# COMPILE GHC skipMany1  = \ ℓ aℓ a         -> AgdaHsReadP.skipMany1  #-}
{-# COMPILE GHC sepBy      = \ aℓ a bℓ b      -> AgdaHsReadP.sepBy      #-}
{-# COMPILE GHC sepBy1     = \ aℓ a bℓ b      -> AgdaHsReadP.sepBy1     #-}
{-# COMPILE GHC endBy      = \ aℓ a bℓ b      -> AgdaHsReadP.endBy      #-}
{-# COMPILE GHC endBy1     = \ aℓ a bℓ b      -> AgdaHsReadP.endBy1     #-}
{-# COMPILE GHC chainr     = \ aℓ a           -> AgdaHsReadP.chainr     #-}
{-# COMPILE GHC chainl     = \ aℓ a           -> AgdaHsReadP.chainl     #-}
{-# COMPILE GHC chainl1    = \ aℓ a           -> AgdaHsReadP.chainl1    #-}
{-# COMPILE GHC chainr1    = \ aℓ a           -> AgdaHsReadP.chainr1    #-}
{-# COMPILE GHC manyTill   = \ aℓ a bℓ b      -> AgdaHsReadP.manyTill   #-}

{-# COMPILE GHC readP-to-S = \ aℓ a -> AgdaHsReadP.readP-to-S #-}
{-# COMPILE GHC readS-to-P = \ aℓ a -> AgdaHsReadP.readS-to-P #-}

module Instances where
    postulate
        Functor[ReadP]     : Functor {aℓ} ReadP
        Applicative[ReadP] : Applicative {aℓ} ReadP
        Alternative[ReadP] : Alternative {aℓ} ReadP
        Monad[ReadP]       : Monad {aℓ} ReadP
        MonadFail[ReadP]   : MonadFail {aℓ} ReadP
        MonadPlus[ReadP]   : MonadPlus {aℓ} ReadP

{-# COMPILE GHC Instances.Functor[ReadP]     = \ aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC Instances.Applicative[ReadP] = \ aℓ -> AgdaApplicative #-}
{-# COMPILE GHC Instances.Alternative[ReadP] = \ aℓ -> AgdaAlternative #-}
{-# COMPILE GHC Instances.Monad[ReadP]       = \ aℓ -> AgdaMonad       #-}
{-# COMPILE GHC Instances.MonadFail[ReadP]   = \ aℓ -> AgdaMonadFail   #-}
{-# COMPILE GHC Instances.MonadPlus[ReadP]   = \ aℓ -> AgdaMonadPlus   #-}
