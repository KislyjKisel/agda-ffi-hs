{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Arrow where

open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Unit       using (⊤)
open import Ffi.Hs.Control.Category using (id; _>>>_; _<<<_)
open import Ffi.Hs.Data.Either      using (Either)
open import Ffi.Hs.Data.Tuple       using (Tuple2)

{-# FOREIGN GHC import qualified Control.Arrow #-}
{-# FOREIGN GHC import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaArrow) #-}

private
    variable
        aℓ bℓ cℓ dℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ
        D : Set dℓ
        Cat : Set aℓ → Set bℓ → Set cℓ
        M : Set aℓ → Set aℓ


infixr 5 _<+>_
infixr 3 _***_ _&&&_
infixr 2 _+++_ _|||_

postulate
    arr    : ⦃ Arrow Cat ⦄ → (A → B) → Cat A B
    first  : ⦃ Arrow {aℓ} {aℓ} Cat ⦄ → Cat A B → Cat (Tuple2 {_} {aℓ} A C) (Tuple2 B C)
    second : ⦃ Arrow {aℓ} {aℓ} Cat ⦄ → Cat A B → Cat (Tuple2 {aℓ} C A) (Tuple2 C B)
    _***_  : ⦃ Arrow Cat ⦄ → Cat A B → Cat C D → Cat (Tuple2 A C) (Tuple2 B D)
    _&&&_  : ⦃ Arrow Cat ⦄ → Cat A B → Cat A C → Cat A (Tuple2 B C)

{-# COMPILE GHC arr    = \ aℓ bℓ cℓ cat a b     AgdaArrow -> Control.Arrow.arr    #-}
{-# COMPILE GHC first  = \ aℓ bℓ cat a b c      AgdaArrow -> Control.Arrow.first  #-}
{-# COMPILE GHC second = \ aℓ bℓ cat a b c      AgdaArrow -> Control.Arrow.second #-}
{-# COMPILE GHC _***_  = \ aℓ bℓ cℓ cat a b c d AgdaArrow -> (Control.Arrow.***)  #-}
{-# COMPILE GHC _&&&_  = \ aℓ bℓ cℓ cat a b c   AgdaArrow -> (Control.Arrow.&&&)  #-}

postulate
    zeroArrow : ⦃ ArrowZero Cat ⦄ → Cat A B
    _<+>_ : ⦃ ArrowPlus Cat ⦄ → Cat A B → Cat A B → Cat A B
    left : ⦃ ArrowChoice {aℓ} {aℓ} Cat ⦄ → Cat A B → Cat (Either {aℓ} {aℓ} A C) (Either B C)
    right : ⦃ ArrowChoice {aℓ} {aℓ} Cat ⦄ → Cat A B → Cat (Either {aℓ} C A) (Either C B)
    _+++_ : ⦃ ArrowChoice Cat ⦄ → Cat A B → Cat C D → Cat (Either A C) (Either B D)
    _|||_ : ⦃ ArrowChoice Cat ⦄ → Cat A C → Cat B C → Cat (Either A B) C
    app : ⦃ ArrowApply {aℓ} {bℓ} {aℓ} Cat ⦄ → Cat (Tuple2 (Cat A B) A) B
    leftApp : ⦃ ArrowApply {aℓ} {aℓ} Cat ⦄ → Cat A B → Cat (Either {aℓ} {aℓ} A C) (Either B C)
    loop : ⦃ ArrowLoop {aℓ} {aℓ} Cat ⦄ → Cat (Tuple2 {aℓ} {aℓ} A C) (Tuple2 B C) → Cat A B

{-# COMPILE GHC zeroArrow = \ aℓ bℓ cℓ cat a b     AgdaArrowZero   -> Control.Arrow.zeroArrow  #-}
{-# COMPILE GHC _<+>_     = \ aℓ bℓ cℓ cat a b     AgdaArrowPlus   -> (Control.Arrow.<+>)      #-}
{-# COMPILE GHC left      = \ aℓ cℓ cat a b        AgdaArrowChoice -> Control.Arrow.left       #-}
{-# COMPILE GHC right     = \ aℓ cℓ cat a b        AgdaArrowChoice -> Control.Arrow.right      #-}
{-# COMPILE GHC _+++_     = \ aℓ bℓ cℓ cat a b c d AgdaArrowChoice -> (Control.Arrow.+++)      #-}
{-# COMPILE GHC _|||_     = \ aℓ bℓ cℓ cat a b c   AgdaArrowChoice -> (Control.Arrow.|||)      #-}
{-# COMPILE GHC app       = \ aℓ bℓ cat a b        AgdaArrowApply  -> Control.Arrow.app        #-}
{-# COMPILE GHC leftApp   = \ aℓ cℓ cat a b c      AgdaArrowApply  -> Control.Arrow.leftApp    #-}
{-# COMPILE GHC loop      = \ aℓ cℓ cat a b c      AgdaArrowLoop   -> Control.Arrow.loop       #-}

data ArrowMonad (Cat : Set aℓ → Set bℓ → Set cℓ) (B : Set bℓ) : Set (aℓ ⊔ bℓ ⊔ cℓ) where
    mkArrowMonad : Cat ⊤ B → ArrowMonad Cat B

{-# FOREIGN GHC type AgdaArrowMonad aℓ bℓ cℓ = Control.Arrow.ArrowMonad #-}
{-# COMPILE GHC ArrowMonad = data(3) AgdaArrowMonad (Control.Arrow.ArrowMonad) #-}

record Kleisli (M : Set bℓ → Set bℓ) (A : Set aℓ) (B : Set bℓ) : Set (aℓ ⊔ bℓ) where
    constructor mkKleisli
    field
        runKleisli : A → M B

{-# FOREIGN GHC type AgdaKleisli bℓ aℓ = Control.Arrow.Kleisli #-}
{-# COMPILE GHC Kleisli = data(2) AgdaKleisli (Control.Arrow.Kleisli) #-}

module Instances where
    postulate
        Arrow[⟶]       : Arrow {aℓ} {bℓ} (\ a b → (a → b))
        ArrowChoice[⟶] : ArrowChoice {aℓ} {bℓ} (\ a b → (a → b))
        ArrowApply[⟶]  : ArrowApply {aℓ} {bℓ} (\ a b → (a → b))
        ArrowLoop[⟶]   : ArrowLoop {aℓ} {bℓ} (\ a b → (a → b))

        Arrow[Kleisli[M]]         : ⦃ Monad M ⦄       → Arrow {aℓ} (Kleisli M)
        Category[Kleisli[M]]      : ⦃ Monad M ⦄       → Category {aℓ} (Kleisli M)
        ArrowApply[Kleisli[M]]    : ⦃ Monad M ⦄       → ArrowApply {aℓ} (Kleisli M)
        ArrowChoice[Kleisli[M]]   : ⦃ Monad M ⦄       → ArrowChoice {aℓ} (Kleisli M)
        ArrowLoop[Kleisli[M]]     : ⦃ MonadFix M ⦄    → ArrowLoop {aℓ} (Kleisli M)
        ArrowPlus[Kleisli[M]]     : ⦃ MonadPlus M ⦄   → ArrowPlus {aℓ} (Kleisli M)
        ArrowZero[Kleisli[M]]     : ⦃ MonadPlus M ⦄   → ArrowZero {aℓ} (Kleisli M)
        Alternative[Kleisli[M,A]] : ⦃ Alternative M ⦄ → Alternative (Kleisli {aℓ} {aℓ} M A)
        Applicative[Kleisli[M,A]] : ⦃ Applicative M ⦄ → Applicative (Kleisli {aℓ} {aℓ} M A)
        Functor[Kleisli[M,A]]     : ⦃ Functor M ⦄     → Functor (Kleisli {aℓ} {aℓ} M A)
        Monad[Kleisli[M,A]]       : ⦃ Monad M ⦄       → Monad (Kleisli {aℓ} {aℓ} M A)
        MonadPlus[Kleisli[M,A]]   : ⦃ MonadPlus M ⦄   → MonadPlus (Kleisli {aℓ} {aℓ} M A)

        Functor[ArrowMonad[Cat]]      : ⦃ Arrow      {aℓ   } {aℓ} {aℓ} Cat ⦄ → Functor (ArrowMonad Cat)
        Functor[ArrowMonad[Cat]]'     : ⦃ Arrow      {lzero} {aℓ} {aℓ} Cat ⦄ → Functor (ArrowMonad Cat)
        Applicative[ArrowMonad[Cat]]  : ⦃ Arrow      {aℓ   } {aℓ} {aℓ} Cat ⦄ → Applicative (ArrowMonad Cat)
        Applicative[ArrowMonad[Cat]]' : ⦃ Arrow      {lzero} {aℓ} {aℓ} Cat ⦄ → Applicative (ArrowMonad Cat)
        Alternative[ArrowMonad[Cat]]  : ⦃ ArrowPlus  {aℓ   } {aℓ} {aℓ} Cat ⦄ → Alternative (ArrowMonad Cat)
        Alternative[ArrowMonad[Cat]]' : ⦃ ArrowPlus  {lzero} {aℓ} {aℓ} Cat ⦄ → Alternative (ArrowMonad Cat)
        Monad[ArrowMonad[Cat]]        : ⦃ ArrowApply {aℓ   } {aℓ} {aℓ} Cat ⦄ → Monad (ArrowMonad Cat)
        Monad[ArrowMonad[Cat]]'       : ⦃ ArrowApply {lzero} {aℓ} {aℓ} Cat ⦄ → Monad (ArrowMonad Cat)
        MonadPlus[ArrowMonad[Cat]]  : ⦃ ArrowApply {aℓ   } {aℓ} {aℓ} Cat ⦄ → ⦃ ArrowPlus Cat ⦄ → MonadPlus (ArrowMonad Cat)
        MonadPlus[ArrowMonad[Cat]]' : ⦃ ArrowApply {lzero} {aℓ} {aℓ} Cat ⦄ → ⦃ ArrowPlus Cat ⦄ → MonadPlus (ArrowMonad Cat)

        Arrow[Cat]⇒Category[Cat]      : ⦃ Arrow Cat ⦄       → Category Cat
        ArrowZero[Cat]⇒Arrow[Cat]     : ⦃ ArrowZero Cat ⦄   → Arrow Cat
        ArrowPlus[Cat]⇒ArrowZero[Cat] : ⦃ ArrowPlus Cat ⦄   → ArrowZero Cat
        ArrowChoice[Cat]⇒Arrow[Cat]   : ⦃ ArrowChoice Cat ⦄ → Arrow Cat
        ArrowApply[Cat]⇒Arrow[Cat]    : ⦃ ArrowApply Cat ⦄  → Arrow Cat
        ArrowLoop[Cat]⇒Arrow[Cat]     : ⦃ ArrowLoop Cat ⦄   → Arrow Cat

-- todo: compile instances

returnA : ⦃ Arrow Cat ⦄ → Cat A A
returnA = id ⦃ Instances.Arrow[Cat]⇒Category[Cat] ⦄

module _ ⦃ Arrow[Cat] : Arrow {aℓ} {aℓ} Cat ⦄ where

    private
        instance _ = Instances.Arrow[Cat]⇒Category[Cat]

    infixr 1 _^>>_ _>>^_ _<<^_ _^<<_

    _^>>_ : (A → B) → Cat B C → Cat A C
    f ^>> a = arr f >>> a 

    _>>^_ : Cat A B → (B → C) → Cat A C
    a >>^ f = a >>> arr f

    _<<^_ : Cat B C → (A → B) → Cat A C
    a <<^ f = a <<< arr f

    _^<<_ : (B → C) → Cat A B → Cat A C
    f ^<< a = arr f <<< a
 