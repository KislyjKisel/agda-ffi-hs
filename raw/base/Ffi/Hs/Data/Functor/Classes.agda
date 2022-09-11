{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor.Classes where

open import Agda.Builtin.Bool                      using (Bool)
open import Agda.Builtin.Char                      using (Char)
open import Agda.Builtin.List                      using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class                     using (Eq; Ord; Read; Show)
open import Ffi.Hs.Data.Int                        using (Int)
open import Ffi.Hs.Data.Ord                        using (Ordering)
open import Ffi.Hs.Text.ParserCombinators.ReadP    using (ReadS)
open import Ffi.Hs.Text.ParserCombinators.ReadPrec using (ReadPrec)
open import Ffi.Hs.Text.Show                       using (ShowS)

open Ffi.Hs.-base.Class public
    using (Eq1; Ord1; Read1; Show1; Eq2; Ord2; Read2; Show2)

{-# FOREIGN GHC
import qualified Data.Functor.Classes
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaEq, AgdaOrd, AgdaRead, AgdaShow,
    , AgdaEq1, AgdaOrd1, AgdaRead1, AgdaShow1
    , AgdaEq2, AgdaOrd2, AgdaRead2, AgdaShow2
    )
#-}

private
    variable
        aℓ bℓ cℓ : Level
        A B C D : Set aℓ

module _ where

    private
        variable
            F : Set aℓ → Set bℓ

    postulate
        liftEq : ⦃ Eq1 F ⦄ → (A → B → Bool) → F A → F B → Bool
        eq1 : ⦃ Eq1 F ⦄ → ⦃ Eq A ⦄ → F A → F A → Bool

        liftCompare : ⦃ Ord1 F ⦄ → (A → B → Ordering) → F A → F B → Ordering
        compare1 : ⦃ Ord1 F ⦄ → ⦃ Ord A ⦄ → F A → F A → Ordering
        Ord1[F]⇒Eq1[F] : ⦃ Ord1 F ⦄ → Eq1 F

        liftReadsPrec    : ⦃ Read1 F ⦄ → (Int → ReadS A) → ReadS (List A) → Int → ReadS (F A)
        liftReadList     : ⦃ Read1 F ⦄ → (Int → ReadS A) → ReadS (List A) → ReadS (List (F A))
        liftReadPrec     : ⦃ Read1 F ⦄ → ReadPrec A → ReadPrec (List A) → ReadPrec (F A)
        liftReadListPrec : ⦃ Read1 F ⦄ → ReadPrec A → ReadPrec (List A) → ReadPrec (List (F A))
        readsPrec1              : ⦃ Read1 F ⦄ → ⦃ Read A ⦄ → Int → ReadS (F A)
        readPrec1               : ⦃ Read1 F ⦄ → ⦃ Read A ⦄ → ReadPrec (F A)
        liftReadListDefault     : ⦃ Read1 F ⦄ → (Int → ReadS A) → ReadS (List A) → ReadS (List (F A))
        liftReadListPrecDefault : ⦃ Read1 F ⦄ → ReadPrec A → ReadPrec (List A) → ReadPrec (List (F A))

        liftShowsPrec : ⦃ Show1 F ⦄ → (Int → A → ShowS) → (List A → ShowS) → Int → F A → ShowS
        liftShowList  : ⦃ Show1 F ⦄ → (Int → A → ShowS) → (List A → ShowS) → List (F A) → ShowS
        showsPrec1 : ⦃ Show1 F ⦄ → ⦃ Show A ⦄ → Int → F A → ShowS

{-# COMPILE GHC liftEq = \ aℓ bℓ f a b AgdaEq1      -> Data.Functor.Classes.liftEq #-}
{-# COMPILE GHC eq1    = \ aℓ bℓ f a AgdaEq1 AgdaEq -> Data.Functor.Classes.eq1    #-}

{-# COMPILE GHC liftCompare = \ aℓ bℓ f a b AgdaOrd1       -> Data.Functor.Classes.liftCompare #-}
{-# COMPILE GHC compare1    = \ aℓ bℓ f a AgdaOrd1 AgdaOrd -> Data.Functor.Classes.compare1    #-}
{-# COMPILE GHC Ord1[F]⇒Eq1[F] = \ aℓ bℓ f AgdaOrd1 -> AgdaEq1 #-}

{-# COMPILE GHC liftReadsPrec           = \ aℓ bℓ f a AgdaRead1          -> Data.Functor.Classes.liftReadsPrec           #-}
{-# COMPILE GHC liftReadList            = \ aℓ bℓ f a AgdaRead1          -> Data.Functor.Classes.liftReadList            #-}
{-# COMPILE GHC liftReadPrec            = \ aℓ bℓ f a AgdaRead1          -> Data.Functor.Classes.liftReadPrec            #-}
{-# COMPILE GHC liftReadListPrec        = \ aℓ bℓ f a AgdaRead1          -> Data.Functor.Classes.liftReadListPrec        #-}
{-# COMPILE GHC readsPrec1              = \ aℓ bℓ f a AgdaRead1 AgdaRead -> Data.Functor.Classes.readsPrec1              #-}
{-# COMPILE GHC readPrec1               = \ aℓ bℓ f a AgdaRead1 AgdaRead -> Data.Functor.Classes.readPrec1               #-}
{-# COMPILE GHC liftReadListDefault     = \ aℓ bℓ f a AgdaRead1          -> Data.Functor.Classes.liftReadListDefault     #-}
{-# COMPILE GHC liftReadListPrecDefault = \ aℓ bℓ f a AgdaRead1          -> Data.Functor.Classes.liftReadListPrecDefault #-}

{-# COMPILE GHC liftShowsPrec = \ aℓ bℓ f a AgdaShow1          -> Data.Functor.Classes.liftShowsPrec #-}
{-# COMPILE GHC liftShowList  = \ aℓ bℓ f a AgdaShow1          -> Data.Functor.Classes.liftShowList  #-}
{-# COMPILE GHC showsPrec1    = \ aℓ bℓ f a AgdaShow1 AgdaShow -> Data.Functor.Classes.showsPrec1    #-}

module _ where

    private
        variable
            F : Set aℓ → Set bℓ → Set cℓ

    postulate
        liftEq2 : ⦃ Eq2 F ⦄ → (A → B → Bool) → (C → D → Bool) → F A C → F B D → Bool
        eq2 : ⦃ Eq2 F ⦄ → ⦃ Eq A ⦄ → ⦃ Eq B ⦄ → F A B → F A B → Bool

        liftCompare2 : ⦃ Ord2 F ⦄ → (A → B → Ordering) → (C → D → Ordering) → F A C → F B D → Ordering
        compare2 : ⦃ Ord2 F ⦄ → ⦃ Ord A ⦄ → ⦃ Ord B ⦄ → F A B → F A B → Ordering
        Ord2[F]⇒Eq2[F] : ⦃ Ord2 F ⦄ → Eq2 F

        liftReadsPrec2    : ⦃ Read2 F ⦄ → (Int → ReadS A) → ReadS (List A) → (Int → ReadS B) → ReadS (List B) → Int → ReadS (F A B)
        liftReadList2     : ⦃ Read2 F ⦄ → (Int → ReadS A) → ReadS (List A) → (Int → ReadS B) → ReadS (List B) → ReadS (List (F A B))
        liftReadPrec2     : ⦃ Read2 F ⦄ → ReadPrec A → ReadPrec (List A) → ReadPrec B → ReadPrec (List B) → ReadPrec (F A B)
        liftReadListPrec2 : ⦃ Read2 F ⦄ → ReadPrec A → ReadPrec (List A) → ReadPrec B → ReadPrec (List B) → ReadPrec (List (F A B))
        readsPrec2               : ⦃ Read2 F ⦄ → ⦃ Read A ⦄ → ⦃ Read B ⦄ → Int → ReadS (F A B)
        readPrec2                : ⦃ Read2 F ⦄ → ⦃ Read A ⦄ → ⦃ Read B ⦄ → ReadPrec (F A B)
        liftReadList2Default     : ⦃ Read2 F ⦄ → (Int → ReadS A) → ReadS (List A) → (Int → ReadS B) → ReadS (List B) → ReadS (List (F A B))
        liftReadListPrec2Default : ⦃ Read2 F ⦄ → ReadPrec A → ReadPrec (List A) → ReadPrec B → ReadPrec (List B) → ReadPrec (List (F A B))

        liftShowsPrec2 : ⦃ Show2 F ⦄ → (Int → A → ShowS) → (List A → ShowS) → (Int → B → ShowS) → (List B → ShowS) → Int → F A B → ShowS
        liftShowList2  : ⦃ Show2 F ⦄ → (Int → A → ShowS) → (List A → ShowS) → (Int → B → ShowS) → (List B → ShowS) → List (F A B) → ShowS
        showsPrec2 : ⦃ Show2 F ⦄ → ⦃ Show A ⦄ → ⦃ Show B ⦄ → Int → F A B → ShowS

{-# COMPILE GHC liftEq2 = \ aℓ bℓ cℓ f a b c d AgdaEq2           -> Data.Functor.Classes.liftEq2 #-}
{-# COMPILE GHC eq2     = \ aℓ bℓ cℓ f a b AgdaEq2 AgdaEq AgdaEq -> Data.Functor.Classes.eq2     #-}

{-# COMPILE GHC liftCompare2 = \ aℓ bℓ cℓ f a b c d AgdaOrd2             -> Data.Functor.Classes.liftCompare2 #-}
{-# COMPILE GHC compare2     = \ aℓ bℓ cℓ f a b AgdaOrd2 AgdaOrd AgdaOrd -> Data.Functor.Classes.compare2     #-}
{-# COMPILE GHC Ord2[F]⇒Eq2[F] = \ aℓ bℓ cℓ f AgdaOrd2 -> AgdaEq2 #-}

{-# COMPILE GHC liftReadsPrec2           = \ aℓ bℓ cℓ f a b AgdaRead2                   -> Data.Functor.Classes.liftReadsPrec2           #-}
{-# COMPILE GHC liftReadList2            = \ aℓ bℓ cℓ f a b AgdaRead2                   -> Data.Functor.Classes.liftReadList2            #-}
{-# COMPILE GHC liftReadPrec2            = \ aℓ bℓ cℓ f a b AgdaRead2                   -> Data.Functor.Classes.liftReadPrec2            #-}
{-# COMPILE GHC liftReadListPrec2        = \ aℓ bℓ cℓ f a b AgdaRead2                   -> Data.Functor.Classes.liftReadListPrec2        #-}
{-# COMPILE GHC readsPrec2               = \ aℓ bℓ cℓ f a b AgdaRead2 AgdaRead AgdaRead -> Data.Functor.Classes.readsPrec2               #-}
{-# COMPILE GHC readPrec2                = \ aℓ bℓ cℓ f a b AgdaRead2 AgdaRead AgdaRead -> Data.Functor.Classes.readPrec2                #-}
{-# COMPILE GHC liftReadList2Default     = \ aℓ bℓ cℓ f a b AgdaRead2                   -> Data.Functor.Classes.liftReadList2Default     #-}
{-# COMPILE GHC liftReadListPrec2Default = \ aℓ bℓ cℓ f a b AgdaRead2                   -> Data.Functor.Classes.liftReadListPrec2Default #-}

{-# COMPILE GHC liftShowsPrec2 = \ aℓ bℓ cℓ f a b AgdaShow2                   -> Data.Functor.Classes.liftShowsPrec2 #-}
{-# COMPILE GHC liftShowList2  = \ aℓ bℓ cℓ f a b AgdaShow2                   -> Data.Functor.Classes.liftShowList2  #-}
{-# COMPILE GHC showsPrec2     = \ aℓ bℓ cℓ f a b AgdaShow2 AgdaShow AgdaShow -> Data.Functor.Classes.showsPrec2     #-}

postulate
    readsData       : (List Char → ReadS A) → Int → ReadS A
    readData        : ReadPrec A → ReadPrec A
    readsUnaryWith  : (Int → ReadS A) → List Char → (A → B) → List Char → ReadS B
    readUnaryWith   : ReadPrec A → List Char → (A → B) → ReadPrec B
    readsBinaryWith : (Int → ReadS A) → (Int → ReadS B) → List Char → (A → B → C) → List Char → ReadS C
    readBinaryWith  : ReadPrec A → ReadPrec B → List Char → (A → B → C) → ReadPrec C
    showsUnaryWith  : (Int → A → ShowS) → List Char → Int → A → ShowS
    showsBinaryWith : (Int → A → ShowS) → (Int → B → ShowS) → List Char → Int → A → B → ShowS

{-# COMPILE GHC readsData       = \ aℓ a           -> Data.Functor.Classes.readsData       #-}
{-# COMPILE GHC readData        = \ aℓ a           -> Data.Functor.Classes.readData        #-}
{-# COMPILE GHC readsUnaryWith  = \ aℓ a bℓ b      -> Data.Functor.Classes.readsUnaryWith  #-}
{-# COMPILE GHC readUnaryWith   = \ aℓ a bℓ b      -> Data.Functor.Classes.readUnaryWith   #-}
{-# COMPILE GHC readsBinaryWith = \ aℓ a bℓ b cℓ c -> Data.Functor.Classes.readsBinaryWith #-}
{-# COMPILE GHC readBinaryWith  = \ aℓ a bℓ b cℓ c -> Data.Functor.Classes.readBinaryWith  #-}
{-# COMPILE GHC showsUnaryWith  = \ aℓ a           -> Data.Functor.Classes.showsUnaryWith  #-}
{-# COMPILE GHC showsBinaryWith = \ aℓ a bℓ b      -> Data.Functor.Classes.showsBinaryWith #-}
