{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.ObjectName where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive     using (Level)
open import Ffi.Hs.-base.Class using (MonadIO)
open import Ffi.Hs.-base.Level using (Liftℓ)
open import Ffi.Hs.-base.Unit  using (⊤′)
open import Ffi.Hs.Data.Int    using (Int)

{-# FOREIGN GHC
import qualified Data.ObjectName
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaMonadIO(AgdaMonadIO))
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        M : Set aℓ → Set aℓ


postulate
    ObjectName : Set aℓ → Set aℓ

    isObjectName      : ⦃ ObjectName A ⦄ → ⦃ MonadIO M ⦄ → A → M (Liftℓ _ Bool)
    deleteObjectName  : ⦃ ObjectName A ⦄ → ⦃ MonadIO M ⦄ → A → M ⊤′
    deleteObjectNames : ⦃ ObjectName A ⦄ → ⦃ MonadIO M ⦄ → List A → M ⊤′

{-# FOREIGN GHC data AgdaObjectName aℓ a = Data.ObjectName.ObjectName a => AgdaObjectName #-}
{-# COMPILE GHC ObjectName = type(0) AgdaObjectName #-}

{-# COMPILE GHC isObjectName      = \ aℓ a mℓ m AgdaObjectName AgdaMonadIO -> Data.ObjectName.isObjectName      #-}
{-# COMPILE GHC deleteObjectName  = \ aℓ a mℓ m AgdaObjectName AgdaMonadIO -> Data.ObjectName.deleteObjectName  #-}
{-# COMPILE GHC deleteObjectNames = \ aℓ a mℓ m AgdaObjectName AgdaMonadIO -> Data.ObjectName.deleteObjectNames #-}


postulate
    GeneratableObjectName : Set aℓ → Set aℓ

    GeneratableObjectName[A]⇒ObjectName[A] : ⦃ GeneratableObjectName A ⦄ → ObjectName A

    genObjectName  : ⦃ GeneratableObjectName A ⦄ → ⦃ MonadIO M ⦄ → M A
    genObjectNames : ⦃ GeneratableObjectName A ⦄ → ⦃ MonadIO M ⦄ → Int → M (List A)

{-# FOREIGN GHC data AgdaGeneratableObjectName aℓ a = Data.ObjectName.GeneratableObjectName a => AgdaGeneratableObjectName #-}
{-# COMPILE GHC GeneratableObjectName = type(0) AgdaGeneratableObjectName #-}

{-# COMPILE GHC GeneratableObjectName[A]⇒ObjectName[A] = \ aℓ a AgdaGeneratableObjectName -> AgdaObjectName #-}

{-# COMPILE GHC genObjectName  = \ aℓ a m AgdaGeneratableObjectName AgdaMonadIO -> Data.ObjectName.genObjectName  #-}
{-# COMPILE GHC genObjectNames = \ aℓ a m AgdaGeneratableObjectName AgdaMonadIO -> Data.ObjectName.genObjectNames #-}
