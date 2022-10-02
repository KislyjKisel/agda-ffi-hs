{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.Metric where

open import Agda.Builtin.List            using (List)
open import Agda.Builtin.Maybe           using (Maybe)
open import Agda.Primitive               using (Level)
open import Ffi.Hs.-base.Class           using (Num; Floating; Fractional)
open import Ffi.Hs.Control.Applicative   using (ZipList)
open import Ffi.Hs.Data.Functor.Compose  using (Compose)
open import Ffi.Hs.Data.Functor.Identity using (Identity)
open import Ffi.Hs.Data.Functor.Product  using (Product)
open import Ffi.Hs.Data.Vector           using (Vector)
open import Ffi.Hs.Linear.Epsilon        using (Epsilon)
open import Ffi.Hs.Linear.Vector         using (Additive)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Linear.Metric
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Linear.Vector (AgdaAdditive(AgdaAdditive))
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        F G : Set aℓ → Set aℓ

postulate
    Metric : (Set aℓ → Set aℓ) → Set aℓ

    Metric[F]⇒Additive[F] : ⦃ Metric F ⦄ → Additive F

    Metric[List]         : Metric {aℓ} List
    Metric[Maybe]        : Metric {aℓ} Maybe
    Metric[ZipList]      : Metric {aℓ} ZipList
    Metric[Identity]     : Metric {aℓ} Identity
    Metric[Vector]       : Metric {aℓ} Vector
    Metric[Product[F,G]] : ⦃ Metric F ⦄ → ⦃ Metric G ⦄ → Metric (Product F G)
    Metric[Compose[F,G]] : ⦃ Metric F ⦄ → ⦃ Metric G ⦄ → Metric (Compose F G)
    -- todo: instances for IntMap, Map, HashMap

    dot       : ⦃ Metric F ⦄ → ⦃ Num A ⦄ → F A → F A → A
    quadrance : ⦃ Metric F ⦄ → ⦃ Num A ⦄ → F A → A
    qd        : ⦃ Metric F ⦄ → ⦃ Num A ⦄ → F A → F A → A
    distance  : ⦃ Metric F ⦄ → ⦃ Floating A ⦄ → F A → F A → A
    norm      : ⦃ Metric F ⦄ → ⦃ Floating A ⦄ → F A → A
    signorm   : ⦃ Metric F ⦄ → ⦃ Floating A ⦄ → F A → F A

    normalize : ⦃ Floating A ⦄ → ⦃ Metric F ⦄ → ⦃ Epsilon A ⦄ → F A → F A
    project   : ⦃ Metric F ⦄ → ⦃ Fractional A ⦄ → F A → F A → F A

{-# FOREIGN GHC data AgdaMetric aℓ a = Linear.Metric.Metric a => AgdaMetric #-}
{-# COMPILE GHC Metric = type(0) AgdaMetric #-}

{-# COMPILE GHC Metric[F]⇒Additive[F] = \ fℓ f AgdaMetric -> AgdaAdditive #-}

{-# COMPILE GHC Metric[List]         = \ aℓ     -> AgdaMetric #-}
{-# COMPILE GHC Metric[Maybe]        = \ aℓ     -> AgdaMetric #-}
{-# COMPILE GHC Metric[ZipList]      = \ aℓ     -> AgdaMetric #-}
{-# COMPILE GHC Metric[Identity]     = \ aℓ     -> AgdaMetric #-}
{-# COMPILE GHC Metric[Vector]       = \ aℓ     -> AgdaMetric #-}
{-# COMPILE GHC Metric[Product[F,G]] = \ aℓ f g -> AgdaMetric #-}
{-# COMPILE GHC Metric[Compose[F,G]] = \ aℓ f g -> AgdaMetric #-}

{-# COMPILE GHC dot       = \ fℓ f a AgdaMetric AgdaNum      -> Linear.Metric.dot       #-}
{-# COMPILE GHC quadrance = \ fℓ f a AgdaMetric AgdaNum      -> Linear.Metric.quadrance #-}
{-# COMPILE GHC qd        = \ fℓ f a AgdaMetric AgdaNum      -> Linear.Metric.qd        #-}
{-# COMPILE GHC distance  = \ fℓ f a AgdaMetric AgdaFloating -> Linear.Metric.distance  #-}
{-# COMPILE GHC norm      = \ fℓ f a AgdaMetric AgdaFloating -> Linear.Metric.norm      #-}
{-# COMPILE GHC signorm   = \ fℓ f a AgdaMetric AgdaFloating -> Linear.Metric.signorm   #-}

{-# COMPILE GHC normalize = \ aℓ a f AgdaFloating AgdaMetric AgdaEpsilon -> Linear.Metric.normalize #-}
{-# COMPILE GHC project   = \ fℓ f a AgdaMetric AgdaFractional           -> Linear.Metric.project   #-}
