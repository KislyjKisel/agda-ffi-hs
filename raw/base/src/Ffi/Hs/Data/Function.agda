
        Monad[A⟶] : {A : Set aℓ} → Monad {aℓ} (λ b → (A → b))
        Applicative[A⟶] : {A : Set fℓ} → Applicative {fℓ} (λ b → (A → b))
{-# COMPILE GHC Instances.Applicative[A⟶] = \ fℓ a -> AgdaApplicative #-}