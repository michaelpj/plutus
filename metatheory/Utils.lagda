\begin{code}
module Utils where

data Maybe (A : Set) : Set where
  just : A → Maybe A
  nothing : Maybe A

{-# COMPILE GHC Maybe = data Maybe (Just | Nothing) #-}

maybe : {A B : Set} → (A → B) → B → Maybe A → B
maybe f b (just a) = f a
maybe f b nothing  = b

map : {A B : Set} → (A → B) → Maybe A → Maybe B
map f (just a) = just (f a)
map f nothing  = nothing

open import Relation.Nullary

decIf : ∀{A B : Set} → Dec A → B → B → B
decIf (yes p) t f = t
decIf (no ¬p) t f = f

\end{code}
