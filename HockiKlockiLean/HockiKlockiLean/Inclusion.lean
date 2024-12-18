import Mathlib.Data.Prod.Basic
import Mathlib.Data.Set.Basic
import Mathlib.Logic.Relation

def Dim : Type := String deriving BEq, Hashable, Repr
def DimSetVar : Type := String deriving BEq, Hashable, Repr

-- R_a

-- X R_a Y

-- (a ∈ f(X) → a ∈ f(Y)) ∧ (a ∈ f(Y) → a ∈ U f(R_a^{-1}(Y))) → f wf R_a

abbrev DimSetVarRel : Type := Set (DimSetVar × DimSetVar)

inductive WellFormed (a : Dim) (R : DimSetVarRel) (f : DimSetVar → Set Dim) : Prop where
| wellFormed : (∀ {X Y}, (X, Y) ∈ R → a ∈ f X → a ∈ f Y) → WellFormed a R f

theorem empty_relation_well_formed
    {a : Dim}
    (f : DimSetVar → Set Dim)
    :
    WellFormed a ∅ f
  := by
    have h1 : ∀ {X Y}, (X, Y) ∈ (∅ : DimSetVarRel) → a ∈ f X → a ∈ f Y := by
      intro x y h2
      by_contra
      exact Set.not_mem_empty (x, y) h2
    exact WellFormed.wellFormed h1

theorem transitive_step_well_formed
    {a : Dim}
    (f : DimSetVar → Set Dim)
    (R : DimSetVarRel)
    (wf : WellFormed a R f)
    {X Y Z : DimSetVar}
    (xy_in_R: (X, Y) ∈ R)
    (yz_in_R: (Y, Z) ∈ R)
    :
    WellFormed a (insert (X, Z) R) f
  := by
    have R_wf_proof : (∀ {X Y}, (X, Y) ∈ R → a ∈ f X → a ∈ f Y) := match wf with
      | WellFormed.wellFormed proof => proof

    have xz_wf : a ∈ f X → a ∈ f Z := by
      have h_xy := R_wf_proof xy_in_R
      have h_yz := R_wf_proof yz_in_R
      intro a_in_fx
      exact h_yz (h_xy a_in_fx)

    have h1 : (∀ {x u}, (x, u) ∈ (insert (X, Z) R) → a ∈ f x → a ∈ f u) := by
      intro x u insert_xz a_in_fx
      rw [Set.mem_insert_iff] at insert_xz
      match insert_xz with
        | Or.inl x_u_eq_X_Z =>
          simp at x_u_eq_X_Z
          rw [← x_u_eq_X_Z.left, ← x_u_eq_X_Z.right] at xz_wf
          exact xz_wf a_in_fx
        | Or.inr b => exact (R_wf_proof b) a_in_fx
    exact WellFormed.wellFormed h1
