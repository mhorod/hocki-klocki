import Mathlib.Data.Prod.Basic
import Mathlib.Data.Set.Basic
import Mathlib.Logic.Relation

import HockiKlockiLean.Model

abbrev DimSetVarRel : Type := DimSetVar → DimSetVar → Prop

def wf (a : Dim) (R : DimSetVarRel) (f : DimSetVar → Set Dim) : Prop := ∀ {X Y}, R X Y → a ∈ f X → a ∈ f Y

theorem empty_relation_well_formed
    {a : Dim}
    (f : DimSetVar → Set Dim)
    :
    wf a EmptyRelation f
  := by
  intro _ _
  intro RXY
  simp at RXY

@[simp]
def addPair (R : DimSetVarRel) (X Y : DimSetVar) : DimSetVarRel := fun a b => (R a b) ∨ (a = X ∧ b = Y)

-- :thunk: unused
theorem transitive_step_well_formed
    {a : Dim}
    (f : DimSetVar → Set Dim)
    (R : DimSetVarRel)
    (f_wf_R : wf a R f)
    {X Y Z : DimSetVar}
    (XY_in_R: R X Y)
    (YZ_in_R: R Y Z)
    :
    wf a (addPair R X Z) f
  := by
  unfold wf
  have XZ_wf : a ∈ f X → a ∈ f Z := by
    have h_XY := f_wf_R XY_in_R
    have h_YZ := f_wf_R YZ_in_R
    intro a_in_fX
    exact h_YZ (h_XY a_in_fX)
  intro i j R'ij
  simp at R'ij
  match R'ij with
  | Or.inl Rij => exact f_wf_R Rij
  | Or.inr ij_eq_XZ =>
    rw [← ij_eq_XZ.left, ← ij_eq_XZ.right] at XZ_wf
    exact XZ_wf


theorem transitive_closure_well_formed
  {a : Dim}
  (f : DimSetVar → Set Dim)
  (R : DimSetVarRel)
  (f_wf_R : wf a R f)
  :
  wf a (Relation.TransGen R) f
  := by
    unfold wf
    unfold wf at f_wf_R
    intro X Y R'XY
    induction R'XY with
    | single h => exact f_wf_R h
    | tail R'Xb Rbc ih =>
      intro a_in_fX
      have a_in_fb := ih a_in_fX
      exact f_wf_R Rbc a_in_fb
