import HockiKlockiLean.Model

structure FilteredDimSetVar where
  dim_set_var : DimSetVar
  filtered : Set Dim

def NotIn := Dim → DimSetVar → Prop

def InUnion := Dim → Set DimSetVar → Prop

def InducedBy := DimSetVar → Set FilteredDimSetVar → Prop

def NotIn_wf (notIn : NotIn) (f : DimSetAssignment)
    := ∀ {a X}, notIn a X → a ∉ f X

def InUnion_wf (inUnion : InUnion) (f : DimSetAssignment)
    := ∀ {a Xs}, inUnion a Xs → a ∈ (⋃ X ∈ Xs, f X)

def InducedBy_wf (inducedBy : InducedBy) (f : DimSetAssignment)
    := ∀ {induced inducers},
      inducedBy induced inducers
      →
      ∀ inducer ∈ inducers,
      ∀ a, a ∉ inducer.filtered
      → a ∈ f inducer.dim_set_var → a ∈ f induced
