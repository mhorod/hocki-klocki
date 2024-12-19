import HockiKlockiLean.Model
import HockiKlockiLean.Inclusion

structure FilteredDimSetVar where
  dim_set_var : DimSetVar
  filtered : Set Dim

def In := Dim → DimSetVar → Prop

def NotIn := Dim → DimSetVar → Prop

def InUnion := Dim → Set DimSetVar → Prop

inductive NotInGen (rel : NotIn) : NotIn
| given {a X} : rel a X → NotInGen rel a X
| induced {a X Z} {R : DimSetVarRel a} : rel a X → R Z X → NotInGen rel a Z

inductive InGen (rel : In) : In
| given {a X} : rel a X → InGen rel a X
| induced {a X Y} {R : DimSetVarRel a} : rel a X → R X Y → InGen rel a X

inductive InUnionGen (rel : InUnion) : InUnion
| given {a Xs} : rel a Xs → InUnionGen rel a Xs
| induced {a Xs} {R : DimSetVarRel a} : rel a Xs → InUnionGen rel a {Z | ∃ X ∈ Xs, R Z X}

def In_wf (rel : In) (f : DimSetAssignment) :=
  ∀ {a X}, rel a X → a ∈ f X

def NotIn_wf (rel : NotIn) (f : DimSetAssignment) :=
  ∀ {a X}, rel a X → a ∉ f X

def InUnion_wf (rel : InUnion) (f : DimSetAssignment) :=
  ∀ {a Xs}, rel a Xs → a ∈ (⋃ X ∈ Xs, f X)

structure SchemaTy where
  ins : Set In
  notIns : Set NotIn
  inUnions : Set InUnion
  relations : (a : Dim) → DimSetVarRel a

def assignment_wf (ty : SchemaTy) (f : DimSetAssignment) :=
  ∀ c ∈ ty.ins, In_wf c f
  → ∀ c ∈ ty.notIns, NotIn_wf c f
  → ∀ c ∈ ty.inUnions, InUnion_wf c f
  → ∀ a, R_wf (ty.relations a) f
  → Prop
