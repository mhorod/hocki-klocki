import Mathlib.Data.Set.Basic
import Mathlib.Order.SetNotation

def Dim : Type := String deriving BEq, Hashable, Repr
def DimSetVar : Type := String deriving BEq, Hashable, Repr
def DimSetAssignment : Type := DimSetVar → Set Dim
def DimSetVarRel (_ : Dim) : Type := DimSetVar → DimSetVar → Prop
