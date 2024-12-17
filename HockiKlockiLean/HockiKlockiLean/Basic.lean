import Std.Data.HashSet
import Std.Data.HashMap
open Std

def hello : String := "world"

def Dim : Type := String deriving BEq, Hashable, Repr
def DimSetVar : Type := String deriving BEq, Hashable, Repr

structure FilteredDimSetVar where
  dim_set_var : DimSetVar
  filtered : HashSet Dim
deriving Repr

inductive Constraint where
  | in_union : Dim → HashSet DimSetVar → Constraint
  | not_in : Dim → DimSetVar → Constraint
  | induced_by : DimSetVar → List FilteredDimSetVar → Constraint
deriving Repr
open Constraint



def x : Constraint := Constraint.in_union "a" HashSet.empty

def well_formed_constraint
  (mapping : HashMap DimSetVar (HashSet Dim))
  (constraint : Constraint)
  : Bool
  := match constraint with
    | in_union dim dimSetVars =>
      dimSetVars.toList.all mapping.contains
      && dimSetVars.toList.any (fun d => (mapping.get! d).contains dim)

    | not_in dim dimSetVar =>
      mapping.contains dimSetVar
      &&  !(mapping.get! dimSetVar).contains dim

    | induced_by dimSetVar filtered => mapping.contains dimSetVar
      && (filtered.map (FilteredDimSetVar.dim_set_var)).all mapping.contains

def well_formed_constraints
  (mapping : HashMap DimSetVar (HashSet Dim))
  (constraints : List Constraint)
  : Bool
  := constraints.all (well_formed_constraint mapping)
