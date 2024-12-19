import Mathlib.Data.Set.Basic

import HockiKlockiLean.Model

structure Schema {n : Nat} where
  inputs: Set DimSetVar
