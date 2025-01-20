-- This module has nothing much to do with Hocki-Klocki
-- Instead, this is an exploration of how Higher Order Abstraction Syntax works, and how could we use it

import Mathlib.Logic.Relation

---- simply-typed λ-calculus
inductive TermTy
  | base
  | fn : TermTy → TermTy → TermTy

notation a " -> " b => TermTy.fn a b

inductive Term' (rep : TermTy → Type) : TermTy → Type
  | var : {ty : TermTy} → rep ty → Term' rep ty
  | lam : {X Y : TermTy} → (rep X → Term' rep Y) → Term' rep (.fn X Y)
  | app : {X Y : TermTy} → (Term' rep (.fn X Y)) → Term' rep X → Term' rep Y


def Term (ty : TermTy) := {rep : TermTy → Type} → Term' rep ty

def ChurchNumeralTy : TermTy := .fn (.fn .base .base) (.fn .base .base)

def ChurchNumeralTerm := Term ChurchNumeralTy

def zero : Term ChurchNumeralTy :=.lam (fun _f => .lam fun x => .var x)

def succ : Term (.fn ChurchNumeralTy ChurchNumeralTy) :=
  .lam fun n => .lam fun f => .lam fun x => .app (.var f) (.app (.app (.var n) (.var f)) (.var x))

def one : ChurchNumeralTerm := .app succ zero


def add : Term (.fn ChurchNumeralTy (.fn ChurchNumeralTy ChurchNumeralTy)) :=
  .lam fun a => .lam fun b =>
      .lam fun f => .lam fun x =>
          .app (.app (.var b) (.var f)) (.app (.app (.var a) (.var f)) (.var x))


@[simp]
def inc (n : Nat) := n + 1

def TermTy.denote : TermTy → Type
  | base => Nat
  | fn a b => a.denote → b.denote

@[simp]
def denote {ty : TermTy} : Term' TermTy.denote ty → ty.denote
  | .var x => x
  | .lam f => fun x => denote (f x)
  | .app a b => (denote a) (denote b)

theorem one_inc_zero_is_one : denote one inc (0 : Nat) = (1 : Nat) := by simp


def squash {rep : TermTy → Type} {ty : TermTy} (t : Term' (Term' rep) ty) : Term' rep ty :=
  match t with
  | .var x => x
  | .lam f => .lam fun x => squash (f (.var x))
  | .app a b => .app (squash a) (squash b)




def TermTy.toString : TermTy → String
  | .base => "base"
  | .fn a b => "(" ++ a.toString ++ " → " ++ b.toString ++ ")"

def Term'.toString {ty : TermTy} (t : Term' (fun _ => String) ty) (i : Nat := 1) : String :=
  match t with
    | .var x => x
    | .lam f => let x := s!"x{i}"
                s!"(λ{x} . {(f x).toString (i + 1)})"
    | .app a b => "(" ++ a.toString i ++ " " ++ b.toString i ++ ")"



inductive BetaReduces : {rep : TermTy → Type} → {ty : TermTy} → Term' (Term' rep) ty → Term' (Term' rep) ty → Prop
  | beta : ∀ {f x}, BetaReduces (.app (.lam f) x) (f (squash x))
  | appLeft : ∀ {f f' x}, BetaReduces f f' → BetaReduces (.app f x) (.app f' x)
  | appRight : ∀ {f x x'}, BetaReduces x x' → BetaReduces (.app f x) (.app f x')
  | lam : ∀ {f f'}, (∀ x, BetaReduces (f x) (f' x)) → BetaReduces (.lam f) (.lam f')

@[simp]
def reduceTopLevel
    {ty : TermTy}
    {rep : TermTy → Type}
    (t : Term' (Term' rep) ty)
    : Term' (Term' rep) ty :=
  match t with
    | .app (.lam f) x => f (squash x)
    | _ => t

@[simp]
def reduceAppLeft {rep : TermTy → Type} {ty : TermTy} (t : Term' (Term' rep) ty) : Term' (Term' rep) ty :=
  match t with
    | .app f x => .app (reduceTopLevel f) x
    | _ => t

@[simp]
def reduceAppRight {rep : TermTy → Type} {ty : TermTy} (t : Term' (Term' rep) ty) : Term' (Term' rep) ty :=
  match t with
    | .app f x => .app f (reduceTopLevel x)
    | _ => t

@[simp]
def reduceLam {rep : TermTy → Type} {ty : TermTy} (t : Term' (Term' rep) ty) : Term' (Term' rep) ty :=
  match t with
    | .lam f => .lam (fun x => reduceTopLevel (f x))
    | .app a b => .app a b
    | .var x => .var x

def BetaEquiv {rep : TermTy → Type} {ty : TermTy} := Relation.EqvGen (@BetaReduces rep ty)

notation a " →β " b => BetaReduces a b
notation a " ≡β " b => BetaEquiv a b


def S {rep : TermTy → Type} {ρ σ τ : TermTy} : Term' rep ((σ -> τ -> ρ) -> (σ -> τ) -> σ -> ρ) :=
  .lam fun x => .lam fun y => .lam fun z => .app (.app (.var x) (.var z)) (.app (.var y) (.var z))

def K {rep : TermTy → Type} {σ τ} : Term' rep (σ -> τ -> σ) :=
  .lam fun a => .lam fun _ => .var a

def I {rep : TermTy → Type} {σ : TermTy} : Term' rep (σ -> σ) :=
  .lam fun a => .var a



theorem calling_K_on_two_terms_returns_first
  : (denote (@K TermTy.denote .base .base)) (1 : Nat) (2 : Nat) = (1 : Nat) := by simp

theorem I_beta_equiv_I : ∀ {rep ty}, BetaEquiv (@I (Term' rep) ty) (@I (Term' rep) ty) := by
  intro rep ty
  apply Relation.EqvGen.refl
