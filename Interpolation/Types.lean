-- Types.lean
namespace Interpolation

structure Point where
  x : Float
  y : Float
deriving Repr, BEq, Inhabited

structure Config where
  method : List String
  step : Float
  windowSize : Nat
deriving Repr

-- Экземпляр Inhabited для Config
instance : Inhabited Config where
  default := {
    method := []
    step := 1.0
    windowSize := 4
  }

structure InterpolationResult where
  method : String
  point : Point
deriving Repr

-- Доказательство, что список точек отсортирован
def IsSorted (points : List Point) : Prop :=
  ∀ i j, i < j → j < points.length → 
    match points[i]?, points[j]? with
    | some pi, some pj => pi.x ≤ pj.x
    | _, _ => True

-- Доказательство, что точка находится в диапазоне
def InRange (p : Point) (points : List Point) : Prop :=
  match points.head?, points.getLast? with
  | some first, some last => first.x ≤ p.x ∧ p.x ≤ last.x
  | _, _ => False

end Interpolation
