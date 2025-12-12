-- Types.lean
namespace Interpolation

structure Point where
  x : Float
  y : Float
deriving Repr, BEq, Inhabited

-- Доказательство корректности конфигурации
structure Config where
  method : List String
  step : Float
  windowSize : Nat
  step_positive : 0 < step
  window_positive : 0 < windowSize

instance : Inhabited Config where
  default := {
    method := []
    step := 1.0
    windowSize := 4
    step_positive := by norm_num
    window_positive := by norm_num
  }

structure InterpolationResult where
  method : String
  point : Point
deriving Repr

-- Доказательство сортированности списка точек
def IsSorted (points : List Point) : Prop :=
  ∀ i j, i < j → j < points.length → 
    match points[i]?, points[j]? with
    | some pi, some pj => pi.x ≤ pj.x
    | _, _ => True

-- Доказательство, что точка в диапазоне интерполяции
def InRange (x : Float) (points : List Point) : Prop :=
  points.length ≥ 2 ∧
  match points.head?, points.getLast? with
  | some first, some last => first.x ≤ x ∧ x ≤ last.x
  | _, _ => False

end Interpolation
