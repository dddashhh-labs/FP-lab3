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
  stepPositive : 0 < step := by native_decide
  windowSizePositive : 0 < windowSize := by native_decide
deriving Repr, Inhabited

structure InterpolationResult where
  method : String
  point : Point
deriving Repr

-- Доказательство, что список точек отсортирован
def IsSorted (points : List Point) : Prop :=
  ∀ i j, i < j → j < points.length → 
    (points.get ⟨i, by omega⟩).x ≤ (points.get ⟨j, by omega⟩).x

-- Доказательство, что точка находится в диапазоне
def InRange (p : Point) (points : List Point) : Prop :=
  match points.head?, points.getLast? with
  | some first, some last => first.x ≤ p.x ∧ p.x ≤ last.x
  | _, _ => False

end Interpolation
