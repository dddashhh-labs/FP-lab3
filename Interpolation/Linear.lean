-- Linear.lean
import Interpolation.Types

namespace Interpolation.Linear
open Interpolation

-- Доказательство корректности линейной интерполяции
theorem linear_interpolation_correct (p1 p2 : Point) (x : Float)
  (h : p1.x ≤ x ∧ x ≤ p2.x) (hdiff : p1.x ≠ p2.x) :
  ∃ y, y = interpolate p1 p2 x ∧ 
    (min p1.y p2.y ≤ y ∧ y ≤ max p1.y p2.y) := by
  sorry

def interpolate (p1 p2 : Point) (x : Float) : Float :=
  if (p2.x - p1.x).abs < 0.0001 then 
    p1.y
  else
    let slope := (p2.y - p1.y) / (p2.x - p1.x)
    p1.y + slope * (x - p1.x)

-- Интерполяция для списка X координат
def interpolateMany (p1 p2 : Point) (xs : List Float) : List Point :=
  xs.map fun x => Point.mk x (interpolate p1 p2 x)

end Interpolation.Linear
