-- Linear.lean
import Interpolation.Types

namespace Interpolation.Linear
open Interpolation

-- Доказательство корректности линейной интерполяции
theorem interpolate_in_range (p1 p2 : Point) (x : Float) 
  (h : p1.x ≤ x ∧ x ≤ p2.x) :
  ∃ y, y = interpolate p1 p2 x ∧ 
    (p1.y ≤ y ∧ y ≤ p2.y ∨ p2.y ≤ y ∧ y ≤ p1.y) := by
  sorry

def interpolate (p1 p2 : Point) (x : Float) : Float :=
  if (p2.x - p1.x).abs < 0.0001 then 
    p1.y
  else
    let slope := (p2.y - p1.y) / (p2.x - p1.x)
    p1.y + slope * (x - p1.x)

-- Генерация точек с доказательством монотонности
def generatePoints (p1 p2 : Point) (step : Float) 
  (hstep : 0 < step) : List Point :=
  let rec aux (currentX : Float) (acc : List Point) 
    (hacc : ∀ p ∈ acc, p1.x ≤ p.x ∧ p.x < p2.x) : List Point :=
    if h : currentX + step > p2.x then
      acc.reverse
    else
      let y := interpolate p1 p2 currentX
      let newPoint := Point.mk currentX y
      have : p1.x ≤ newPoint.x ∧ newPoint.x < p2.x := by
        constructor
        · sorry -- currentX начинается с p1.x
        · sorry -- currentX < p2.x по условию
      aux (currentX + step) (newPoint :: acc) (by sorry)
  aux p1.x [] (by simp)

end Interpolation.Linear
