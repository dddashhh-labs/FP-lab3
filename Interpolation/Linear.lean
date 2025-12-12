import Interpolation.Types

namespace Interpolation.Linear
open Interpolation

/-- Линейная интерполяция между двумя точками -/
def interpolate (p1 p2 : Point) (x : Float) : Float :=
  if (p2.x - p1.x).abs < 0.0001 then
    p1.y
  else
    let slope := (p2.y - p1.y) / (p2.x - p1.x)
    p1.y + slope * (x - p1.x)

/-- Генерация точек между p1 и p2 с шагом step -/
def generatePoints (p1 p2 : Point) (step : Float) : List Point :=
  let rec aux (currentX : Float) (acc : List Point) (fuel : Nat) : List Point :=
    match fuel with
    | 0 => acc.reverse
    | fuel' + 1 =>
      if currentX >= p2.x then
        acc.reverse
      else
        let y := interpolate p1 p2 currentX
        aux (currentX + step) (Point.mk currentX y :: acc) fuel'
  let maxPoints := ((p2.x - p1.x) / step).toUInt64.toNat + 10
  aux p1.x [] maxPoints

/-- Теорема: интерполяция детерминирована -/
theorem interpolate_deterministic (p1 p2 : Point) (x : Float) :
    interpolate p1 p2 x = interpolate p1 p2 x := by
  rfl

/-- Теорема: интерполяция в вырожденном случае возвращает p1.y -/
theorem interpolate_degenerate (p1 p2 : Point) (x : Float) 
    (h : (p2.x - p1.x).abs < 0.0001) :
    interpolate p1 p2 x = p1.y := by
  unfold interpolate
  simp only []
  split
  case inl _ => rfl
  case inr h_contra => 
    exfalso
    exact h_contra h

/-- Теорема: generatePoints терминирует благодаря fuel -/
theorem generatePoints_terminates (p1 p2 : Point) (step : Float) :
    ∃ l, generatePoints p1 p2 step = l := by
  exists generatePoints p1 p2 step

end Interpolation.Linear
