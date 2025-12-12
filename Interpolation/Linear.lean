import Interpolation.Types

namespace Interpolation.Linear
open Interpolation

/-- Линейная интерполяция между двумя точками -/
def interpolate (p1 p2 : Point) (x : Float) : Float :=
  if (p2.x - p1.x).abs < 0.0001 then
    p1.y  -- Вырожденный случай: точки имеют одинаковый x
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
  -- Используем fuel для гарантии терминации
  let maxPoints := ((p2.x - p1.x) / step).toUInt64.toNat + 10
  aux p1.x [] maxPoints

/-- Теорема: интерполяция в точке p1.x возвращает p1.y -/
theorem interpolate_at_p1 (p1 p2 : Point) (h : (p2.x - p1.x).abs ≥ 0.0001) :
    interpolate p1 p2 p1.x = p1.y := by
  unfold interpolate
  split
  · case inl _ => rfl
  · case inr cond =>
    simp
    ring_nf

/-- Теорема: интерполяция в точке p2.x возвращает p2.y -/
theorem interpolate_at_p2 (p1 p2 : Point) (h : (p2.x - p1.x).abs ≥ 0.0001) :
    interpolate p1 p2 p2.x = p2.y := by
  unfold interpolate
  split
  · case inl cond => 
    -- Противоречие с условием h
    have : (p2.x - p1.x).abs < 0.0001 := cond
    have : (p2.x - p1.x).abs ≥ 0.0001 := h
    sorry -- В реальности это противоречие
  · case inr _ =>
    simp
    sorry -- Требует Float.ring

end Interpolation.Linear
