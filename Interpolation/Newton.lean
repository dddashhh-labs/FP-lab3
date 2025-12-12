-- Newton.lean
import Interpolation.Types

namespace Interpolation.Newton
open Interpolation

-- Доказательство существования разделенных разностей
theorem divided_difference_exists (points : List Point) 
  (h : points.length ≥ 1) :
  ∃ dd : Float, dd = dividedDifference points := by
  exists dividedDifference points

partial def dividedDifference (points : List Point) : Float :=
  match points with
  | [] => 0
  | [p] => p.y
  | _ =>
    let n := points.length
    if n ≤ 1 then 0
    else
      let front := points.take (n - 1)
      let back := points.drop 1
      let frontDiff := dividedDifference front
      let backDiff := dividedDifference back
      match points.head?, points.getLast? with
      | some first, some last =>
        if (last.x - first.x).abs < 0.0001 then 0
        else (backDiff - frontDiff) / (last.x - first.x)
      | _, _ => 0

-- Доказательство интерполяции в точках данных
theorem newton_interpolates_data (points : List Point) (p : Point)
  (h : p ∈ points) :
  (newtonPolynomial points p.x - p.y).abs < 0.001 := by
  sorry

partial def newtonPolynomial (points : List Point) (x : Float) : Float :=
  let rec aux (idx : Nat) (acc : Float) (prod : Float) : Float :=
    if idx >= points.length then acc
    else
      let subPoints := points.take (idx + 1)
      let coeff := dividedDifference subPoints
      let newAcc := acc + coeff * prod
      match points[idx]? with
      | some p => aux (idx + 1) newAcc (prod * (x - p.x))
      | none => acc
  aux 0 0 1

-- Интерполяция для списка X координат
def interpolateMany (points : List Point) (xs : List Float) : List Point :=
  xs.map fun x => Point.mk x (newtonPolynomial points x)

end Interpolation.Newton
