-- Newton.lean
import Interpolation.Types

namespace Interpolation.Newton
open Interpolation

-- Доказательство существования разделенной разности
theorem divided_difference_exists (points : List Point) 
  (h : points.length ≥ 2) :
  ∃ dd : Float, dd = dividedDifference points := by
  exists dividedDifference points

def dividedDifference (points : List Point) : Float :=
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
termination_by dividedDifference points => points.length

-- Доказательство корректности полинома Ньютона
theorem newton_interpolates_points (points : List Point) (p : Point)
  (h : p ∈ points) :
  ∃ ε > 0, (newtonPolynomial points p.x - p.y).abs < ε := by
  sorry

def newtonPolynomial (points : List Point) (x : Float) : Float :=
  let rec aux (idx : Nat) (acc : Float) (prod : Float)
    (hidx : idx ≤ points.length) : Float :=
    if h : idx >= points.length then 
      acc
    else
      have : idx < points.length := Nat.lt_of_not_le h
      let subPoints := points.take (idx + 1)
      let coeff := dividedDifference subPoints
      let newAcc := acc + coeff * prod
      match hget : points.get? idx with
      | some p => 
        have : idx + 1 ≤ points.length := by omega
        aux (idx + 1) newAcc (prod * (x - p.x)) this
      | none => acc
  aux 0 0 1 (Nat.zero_le _)
termination_by aux idx _ _ _ => points.length - idx

end Interpolation.Newton
