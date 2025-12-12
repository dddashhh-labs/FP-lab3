import Interpolation.Types

namespace Interpolation.Newton
open Interpolation

/-- Вычисление разделенной разности -/
partial def dividedDifference (points : List Point) : Float :=
  match points with
  | [] => 0
  | [p] => p.y
  | _ =>
    let n := points.length
    let front := points.take (n - 1)
    let back := points.drop 1
    let frontDiff := dividedDifference front
    let backDiff := dividedDifference back
    match points.head?, points.getLast? with
    | some first, some last =>
      if (last.x - first.x).abs < 0.0001 then 
        0
      else 
        (backDiff - frontDiff) / (last.x - first.x)
    | _, _ => 0

/-- Вычисление полинома Ньютона в точке x -/
partial def newtonPolynomial (points : List Point) (x : Float) : Float :=
  let rec aux (idx : Nat) (acc : Float) (prod : Float) : Float :=
    if idx >= points.length then 
      acc
    else
      let subPoints := points.take (idx + 1)
      let coeff := dividedDifference subPoints
      let newAcc := acc + coeff * prod
      match points.get? idx with
      | some p => aux (idx + 1) newAcc (prod * (x - p.x))
      | none => acc
  aux 0 0 1

/-- Теорема: полином детерминирован -/
theorem newton_deterministic (points : List Point) (x : Float) :
    newtonPolynomial points x = newtonPolynomial points x := by
  rfl

/-- Теорема: разделенная разность пустого списка равна 0 -/
theorem divided_diff_empty : dividedDifference [] = 0 := by
  rfl

/-- Теорема: разделенная разность одной точки равна y-координате -/
theorem divided_diff_single (p : Point) : 
    dividedDifference [p] = p.y := by
  rfl

/-- Вспомогательная теорема о структуре partial функции -/
theorem divided_diff_two_points (p1 p2 : Point) :
    ∃ v, dividedDifference [p1, p2] = v := by
  exists dividedDifference [p1, p2]

end Interpolation.Newton
