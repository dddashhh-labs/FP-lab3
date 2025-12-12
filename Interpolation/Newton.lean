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
        0  -- Защита от деления на ноль
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

/-- Теорема: полином степени 0 (одна точка) возвращает значение этой точки -/
theorem newton_single_point (p : Point) : 
    newtonPolynomial [p] p.x = p.y := by
  unfold newtonPolynomial
  simp
  unfold dividedDifference
  simp

/-- Теорема: разделенная разность одной точки равна y-координате -/
theorem divided_diff_single (p : Point) : 
    dividedDifference [p] = p.y := by
  unfold dividedDifference
  simp

end Interpolation.Newton
