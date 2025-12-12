-- Newton.lean
import Interpolation.Types

namespace Interpolation.Newton
open Interpolation

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

partial def newtonPolynomial (points : List Point) (x : Float) : Float :=
  let rec aux (idx : Nat) (acc : Float) (prod : Float) : Float :=
    if idx >= points.length then 
      acc
    else
      let subPoints := points.take (idx + 1)
      let coeff := dividedDifference subPoints
      let newAcc := acc + coeff * prod
      match points[idx]? with
      | some p => aux (idx + 1) newAcc (prod * (x - p.x))
      | none => acc
  aux 0 0 1

end Interpolation.Newton
