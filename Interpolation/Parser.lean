-- Parser.lean
import Interpolation.Types

namespace Interpolation.Parser
open Interpolation

def parseFloat (s : String) : Option Float :=
  let trimmed := s.trim
  if trimmed.isEmpty then none
  else
    -- Обработка отрицательных чисел
    let (isNeg, numStr) := if trimmed.startsWith "-" 
      then (true, trimmed.drop 1)
      else (false, trimmed)
    
    match numStr.splitOn "." with
    | [intPart, fracPart] =>
      match intPart.toNat?, fracPart.toNat? with
      | some i, some f =>
        let fracLen := fracPart.length
        let divisor := (10 ^ fracLen).toFloat
        let result := i.toFloat + f.toFloat / divisor
        some (if isNeg then -result else result)
      | some i, none => 
        some (if isNeg then -(i.toFloat) else i.toFloat)
      | _, _ => none
    | [intPart] =>
      match intPart.toNat? with
      | some i => some (if isNeg then -(i.toFloat) else i.toFloat)
      | none => none
    | _ => none

def parseLine (line : String) : Option Point := do
  let line := line.trim
  if line.isEmpty then none
  else
    let parts := if line.contains ';' then
      line.splitOn ";"
    else if line.contains '\t' then
      line.splitOn "\t"
    else
      line.splitOn " "
    match parts.filter (fun s => !s.trim.isEmpty) with
    | [xs, ys] =>
      match parseFloat xs.trim, parseFloat ys.trim with
      | some x, some y => some (Point.mk x y)
      | _, _ => none
    | _ => none

end Interpolation.Parser
