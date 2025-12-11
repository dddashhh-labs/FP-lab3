import Interpolation.Types

namespace Interpolation.Parser

open Interpolation

def parseFloat (s : String) : Option Float :=
  match s.toNat? with
  | some n => some (n.toFloat)
  | none =>
    let trimmed := s.trim
    if trimmed.isEmpty then none
    else
      match trimmed.splitOn "." with
      | [intPart, fracPart] =>
        match intPart.toNat?, fracPart.toNat? with
        | some i, some f =>
          let fracLen := fracPart.length
          let divisor := (10 ^ fracLen).toFloat
          some (i.toFloat + f.toFloat / divisor)
        | some i, none => some i.toFloat
        | _,_ => none
      | [intPart] =>
        match intPart.toNat? with
        | some i => some i.toFloat
        | none =>
          if intPart.startsWith "-" then
            match (intPart.drop 1).toNat? with
            | some i => some (-(i.toFloat))
            | none => none
          else none
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
      | _,_ => none
    | _ => none

end Interpolation.Parser
