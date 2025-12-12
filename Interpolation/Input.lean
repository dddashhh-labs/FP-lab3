-- Input.lean (обработка входного потока)
import Interpolation.Parser

namespace Interpolation.Input
open Interpolation

-- Чтение потока точек из stdin
partial def readPoints : IO (List Point) := do
  let stdin ← IO.getStdin
  let mut points : List Point := []
  
  repeat
    try
      let line ← stdin.getLine
      if line.trim.isEmpty then break
      
      match Parser.parseLine line with
      | some point => points := points ++ [point]
      | none => IO.eprintln s!"Warning: Could not parse line: {line}"
    catch _ =>
      break
  
  return points

-- Потоковое чтение с callback
partial def streamPoints (process : Point → IO Unit) : IO Unit := do
  let stdin ← IO.getStdin
  
  repeat
    try
      let line ← stdin.getLine
      if line.trim.isEmpty then break
      
      match Parser.parseLine line with
      | some point => process point
      | none => IO.eprintln s!"Warning: Could not parse line: {line}"
    catch _ =>
      break

end Interpolation.Input
