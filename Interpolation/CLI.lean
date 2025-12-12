-- CLI.lean
import Interpolation.Types
import Interpolation.Parser

namespace Interpolation.CLI
open Interpolation

def parseArgs (args : List String) : Option Config := do
  let rec aux (remaining : List String) (methods : List String) 
    (step : Float) (windowSize : Nat) : Option Config :=
    match remaining with
    | [] => 
      some { method := methods, step := step, windowSize := windowSize }
    | "--linear" :: rest => 
      aux rest (methods ++ ["linear"]) step windowSize
    | "--newton" :: rest => 
      aux rest (methods ++ ["newton"]) step windowSize
    | "--lagrange" :: rest => 
      aux rest (methods ++ ["lagrange"]) step windowSize
    | "--step" :: s :: rest =>
      match Parser.parseFloat s with
      | some newStep => aux rest methods newStep windowSize
      | none => none
    | "-n" :: n :: rest =>
      match n.toNat? with
      | some size => aux rest methods step size
      | none => none
    | _ :: rest => aux rest methods step windowSize
  aux args [] 1.0 4

def printHelp : IO Unit := do
  IO.println "Usage: interpolation [OPTIONS]"
  IO.println ""
  IO.println "Options:"
  IO.println "  --linear           Use linear interpolation"
  IO.println "  --newton           Use Newton interpolation"
  IO.println "  --lagrange         Use Lagrange interpolation"
  IO.println "  --step STEP        Set discretization step (default: 1.0)"
  IO.println "  -n SIZE            Set window size for Newton/Lagrange (default: 4)"
  IO.println ""
  IO.println "Input format: x;y or x\\ty (one point per line)"
  IO.println "Data should be sorted by x in ascending order"

end Interpolation.CLI
