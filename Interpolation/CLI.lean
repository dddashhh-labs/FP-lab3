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
      if h1 : 0 < step then
        if h2 : 0 < windowSize then
          some { 
            method := methods
            step := step
            windowSize := windowSize
            step_positive := h1
            window_positive := h2
          }
        else none
      else none
    | "--linear" :: rest => aux rest (methods ++ ["linear"]) step windowSize
    | "--newton" :: rest => aux rest (methods ++ ["newton"]) step windowSize
    | "--step" :: s :: rest =>
      match Parser.parseFloat s with
      | some newStep => aux rest methods newStep windowSize
      | none => none
    | "-n" :: n :: rest =>
      match n.toNat? with
      | some size => if size > 0 then aux rest methods step size else none
      | none => none
    | _ :: rest => aux rest methods step windowSize
  aux args [] 1.0 4

def printHelp : IO Unit := do
  IO.println "Usage: interpolation [OPTIONS]"
  IO.println "Options:"
  IO.println "  --linear           Use linear interpolation"
  IO.println "  --newton           Use Newton interpolation"
  IO.println "  --step STEP        Discretization step (default: 1.0)"
  IO.println "  -n SIZE            Window size for Newton (default: 4)"

end Interpolation.CLI
