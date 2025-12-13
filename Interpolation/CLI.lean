import Interpolation.Types
import Interpolation.Parser

namespace Interpolation.CLI

open Interpolation

def parseArgs (args : List String) : Option Config := do
  let rec aux (remaining : List String) (config : Config) : Option Config :=
    match remaining with
    | [] => some config
    | "--linear" :: rest =>
      aux rest (Config.mk (config.method ++ ["linear"]) config.step config.windowSize)
    | "--newton" :: rest =>
      aux rest (Config.mk (config.method ++ ["newton"]) config.step config.windowSize)
    | "--lagrange" :: rest =>
      aux rest (Config.mk (config.method ++ ["lagrange"]) config.step config.windowSize)
    | "--step" :: s :: rest =>
      match Parser.parseFloat s with
      | some step => aux rest (Config.mk config.method step config.windowSize)
      | none => none
    | "-n" :: n :: rest =>
      match n.toNat? with
      | some size => aux rest (Config.mk config.method config.step size)
      | none => none
    | _ :: rest => aux rest config
  let defaultConfig : Config := Config.mk [] 1.0 4
  aux args defaultConfig

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
