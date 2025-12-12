-- Main.lean
import Interpolation.Types
import Interpolation.Stream
import Interpolation.CLI

open Interpolation

def main (args : List String) : IO UInt32 := do
  match CLI.parseArgs args with
  | none =>
    CLI.printHelp
    return 1
  | some config =>
    if config.method.isEmpty then
      IO.eprintln "Error: No interpolation method specified"
      CLI.printHelp
      return 1
    
    for method in config.method do
      match method with
      | "linear" =>
        Stream.processLinear config.step config.step_positive
      | "newton" =>
        Stream.processNewton config.windowSize config.step 
          config.window_positive config.step_positive
      | _ =>
        IO.eprintln s!"Unknown method: {method}"
    
    return 0
