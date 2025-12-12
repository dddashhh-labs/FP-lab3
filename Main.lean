-- Main.lean
import Interpolation.Types
import Interpolation.Linear
import Interpolation.Newton
import Interpolation.Parser
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
        Stream.processStreamLinear config.step config.stepPositive
      | "newton" =>
        Stream.processStreamNewton config.windowSize config.step 
          config.windowSizePositive config.stepPositive
      | "lagrange" =>
        IO.eprintln "Lagrange interpolation not yet implemented"
      | _ =>
        IO.eprintln s!"Unknown method: {method}"
    return 0
