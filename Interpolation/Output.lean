-- Output.lean
import Interpolation.Types

namespace Interpolation.Output
open Interpolation

def formatResult (result : InterpolationResult) : String :=
  s!"{result.method}: {result.point.x} {result.point.y}"

def printResults (results : List InterpolationResult) : IO Unit := do
  for result in results do
    IO.println (formatResult result)

end Interpolation.Output
