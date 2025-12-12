-- Output.lean (печать выходных данных)
import Interpolation.Types

namespace Interpolation.Output
open Interpolation

def printPoint (method : String) (point : Point) : IO Unit :=
  IO.println s!"{method}: {point.x} {point.y}"

def printPoints (method : String) (points : List Point) : IO Unit :=
  for p in points do
    printPoint method p

end Interpolation.Output
