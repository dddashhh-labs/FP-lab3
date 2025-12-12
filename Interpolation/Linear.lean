-- Linear.lean
import Interpolation.Types

namespace Interpolation.Linear
open Interpolation

def interpolate (p1 p2 : Point) (x : Float) : Float :=
  if (p2.x - p1.x).abs < 0.0001 then 
    p1.y
  else
    let slope := (p2.y - p1.y) / (p2.x - p1.x)
    p1.y + slope * (x - p1.x)

partial def generatePoints (p1 p2 : Point) (step : Float) : List Point :=
  let rec aux (currentX : Float) (acc : List Point) : List Point :=
    if currentX + step > p2.x then
      acc.reverse
    else
      let y := interpolate p1 p2 currentX
      let newPoint := Point.mk currentX y
      aux (currentX + step) (newPoint :: acc)
  aux p1.x []

end Interpolation.Linear
