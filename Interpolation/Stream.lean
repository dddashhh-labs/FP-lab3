import Interpolation.Types
import Interpolation.Parser
import Interpolation.Linear
import Interpolation.Newton

namespace Interpolation.Stream

open Interpolation

structure SlidingWindow where
  points : List Point
  maxSize : Nat
deriving Inhabited

def SlidingWindow.add (window : SlidingWindow) (point : Point) : SlidingWindow :=
  let newPoints := window.points ++ [point]
  let finalPoints := if newPoints.length > window.maxSize then
    newPoints.drop 1
  else
    newPoints
  SlidingWindow.mk finalPoints window.maxSize

def SlidingWindow.isReady (window : SlidingWindow) : Bool :=
  window.points.length >= 2

def SlidingWindow.isFull (window : SlidingWindow) : Bool :=
  window.points.length == window.maxSize

partial def processStreamLinear (step : Float) : IO Unit := do
  let stdin ← IO.getStdin
  let mut prevPoint : Option Point := none
  let mut currentPoint : Option Point := none
  repeat
    try
      let line ← stdin.getLine
      if line.trim.isEmpty then
        break
      match Parser.parseLine line with
      | some point =>
        prevPoint := currentPoint
        currentPoint := some point
        match prevPoint, currentPoint with
        | some prev, some curr =>
          let rec generateLinear (x : Float) (acc : List Point) : List Point :=
            if x >= curr.x then acc.reverse
            else
              let y := Linear.interpolate prev curr x
              generateLinear (x + step) (Point.mk x y :: acc)
          let interpolated := generateLinear prev.x []
          for p in interpolated do
            IO.println s!"linear: {p.x} {p.y}"
        | none, some curr =>
          IO.println s!"linear: {curr.x} {curr.y}"
        | _, _ => pure ()
      | none =>
        IO.eprintln s!"Warning: Could not parse line: {line}"
    catch _ =>
      break
  match currentPoint with
  | some last => IO.println s!"linear: {last.x} {last.y}"
  | none => pure ()

partial def processStreamNewton (windowSize : Nat) (step : Float) : IO Unit := do
  let stdin ← IO.getStdin
  let mut window := SlidingWindow.mk [] windowSize
  let mut hasOutput := false
  repeat
    try
      let line ← stdin.getLine
      if line.trim.isEmpty then
        break
      match Parser.parseLine line with
      | some point =>
        window := window.add point
        if window.isFull && !hasOutput then
          let points := window.points
          match points.head?, points.getLast? with
          | some first, some last =>
            let rec generateInitial (x : Float) (acc : List Point) : List Point :=
              if x > last.x - step then acc.reverse
              else
                let y := Newton.newtonPolynomial points x
                generateInitial (x + step) (Point.mk x y :: acc)
            let results := generateInitial first.x []
            for p in results do
              IO.println s!"newton: {p.x} {p.y}"
            hasOutput := true
          | _, _ => pure ()
        else if window.isFull && hasOutput then
          let points := window.points
          let midIdx := windowSize / 2
          match points.get? midIdx with
          | some midPoint =>
            let y := Newton.newtonPolynomial points midPoint.x
            IO.println s!"newton: {midPoint.x} {y}"
          | none => pure ()
      | none =>
        IO.eprintln s!"Warning: Could not parse line: {line}"
    catch _ =>
      break
  if window.points.length >= 2 then
    let points := window.points
    let startX := if hasOutput then
      match points.get? (windowSize / 2) with
      | some p => p.x + step
      | none =>
        match points.head? with
        | some p => p.x
        | none => 0.0
    else
      match points.head? with
      | some p => p.x
      | none => 0.0
    match points.getLast? with
    | some last =>
      let rec generateFinal (x : Float) (acc : List Point) : List Point :=
        if x > last.x then acc.reverse
        else
          let y := Newton.newtonPolynomial points x
          generateFinal (x + step) (Point.mk x y :: acc)
      let results := generateFinal startX []
      for p in results do
        IO.println s!"newton: {p.x} {p.y}"
    | none => pure ()

end Interpolation.Stream
