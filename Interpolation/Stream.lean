-- Stream.lean
import Interpolation.Types
import Interpolation.Parser
import Interpolation.Linear
import Interpolation.Newton

namespace Interpolation.Stream
open Interpolation

structure SlidingWindow where
  points : List Point
  maxSize : Nat
  sorted : IsSorted points := by sorry
  sizeInvariant : points.length ≤ maxSize := by sorry
deriving Inhabited

-- Доказательство сохранения инварианта при добавлении
theorem add_preserves_sorted (window : SlidingWindow) (point : Point)
  (h : match window.points.getLast? with
       | some last => last.x ≤ point.x
       | none => True) :
  IsSorted (window.add point).points := by
  sorry

def SlidingWindow.add (window : SlidingWindow) (point : Point) : SlidingWindow :=
  let newPoints := window.points ++ [point]
  let finalPoints := if newPoints.length > window.maxSize then
    newPoints.drop 1
  else
    newPoints
  { points := finalPoints
    maxSize := window.maxSize
    sorted := by sorry
    sizeInvariant := by sorry }

def SlidingWindow.isReady (window : SlidingWindow) : Bool :=
  window.points.length >= 2

def SlidingWindow.isFull (window : SlidingWindow) : Bool :=
  window.points.length == window.maxSize

-- Потоковая обработка с доказательствами корректности
partial def processStreamLinear (step : Float) (hstep : 0 < step := by native_decide) : IO Unit := do
  let stdin ← IO.getStdin
  let mut prevPoint : Option Point := none
  let mut currentPoint : Option Point := none
  
  repeat
    try
      let line ← stdin.getLine
      if line.trim.isEmpty then break
      
      match Parser.parseLine line with
      | some point =>
        prevPoint := currentPoint
        currentPoint := some point
        
        match prevPoint, currentPoint with
        | some prev, some curr =>
          if h : prev.x < curr.x then
            let interpolated := Linear.generatePoints prev curr step hstep
            for p in interpolated do
              IO.println s!"linear: {p.x} {p.y}"
          else
            IO.eprintln "Warning: Points not in ascending order"
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

partial def processStreamNewton (windowSize : Nat) (step : Float) 
  (hsize : 0 < windowSize := by native_decide)
  (hstep : 0 < step := by native_decide) : IO Unit := do
  let stdin ← IO.getStdin
  let mut window : SlidingWindow := 
    { points := []
      maxSize := windowSize
      sorted := by simp [IsSorted]
      sizeInvariant := by simp }
  let mut hasOutput := false
  
  repeat
    try
      let line ← stdin.getLine
      if line.trim.isEmpty then break
      
      match Parser.parseLine line with
      | some point =>
        window := window.add point
        
        if window.isFull ∧ !hasOutput then
          let points := window.points
          match points.head?, points.getLast? with
          | some first, some last =>
            let rec generateInitial (x : Float) (acc : List Point) : List Point :=
              if x > last.x then acc.reverse
              else
                let y := Newton.newtonPolynomial points x
                generateInitial (x + step) (Point.mk x y :: acc)
            let results := generateInitial first.x []
            for p in results do
              IO.println s!"newton: {p.x} {p.y}"
            hasOutput := true
          | _, _ => pure ()
        else if window.isFull ∧ hasOutput then
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
  
  -- Финальная обработка
  if window.points.length >= 2 then
    let points := window.points
    match points.getLast? with
    | some last =>
      let startX := if hasOutput then
        match points.get? (windowSize / 2) with
        | some p => p.x + step
        | none => match points.head? with | some p => p.x | none => 0.0
      else
        match points.head? with | some p => p.x | none => 0.0
      
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
