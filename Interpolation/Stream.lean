import Interpolation.Types
import Interpolation.Parser
import Interpolation.Linear
import Interpolation.Newton

namespace Interpolation.Stream
open Interpolation

structure LinearState where
  prevPoint : Option Point
  prevPrevPoint : Option Point
  lastOutput : Float
deriving Inhabited

structure NewtonState where
  window : List Point
  windowSize : Nat
  lastOutput : Option Float
  firstWindowProcessed : Bool
deriving Inhabited

def generateInRange (start : Float) (endX : Float) (step : Float)
    (interpolate : Float → Float) (inclusive : Bool := false) : List Point :=
  let maxSteps := 10000
  let rec aux (currentX : Float) (acc : List Point) (stepsLeft : Nat) : List Point :=
    match stepsLeft with
    | 0 => acc.reverse
    | n + 1 =>
      let shouldContinue := if inclusive then currentX <= endX else currentX < endX
      if shouldContinue then
        let y := interpolate currentX
        aux (currentX + step) (Point.mk currentX y :: acc) n
      else
        acc.reverse
  aux start [] maxSteps
termination_by aux _ _ stepsLeft => stepsLeft

def processLinearPoint (state : LinearState) (newPoint : Point) (step : Float)
    : LinearState × List Point :=
  match state.prevPoint with
  | none =>
    let output := [newPoint]
    let newState := LinearState.mk (some newPoint) none newPoint.x
    (newState, output)
  | some prev =>
    if newPoint.x <= prev.x then
      (state, [])
    else
      let interpolate := fun x => Linear.interpolate prev newPoint x
      let startX := state.lastOutput + step
      let points := generateInRange startX newPoint.x step interpolate false
      
      let newLastOutput := match points.getLast? with
        | some lastPt => lastPt.x
        | none => state.lastOutput
      
      let newState := LinearState.mk (some newPoint) (some prev) newLastOutput
      (newState, points)

def finalizeLinear (state : LinearState) (step : Float) : List Point :=
  match state.prevPoint, state.prevPrevPoint with
  | some lastPoint, some prevPoint =>
    let interpolate := fun x => Linear.interpolate prevPoint lastPoint x
    let startX := state.lastOutput + step
    if startX <= lastPoint.x then
      generateInRange startX lastPoint.x step interpolate true
    else
      []
  | some lastPoint, none =>
    if state.lastOutput < lastPoint.x then
      [lastPoint]
    else
      []
  | none, _ => []

def processNewtonPoint (state : NewtonState) (newPoint : Point) (step : Float)
    : NewtonState × List Point :=
  let newWindow := state.window ++ [newPoint]
  let finalWindow := if newWindow.length > state.windowSize then
    newWindow.drop 1
  else
    newWindow
  
  let isValid := match finalWindow.getLast?, finalWindow.reverse.tail? with
    | some last, some rest =>
      match rest.head? with
      | some prev => last.x > prev.x
      | none => true
    | _, _ => true
  
  if !isValid then
    (state, [])
  else if finalWindow.length < state.windowSize then
    (NewtonState.mk finalWindow state.windowSize state.lastOutput false, [])
  else
    let interpolate := fun x => Newton.newtonPolynomial finalWindow x
    
    if !state.firstWindowProcessed then
      match finalWindow.head? with
      | none => (state, [])
      | some first =>
        let midIdx := state.windowSize / 2
        match finalWindow.get? midIdx with
        | none => (state, [])
        | some mid =>
          let points := generateInRange first.x mid.x step interpolate true
          let newState := NewtonState.mk finalWindow state.windowSize (some mid.x) true
          (newState, points)
    else
      let midIdx := state.windowSize / 2
      match finalWindow.get? midIdx with
      | none => (state, [])
      | some mid =>
        match state.lastOutput with
        | some lastOut =>
          if mid.x <= lastOut then
            (NewtonState.mk finalWindow state.windowSize state.lastOutput true, [])
          else
            let y := interpolate mid.x
            let point := Point.mk mid.x y
            let newState := NewtonState.mk finalWindow state.windowSize (some mid.x) true
            (newState, [point])
        | none =>
          let y := interpolate mid.x
          let point := Point.mk mid.x y
          let newState := NewtonState.mk finalWindow state.windowSize (some mid.x) true
          (newState, [point])

def finalizeNewton (state : NewtonState) (step : Float) : List Point :=
  if state.window.length < 2 then
    []
  else
    let interpolate := fun x => Newton.newtonPolynomial state.window x
    match state.lastOutput, state.window.getLast? with
    | some lastOut, some lastPoint =>
      let startX := lastOut + step
      if startX <= lastPoint.x then
        generateInRange startX lastPoint.x step interpolate true
      else
        []
    | none, some lastPoint =>
      match state.window.head? with
      | some first =>
        generateInRange first.x lastPoint.x step interpolate true
      | none => []
    | _, none => []

partial def processStreamLinear (step : Float) : IO Unit := do
  let stdin ← IO.getStdin
  let mut state : LinearState := LinearState.mk none none 0.0
  
  repeat
    try
      let line ← stdin.getLine
      if line.trim.isEmpty then
        break
      
      match Parser.parseLine line with
      | some point =>
        let (newState, outputs) := processLinearPoint state point step
        state := newState
        for p in outputs do
          IO.println s!"linear: {p.x} {p.y}"
      | none =>
        IO.eprintln s!"Warning: Could not parse line: {line}"
    catch _ =>
      break
  
  let finalOutputs := finalizeLinear state step
  for p in finalOutputs do
    IO.println s!"linear: {p.x} {p.y}"

partial def processStreamNewton (windowSize : Nat) (step : Float) : IO Unit := do
  let stdin ← IO.getStdin
  let mut state : NewtonState := NewtonState.mk [] windowSize none false
  
  repeat
    try
      let line ← stdin.getLine
      if line.trim.isEmpty then
        break
      
      match Parser.parseLine line with
      | some point =>
        let (newState, outputs) := processNewtonPoint state point step
        state := newState
        for p in outputs do
          IO.println s!"newton: {p.x} {p.y}"
      | none =>
        IO.eprintln s!"Warning: Could not parse line: {line}"
    catch _ =>
      break
  
  let finalOutputs := finalizeNewton state step
  for p in finalOutputs do
    IO.println s!"newton: {p.x} {p.y}"

end Interpolation.Stream
