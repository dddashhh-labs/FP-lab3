-- Stream.lean (главная потоковая обработка)
import Interpolation.Types
import Interpolation.Input
import Interpolation.Output
import Interpolation.StreamProcessor
import Interpolation.SlidingWindow

namespace Interpolation.Stream
open Interpolation

-- Линейная интерполяция в потоковом режиме
partial def processLinear (step : Float) (h : 0 < step) : IO Unit := do
  let mut state : StreamProcessor.LinearState := default
  
  Input.streamPoints fun point => do
    let (newState, results) := StreamProcessor.processLinearPoint state point step h
    state := newState
    Output.printPoints "linear" results
  
  let finalResults := StreamProcessor.finalizeLinear state
  Output.printPoints "linear" finalResults

-- Интерполяция Ньютона в потоковом режиме
partial def processNewton (windowSize : Nat) (step : Float) 
  (hsize : 0 < windowSize) (hstep : 0 < step) : IO Unit := do
  let window := SlidingWindow.empty windowSize hsize
  let mut state : StreamProcessor.NewtonState := {
    window := window
    hasOutput := false
    lastOutputX := none
  }
  
  Input.streamPoints fun point => do
    let hsorted : match state.window.points.getLast? with
                  | some last => last.x ≤ point.x
                  | none => True := by sorry
    let (newState, results) := StreamProcessor.processNewtonPoint state point step hstep hsorted
    state := newState
    Output.printPoints "newton" results
  
  let finalResults := StreamProcessor.finalizeNewton state step hstep
  Output.printPoints "newton" finalResults

end Interpolation.Stream
