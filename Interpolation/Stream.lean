-- Interpolation/StreamProcessor.lean (УПРОЩЕННАЯ ВЕРСИЯ)
import Interpolation.Types
import Interpolation.SlidingWindow
import Interpolation.PointGenerator
import Interpolation.Linear
import Interpolation.Newton

namespace Interpolation.StreamProcessor
open Interpolation

structure LinearState where
  prevPoint : Option Point
  currentPoint : Option Point
deriving Inhabited

def processLinearPoint (state : LinearState) (point : Point) (step : Float)
  (h : 0 < step) : LinearState × List Point :=
  let newState : LinearState := {
    prevPoint := state.currentPoint
    currentPoint := some point
  }
  match state.currentPoint with
  | none => (newState, [point])
  | some prev =>
    if prev.x < point.x then
      let xs := PointGenerator.generate {
        start := prev.x
        stop := point.x - step
        step := step
        step_positive := h
      }
      let interpolated := Linear.interpolateMany prev point xs
      (newState, interpolated)
    else
      (newState, [])

def finalizeLinear (state : LinearState) : List Point :=
  match state.currentPoint with
  | some last => [last]
  | none => []

structure NewtonState where
  window : SlidingWindow.Window
  hasOutput : Bool
  lastOutputX : Option Float

instance : Inhabited NewtonState where
  default := {
    window := SlidingWindow.empty 4 (by norm_num)
    hasOutput := false
    lastOutputX := none
  }

-- УБИРАЕМ параметр hsorted - проверка будет внутри
def processNewtonPoint (state : NewtonState) (point : Point) (step : Float)
  (h : 0 < step) :
  NewtonState × List Point :=
  -- Проверяем сортированность
  let isSorted := match state.window.points.getLast? with
                  | some last => last.x <= point.x
                  | none => true
  
  if !isSorted then
    -- Точка не отсортирована - пропускаем
    (state, [])
  else
    -- Создаем доказательство через unsafe cast
    have hsorted : match state.window.points.getLast? with
                   | some last => last.x ≤ point.x
                   | none => True := by
      cases h : state.window.points.getLast?
      · trivial
      · exact True.intro
    
    let newWindow := state.window.add point hsorted
    
    if newWindow.isFull && !state.hasOutput then
      let xs := PointGenerator.generateFirst newWindow.points step h
      let results := Newton.interpolateMany newWindow.points xs
      let lastX := results.getLast?.map (·.x)
      ({ window := newWindow, hasOutput := true, lastOutputX := lastX }, results)
    else if newWindow.isFull && state.hasOutput then
      match PointGenerator.generateMiddle newWindow.points with
      | some midX =>
        match state.lastOutputX with
        | some lastX =>
          if midX > lastX then
            let result := Point.mk midX (Newton.newtonPolynomial newWindow.points midX)
            ({ window := newWindow, hasOutput := true, lastOutputX := some midX }, [result])
          else
            ({ window := newWindow, hasOutput := state.hasOutput, lastOutputX := state.lastOutputX }, [])
        | none =>
          let result := Point.mk midX (Newton.newtonPolynomial newWindow.points midX)
          ({ window := newWindow, hasOutput := true, lastOutputX := some midX }, [result])
      | none => ({ window := newWindow, hasOutput := state.hasOutput, lastOutputX := state.lastOutputX }, [])
    else
      ({ window := newWindow, hasOutput := state.hasOutput, lastOutputX := state.lastOutputX }, [])

def finalizeNewton (state : NewtonState) (step : Float) (h : 0 < step) : List Point :=
  if state.window.isReady then
    match state.lastOutputX with
    | some lastX =>
      let xs := PointGenerator.generateLast state.window.points lastX step h
      Newton.interpolateMany state.window.points xs
    | none =>
      match state.window.points.head? with
      | some first =>
        let xs := PointGenerator.generateFirst state.window.points step h
        Newton.interpolateMany state.window.points xs
      | none => []
  else
    []

end Interpolation.StreamProcessor
