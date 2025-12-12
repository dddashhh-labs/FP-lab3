-- StreamProcessor.lean  
import Interpolation.Types
import Interpolation.SlidingWindow
import Interpolation.PointGenerator
import Interpolation.Linear
import Interpolation.Newton

namespace Interpolation.StreamProcessor
open Interpolation

-- ПРАВИЛЬНАЯ архитектура: вход -> обработка -> выход

structure LinearState where
  prevPoint : Option Point
  currentPoint : Option Point
deriving Inhabited

-- Обработка одной точки для линейной интерполяции
def processLinearPoint (state : LinearState) (point : Point) (step : Float) 
  (h : 0 < step) : LinearState × List Point :=
  let newState : LinearState := {
    prevPoint := state.currentPoint
    currentPoint := some point
  }
  match state.currentPoint with
  | none => (newState, [point])  -- Первая точка
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
      (newState, [])  -- Точки не по возрастанию

-- Финализация линейной интерполяции
def finalizeLinear (state : LinearState) : List Point :=
  match state.currentPoint with
  | some last => [last]
  | none => []

structure NewtonState where
  window : SlidingWindow.Window
  hasOutput : Bool
  lastOutputX : Option Float
deriving Inhabited

-- Обработка одной точки для интерполяции Ньютона
def processNewtonPoint (state : NewtonState) (point : Point) (step : Float)
  (h : 0 < step) 
  (hsorted : match state.window.points.getLast? with
             | some last => last.x ≤ point.x
             | none => True) : 
  NewtonState × List Point :=
  let newWindow := state.window.add point hsorted
  
  if newWindow.isFull && !state.hasOutput then
    -- Первое полное окно - выводим все точки
    let xs := PointGenerator.generateFirst newWindow.points step h
    let results := Newton.interpolateMany newWindow.points xs
    let lastX := results.getLast?.map (·.x)
    ({ window := newWindow, hasOutput := true, lastOutputX := lastX }, results)
  else if newWindow.isFull && state.hasOutput then
    -- Последующие окна - выводим центральную точку
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
    | none => (state, [])
  else
    ({ window := newWindow, hasOutput := state.hasOutput, lastOutputX := state.lastOutputX }, [])

-- Финализация интерполяции Ньютона  
def finalizeNewton (state : NewtonState) (step : Float) (h : 0 < step) : List Point :=
  if state.window.isReady then
    match state.lastOutputX with
    | some lastX =>
      let xs := PointGenerator.generateLast state.window.points lastX step h
      Newton.interpolateMany state.window.points xs
    | none =>
      -- Окно не заполнилось - выводим все
      match state.window.points.head? with
      | some first =>
        let xs := PointGenerator.generateFirst state.window.points step h
        Newton.interpolateMany state.window.points xs
      | none => []
  else
    []

end Interpolation.StreamProcessor
