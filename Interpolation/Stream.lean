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
deriving Inhabited

def SlidingWindow.empty (maxSize : Nat) : SlidingWindow :=
  { points := [], maxSize := maxSize }

def SlidingWindow.add (window : SlidingWindow) (point : Point) : SlidingWindow :=
  let newPoints := window.points ++ [point]
  let finalPoints := if newPoints.length > window.maxSize then
    newPoints.drop 1
  else
    newPoints
  { points := finalPoints, maxSize := window.maxSize }

def SlidingWindow.isReady (window : SlidingWindow) : Bool :=
  window.points.length >= 2

def SlidingWindow.isFull (window : SlidingWindow) : Bool :=
  window.points.length == window.maxSize

-- Потоковая обработка линейной интерполяции
partial def processStreamLinear (step : Float) : IO Unit := do
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
          if prev.x < curr.x then
            let interpolated := Linear.generatePoints prev curr step
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

-- Потоковая обработка интерполяции Ньютона
partial def processStreamNewton (windowSize : Nat) (step : Float) : IO Unit := do
  let stdin ← IO.getStdin
  let mut window := SlidingWindow.empty windowSize
  let mut hasOutput := false
  let mut lastOutputX : Option Float := none
  
  repeat
    try
      let line ← stdin.getLine
      if line.trim.isEmpty then break
      
      match Parser.parseLine line with
      | some point =>
        window := window.add point
        
        if window.isFull && !hasOutput then
          -- Первое окно - выводим все точки от начала до середины
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
              lastOutputX := some p.x
            hasOutput := true
          | _, _ => pure ()
        else if window.isFull && hasOutput then
          -- Последующие окна - выводим только центральную точку
          let points := window.points
          let midIdx := windowSize / 2
          match points[midIdx]? with
          | some midPoint =>
            -- Проверяем, не выводили ли мы уже эту точку
            match lastOutputX with
            | some lastX =>
              if midPoint.x > lastX then
                let y := Newton.newtonPolynomial points midPoint.x
                IO.println s!"newton: {midPoint.x} {y}"
                lastOutputX := some midPoint.x
            | none =>
              let y := Newton.newtonPolynomial points midPoint.x
              IO.println s!"newton: {midPoint.x} {y}"
              lastOutputX := some midPoint.x
          | none => pure ()
      | none =>
        IO.eprintln s!"Warning: Could not parse line: {line}"
    catch _ =>
      break
  
  -- Финальная обработка - выводим оставшиеся точки
  if window.points.length >= 2 then
    let points := window.points
    match points.getLast?, lastOutputX with
    | some last, some lastX =>
      let startX := lastX + step
      let rec generateFinal (x : Float) (acc : List Point) : List Point :=
        if x > last.x then acc.reverse
        else
          let y := Newton.newtonPolynomial points x
          generateFinal (x + step) (Point.mk x y :: acc)
      let results := generateFinal startX []
      for p in results do
        IO.println s!"newton: {p.x} {p.y}"
    | some last, none =>
      -- Если не было вывода, выводим все точки
      match points.head? with
      | some first =>
        let rec generateAll (x : Float) (acc : List Point) : List Point :=
          if x > last.x then acc.reverse
          else
            let y := Newton.newtonPolynomial points x
            generateAll (x + step) (Point.mk x y :: acc)
        let results := generateAll first.x []
        for p in results do
          IO.println s!"newton: {p.x} {p.y}"
      | none => pure ()
    | _, _ => pure ()

end Interpolation.Stream
