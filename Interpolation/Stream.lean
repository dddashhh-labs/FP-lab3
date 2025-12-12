import Interpolation.Types
import Interpolation.Parser
import Interpolation.Linear
import Interpolation.Newton

namespace Interpolation.Stream
open Interpolation

-- Ленивый поток входных данных
partial def readPoints : IO (List Point) := do
  let stdin ← IO.getStdin
  let rec aux (acc : List Point) : IO (List Point) := do
    try
      let line ← stdin.getLine
      if line.trim.isEmpty then
        return acc.reverse
      else
        match Parser.parseLine line with
        | some point => aux (point :: acc)
        | none => 
          IO.eprintln s!"Warning: Could not parse line: {line}"
          aux acc
    catch _ =>
      return acc.reverse
  aux []

-- Генератор точек для интерполяции
def generateXPoints (start finish step : Float) : List Float :=
  let rec aux (current : Float) (acc : List Float) : List Float :=
    if current > finish then
      acc.reverse
    else
      aux (current + step) (current :: acc)
  aux start []

-- Линейная интерполяция в функциональном стиле
def processLinearFunctional (points : List Point) (step : Float) : List InterpolationResult :=
  let rec processWindow (remaining : List Point) (acc : List InterpolationResult) : List InterpolationResult :=
    match remaining with
    | [] => acc.reverse
    | [_] => acc.reverse
    | p1 :: p2 :: rest =>
      -- Для первого окна генерируем все точки до p2
      let xPoints := if acc.isEmpty then
        generateXPoints p1.x p2.x step
      else
        -- Для остальных окон - только одну точку (центральную)
        [p1.x]
      
      let results := xPoints.map fun x =>
        let y := Linear.interpolate p1 p2 x
        InterpolationResult.mk "linear" (Point.mk x y)
      
      processWindow (p2 :: rest) (acc ++ results)
  
  let results := processWindow points []
  
  -- Добавляем последнюю точку
  match points.getLast? with
  | some last => results ++ [InterpolationResult.mk "linear" last]
  | none => results

-- Newton интерполяция в функциональном стиле
def processNewtonFunctional (points : List Point) (windowSize : Nat) (step : Float) : List InterpolationResult :=
  let rec slidingWindow (remaining : List Point) (acc : List InterpolationResult) : List InterpolationResult :=
    if remaining.length < windowSize then
      -- Последнее окно - генерируем все оставшиеся точки
      if remaining.length >= 2 then
        match remaining.head?, remaining.getLast? with
        | some first, some last =>
          let startX := if acc.isEmpty then first.x else
            match acc.getLast? with
            | some lastResult => lastResult.point.x + step
            | none => first.x
          let xPoints := generateXPoints startX last.x step
          let results := xPoints.map fun x =>
            let y := Newton.newtonPolynomial remaining x
            InterpolationResult.mk "newton" (Point.mk x y)
          acc.reverse ++ results ++ [InterpolationResult.mk "newton" last]
        | _, _ => acc.reverse
      else
        acc.reverse
    else
      let window := remaining.take windowSize
      match window.head?, window.getLast? with
      | some first, some last =>
        if acc.isEmpty then
          -- Первое окно - генерируем все точки
          let xPoints := generateXPoints first.x last.x step
          let results := xPoints.map fun x =>
            let y := Newton.newtonPolynomial window x
            InterpolationResult.mk "newton" (Point.mk x y)
          slidingWindow (remaining.drop 1) (results.reverse ++ acc)
        else
          -- Средние окна - генерируем только центральную точку
          let centerIdx := windowSize / 2
          match window.get? centerIdx with
          | some centerPoint =>
            let y := Newton.newtonPolynomial window centerPoint.x
            let result := InterpolationResult.mk "newton" (Point.mk centerPoint.x y)
            slidingWindow (remaining.drop 1) (result :: acc)
          | none => slidingWindow (remaining.drop 1) acc
      | _, _ => acc.reverse
  
  slidingWindow points []

-- Потоковая обработка для линейной интерполяции
partial def processStreamLinear (step : Float) : IO Unit := do
  let stdin ← IO.getStdin
  
  let rec processStream (prevPoint : Option Point) : IO Unit := do
    try
      let line ← stdin.getLine
      if line.trim.isEmpty then
        -- EOF - выводим последнюю точку
        match prevPoint with
        | some p => IO.println s!"linear: {p.x} {p.y}"
        | none => pure ()
      else
        match Parser.parseLine line with
        | some currentPoint =>
          match prevPoint with
          | some prev =>
            -- Генерируем точки между prev и current
            let xPoints := generateXPoints prev.x currentPoint.x step
            for x in xPoints do
              if x < currentPoint.x then
                let y := Linear.interpolate prev currentPoint x
                IO.println s!"linear: {x} {y}"
          | none =>
            -- Первая точка
            IO.println s!"linear: {currentPoint.x} {currentPoint.y}"
          
          processStream (some currentPoint)
        | none =>
          IO.eprintln s!"Warning: Could not parse line: {line}"
          processStream prevPoint
    catch _ =>
      -- EOF
      match prevPoint with
      | some p => IO.println s!"linear: {p.x} {p.y}"
      | none => pure ()
  
  processStream none

-- Потоковая обработка для Newton интерполяции
partial def processStreamNewton (windowSize : Nat) (step : Float) : IO Unit := do
  let stdin ← IO.getStdin
  
  let rec processStream (buffer : List Point) (outputStarted : Bool) (lastOutputX : Float) : IO Unit := do
    try
      let line ← stdin.getLine
      if line.trim.isEmpty then
        -- EOF - выводим оставшиеся точки
        if buffer.length >= 2 then
          match buffer.head?, buffer.getLast? with
          | some first, some last =>
            let startX := if outputStarted then lastOutputX + step else first.x
            let xPoints := generateXPoints startX last.x step
            for x in xPoints do
              let y := Newton.newtonPolynomial buffer x
              IO.println s!"newton: {x} {y}"
            IO.println s!"newton: {last.x} {last.y}"
          | _, _ => pure ()
      else
        match Parser.parseLine line with
        | some point =>
          let newBuffer := buffer ++ [point]
          
          if newBuffer.length < windowSize then
            -- Еще не набрали окно
            processStream newBuffer outputStarted lastOutputX
          else if newBuffer.length == windowSize && not outputStarted then
            -- Первое полное окно - выводим все точки
            match newBuffer.head?, newBuffer.getLast? with
            | some first, some last =>
              let xPoints := generateXPoints first.x last.x step
              for x in xPoints do
                let y := Newton.newtonPolynomial newBuffer x
                IO.println s!"newton: {x} {y}"
              let finalBuffer := newBuffer.drop 1
              processStream finalBuffer true last.x
            | _, _ => processStream newBuffer outputStarted lastOutputX
          else
            -- Скользящее окно - выводим центральную точку
            let window := newBuffer.drop (newBuffer.length - windowSize)
            let centerIdx := windowSize / 2
            match window.get? centerIdx with
            | some centerPoint =>
              let y := Newton.newtonPolynomial window centerPoint.x
              IO.println s!"newton: {centerPoint.x} {y}"
              let finalBuffer := newBuffer.drop 1
              processStream finalBuffer true centerPoint.x
            | none => processStream newBuffer outputStarted lastOutputX
        | none =>
          IO.eprintln s!"Warning: Could not parse line: {line}"
          processStream buffer outputStarted lastOutputX
    catch _ =>
      -- EOF
      if buffer.length >= 2 then
        match buffer.head?, buffer.getLast? with
        | some first, some last =>
          let startX := if outputStarted then lastOutputX + step else first.x
          let xPoints := generateXPoints startX last.x step
          for x in xPoints do
            let y := Newton.newtonPolynomial buffer x
            IO.println s!"newton: {x} {y}"
          IO.println s!"newton: {last.x} {last.y}"
        | _, _ => pure ()
  
  processStream [] false 0.0

end Interpolation.Stream
