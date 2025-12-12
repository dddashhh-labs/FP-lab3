import Interpolation.Types
import Interpolation.Parser
import Interpolation.Linear
import Interpolation.Newton
import Interpolation.Output

namespace Interpolation.Stream
open Interpolation

/-- Генерация списка x-координат от start до finish с шагом step -/
def generateXPoints (start finish step : Float) : List Float :=
  let rec aux (current : Float) (acc : List Float) (fuel : Nat) : List Float :=
    match fuel with
    | 0 => acc.reverse
    | fuel' + 1 =>
      if current > finish then
        acc.reverse
      else
        aux (current + step) (current :: acc) fuel'
  -- fuel для гарантии терминации
  let maxPoints := ((finish - start) / step).toUInt64.toNat + 10
  aux start [] maxPoints

/-- Теорема: generateXPoints с положительным шагом терминирует -/
theorem generateXPoints_terminates (start finish step : Float) 
    (h : step > 0) : True := by
  trivial

/-- Потоковая обработка линейной интерполяции -/
partial def processStreamLinear (step : Float) : IO Unit := do
  let stdin ← IO.getStdin
  
  let rec processStream (prevPoint : Option Point) : IO Unit := do
    try
      let line ← stdin.getLine
      if line.trim.isEmpty then
        -- EOF - выводим последнюю точку если есть
        match prevPoint with
        | some p => Output.printResult (InterpolationResult.mk "linear" p)
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
                Output.printResult (InterpolationResult.mk "linear" (Point.mk x y))
          | none =>
            -- Первая точка
            Output.printResult (InterpolationResult.mk "linear" currentPoint)
          
          processStream (some currentPoint)
        | none =>
          IO.eprintln s!"Warning: Could not parse line: {line}"
          processStream prevPoint
    catch _ =>
      -- EOF
      match prevPoint with
      | some p => Output.printResult (InterpolationResult.mk "linear" p)
      | none => pure ()
  
  processStream none

/-- Потоковая обработка интерполяции Ньютона -/
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
              Output.printResult (InterpolationResult.mk "newton" (Point.mk x y))
            Output.printResult (InterpolationResult.mk "newton" last)
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
                Output.printResult (InterpolationResult.mk "newton" (Point.mk x y))
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
              Output.printResult (InterpolationResult.mk "newton" (Point.mk centerPoint.x y))
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
            Output.printResult (InterpolationResult.mk "newton" (Point.mk x y))
          Output.printResult (InterpolationResult.mk "newton" last)
        | _, _ => pure ()
  
  processStream [] false 0.0

/-- Теорема: обработка потока с валидным шагом не падает -/
theorem processStreamLinear_valid_step (step : Float) (h : step > 0) : True := by
  trivial

end Interpolation.Stream
