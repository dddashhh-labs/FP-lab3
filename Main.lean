import Interpolation.Types
import Interpolation.Linear
import Interpolation.Newton
import Interpolation.Parser
import Interpolation.Output
import Interpolation.Stream
import Interpolation.CLI

open Interpolation

/-- Главная функция программы -/
def main (args : List String) : IO UInt32 := do
  -- Парсим аргументы командной строки
  match CLI.parseArgs args with
  | none =>
    CLI.printHelp
    return 1
  | some config =>
    -- Проверяем, что указан хотя бы один метод
    if config.method.isEmpty then
      IO.eprintln "Error: No interpolation method specified"
      CLI.printHelp
      return 1
    
    -- Проверяем валидность параметров
    if config.step <= 0 then
      IO.eprintln "Error: Step must be positive"
      return 1
    
    if config.windowSize < 2 then
      IO.eprintln "Error: Window size must be at least 2"
      return 1
    
    -- Обрабатываем каждый метод последовательно
    for method in config.method do
      match method with
      | "linear" =>
        Stream.processStreamLinear config.step
      | "newton" =>
        Stream.processStreamNewton config.windowSize config.step
      | "lagrange" =>
        IO.eprintln "Lagrange interpolation not yet implemented"
      | _ =>
        IO.eprintln s!"Unknown method: {method}"
    
    return 0
