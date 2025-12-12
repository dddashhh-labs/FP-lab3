import Interpolation.Types
import Interpolation.Parser

namespace Interpolation.CLI
open Interpolation

/-- Парсинг аргументов командной строки -/
def parseArgs (args : List String) : Option Config := do
  let rec aux (remaining : List String) (config : Config) : Option Config :=
    match remaining with
    | [] => some config
    | "--linear" :: rest =>
      aux rest (Config.mk (config.method ++ ["linear"]) config.step config.windowSize)
    | "--newton" :: rest =>
      aux rest (Config.mk (config.method ++ ["newton"]) config.step config.windowSize)
    | "--lagrange" :: rest =>
      aux rest (Config.mk (config.method ++ ["lagrange"]) config.step config.windowSize)
    | "--step" :: s :: rest =>
      match Parser.parseFloat s with
      | some step => 
        if step > 0 then
          aux rest (Config.mk config.method step config.windowSize)
        else
          none
      | none => none
    | "-n" :: n :: rest =>
      match n.toNat? with
      | some size => 
        if size >= 2 then
          aux rest (Config.mk config.method config.step size)
        else
          none
      | none => none
    | "--help" :: _ => none
    | _ :: rest => aux rest config
  
  let defaultConfig : Config := Config.mk [] 1.0 4
  aux args defaultConfig

/-- Вывод справки -/
def printHelp : IO Unit := do
  IO.println "Usage: interpolation [OPTIONS]"
  IO.println ""
  IO.println "Streaming interpolation calculator"
  IO.println ""
  IO.println "Options:"
  IO.println "  --linear           Use linear interpolation"
  IO.println "  --newton           Use Newton polynomial interpolation"
  IO.println "  --lagrange         Use Lagrange polynomial interpolation (not implemented)"
  IO.println "  --step STEP        Set discretization step (default: 1.0, must be > 0)"
  IO.println "  -n SIZE            Set window size for Newton/Lagrange (default: 4, must be >= 2)"
  IO.println "  --help             Show this help message"
  IO.println ""
  IO.println "Input format:"
  IO.println "  One point per line in format: x;y or x\\ty or x y"
  IO.println "  Data must be sorted by x in ascending order"
  IO.println ""
  IO.println "Examples:"
  IO.println "  echo -e '0 0\\n1 1\\n2 2' | interpolation --linear --step 0.5"
  IO.println "  cat data.csv | interpolation --newton -n 4 --step 0.25"

/-- Теорема: парсинг пустых аргументов возвращает конфиг по умолчанию -/
theorem parseArgs_empty : 
    parseArgs [] = some (Config.mk [] 1.0 4) := by
  unfold parseArgs
  simp

/-- Теорема: парсинг --linear добавляет метод в список -/
theorem parseArgs_linear : 
    ∃ cfg, parseArgs ["--linear"] = some cfg ∧ cfg.method = ["linear"] := by
  unfold parseArgs
  simp
  exists Config.mk ["linear"] 1.0 4
  constructor
  · rfl
  · rfl

end Interpolation.CLI
