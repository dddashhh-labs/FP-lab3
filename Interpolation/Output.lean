import Interpolation.Types

namespace Interpolation.Output
open Interpolation

/-- Форматирование результата интерполяции для вывода -/
def formatResult (result : InterpolationResult) : String :=
  s!"{result.method}: {result.point.x} {result.point.y}"

/-- Вывод списка результатов -/
def printResults (results : List InterpolationResult) : IO Unit := do
  for result in results do
    IO.println (formatResult result)

/-- Вывод одного результата -/
def printResult (result : InterpolationResult) : IO Unit := do
  IO.println (formatResult result)

/-- Теорема: форматирование не возвращает пустую строку -/
theorem formatResult_nonempty (result : InterpolationResult) : 
    (formatResult result).length > 0 := by
  unfold formatResult
  simp
  sorry -- Требует доказательства для интерполяции строк

end Interpolation.Output
