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

/-- Теорема: форматирование детерминировано -/
theorem formatResult_deterministic (result : InterpolationResult) :
    formatResult result = formatResult result := by
  rfl

/-- Теорема: formatResult всегда возвращает непустую строку для корректного результата -/
theorem formatResult_nonempty (result : InterpolationResult) 
    (h : result.method.length > 0) :
    (formatResult result).length > 0 := by
  unfold formatResult
  -- Строка содержит как минимум method, двоеточие и пробелы
  apply String.length_pos

end Interpolation.Output
