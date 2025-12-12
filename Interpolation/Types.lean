namespace Interpolation

/-- Точка на плоскости -/
structure Point where
  x : Float
  y : Float
deriving Repr, BEq, Inhabited

/-- Конфигурация программы из аргументов командной строки -/
structure Config where
  method : List String
  step : Float
  windowSize : Nat
deriving Repr, Inhabited

/-- Результат интерполяции -/
structure InterpolationResult where
  method : String
  point : Point
deriving Repr

/-- Скользящее окно для потоковой обработки -/
structure SlidingWindow where
  points : List Point
  maxSize : Nat
deriving Inhabited

namespace SlidingWindow

/-- Добавление точки в окно -/
def add (window : SlidingWindow) (point : Point) : SlidingWindow :=
  let newPoints := window.points ++ [point]
  let finalPoints := if newPoints.length > window.maxSize then
    newPoints.drop 1
  else
    newPoints
  SlidingWindow.mk finalPoints window.maxSize

/-- Проверка готовности окна (минимум 2 точки) -/
def isReady (window : SlidingWindow) : Bool :=
  window.points.length >= 2

/-- Проверка заполненности окна -/
def isFull (window : SlidingWindow) : Bool :=
  window.points.length == window.maxSize

/-- Теорема: добавление точки не увеличивает размер окна больше максимума -/
theorem add_respects_max_size (window : SlidingWindow) (point : Point) :
    (add window point).points.length ≤ window.maxSize := by
  unfold add
  simp only []
  split
  · -- case: length > maxSize
    simp [List.length_drop]
    apply Nat.sub_le
  · -- case: length ≤ maxSize
    simp [List.length_append, List.length_cons, List.length_nil]
    intro h
    exact Nat.le_of_not_lt h

/-- Теорема: если окно было полным, оно останется полным после добавления -/
theorem full_stays_full (window : SlidingWindow) (point : Point) 
    (h : isFull window) : isFull (add window point) := by
  unfold isFull add
  simp only [] at h ⊢
  split
  · -- case: length > maxSize
    simp [List.length_drop]
    have : window.points.length = window.maxSize := h
    simp [this, List.length_append, List.length_cons, List.length_nil]
  · -- case: length ≤ maxSize
    intro h_not_gt
    simp [List.length_append, List.length_cons, List.length_nil] at h_not_gt
    have : window.points.length = window.maxSize := h
    simp [this] at h_not_gt
    exact absurd (Nat.lt_succ_self window.maxSize) h_not_gt

end SlidingWindow

end Interpolation
