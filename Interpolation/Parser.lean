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

/-- Вспомогательная лемма о длине списка после drop -/
lemma length_drop_le {α : Type _} (n : Nat) (l : List α) : 
    (l.drop n).length ≤ l.length := by
  induction l generalizing n with
  | nil => simp
  | cons _ _ ih =>
    cases n with
    | zero => simp
    | succ n' => 
      simp [List.drop]
      exact Nat.le_trans (ih n') (Nat.le_succ _)

/-- Теорема: добавление точки не увеличивает размер окна больше максимума -/
theorem add_respects_max_size (window : SlidingWindow) (point : Point) :
    (add window point).points.length ≤ window.maxSize := by
  unfold add
  simp only []
  split
  case inl h =>
    have h1 : (window.points ++ [point]).length = window.points.length + 1 := by
      simp [List.length_append]
    simp only [h1]
    have h2 : (window.points ++ [point]).drop 1 = window.points ++ [] := by
      cases window.points with
      | nil => simp
      | cons _ _ => simp [List.drop]
    simp only [h2, List.append_nil]
    exact Nat.le_of_lt h
  case inr h =>
    simp [List.length_append]
    exact Nat.le_of_not_lt h

/-- Вспомогательная лемма о том, что length списка + 1 > maxSize означает length >= maxSize -/
lemma length_succ_gt_implies_ge {n m : Nat} : n + 1 > m → n ≥ m := by
  intro h
  exact Nat.le_of_lt_succ h

/-- Теорема: если окно было полным, оно останется полным после добавления -/
theorem full_stays_full (window : SlidingWindow) (point : Point) 
    (h : isFull window) : isFull (add window point) := by
  unfold isFull add
  simp only [] at h ⊢
  split
  case inl h_gt =>
    have h_eq : window.points.length = window.maxSize := h
    have h1 : (window.points ++ [point]).length = window.maxSize + 1 := by
      simp [List.length_append, h_eq]
    simp only [h1]
    cases window.points with
    | nil => 
      simp at h_eq
      cases window.maxSize with
      | zero => rfl
      | succ _ => contradiction
    | cons _ tail =>
      simp [List.drop]
      have : (tail ++ [point]).length + 1 = window.maxSize + 1 := by
        simp [List.length_append]
        have : (Point :: tail).length = window.maxSize := h
        simp at this
        exact this
      simp [List.length_append] at this
      exact Nat.add_right_cancel this
  case inr h_not_gt =>
    exfalso
    have h_eq : window.points.length = window.maxSize := h
    have : (window.points ++ [point]).length = window.maxSize + 1 := by
      simp [List.length_append, h_eq]
    simp [this] at h_not_gt

end SlidingWindow

end Interpolation
