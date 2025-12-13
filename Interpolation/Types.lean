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
theorem length_drop_le {α : Type _} (n : Nat) (l : List α) :
    (l.drop n).length ≤ l.length := by
  induction l generalizing n
  case nil =>
    cases n
    · simp [List.drop]
    · simp [List.drop]
  case cons head tail ih =>
    cases n
    · simp [List.drop]
    · simp [List.drop]
      exact Nat.le_trans (ih _) (Nat.le_succ _)

/-- Вспомогательная лемма: если не a > b, то a ≤ b -/
theorem le_of_not_gt {a b : Nat} (h : ¬(a > b)) : a ≤ b := by
  cases Nat.lt_or_ge a b with
  | inl hlt => exact Nat.le_of_lt hlt
  | inr hge =>
    cases Nat.eq_or_lt_of_le hge with
    | inl heq => 
      subst heq
      exact Nat.le_refl a
    | inr hgt =>
      exfalso
      exact h hgt

/-- Теорема: добавление точки не увеличивает размер окна больше максимума -/
theorem add_respects_max_size (window : SlidingWindow) (point : Point) :
    (add window point).points.length ≤ window.maxSize := by
  unfold add
  simp only []
  split
  case inl h =>
    cases window.points with
    | nil =>
      simp [List.drop] at *
      exact Nat.le_of_lt h
    | cons head tail =>
      simp [List.drop, List.length_append]
      have h_gt : tail.length + 1 + 1 > window.maxSize := by
        simp [List.length_append] at h
        exact h
      cases window.maxSize with
      | zero => 
        simp at h_gt
      | succ m =>
        simp at h_gt
        exact Nat.le_of_succ_le_succ (Nat.le_of_lt h_gt)
  case inr h =>
    exact le_of_not_gt h

/-- Теорема: если окно было полным, оно останется полным после добавления -/
theorem full_stays_full (window : SlidingWindow) (point : Point) 
    (h : isFull window) : isFull (add window point) := by
  unfold isFull add
  simp only [] at h ⊢
  have h_eq : window.points.length = window.maxSize := 
    Nat.eq_of_beq_eq_true h
  split
  case inl h_gt =>
    cases window.points with
    | nil =>
      simp at h_eq
      subst h_eq
      simp [List.drop]
      exact h
    | cons head tail =>
      simp [List.drop, List.length_append]
      have h1 : tail.length + 1 = window.maxSize := by
        simp at h_eq
        exact h_eq
      cases window.maxSize with
      | zero => 
        simp at h1
      | succ n =>
        simp at h1
        subst h1
        simp
  case inr h_not_gt =>
    have h_len : (window.points ++ [point]).length = window.points.length + 1 := by
      simp [List.length_append]
    subst h_eq
    simp [h_len] at h_not_gt

end SlidingWindow

end Interpolation
