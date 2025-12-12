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
      rw [heq]
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
    -- h : (window.points ++ [point]).length > window.maxSize
    cases window.points with
    | nil =>
      simp [List.drop] at *
      cases window.maxSize with
      | zero => 
        simp at h
      | succ n =>
        simp at h
    | cons head tail =>
      simp [List.drop]
      have h1 : (head :: tail).length + 1 > window.maxSize := by
        simp [List.length_append] at h
        exact h
      have h2 : tail.length + 1 = (head :: tail).length := by simp
      rw [List.length_append]
      simp
      have h3 : tail.length < window.maxSize := by
        have : (head :: tail).length > window.maxSize - 1 := by
          cases window.maxSize with
          | zero => 
            simp at h1
          | succ m =>
            simp at h1
            exact h1
        simp at this
        cases window.maxSize with
        | zero => simp at h1
        | succ m =>
          simp at h1
          exact Nat.lt_of_succ_lt h1
      exact Nat.le_of_lt h3
  case inr h =>
    -- h : ¬(window.points ++ [point]).length > window.maxSize
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
    -- h_gt : (window.points ++ [point]).length > window.maxSize
    cases window.points with
    | nil =>
      simp at h_eq
      subst h_eq
      simp [List.drop]
      exact h
    | cons head tail =>
      simp [List.drop]
      have h1 : (head :: tail).length = window.maxSize := h_eq
      simp at h1
      rw [List.length_append]
      simp
      cases window.maxSize with
      | zero => 
        simp at h1
      | succ n =>
        simp at h1
        rw [h1]
  case inr h_not_gt =>
    -- h_not_gt : ¬(window.points ++ [point]).length > window.maxSize
    exfalso
    have h_len : (window.points ++ [point]).length = window.points.length + 1 := by
      rw [List.length_append]
      simp
    rw [h_len, h_eq] at h_not_gt
    have : window.maxSize + 1 > window.maxSize := Nat.lt_succ_self _
    exact h_not_gt this

end SlidingWindow

end Interpolation
