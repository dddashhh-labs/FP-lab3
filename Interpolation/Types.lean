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
      have h := ih _
      apply Nat.le_trans h
      apply Nat.le_succ

/-- Вспомогательная лемма: если a > b, то a ≥ b -/
theorem le_of_not_gt {a b : Nat} (h : ¬(a > b)) : a ≤ b := by
  cases Nat.lt_or_ge a b
  case inl hlt => exact Nat.le_of_lt hlt
  case inr hge => 
    cases Nat.eq_or_lt_of_le hge
    case inl heq => rw [heq]
    case inr hgt => 
      exfalso
      apply h
      exact hgt

/-- Теорема: добавление точки не увеличивает размер окна больше максимума -/
theorem add_respects_max_size (window : SlidingWindow) (point : Point) :
    (add window point).points.length ≤ window.maxSize := by
  unfold add
  simp only []
  split
  case inl h =>
    -- h : (window.points ++ [point]).length > window.maxSize
    cases window.points
    case nil =>
      simp [List.drop] at *
      cases window.maxSize
      case zero => 
        simp at h
      case succ n =>
        simp at h
        exact Nat.le_refl _
    case cons head tail =>
      simp [List.drop]
      have h_len : (head :: tail ++ [point]).length = (head :: tail).length + 1 := by
        simp [List.length_append]
      rw [h_len] at h
      simp [List.length_append]
      have : tail.length + 1 = (head :: tail).length := by simp
      rw [← this]
      apply Nat.le_of_succ_le_succ
      exact Nat.le_of_lt h
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
    cases window.points
    case nil =>
      simp at h_eq
      subst h_eq
      simp [List.drop] at *
      exact h
    case cons head tail =>
      simp [List.drop]
      have h1 : (head :: tail).length = window.maxSize := h_eq
      simp at h1
      have h2 : (tail ++ [point]).length = tail.length + 1 := by 
        rw [List.length_append]
        simp
      rw [h2, h1]
      cases window.maxSize
      case zero => 
        simp at h1
      case succ n =>
        simp
        exact Nat.eq_of_beq_eq_true rfl
  case inr h_not_gt =>
    -- h_not_gt : ¬(window.points ++ [point]).length > window.maxSize
    exfalso
    have h_len : (window.points ++ [point]).length = window.points.length + 1 := by
      rw [List.length_append]
      simp
    rw [h_len, h_eq] at h_not_gt
    apply h_not_gt
    exact Nat.lt_succ_self _

end SlidingWindow

end Interpolation
