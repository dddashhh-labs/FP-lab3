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
  | nil => 
    cases n <;> simp [List.drop]
  | cons head tail ih =>
    cases n with
    | zero => simp [List.drop]
    | succ n' => 
      simp [List.drop]
      calc (tail.drop n').length 
        _ ≤ tail.length := ih n'
        _ ≤ (head :: tail).length := by simp; apply Nat.le_succ

/-- Теорема: добавление точки не увеличивает размер окна больше максимума -/
theorem add_respects_max_size (window : SlidingWindow) (point : Point) :
    (add window point).points.length ≤ window.maxSize := by
  unfold add
  simp only []
  split
  case inl h =>
    -- h : (window.points ++ [point]).length > window.maxSize
    have h_len : (window.points ++ [point]).length = window.points.length + 1 := by
      rw [List.length_append, List.length_cons, List.length_nil]
      simp
    rw [h_len] at h
    clear h_len
    -- Теперь нужно показать, что (window.points ++ [point]).drop 1 имеет длину ≤ maxSize
    have : ((window.points ++ [point]).drop 1).length ≤ (window.points ++ [point]).length := 
      length_drop_le 1 (window.points ++ [point])
    calc ((window.points ++ [point]).drop 1).length
      _ ≤ (window.points ++ [point]).length := this
      _ = window.points.length + 1 := by rw [List.length_append]; simp
      _ ≤ window.maxSize + 1 := Nat.succ_le_succ (Nat.le_of_lt h)
      _ = window.maxSize + 1 := rfl
    -- Нужно более точное доказательство
    cases window.points with
    | nil => 
      simp [List.drop] at *
      apply Nat.le_of_lt h
    | cons head tail =>
      simp [List.drop]
      calc (tail ++ [point]).length
        _ = tail.length + 1 := by rw [List.length_append]; simp
        _ = (head :: tail).length := by simp
        _ < window.maxSize + 1 := h
        _ = window.maxSize + 1 := rfl
      apply Nat.le_of_lt
      calc tail.length + 1
        _ = (head :: tail).length := by simp
        _ < window.maxSize + 1 := h
        _ = window.maxSize.succ := rfl
      apply Nat.lt_succ_iff.mp
      exact h
  case inr h =>
    -- h : ¬(window.points ++ [point]).length > window.maxSize
    have : (window.points ++ [point]).length ≤ window.maxSize := Nat.le_of_not_lt h
    exact this

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
      simp [List.drop] at *
      exact h
    | cons head tail =>
      simp [List.drop]
      have h1 : (head :: tail).length = window.maxSize := h_eq
      simp at h1
      have h2 : (tail ++ [point]).length = tail.length + 1 := by 
        rw [List.length_append]; simp
      rw [h2, h1]
      apply Nat.succ_pred_eq_of_pos
      cases window.maxSize with
      | zero => simp at h1
      | succ n => apply Nat.succ_pos
  case inr h_not_gt =>
    -- h_not_gt : ¬(window.points ++ [point]).length > window.maxSize
    exfalso
    have h_len : (window.points ++ [point]).length = window.points.length + 1 := by
      rw [List.length_append]; simp
    rw [h_len, h_eq] at h_not_gt
    apply h_not_gt
    apply Nat.lt_succ_self

end SlidingWindow

end Interpolation
