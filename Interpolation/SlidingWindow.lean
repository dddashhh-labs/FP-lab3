-- SlidingWindow.lean
import Interpolation.Types

namespace Interpolation.SlidingWindow
open Interpolation

structure Window where
  points : List Point
  maxSize : Nat
  size_invariant : points.length ≤ maxSize
  sorted : IsSorted points

-- Доказательство сохранения инвариантов
theorem add_preserves_invariants (w : Window) (p : Point)
  (h : match w.points.getLast? with
       | some last => last.x ≤ p.x
       | none => True) :
  ∃ w' : Window, w'.points.length ≤ w'.maxSize ∧ IsSorted w'.points := by
  sorry

def empty (maxSize : Nat) (h : 0 < maxSize) : Window :=
  { points := []
    maxSize := maxSize
    size_invariant := by simp
    sorted := by simp [IsSorted] }

def add (w : Window) (p : Point) 
  (h : match w.points.getLast? with
       | some last => last.x ≤ p.x
       | none => True) : Window :=
  let newPoints := w.points ++ [p]
  let finalPoints := if newPoints.length > w.maxSize then
    newPoints.drop 1
  else
    newPoints
  { points := finalPoints
    maxSize := w.maxSize
    size_invariant := by sorry
    sorted := by sorry }

def isFull (w : Window) : Bool :=
  w.points.length == w.maxSize

def isReady (w : Window) : Bool :=
  w.points.length >= 2

end Interpolation.SlidingWindow
