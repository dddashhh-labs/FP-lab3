namespace Interpolation

structure Point where
  x : Float
  y : Float
deriving Repr, BEq, Inhabited

structure Config where
  method : List String
  step : Float
  windowSize : Nat
deriving Repr, Inhabited

structure InterpolationResult where
  method : String
  point : Point
deriving Repr

end Interpolation
