import Lake
open Lake DSL

package «interpolation» where
  -- add package configuration options here

lean_lib «Interpolation» where
  -- add library configuration options here
  roots := #[
    `Interpolation.Types,
    `Interpolation.Parser,
    `Interpolation.Linear,
    `Interpolation.Newton,
    `Interpolation.PointGenerator,
    `Interpolation.SlidingWindow,
    `Interpolation.StreamProcessor,
    `Interpolation.Input,
    `Interpolation.Output,
    `Interpolation.Stream,
    `Interpolation.CLI
  ]

@[default_target]
lean_exe «interpolation» where
  root := `Main
