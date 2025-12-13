import Lake
open Lake DSL

package «fp-lab»

lean_lib «Interpolation» where
  roots := #[`Interpolation]

@[default_target]
lean_exe «interpolation» where
  root := `Main
