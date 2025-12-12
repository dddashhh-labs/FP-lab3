import Lake
open Lake DSL

package «interpolation» where
  -- add package configuration options here

lean_lib «Interpolation» where
  -- Явный порядок компиляции модулей
  globs := #[.andSubmodules `Interpolation]

@[default_target]
lean_exe «interpolation» where
  root := `Main
  supportInterpreter := true
