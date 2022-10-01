# Agda FFI to Haskell libraries


## Contents

### (Almost) complete raw bindings to
* `array-0.5.4.0`
* `dear-imgui-2.1.1` (no GLFW, partial Vulkan)
* `deepseq-1.4.8.0`
* `JuicyPixels-3.3.8` (no internals)
* `StateVar-1.2.2`
* `stm-2.5.1.0`
* `unliftio-core-2.0.1` (no `UnliftIO` type)

### Substantial raw bindings to
* `base-4.17.0.0` (no GHC.\*, Type.\*, Data.Type.\*, Data.Bi\*)
* `scientific-0.3.7.0` (no Text/ByteString builders)
* `sdl2-2.5.3.3` (no SDL.Time, SDL.Input.Keyboard\[.Codes\], SDL.Hint, SDL.Internal.\*, SDL.Raw.\*)

### Poor raw bindings (for use by other bindings only) to
* `bytestring-0.11.3.1` (only Data.ByteString(.Internal), Data.ByteString.Lazy(.Internal))
* `exceptions`
* `linear`
* `primitive`
* `text-2.0.1` (only Data.Text)
* `transformers-0.6.0.4` (core, only IdentityT)
* `vector-0.12.3.1` (only storable)

### Planned bindings to
* `containers`
* `aeson`
* `binary`
* ...


## Issues

* __This repo uses agda with modified "compile" pragma handling (agda pr 6050). For instance, handling haskell instances for monads requires this.__
* Many modules haven't been compiled and can contain wrong ffi pragmas.
Almost nothing has been tested.
* All MAlonzo generated code, including libraries, ends up in the project being compiled (agda issue 2330).
Thus, all code generated from bindings compiles under single cabal file, which in turn must specify all Haskell dependencies, including transitive ones. (Can cause conflicting module names? fix: PackageImports, but only on hs side)
* `dear-imgui-2.1.1` has dependency constraints incompatible with base-4.17.0.0, new TH etc.
Temporary fix - `allow-newer` in `cabal.project`.
* Preprocessor (like hs CPP ext) is required for correct handling of some library flags, platform dependent modules etc.
* Anything involving kinds other than `Type` is unsupported.
* Glue code for stdlib (and mb smth else) is planned.
* Not sure about handling type families.


## Structure

* `Ffi.Hs.-(lib).*` &#8211; internal helper modules.
* `Ffi.Hs.*` &#8211; raw bindings to Haskell modules with the same names (w/o `Ffi.Hs.` prefix).
* `*-Instanced` &#8211; corresponding module's haskell instances redeclared in the `instance` block (?,todo).


## Notes

* Raw binding libs don't have any agda dependencies besides other bindings.
* `print-libs.sh` prints `.agda-lib` paths which can be pasted into `~/.agda/libraries` file for installation.
* `--ghc-dont-call-ghc` agda compilation flag is recommended as it avoids running ghc on generated code which may require libraries unavailable to plain ghc.

