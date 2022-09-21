{-# OPTIONS --without-K #-}

module Main where

open import Ffi.Hs.System.IO using (IO; Functor[IO]; Monad[IO]; MonadIO[IO])
open import Ffi.Hs.-base.Unit using (⊤)
open import Ffi.Hs.Control.Monad using (_>>=_; _>>_)
open import Ffi.Hs.Data.Functor using (_<$>_)
open import Ffi.Hs.-base.Level using (liftℓ; unliftℓ)
open import Ffi.Hs.Data.Function using (_∘_; _$_)

import Ffi.Hs.SDL.Init as SDL
import Ffi.Hs.SDL.Video as SDL
import Ffi.Hs.SDL.Video.OpenGL as SDL
import Ffi.Hs.SDL.Event as SDL


instance
    _ = Functor[IO]
    _ = Monad[IO]
    _ = MonadIO[IO]

{-# NON_TERMINATING #-}
main : IO ⊤
main = do
    SDL.initializeAll
    window ← unliftℓ <$> SDL.createWindow "Agda SDL2 example" (record SDL.defaultWindow
        { windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
        })
    _ ← SDL.glCreateContext window
    loop
    SDL.destroyWindow window
    unliftℓ <$> SDL.quit

    where
        loop : IO ⊤
        loop = do
            events ← unliftℓ <$> SDL.pollEvents
            loop
