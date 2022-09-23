{-# OPTIONS --without-K #-}

module Main where

open import Agda.Primitive             using (lzero)
open import Ffi.Hs.-base.Level         using (liftℓ; unliftℓ)
open import Ffi.Hs.-base.Unit          using (⊤; ⊤′)
open import Ffi.Hs.Control.Applicative using (unless)
open import Ffi.Hs.Control.Monad       using (_>>=_; _>>_; _=<<_; return)
open import Ffi.Hs.Data.Eq             using (_==_)
open import Ffi.Hs.Data.Foldable       using (any)
open import Ffi.Hs.Data.Function       using (_∘_; _$_)
open import Ffi.Hs.Data.Functor        using (_<$>_)
open import Ffi.Hs.Data.List as List   using (map)
open import Ffi.Hs.System.IO as IO     using (IO)

import Ffi.Hs.SDL.Init           as SDL
import Ffi.Hs.SDL.Video          as SDL
import Ffi.Hs.SDL.Video.OpenGL   as SDL
import Ffi.Hs.SDL.Event          as SDL
import Ffi.Hs.SDL.Internal.Types as SDL

import Ffi.Hs.DearImGui            as ImGui
import Ffi.Hs.DearImGui.SDL        as ImGui
import Ffi.Hs.DearImGui.SDL.OpenGL as ImGui
import Ffi.Hs.DearImGui.OpenGL3    as ImGui

instance
    _ = IO.Functor[IO]
    _ = IO.Monad[IO]
    _ = IO.MonadIO[IO]
    _ = IO.Applicative[IO]

    _ = SDL.Eq[EventPayload]
    _ = List.Foldable[List]

{-# NON_TERMINATING #-}
loop : SDL.Window → IO ⊤′
loop window = unlessQuit do
    ImGui.openGL3NewFrame
    ImGui.sdl2NewFrame
    ImGui.newFrame

    ImGui.showDemoWindow

    ImGui.render
    ImGui.openGL3RenderDrawData =<< unliftℓ <$> ImGui.getDrawData
    SDL.glSwapWindow window
    loop window

    where
    unlessQuit : IO ⊤′ → IO ⊤′
    unlessQuit act = do
        events ← unliftℓ <$> ImGui.pollEventsWithImGui
        let quit = any ⦃ List.Foldable[List] ⦄ ((SDL.QuitEvent ==_) ∘ SDL.Event.eventPayload) events
        unless quit act


main : IO ⊤
main = do
    SDL.initializeAll
    window ← unliftℓ <$> SDL.createWindow "Agda SDL2 example" (record SDL.defaultWindow
        { windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
        })
    glContext ← unliftℓ <$> SDL.glCreateContext window
    imguiContext ← unliftℓ <$> ImGui.createContext
    imguiSdl ← unliftℓ <$> ImGui.sdl2InitForOpenGL window glContext
    imguiGl ← unliftℓ <$> ImGui.openGL3Init
    loop window
    SDL.destroyWindow window
    ImGui.openGL3Shutdown
    ImGui.sdl2Shutdown
    ImGui.destroyContext imguiContext
    SDL.glDeleteContext glContext
    SDL.quit
    return _
