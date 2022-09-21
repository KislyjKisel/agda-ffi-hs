{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Input.Keyboard where

{-# FOREIGN GHC import qualified SDL.Input.Keyboard #-}

postulate
    Keysym : Set

{-# COMPILE GHC Keysym = type SDL.Input.Keyboard.Keysym #-}