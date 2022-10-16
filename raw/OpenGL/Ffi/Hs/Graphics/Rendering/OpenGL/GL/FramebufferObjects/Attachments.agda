{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments where

open import Agda.Builtin.IO           using (IO)
open import Agda.Builtin.Maybe        using (Maybe)
open import Agda.Builtin.Unit         using (⊤)
open import Agda.Primitive            using (Level)
open import Ffi.Hs.-base.Class        using (Eq; Ord; Show)
open import Ffi.Hs.-OpenGL.BufferMode using (BufferMode)
open import Ffi.Hs.Graphics.GL.Types  using (GLuint; GLenum; GLint)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Objects
    using (TextureObject)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Specification
    using (TextureTarget1D; TextureTarget2D; TextureTarget3D)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects
    using (FramebufferTarget)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects
    using (RenderbufferTarget; RenderbufferObject)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


postulate
    FramebufferAttachment : Set aℓ → Set aℓ

    FramebufferAttachment[A]⇒Show[A] : ⦃ FramebufferAttachment A ⦄ → Show A

    marshalAttachment       : ⦃ FramebufferAttachment A ⦄ → A → Maybe GLenum
    unmarshalAttachment     : ⦃ FramebufferAttachment A ⦄ → GLenum → A
    unmarshalAttachmentSafe : ⦃ FramebufferAttachment A ⦄ → GLenum → Maybe A

{-# FOREIGN GHC
data AgdaFramebufferAttachment aℓ a =
    Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.FramebufferAttachment a =>
    AgdaFramebufferAttachment
#-}
{-# COMPILE GHC FramebufferAttachment = type(0) AgdaFramebufferAttachment #-}

{-# COMPILE GHC FramebufferAttachment[A]⇒Show[A] = \ aℓ a AgdaFramebufferAttachment -> AgdaShow #-}

{-# COMPILE GHC marshalAttachment       = \ aℓ a AgdaFramebufferAttachment -> Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.marshalAttachment       #-}
{-# COMPILE GHC unmarshalAttachment     = \ aℓ a AgdaFramebufferAttachment -> Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.unmarshalAttachment     #-}
{-# COMPILE GHC unmarshalAttachmentSafe = \ aℓ a AgdaFramebufferAttachment -> Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.unmarshalAttachmentSafe #-}


data FramebufferObjectAttachment : Set where
    ColorAttachment        : GLuint → FramebufferObjectAttachment
    DepthAttachment        : FramebufferObjectAttachment
    StencilAttachment      : FramebufferObjectAttachment
    DepthStencilAttachment : FramebufferObjectAttachment

{-# COMPILE GHC FramebufferObjectAttachment = data Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.FramebufferObjectAttachment
    ( Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.ColorAttachment
    | Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.DepthAttachment
    | Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.StencilAttachment
    | Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.DepthStencilAttachment
    ) #-}

postulate
    Eq[FramebufferObjectAttachment]                    : Eq FramebufferObjectAttachment
    Ord[FramebufferObjectAttachment]                   : Ord FramebufferObjectAttachment
    Show[FramebufferObjectAttachment]                  : Show FramebufferObjectAttachment
    FramebufferAttachment[FramebufferObjectAttachment] : FramebufferAttachment FramebufferObjectAttachment

{-# COMPILE GHC Eq[FramebufferObjectAttachment]                    = AgdaEq                    #-}
{-# COMPILE GHC Ord[FramebufferObjectAttachment]                   = AgdaOrd                   #-}
{-# COMPILE GHC Show[FramebufferObjectAttachment]                  = AgdaShow                  #-}
{-# COMPILE GHC FramebufferAttachment[FramebufferObjectAttachment] = AgdaFramebufferAttachment #-}

postulate
    fboaToBufferMode        : FramebufferObjectAttachment → Maybe BufferMode
    fboaFromBufferMode      : BufferMode → Maybe FramebufferObjectAttachment
    framebufferRenderbuffer : FramebufferTarget → FramebufferObjectAttachment → RenderbufferTarget → RenderbufferObject → IO ⊤
    framebufferTexture1D    : FramebufferTarget → FramebufferObjectAttachment → TextureTarget1D → TextureObject → Level → IO ⊤
    framebufferTexture2D    : FramebufferTarget → FramebufferObjectAttachment → TextureTarget2D → TextureObject → Level → IO ⊤
    framebufferTexture3D    : FramebufferTarget → FramebufferObjectAttachment → TextureTarget3D → TextureObject → Level → GLint → IO ⊤
    framebufferTextureLayer : FramebufferTarget → FramebufferObjectAttachment → TextureObject → Level → GLint → IO ⊤

{-# COMPILE GHC fboaToBufferMode        = Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.fboaToBufferMode        #-}
{-# COMPILE GHC fboaFromBufferMode      = Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.fboaFromBufferMode      #-}
{-# COMPILE GHC framebufferRenderbuffer = Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.framebufferRenderbuffer #-}
{-# COMPILE GHC framebufferTexture1D    = Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.framebufferTexture1D    #-}
{-# COMPILE GHC framebufferTexture2D    = Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.framebufferTexture2D    #-}
{-# COMPILE GHC framebufferTexture3D    = Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.framebufferTexture3D    #-}
{-# COMPILE GHC framebufferTextureLayer = Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments.framebufferTextureLayer #-}
