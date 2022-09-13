{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent where

open import Agda.Builtin.Bool             using (Bool)
open import Agda.Builtin.IO               using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class            using (Show; Eq; Exception; Ord)
open import Ffi.Hs.-base.STM              using (STM)
open import Ffi.Hs.-base.Unit             using (⊤)
open import Ffi.Hs.GHC.Exception.Type     using (SomeException)
open import Ffi.Hs.Data.Either            using (Either)
open import Ffi.Hs.Data.Int               using (Int)
open import Ffi.Hs.Data.Tuple             using (Tuple2)
open import Ffi.Hs.System.Mem.Weak        using (Weak)
open import Ffi.Hs.System.Posix.Types     using (Fd)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Concurrent
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ seℓ : Level
        A : Set aℓ

postulate
    ThreadId                : Set
    myThreadId              : IO ThreadId
    forkIO                  : IO (⊤ {aℓ}) → IO ThreadId
    forkFinally             : IO A → (Either (SomeException {seℓ}) A → IO (⊤ {lzero})) → IO ThreadId
    forkIOWithUnmask        : ((∀{aℓ}{A : Set aℓ} → IO A → IO A) → IO (⊤ {lzero})) → IO ThreadId
    killThread              : ThreadId → IO (⊤ {lzero})
    throwTo                 : ⦃ Exception A ⦄ → ThreadId → A → IO (⊤ {lzero})
    forkOn                  : Int → IO (⊤ {aℓ}) → IO ThreadId
    forkOnWithUnmask        : Int → ((∀{aℓ}{A : Set aℓ} → IO A → IO A) → IO (⊤ {lzero})) → IO ThreadId
    getNumCapabilities      : IO Int
    setNumCapabilities      : Int → IO (⊤ {lzero})
    threadCapability        : ThreadId → IO (Tuple2 Int Bool)
    yield                   : IO (⊤ {lzero})
    threadDelay             : Int → IO (⊤ {lzero})
    threadWaitRead          : Fd → IO (⊤ {lzero})
    threadWaitWrite         : Fd → IO (⊤ {lzero})
    threadWaitReadSTM       : Fd → IO (Tuple2 (STM (⊤ {lzero})) (IO (⊤ {lzero})))
    threadWaitWriteSTM      : Fd → IO (Tuple2 (STM (⊤ {lzero})) (IO (⊤ {lzero})))
    rtsSupportsBoundThreads : Bool
    forkOS                  : IO (⊤ {lzero}) → IO ThreadId
    forkOSWithUnmask        : ((∀{aℓ}{A : Set aℓ} → IO A → IO A) → IO (⊤ {lzero})) → IO ThreadId
    isCurrentThreadBound    : IO Bool
    runInBoundThread        : IO A → IO A
    runInUnboundThread      : IO A → IO A
    mkWeakThreadId          : ThreadId → IO (Weak ThreadId)

{-# COMPILE GHC ThreadId = type Control.Concurrent.ThreadId #-}
{-# COMPILE GHC myThreadId              =                         Control.Concurrent.myThreadId                                 #-}
{-# COMPILE GHC forkIO                  = \ aℓ                 -> Control.Concurrent.forkIO                                     #-}
{-# COMPILE GHC forkIOWithUnmask        = \ f                  -> Control.Concurrent.forkIOWithUnmask (\ g -> f (\ _ _ -> g))   #-}
{-# COMPILE GHC forkFinally             = \ aℓ a seℓ           -> Control.Concurrent.forkFinally                                #-}
{-# COMPILE GHC killThread              =                         Control.Concurrent.killThread                                 #-}
{-# COMPILE GHC throwTo                 = \ aℓ a AgdaException -> Control.Concurrent.throwTo                                    #-}
{-# COMPILE GHC forkOn                  = \ aℓ                 -> Control.Concurrent.forkOn                                     #-}
{-# COMPILE GHC forkOnWithUnmask        = \ x f                -> Control.Concurrent.forkOnWithUnmask x (\ g -> f (\ _ _ -> g)) #-}
{-# COMPILE GHC getNumCapabilities      =                         Control.Concurrent.getNumCapabilities                         #-}
{-# COMPILE GHC setNumCapabilities      =                         Control.Concurrent.setNumCapabilities                         #-}
{-# COMPILE GHC threadCapability        =                         Control.Concurrent.threadCapability                           #-}
{-# COMPILE GHC yield                   =                         Control.Concurrent.yield                                      #-}
{-# COMPILE GHC threadDelay             =                         Control.Concurrent.threadDelay                                #-}
{-# COMPILE GHC threadWaitRead          =                         Control.Concurrent.threadWaitRead                             #-}
{-# COMPILE GHC threadWaitWrite         =                         Control.Concurrent.threadWaitWrite                            #-}
{-# COMPILE GHC threadWaitReadSTM       =                         Control.Concurrent.threadWaitReadSTM                          #-}
{-# COMPILE GHC threadWaitWriteSTM      =                         Control.Concurrent.threadWaitWriteSTM                         #-}
{-# COMPILE GHC rtsSupportsBoundThreads =                         Control.Concurrent.rtsSupportsBoundThreads                    #-}
{-# COMPILE GHC forkOS                  =                         Control.Concurrent.forkOS                                     #-}
{-# COMPILE GHC forkOSWithUnmask        = \ f                  -> Control.Concurrent.forkOSWithUnmask (\ g -> f (\ _ _ -> g))   #-}
{-# COMPILE GHC isCurrentThreadBound    =                         Control.Concurrent.isCurrentThreadBound                       #-}
{-# COMPILE GHC runInBoundThread        = \ aℓ a               -> Control.Concurrent.runInBoundThread                           #-}
{-# COMPILE GHC runInUnboundThread      = \ aℓ a               -> Control.Concurrent.runInUnboundThread                         #-}
{-# COMPILE GHC mkWeakThreadId          =                         Control.Concurrent.mkWeakThreadId                             #-}

postulate
    Show[ThreadId] : Show ThreadId
    Eq[ThreadId]   : Eq ThreadId
    Ord[ThreadId]  : Ord ThreadId

{-# COMPILE GHC Show[ThreadId] = AgdaShow #-}
{-# COMPILE GHC Eq[ThreadId]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ThreadId]  = AgdaOrd  #-}
