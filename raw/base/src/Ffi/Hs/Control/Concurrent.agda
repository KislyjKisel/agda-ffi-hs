{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent where

open import Agda.Builtin.Bool             using (Bool)
open import Agda.Builtin.IO               using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class            using (Show; Eq; Exception; Ord)
open import Ffi.Hs.-base.STM              using (STM)
open import Ffi.Hs.-base.Unit             using (⊤)
open import Ffi.Hs.Control.Exception.Base using (SomeException)
open import Ffi.Hs.Data.Either            using (Either)
open import Ffi.Hs.Data.Int               using (Int)
open import Ffi.Hs.Data.Tuple             using (Tuple2)
open import Ffi.Hs.System.Mem.Weak        using (Weak)
open import Ffi.Hs.System.Posix.Types     using (Fd)

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    ThreadId                : Set
    myThreadId              : IO ThreadId
    forkIO                  : IO (⊤ {aℓ}) → IO ThreadId
    forkFinally             : IO A → (Either SomeException A → IO (⊤ {lzero})) → IO ThreadId
    forkIOWithUnmask        : ((∀{aℓ}{A : Set aℓ} → IO A → IO A) → IO (⊤ {lzero})) → IO ThreadId
    killThread              : ThreadId → IO (⊤ {lzero})
    throwTo                 : ⦃ Exception A ⦄ → ThreadId → A → IO (⊤ {lzero})
    forkOn                  : Int → IO (⊤ {aℓ}) → IO ThreadId
    forkOnWithUnmask        : Int → ((∀{aℓ}{A : Set aℓ} → IO A → IO A) → IO (⊤ {lzero})) → IO ThreadId
    getNumCapabilities      : IO Int
    setNumCapabilities      : Int → IO (⊤ {lzero})
    threadCapacity          : ThreadId → IO (Tuple2 Int Bool)
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


{-# FOREIGN GHC import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaEq, AgdaShow, AgdaOrd, AgdaException) #-}
{-# FOREIGN GHC import qualified Control.Concurrent as AgdaHsConCon #-}

{-# COMPILE GHC ThreadId                = type AgdaHsConCon.ThreadId                              #-}
{-# COMPILE GHC myThreadId              = AgdaHsConCon.myThreadId                                 #-}
{-# COMPILE GHC forkIO                  = \ aℓ -> AgdaHsConCon.forkIO                             #-}
{-# COMPILE GHC forkIOWithUnmask        = \ f -> AgdaHsConCon.forkIOWithUnmask (\ x -> f () () x) #-}
{-# COMPILE GHC forkFinally             = \ aℓ a -> AgdaHsConCon.forkFinally                      #-}
{-# COMPILE GHC killThread              = AgdaHsConCon.killThread                                 #-}
{-# COMPILE GHC throwTo                 = \ aℓ a AgdaException -> AgdaHsConCon.throwTo            #-}
{-# COMPILE GHC forkOn                  = \ aℓ -> AgdaHsConCon.forkOn                             #-}
{-# COMPILE GHC forkOnWithUnmask        = \ f -> AgdaHsConCon.forkOnWithUnmask (\ x -> f () () x) #-}
{-# COMPILE GHC getNumCapabilities      = AgdaHsConCon.getNumCapabilities                         #-}
{-# COMPILE GHC setNumCapabilities      = AgdaHsConCon.setNumCapabilities                         #-}
{-# COMPILE GHC threadCapacity          = AgdaHsConCon.threadCapacity                             #-}
{-# COMPILE GHC yield                   = AgdaHsConCon.yield                                      #-}
{-# COMPILE GHC threadDelay             = AgdaHsConCon.threadDelay                                #-}
{-# COMPILE GHC threadWaitRead          = AgdaHsConCon.threadWaitRead                             #-}
{-# COMPILE GHC threadWaitWrite         = AgdaHsConCon.threadWaitWrite                            #-}
{-# COMPILE GHC threadWaitReadSTM       = AgdaHsConCon.threadWaitReadSTM                          #-}
{-# COMPILE GHC threadWaitWriteSTM      = AgdaHsConCon.threadWaitWriteSTM                         #-}
{-# COMPILE GHC rtsSupportsBoundThreads = AgdaHsConCon.rtsSupportsBoundThreads                    #-}
{-# COMPILE GHC forkOS                  = AgdaHsConCon.forkOS                                     #-}
{-# COMPILE GHC forkOSWithUnmask        = \ f -> AgdaHsConCon.forkOSWithUnmask (\ x -> f () () x) #-}
{-# COMPILE GHC isCurrentThreadBound    = AgdaHsConCon.isCurrentThreadBound                       #-}
{-# COMPILE GHC runInBoundThread        = \ aℓ a -> AgdaHsConCon.runInBoundThread                 #-}
{-# COMPILE GHC runInUnboundThread      = \ aℓ a -> AgdaHsConCon.runInUnboundThread               #-}
{-# COMPILE GHC mkWeakThreadId          = AgdaHsConCon.mkWeakThreadId                             #-}

module Instances where
    postulate
        Show[ThreadId] : Show ThreadId
        Eq[ThreadId]   : Eq ThreadId
        Ord[ThreadId]  : Ord ThreadId

{-# COMPILE GHC Instances.Show[ThreadId] = AgdaShow #-}
{-# COMPILE GHC Instances.Eq[ThreadId]   = AgdaEq   #-}
{-# COMPILE GHC Instances.Ord[ThreadId]  = AgdaOrd  #-}
