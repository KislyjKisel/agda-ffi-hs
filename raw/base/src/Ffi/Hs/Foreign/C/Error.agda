{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.C.Error where

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Builtin.Char      using (Char)
open import Agda.Builtin.IO        using (IO)
open import Agda.Builtin.List      using (List)
open import Agda.Builtin.Maybe     using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class     using (Eq; Num)
open import Ffi.Hs.-base.Unit      using (⊤)
open import Ffi.Hs.Foreign.C.Types using (CInt)
open import Ffi.Hs.Foreign.Ptr     using (Ptr)
open import Ffi.Hs.System.IO       using (Handle)
open import Ffi.Hs.System.IO.Error using (IOError)

{-# FOREIGN GHC
import qualified Foreign.C.Error
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaEq, AgdaNum
    )
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ

data Errno : Set where
    mkErrno : CInt → Errno

{-# COMPILE GHC Errno = Foreign.C.Error.Errno #-}

postulate
    Eq[Errno] : Eq Errno

{-# COMPILE GHC Eq[Errno] = AgdaEq #-}

postulate
    isValidErrno   : Errno → Bool
    getErrno       : IO Errno
    resetErrno     : IO (⊤ {lzero})
    errnoToIOError : List Char → Errno → Maybe Handle → Maybe (List Char) → IOError
    throwErrno     : List Char → IO A

{-# COMPILE GHC isValidErrno   =           Foreign.C.Error.isValidErrno   #-}
{-# COMPILE GHC getErrno       =           Foreign.C.Error.getErrno       #-}
{-# COMPILE GHC resetErrno     =           Foreign.C.Error.resetErrno     #-}
{-# COMPILE GHC errnoToIOError =           Foreign.C.Error.errnoToIOError #-}
{-# COMPILE GHC throwErrno     = \ aℓ a -> Foreign.C.Error.throwErrno     #-}

postulate
    throwErrnoIf           : (A → Bool) → List Char → IO A → IO A
    throwErrnoIf-void      : (A → Bool) → List Char → IO A → IO (⊤ {lzero})
    throwErrnoIfRetry      : (A → Bool) → List Char → IO A → IO A
    throwErrnoIfRetry-void : (A → Bool) → List Char → IO A → IO (⊤ {lzero})

    throwErrnoIfMinus1           : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → List Char → IO A → IO A
    throwErrnoIfMinus1-void      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → List Char → IO A → IO (⊤ {lzero})
    throwErrnoIfMinus1Retry      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → List Char → IO A → IO A
    throwErrnoIfMinus1Retry-void : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → List Char → IO A → IO (⊤ {lzero})

    throwErrnoIfNull               : List Char → IO (Ptr A) → IO (Ptr A)
    throwErrnoIfNullRetry          : List Char → IO (Ptr A) → IO (Ptr A)
    throwErrnoIfRetryMayBlock      : (A → Bool) → List Char → IO A → IO B → IO A
    throwErrnoIfRetryMayBlock-void : (A → Bool) → List Char → IO A → IO B → IO (⊤ {lzero})

    throwErrnoIfMinus1RetryMayBlock      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → List Char → IO A → IO B → IO A
    throwErrnoIfMinus1RetryMayBlock-void : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → List Char → IO A → IO B → IO (⊤ {lzero})

    throwErrnoIfNullRetryMayBlock : List Char → IO (Ptr A) → IO B → IO (Ptr A)
    throwErrnoPath                : List Char → List Char → IO A
    throwErrnoPathIf              : (A → Bool) → List Char → List Char → IO A → IO A
    throwErrnoPathIf-void         : (A → Bool) → List Char → List Char → IO A → IO (⊤ {lzero})
    throwErrnoPathIfNull          : List Char → List Char → IO (Ptr A) → IO (Ptr A)

    throwErrnoPathIfMinus1      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → List Char → List Char → IO A → IO A
    throwErrnoPathIfMinus1-void : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → List Char → List Char → IO A → IO (⊤ {lzero})

{-# COMPILE GHC throwErrnoIf                 = \ aℓ a -> Foreign.C.Error.throwErrnoIf       #-}
{-# COMPILE GHC throwErrnoIf-void            = \ aℓ a -> Foreign.C.Error.throwErrnoIf_      #-}
{-# COMPILE GHC throwErrnoIfRetry            = \ aℓ a -> Foreign.C.Error.throwErrnoIfRetry  #-}
{-# COMPILE GHC throwErrnoIfRetry-void       = \ aℓ a -> Foreign.C.Error.throwErrnoIfRetry_ #-}

{-# COMPILE GHC throwErrnoIfMinus1           = \ aℓ a AgdaEq AgdaNum -> Foreign.C.Error.throwErrnoIfMinus1       #-}
{-# COMPILE GHC throwErrnoIfMinus1-void      = \ aℓ a AgdaEq AgdaNum -> Foreign.C.Error.throwErrnoIfMinus1_      #-}
{-# COMPILE GHC throwErrnoIfMinus1Retry      = \ aℓ a AgdaEq AgdaNum -> Foreign.C.Error.throwErrnoIfMinus1Retry  #-}
{-# COMPILE GHC throwErrnoIfMinus1Retry-void = \ aℓ a AgdaEq AgdaNum -> Foreign.C.Error.throwErrnoIfMinus1Retry_ #-}

{-# COMPILE GHC throwErrnoIfNull               = \ aℓ a      -> Foreign.C.Error.throwErrnoIfNull           #-}
{-# COMPILE GHC throwErrnoIfNullRetry          = \ aℓ a      -> Foreign.C.Error.throwErrnoIfNullRetry      #-}
{-# COMPILE GHC throwErrnoIfRetryMayBlock      = \ aℓ a bℓ b -> Foreign.C.Error.throwErrnoIfRetryMayBlock  #-}
{-# COMPILE GHC throwErrnoIfRetryMayBlock-void = \ aℓ a bℓ b -> Foreign.C.Error.throwErrnoIfRetryMayBlock_ #-}

{-# COMPILE GHC throwErrnoIfMinus1RetryMayBlock      =
    \ aℓ a bℓ b AgdaEq AgdaNum -> Foreign.C.Error.throwErrnoIfMinus1RetryMayBlock  #-}
{-# COMPILE GHC throwErrnoIfMinus1RetryMayBlock-void =
    \ aℓ a bℓ b AgdaEq AgdaNum -> Foreign.C.Error.throwErrnoIfMinus1RetryMayBlock_ #-}

{-# COMPILE GHC throwErrnoIfNullRetryMayBlock = \ aℓ a bℓ b -> Foreign.C.Error.throwErrnoIfNullRetryMayBlock #-}
{-# COMPILE GHC throwErrnoPath                = \ aℓ a      -> Foreign.C.Error.throwErrnoPath                #-}
{-# COMPILE GHC throwErrnoPathIf              = \ aℓ a      -> Foreign.C.Error.throwErrnoPathIf              #-}
{-# COMPILE GHC throwErrnoPathIf-void         = \ aℓ a      -> Foreign.C.Error.throwErrnoPathIf_             #-}
{-# COMPILE GHC throwErrnoPathIfNull          = \ aℓ a      -> Foreign.C.Error.throwErrnoPathIfNull          #-}

{-# COMPILE GHC throwErrnoPathIfMinus1        = \ aℓ a AgdaEq AgdaNum -> Foreign.C.Error.throwErrnoPathIfMinus1  #-}
{-# COMPILE GHC throwErrnoPathIfMinus1-void   = \ aℓ a AgdaEq AgdaNum -> Foreign.C.Error.throwErrnoPathIfMinus1_ #-}

postulate
    eOK e2BIG eACCES eADDRINUSE         : Errno
    eADDRNOTAVAIL eADV eAFNOSUPPORT     : Errno
    eAGAIN eALREADY eBADF eBADMSG       : Errno
    eBADRPC eBUSY eCHILD eCOMM          : Errno
    eCONNABORTED eCONNREFUSED           : Errno
    eCONNRESET eDEADLK eDESTADDRREQ     : Errno
    eDIRTY eDOM eDQUOT eEXIST           : Errno
    eFAULT eFBIG eFTYPE eHOSTDOWN       : Errno
    eHOSTUNREACH eIDRM eILSEQ           : Errno
    eINPROGRESS eINTR eINVAL eIO        : Errno
    eISCONN eISDIR eLOOP eMFILE         : Errno
    eMLINK eMSGSIZE eMULTIHOP           : Errno
    eNAMETOOLONG eNETDOWN eNETRESET     : Errno
    eNETUNREACH eNFILE eNOBUFS          : Errno
    eNODATA eNODEV eNOENT eNOEXEC       : Errno
    eNOLCK eNOLINK eNOMEM eNOMSG        : Errno
    eNONET eNOPROTOOPT eNOSPC eNOSR     : Errno
    eNOSTR eNOSYS eNOTBLK eNOTCONN      : Errno
    eNOTDIR eNOTEMPTY eNOTSOCK          : Errno
    eNOTSUP eNOTTY eNXIO eOPNOTSUPP     : Errno
    ePERM ePFNOSUPPORT ePIPE            : Errno
    ePROCLIM ePROCUNAVAIL ePROGMISMATCH : Errno
    ePROGUNAVAIL ePROTO ePROTONOSUPPORT : Errno
    ePROTOTYPE eRANGE eREMCHG eREMOTE   : Errno
    eROFS eRPCMISMATCH eRREMOTE         : Errno
    eSHUTDOWN eSOCKTNOSUPPORT eSPIPE    : Errno
    eSRCH eSRMNT eSTALE eTIME           : Errno
    eTIMEDOUT eTOOMANYREFS eTXTBSY      : Errno
    eUSERS eWOULDBLOCK eXDEV            : Errno

{-# COMPILE GHC eOK             = Foreign.C.Error.eOK             #-}
{-# COMPILE GHC e2BIG           = Foreign.C.Error.e2BIG           #-}
{-# COMPILE GHC eACCES          = Foreign.C.Error.eACCES          #-}
{-# COMPILE GHC eADDRINUSE      = Foreign.C.Error.eADDRINUSE      #-}
{-# COMPILE GHC eADDRNOTAVAIL   = Foreign.C.Error.eADDRNOTAVAIL   #-}
{-# COMPILE GHC eADV            = Foreign.C.Error.eADV            #-}
{-# COMPILE GHC eAFNOSUPPORT    = Foreign.C.Error.eAFNOSUPPORT    #-}
{-# COMPILE GHC eAGAIN          = Foreign.C.Error.eAGAIN          #-}
{-# COMPILE GHC eALREADY        = Foreign.C.Error.eALREADY        #-}
{-# COMPILE GHC eBADF           = Foreign.C.Error.eBADF           #-}
{-# COMPILE GHC eBADMSG         = Foreign.C.Error.eBADMSG         #-}
{-# COMPILE GHC eBADRPC         = Foreign.C.Error.eBADRPC         #-}
{-# COMPILE GHC eBUSY           = Foreign.C.Error.eBUSY           #-}
{-# COMPILE GHC eCHILD          = Foreign.C.Error.eCHILD          #-}
{-# COMPILE GHC eCOMM           = Foreign.C.Error.eCOMM           #-}
{-# COMPILE GHC eCONNABORTED    = Foreign.C.Error.eCONNABORTED    #-}
{-# COMPILE GHC eCONNREFUSED    = Foreign.C.Error.eCONNREFUSED    #-}
{-# COMPILE GHC eCONNRESET      = Foreign.C.Error.eCONNRESET      #-}
{-# COMPILE GHC eDEADLK         = Foreign.C.Error.eDEADLK         #-}
{-# COMPILE GHC eDESTADDRREQ    = Foreign.C.Error.eDESTADDRREQ    #-}
{-# COMPILE GHC eDIRTY          = Foreign.C.Error.eDIRTY          #-}
{-# COMPILE GHC eDOM            = Foreign.C.Error.eDOM            #-}
{-# COMPILE GHC eDQUOT          = Foreign.C.Error.eDQUOT          #-}
{-# COMPILE GHC eEXIST          = Foreign.C.Error.eEXIST          #-}
{-# COMPILE GHC eFAULT          = Foreign.C.Error.eFAULT          #-}
{-# COMPILE GHC eFBIG           = Foreign.C.Error.eFBIG           #-}
{-# COMPILE GHC eFTYPE          = Foreign.C.Error.eFTYPE          #-}
{-# COMPILE GHC eHOSTDOWN       = Foreign.C.Error.eHOSTDOWN       #-}
{-# COMPILE GHC eHOSTUNREACH    = Foreign.C.Error.eHOSTUNREACH    #-}
{-# COMPILE GHC eIDRM           = Foreign.C.Error.eIDRM           #-}
{-# COMPILE GHC eILSEQ          = Foreign.C.Error.eILSEQ          #-}
{-# COMPILE GHC eINPROGRESS     = Foreign.C.Error.eINPROGRESS     #-}
{-# COMPILE GHC eINTR           = Foreign.C.Error.eINTR           #-}
{-# COMPILE GHC eINVAL          = Foreign.C.Error.eINVAL          #-}
{-# COMPILE GHC eIO             = Foreign.C.Error.eIO             #-}
{-# COMPILE GHC eISCONN         = Foreign.C.Error.eISCONN         #-}
{-# COMPILE GHC eISDIR          = Foreign.C.Error.eISDIR          #-}
{-# COMPILE GHC eLOOP           = Foreign.C.Error.eLOOP           #-}
{-# COMPILE GHC eMFILE          = Foreign.C.Error.eMFILE          #-}
{-# COMPILE GHC eMLINK          = Foreign.C.Error.eMLINK          #-}
{-# COMPILE GHC eMSGSIZE        = Foreign.C.Error.eMSGSIZE        #-}
{-# COMPILE GHC eMULTIHOP       = Foreign.C.Error.eMULTIHOP       #-}
{-# COMPILE GHC eNAMETOOLONG    = Foreign.C.Error.eNAMETOOLONG    #-}
{-# COMPILE GHC eNETDOWN        = Foreign.C.Error.eNETDOWN        #-}
{-# COMPILE GHC eNETRESET       = Foreign.C.Error.eNETRESET       #-}
{-# COMPILE GHC eNETUNREACH     = Foreign.C.Error.eNETUNREACH     #-}
{-# COMPILE GHC eNFILE          = Foreign.C.Error.eNFILE          #-}
{-# COMPILE GHC eNOBUFS         = Foreign.C.Error.eNOBUFS         #-}
{-# COMPILE GHC eNODATA         = Foreign.C.Error.eNODATA         #-}
{-# COMPILE GHC eNODEV          = Foreign.C.Error.eNODEV          #-}
{-# COMPILE GHC eNOENT          = Foreign.C.Error.eNOENT          #-}
{-# COMPILE GHC eNOEXEC         = Foreign.C.Error.eNOEXEC         #-}
{-# COMPILE GHC eNOLCK          = Foreign.C.Error.eNOLCK          #-}
{-# COMPILE GHC eNOLINK         = Foreign.C.Error.eNOLINK         #-}
{-# COMPILE GHC eNOMEM          = Foreign.C.Error.eNOMEM          #-}
{-# COMPILE GHC eNOMSG          = Foreign.C.Error.eNOMSG          #-}
{-# COMPILE GHC eNONET          = Foreign.C.Error.eNONET          #-}
{-# COMPILE GHC eNOPROTOOPT     = Foreign.C.Error.eNOPROTOOPT     #-}
{-# COMPILE GHC eNOSPC          = Foreign.C.Error.eNOSPC          #-}
{-# COMPILE GHC eNOSR           = Foreign.C.Error.eNOSR           #-}
{-# COMPILE GHC eNOSTR          = Foreign.C.Error.eNOSTR          #-}
{-# COMPILE GHC eNOSYS          = Foreign.C.Error.eNOSYS          #-}
{-# COMPILE GHC eNOTBLK         = Foreign.C.Error.eNOTBLK         #-}
{-# COMPILE GHC eNOTCONN        = Foreign.C.Error.eNOTCONN        #-}
{-# COMPILE GHC eNOTDIR         = Foreign.C.Error.eNOTDIR         #-}
{-# COMPILE GHC eNOTEMPTY       = Foreign.C.Error.eNOTEMPTY       #-}
{-# COMPILE GHC eNOTSOCK        = Foreign.C.Error.eNOTSOCK        #-}
{-# COMPILE GHC eNOTSUP         = Foreign.C.Error.eNOTSUP         #-}
{-# COMPILE GHC eNOTTY          = Foreign.C.Error.eNOTTY          #-}
{-# COMPILE GHC eNXIO           = Foreign.C.Error.eNXIO           #-}
{-# COMPILE GHC eOPNOTSUPP      = Foreign.C.Error.eOPNOTSUPP      #-}
{-# COMPILE GHC ePERM           = Foreign.C.Error.ePERM           #-}
{-# COMPILE GHC ePFNOSUPPORT    = Foreign.C.Error.ePFNOSUPPORT    #-}
{-# COMPILE GHC ePIPE           = Foreign.C.Error.ePIPE           #-}
{-# COMPILE GHC ePROCLIM        = Foreign.C.Error.ePROCLIM        #-}
{-# COMPILE GHC ePROCUNAVAIL    = Foreign.C.Error.ePROCUNAVAIL    #-}
{-# COMPILE GHC ePROGMISMATCH   = Foreign.C.Error.ePROGMISMATCH   #-}
{-# COMPILE GHC ePROGUNAVAIL    = Foreign.C.Error.ePROGUNAVAIL    #-}
{-# COMPILE GHC ePROTO          = Foreign.C.Error.ePROTO          #-}
{-# COMPILE GHC ePROTONOSUPPORT = Foreign.C.Error.ePROTONOSUPPORT #-}
{-# COMPILE GHC ePROTOTYPE      = Foreign.C.Error.ePROTOTYPE      #-}
{-# COMPILE GHC eRANGE          = Foreign.C.Error.eRANGE          #-}
{-# COMPILE GHC eREMCHG         = Foreign.C.Error.eREMCHG         #-}
{-# COMPILE GHC eREMOTE         = Foreign.C.Error.eREMOTE         #-}
{-# COMPILE GHC eROFS           = Foreign.C.Error.eROFS           #-}
{-# COMPILE GHC eRPCMISMATCH    = Foreign.C.Error.eRPCMISMATCH    #-}
{-# COMPILE GHC eRREMOTE        = Foreign.C.Error.eRREMOTE        #-}
{-# COMPILE GHC eSHUTDOWN       = Foreign.C.Error.eSHUTDOWN       #-}
{-# COMPILE GHC eSOCKTNOSUPPORT = Foreign.C.Error.eSOCKTNOSUPPORT #-}
{-# COMPILE GHC eSPIPE          = Foreign.C.Error.eSPIPE          #-}
{-# COMPILE GHC eSRCH           = Foreign.C.Error.eSRCH           #-}
{-# COMPILE GHC eSRMNT          = Foreign.C.Error.eSRMNT          #-}
{-# COMPILE GHC eSTALE          = Foreign.C.Error.eSTALE          #-}
{-# COMPILE GHC eTIME           = Foreign.C.Error.eTIME           #-}
{-# COMPILE GHC eTIMEDOUT       = Foreign.C.Error.eTIMEDOUT       #-}
{-# COMPILE GHC eTOOMANYREFS    = Foreign.C.Error.eTOOMANYREFS    #-}
{-# COMPILE GHC eTXTBSY         = Foreign.C.Error.eTXTBSY         #-}
{-# COMPILE GHC eUSERS          = Foreign.C.Error.eUSERS          #-}
{-# COMPILE GHC eWOULDBLOCK     = Foreign.C.Error.eWOULDBLOCK     #-}
{-# COMPILE GHC eXDEV           = Foreign.C.Error.eXDEV           #-}
