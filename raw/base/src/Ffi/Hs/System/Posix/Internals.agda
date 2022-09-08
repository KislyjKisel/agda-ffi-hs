{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Posix.Internals where

open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Bool using (Bool)
open import Agda.Builtin.Int using () renaming (Int to Integer)
open import Agda.Primitive
open import Ffi.Hs.Data.Int using (Int)
open import Ffi.Hs.-base.Unit using (⊤)
open import Ffi.Hs.Data.Tuple using (Tuple3)
open import Ffi.Hs.System.Posix.Types using (CDev; CIno)
open import Ffi.Hs.Foreign.Ptr using (Ptr)
open import Ffi.Hs.Foreign.C.Types using (CInt)
open import Ffi.Hs.Foreign.C.String using (CString)
open import Ffi.Hs.GHC.IO.Device using (IODeviceType)

private
    variable
        aℓ : Level
        A : Set aℓ

FD : Set
FD = CInt

CFilePath : Set
CFilePath = CString

postulate
    puts : List Char → IO (⊤ {lzero})
    
    CFLock     : Set
    CGroup     : Set
    CLconv     : Set
    CPasswd    : Set
    CSigaction : Set
    CSigset    : Set
    CStat      : Set
    CTermios   : Set
    CTm        : Set
    CTms       : Set
    CUtimbuf   : Set
    CUtsname   : Set

    fdFileSize            : FD → IO Integer
    fileType              : List Char → IO IODeviceType
    fdStat                : FD → IO (Tuple3 IODeviceType CDev CIno)
    fdType                : FD → IO IODeviceType
    statGetType           : Ptr CStat → IO IODeviceType
    ioe_unknownfiletype   : IOException
    fdGetMode             : FD → IO IOMode
    withFilePath          : List Char → (CString → IO A) → IO A
    newFilePath           : List Char → IO CString
    peekFilePath          : CString → IO (List Char)
    peekFilePathLen       : CStringLen → IO (List Char)
    setEcho               : FD → Bool → IO (⊤ {lzero})
    getEcho               : FD → IO Bool
    setCooked             : FD → Bool → IO Bool
    tcSetAttr             : FD → (Ptr CTermios → IO A) → IO A
    get_saved_termios     : CInt → IO (Ptr CTermios)
    set_saved_termios     : CInt → Ptr CTermios → IO (⊤ {lzero})
    setNonBlockingFD      : FD → Bool → IO (⊤ {lzero})
    setCloseOnExec        : FD → IO (⊤ {lzero})
    c-open                : CFilePath → CInt → CMode → IO CInt
    c_interruptible-open  : CFilePath → CInt → CMode → IO CInt
    c_interruptible-open_ : CFilePath → CInt → CMode → IO CInt
    hostIsThreaded        : Bool
    rtsIsThreaded_        : Int
    c_safe-open           : CFilePath → CInt → CMode → IO CInt
    c_safe-open_          : CFilePath → CInt → CMode → IO CInt
    c_fstat               : CInt → Ptr CStat → IO CInt
    lstat                 : CFilePath → Ptr CStat → IO CInt
    c_lseek               : CInt → COff → CInt → IO COff
    c_access              : CString → CInt → IO CInt
    c_chmod               : CString → CMode → IO CInt
    c_close               : CInt → IO CInt
    c_creat               : CString → CMode → IO CInt
    c_dup                 : CInt → IO CInt
    c_dup2                : CInt → CInt → IO CInt
    c_isatty              : CInt → IO CInt
    -- todo: c_read : CInt → Ptr Word8 → CSize → IO CSsize
    -- todo: c_safe_read : CInt → Ptr Word8 → CSize → IO CSsize
    c_umask               : CMode → IO CMode
    -- todo: c_write : CInt → Ptr Word8 → CSize → IO CSsize
    -- todo: c_safe_write : CInt → Ptr Word8 → CSize → IO CSsize
    c_pipe                : Ptr CInt → IO CInt
    c_unlink              : CString → IO CInt
    c_utime               : CString → Ptr CUtimbuf → IO CInt
    c_getpid              : IO CPid
    c_stat                : CFilePath → Ptr CStat → IO CInt
    c_ftruncate           : CInt → COff → IO CInt
    c_fcntl_read          : CInt → CInt → IO CInt
    c_fcntl_write         : CInt → CInt → CLong → IO CInt
    c_fcntl_lock          : CInt → CInt → Ptr CFLock → IO CInt
    c_fork                : IO CPid
    c_link                : CString → CString → IO CInt
    c_mkfifo              : CString → CMode → IO CInt
    c_sigemptyset         : Ptr CSigset → IO CInt
    c_sigaddset           : Ptr CSigset → CInt → IO CInt
    c_sigprocmask         : CInt → Ptr CSigset → Ptr CSigset → IO CInt
    c_tcgetattr           : CInt → Ptr CTermios → IO CInt
    c_tcsetattr           : CInt → CInt → Ptr CTermios → IO CInt
    c_waitpid             : CPid → Ptr CInt → CInt → IO CPid
    
    o_RDONLY   : CInt
    o_WRONLY   : CInt
    o_RDWR     : CInt
    o_APPEND   : CInt
    o_CREAT    : CInt
    o_EXCL     : CInt
    o_TRUNC    : CInt
    o_NOCTTY   : CInt
    o_NONBLOCK : CInt
    o_BINARY   : CInt

    c_s_isreg  : CMode → CInt
    c_s_ischr  : CMode → CInt
    c_s_isblk  : CMode → CInt
    c_s_isdir  : CMode → CInt
    c_s_isfifo : CMode → CInt

    s_isreg  : CMode → Bool
    s_ischr  : CMode → Bool
    s_isblk  : CMode → Bool
    s_isdir  : CMode → Bool
    s_isfifo : CMode → Bool

    sizeof_stat : Int

    st_mtime : Ptr CStat → IO CTime
    st_size  : Ptr CStat → IO COff
    st_mode  : Ptr CStat → IO CMode
    st_dev   : Ptr CStat → IO CDev
    st_ino   : Ptr CStat → IO CIno

    const_echo : CInt
    const_tcsanow : CInt
    const_icanon : CInt
    const_vmin : CInt
    const_vtime : CInt
    const_sigttou : CInt
    const_sig_block : CInt
    const_sig_setmask : CInt
    const_f_getfl : CInt
    const_f_setfl : CInt
    const_f_setfd : CInt
    const_fd_cloexec : CLong

    sizeof_termios : Int
    sizeof_sigset_t : Int

    c_lflag : Ptr CTermios → IO CTcflag
    poke_c_lflag : Ptr CTermios → CTcflag → IO (⊤ {lzero})
    ptr_c_cc : Ptr CTermios → IO (Ptr Word8)
    s_issock : CMode → Bool
    c_s_issock : CMode → CInt
    
    dEFAULT_BUFFER_SIZE : Int
    sEEK_CUR : CInt
    sEEK_SET : CInt
    sEEK_END : CInt

{-# COMPILE GHC puts = System.Posix.Internals.puts #-}

{-# COMPILE GHC CFLock     = System.Posix.Internals.CFLock     #-}
{-# COMPILE GHC CGroup     = System.Posix.Internals.CGroup     #-}
{-# COMPILE GHC CLconv     = System.Posix.Internals.CLconv     #-}
{-# COMPILE GHC CPasswd    = System.Posix.Internals.CPasswd    #-}
{-# COMPILE GHC CSigaction = System.Posix.Internals.CSigaction #-}
{-# COMPILE GHC CSigset    = System.Posix.Internals.CSigset    #-}
{-# COMPILE GHC CStat      = System.Posix.Internals.CStat      #-}
{-# COMPILE GHC CTermios   = System.Posix.Internals.CTermios   #-}
{-# COMPILE GHC CTm        = System.Posix.Internals.CTm        #-}
{-# COMPILE GHC CTms       = System.Posix.Internals.CTms       #-}
{-# COMPILE GHC CUtimbuf   = System.Posix.Internals.CUtimbuf   #-}
{-# COMPILE GHC CUtsname   = System.Posix.Internals.CUtsname   #-}

{-# COMPILE GHC dEFAULT-BUFFER-SIZE = System.Posix.Internals.dEFAULT_BUFFER_SIZE #-}
{-# COMPILE GHC sEEK-CUR            = System.Posix.Internals.sEEK_CUR            #-}
{-# COMPILE GHC sEEK-SET            = System.Posix.Internals.sEEK_SET            #-}
{-# COMPILE GHC sEEK-END            = System.Posix.Internals.sEEK_END            #-}

-- !