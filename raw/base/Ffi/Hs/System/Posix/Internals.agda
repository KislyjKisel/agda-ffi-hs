{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Posix.Internals where

open import Agda.Builtin.Bool         using (Bool)
open import Agda.Builtin.Char         using (Char)
open import Agda.Builtin.Int          using () renaming (Int to Integer)
open import Agda.Builtin.List         using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit         using (⊤; ⊤′)
open import Ffi.Hs.Data.Int           using (Int)
open import Ffi.Hs.Data.Tuple         using (Tuple3)
open import Ffi.Hs.Data.Word          using (Word8)
open import Ffi.Hs.Foreign.C.String   using (CString; CStringLen)
open import Ffi.Hs.Foreign.C.Types    using (CInt; CLong; CTime)
open import Ffi.Hs.Foreign.Ptr        using (Ptr)
open import Ffi.Hs.GHC.IO.Device      using (IODeviceType)
open import Ffi.Hs.GHC.IO.Exception   using (IOException)
open import Ffi.Hs.System.IO          using (IO; IOMode)
open import Ffi.Hs.System.Posix.Types

private
    variable
        aℓ : Level
        A : Set aℓ

FD : Set
FD = CInt

CFilePath : Set
CFilePath = CString

postulate
    puts : List Char → IO ⊤
    
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
    ioe-unknownfiletype   : IOException
    fdGetMode             : FD → IO IOMode
    withFilePath          : List Char → (CString → IO A) → IO A
    newFilePath           : List Char → IO CString
    peekFilePath          : CString → IO (List Char)
    peekFilePathLen       : CStringLen → IO (List Char)
    setEcho               : FD → Bool → IO ⊤
    getEcho               : FD → IO Bool
    setCooked             : FD → Bool → IO Bool
    tcSetAttr             : FD → (Ptr CTermios → IO A) → IO A
    get-saved-termios     : CInt → IO (Ptr CTermios)
    set-saved-termios     : CInt → Ptr CTermios → IO ⊤
    setNonBlockingFD      : FD → Bool → IO ⊤
    setCloseOnExec        : FD → IO ⊤
    c-open                : CFilePath → CInt → CMode → IO CInt
    c-interruptible-open  : CFilePath → CInt → CMode → IO CInt
    c-interruptible-open- : CFilePath → CInt → CMode → IO CInt
    hostIsThreaded        : Bool
    rtsIsThreaded-        : Int
    c-safe-open           : CFilePath → CInt → CMode → IO CInt
    c-safe-open-          : CFilePath → CInt → CMode → IO CInt
    c-fstat               : CInt → Ptr CStat → IO CInt
    lstat                 : CFilePath → Ptr CStat → IO CInt
    c-lseek               : CInt → COff → CInt → IO COff
    c-access              : CString → CInt → IO CInt
    c-chmod               : CString → CMode → IO CInt
    c-close               : CInt → IO CInt
    c-creat               : CString → CMode → IO CInt
    c-dup                 : CInt → IO CInt
    c-dup2                : CInt → CInt → IO CInt
    c-isatty              : CInt → IO CInt
    -- todo: c-read : CInt → Ptr Word8 → CSize → IO CSsize
    -- todo: c-safe-read : CInt → Ptr Word8 → CSize → IO CSsize
    c-umask               : CMode → IO CMode
    -- todo: c-write : CInt → Ptr Word8 → CSize → IO CSsize
    -- todo: c-safe-write : CInt → Ptr Word8 → CSize → IO CSsize
    c-pipe                : Ptr CInt → IO CInt
    c-unlink              : CString → IO CInt
    c-utime               : CString → Ptr CUtimbuf → IO CInt
    c-getpid              : IO CPid
    c-stat                : CFilePath → Ptr CStat → IO CInt
    c-ftruncate           : CInt → COff → IO CInt
    c-fcntl-read          : CInt → CInt → IO CInt
    c-fcntl-write         : CInt → CInt → CLong → IO CInt
    c-fcntl-lock          : CInt → CInt → Ptr CFLock → IO CInt
    c-fork                : IO CPid
    c-link                : CString → CString → IO CInt
    c-mkfifo              : CString → CMode → IO CInt
    c-sigemptyset         : Ptr CSigset → IO CInt
    c-sigaddset           : Ptr CSigset → CInt → IO CInt
    c-sigprocmask         : CInt → Ptr CSigset → Ptr CSigset → IO CInt
    c-tcgetattr           : CInt → Ptr CTermios → IO CInt
    c-tcsetattr           : CInt → CInt → Ptr CTermios → IO CInt
    c-waitpid             : CPid → Ptr CInt → CInt → IO CPid
    
    o-RDONLY   : CInt
    o-WRONLY   : CInt
    o-RDWR     : CInt
    o-APPEND   : CInt
    o-CREAT    : CInt
    o-EXCL     : CInt
    o-TRUNC    : CInt
    o-NOCTTY   : CInt
    o-NONBLOCK : CInt
    o-BINARY   : CInt

    c-s-isreg  : CMode → CInt
    c-s-ischr  : CMode → CInt
    c-s-isblk  : CMode → CInt
    c-s-isdir  : CMode → CInt
    c-s-isfifo : CMode → CInt

    s-isreg  : CMode → Bool
    s-ischr  : CMode → Bool
    s-isblk  : CMode → Bool
    s-isdir  : CMode → Bool
    s-isfifo : CMode → Bool

    sizeof-stat : Int

    st-mtime : Ptr CStat → IO CTime
    st-size  : Ptr CStat → IO COff
    st-mode  : Ptr CStat → IO CMode
    st-dev   : Ptr CStat → IO CDev
    st-ino   : Ptr CStat → IO CIno

    const-echo        : CInt
    const-tcsanow     : CInt
    const-icanon      : CInt
    const-vmin        : CInt
    const-vtime       : CInt
    const-sigttou     : CInt
    const-sig-block   : CInt
    const-sig-setmask : CInt
    const-f-getfl     : CInt
    const-f-setfl     : CInt
    const-f-setfd     : CInt
    const-fd-cloexec  : CLong

    sizeof-termios  : Int
    sizeof-sigset-t : Int

    c-lflag      : Ptr CTermios → IO CTcflag
    poke-c-lflag : Ptr CTermios → CTcflag → IO ⊤
    ptr-c-cc     : Ptr CTermios → IO (Ptr Word8)
    s-issock     : CMode → Bool
    c-s-issock   : CMode → CInt
    
    dEFAULT-BUFFER-SIZE : Int
    sEEK-CUR : CInt
    sEEK-SET : CInt
    sEEK-END : CInt

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

{-# COMPILE GHC fdFileSize            = System.Posix.Internals.fdFileSize            #-}
{-# COMPILE GHC fileType              = System.Posix.Internals.fileType              #-}
{-# COMPILE GHC fdStat                = System.Posix.Internals.fdStat                #-}
{-# COMPILE GHC fdType                = System.Posix.Internals.fdType                #-}
{-# COMPILE GHC statGetType           = System.Posix.Internals.statGetType           #-}
{-# COMPILE GHC ioe-unknownfiletype   = System.Posix.Internals.ioe_unknownfiletype   #-}
{-# COMPILE GHC fdGetMode             = System.Posix.Internals.fdGetMode             #-}
{-# COMPILE GHC withFilePath          = \ aℓ a -> System.Posix.Internals.withFilePath #-}
{-# COMPILE GHC newFilePath           = System.Posix.Internals.newFilePath           #-}
{-# COMPILE GHC peekFilePath          = System.Posix.Internals.peekFilePath          #-}
{-# COMPILE GHC peekFilePathLen       = System.Posix.Internals.peekFilePathLen       #-}
{-# COMPILE GHC setEcho               = System.Posix.Internals.setEcho               #-}
{-# COMPILE GHC getEcho               = System.Posix.Internals.getEcho               #-}
{-# COMPILE GHC setCooked             = System.Posix.Internals.setCooked             #-}
{-# COMPILE GHC tcSetAttr             = \ aℓ a -> System.Posix.Internals.tcSetAttr   #-}
{-# COMPILE GHC get-saved-termios     = System.Posix.Internals.get_saved_termios     #-}
{-# COMPILE GHC set-saved-termios     = System.Posix.Internals.set_saved_termios     #-}
{-# COMPILE GHC setNonBlockingFD      = System.Posix.Internals.setNonBlockingFD      #-}
{-# COMPILE GHC setCloseOnExec        = System.Posix.Internals.setCloseOnExec        #-}
{-# COMPILE GHC c-open                = System.Posix.Internals.c_open                #-}
{-# COMPILE GHC c-interruptible-open  = System.Posix.Internals.c_interruptible_open  #-}
{-# COMPILE GHC c-interruptible-open- = System.Posix.Internals.c_interruptible_open_ #-}
{-# COMPILE GHC hostIsThreaded        = System.Posix.Internals.hostIsThreaded        #-}
{-# COMPILE GHC rtsIsThreaded-        = System.Posix.Internals.rtsIsThreaded_        #-}
{-# COMPILE GHC c-safe-open           = System.Posix.Internals.c_safe_open           #-}
{-# COMPILE GHC c-safe-open-          = System.Posix.Internals.c_safe_open_          #-}
{-# COMPILE GHC c-fstat               = System.Posix.Internals.c_fstat               #-}
{-# COMPILE GHC lstat                 = System.Posix.Internals.lstat                 #-}
{-# COMPILE GHC c-lseek               = System.Posix.Internals.c_lseek               #-}
{-# COMPILE GHC c-access              = System.Posix.Internals.c_access              #-}
{-# COMPILE GHC c-chmod               = System.Posix.Internals.c_chmod               #-}
{-# COMPILE GHC c-close               = System.Posix.Internals.c_close               #-}
{-# COMPILE GHC c-creat               = System.Posix.Internals.c_creat               #-}
{-# COMPILE GHC c-dup                 = System.Posix.Internals.c_dup                 #-}
{-# COMPILE GHC c-dup2                = System.Posix.Internals.c_dup2                #-}
{-# COMPILE GHC c-isatty              = System.Posix.Internals.c_isatty              #-}
    -- todo:{-# COMPILE GHC c-read = System.Posix.Internals.c_read #-}
    -- todo:{-# COMPILE GHC c-safe-read = System.Posix.Internals.c_safe_read #-}
{-# COMPILE GHC c-umask               = System.Posix.Internals.c_umask               #-}
    -- todo:{-# COMPILE GHC c-write = System.Posix.Internals.c_write #-}
    -- todo:{-# COMPILE GHC c-safe-write = System.Posix.Internals.c_safe_write #-}
{-# COMPILE GHC c-pipe                = System.Posix.Internals.c_pipe                #-}
{-# COMPILE GHC c-unlink              = System.Posix.Internals.c_unlink              #-}
{-# COMPILE GHC c-utime               = System.Posix.Internals.c_utime               #-}
{-# COMPILE GHC c-getpid              = System.Posix.Internals.c_getpid              #-}
{-# COMPILE GHC c-stat                = System.Posix.Internals.c_stat                #-}
{-# COMPILE GHC c-ftruncate           = System.Posix.Internals.c_ftruncate           #-}
{-# COMPILE GHC c-fcntl-read          = System.Posix.Internals.c_fcntl_read          #-}
{-# COMPILE GHC c-fcntl-write         = System.Posix.Internals.c_fcntl_write         #-}
{-# COMPILE GHC c-fcntl-lock          = System.Posix.Internals.c_fcntl_lock          #-}
{-# COMPILE GHC c-fork                = System.Posix.Internals.c_fork                #-}
{-# COMPILE GHC c-link                = System.Posix.Internals.c_link                #-}
{-# COMPILE GHC c-mkfifo              = System.Posix.Internals.c_mkfifo              #-}
{-# COMPILE GHC c-sigemptyset         = System.Posix.Internals.c_sigemptyset         #-}
{-# COMPILE GHC c-sigaddset           = System.Posix.Internals.c_sigaddset           #-}
{-# COMPILE GHC c-sigprocmask         = System.Posix.Internals.c_sigprocmask         #-}
{-# COMPILE GHC c-tcgetattr           = System.Posix.Internals.c_tcgetattr           #-}
{-# COMPILE GHC c-tcsetattr           = System.Posix.Internals.c_tcsetattr           #-}
{-# COMPILE GHC c-waitpid             = System.Posix.Internals.c_waitpid             #-}
    
{-# COMPILE GHC o-RDONLY   = System.Posix.Internals.o_RDONLY   #-}
{-# COMPILE GHC o-WRONLY   = System.Posix.Internals.o_WRONLY   #-}
{-# COMPILE GHC o-RDWR     = System.Posix.Internals.o_RDWR     #-}
{-# COMPILE GHC o-APPEND   = System.Posix.Internals.o_APPEND   #-}
{-# COMPILE GHC o-CREAT    = System.Posix.Internals.o_CREAT    #-}
{-# COMPILE GHC o-EXCL     = System.Posix.Internals.o_EXCL     #-}
{-# COMPILE GHC o-TRUNC    = System.Posix.Internals.o_TRUNC    #-}
{-# COMPILE GHC o-NOCTTY   = System.Posix.Internals.o_NOCTTY   #-}
{-# COMPILE GHC o-NONBLOCK = System.Posix.Internals.o_NONBLOCK #-}
{-# COMPILE GHC o-BINARY   = System.Posix.Internals.o_BINARY   #-}

{-# COMPILE GHC c-s-isreg  = System.Posix.Internals.c_s_isreg  #-}
{-# COMPILE GHC c-s-ischr  = System.Posix.Internals.c_s_ischr  #-}
{-# COMPILE GHC c-s-isblk  = System.Posix.Internals.c_s_isblk  #-}
{-# COMPILE GHC c-s-isdir  = System.Posix.Internals.c_s_isdir  #-}
{-# COMPILE GHC c-s-isfifo = System.Posix.Internals.c_s_isfifo #-}

{-# COMPILE GHC s-isreg  = System.Posix.Internals.s_isreg  #-}
{-# COMPILE GHC s-ischr  = System.Posix.Internals.s_ischr  #-}
{-# COMPILE GHC s-isblk  = System.Posix.Internals.s_isblk  #-}
{-# COMPILE GHC s-isdir  = System.Posix.Internals.s_isdir  #-}
{-# COMPILE GHC s-isfifo = System.Posix.Internals.s_isfifo #-}

{-# COMPILE GHC sizeof-stat = System.Posix.Internals.sizeof_stat #-}

{-# COMPILE GHC st-mtime = System.Posix.Internals.st_mtime #-}
{-# COMPILE GHC st-size  = System.Posix.Internals.st_size  #-}
{-# COMPILE GHC st-mode  = System.Posix.Internals.st_mode  #-}
{-# COMPILE GHC st-dev   = System.Posix.Internals.st_dev   #-}
{-# COMPILE GHC st-ino   = System.Posix.Internals.st_ino   #-}

{-# COMPILE GHC const-echo        = System.Posix.Internals.const_echo        #-}
{-# COMPILE GHC const-tcsanow     = System.Posix.Internals.const_tcsanow     #-}
{-# COMPILE GHC const-icanon      = System.Posix.Internals.const_icanon      #-}
{-# COMPILE GHC const-vmin        = System.Posix.Internals.const_vmin        #-}
{-# COMPILE GHC const-vtime       = System.Posix.Internals.const_vtime       #-}
{-# COMPILE GHC const-sigttou     = System.Posix.Internals.const_sigttou     #-}
{-# COMPILE GHC const-sig-block   = System.Posix.Internals.const_sig_block   #-}
{-# COMPILE GHC const-sig-setmask = System.Posix.Internals.const_sig_setmask #-}
{-# COMPILE GHC const-f-getfl     = System.Posix.Internals.const_f_getfl     #-}
{-# COMPILE GHC const-f-setfl     = System.Posix.Internals.const_f_setfl     #-}
{-# COMPILE GHC const-f-setfd     = System.Posix.Internals.const_f_setfd     #-}
{-# COMPILE GHC const-fd-cloexec  = System.Posix.Internals.const_fd_cloexec  #-}

{-# COMPILE GHC sizeof-termios  = System.Posix.Internals.sizeof_termios  #-}
{-# COMPILE GHC sizeof-sigset-t = System.Posix.Internals.sizeof_sigset_t #-}

{-# COMPILE GHC c-lflag      = System.Posix.Internals.c_lflag      #-}
{-# COMPILE GHC poke-c-lflag = System.Posix.Internals.poke_c_lflag #-}
{-# COMPILE GHC ptr-c-cc     = System.Posix.Internals.ptr_c_cc     #-}
{-# COMPILE GHC s-issock     = System.Posix.Internals.s_issock     #-}
{-# COMPILE GHC c-s-issock   = System.Posix.Internals.c_s_issock   #-}

{-# COMPILE GHC dEFAULT-BUFFER-SIZE = System.Posix.Internals.dEFAULT_BUFFER_SIZE #-}
{-# COMPILE GHC sEEK-CUR            = System.Posix.Internals.sEEK_CUR            #-}
{-# COMPILE GHC sEEK-SET            = System.Posix.Internals.sEEK_SET            #-}
{-# COMPILE GHC sEEK-END            = System.Posix.Internals.sEEK_END            #-}
