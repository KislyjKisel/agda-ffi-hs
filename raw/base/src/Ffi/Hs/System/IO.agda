{-# OPTIONS --without-K #-}

module Ffi.Hs.System.IO where

open import Agda.Builtin.Bool   using (Bool)
open import Agda.Builtin.Char   using (Char)
open import Agda.Builtin.Int    using () renaming (Int to Integer)
open import Agda.Builtin.Maybe  using (Maybe)
open import Agda.Builtin.List   using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Tuple  using (Tuple2)
open import Ffi.Hs.Foreign.Ptr using (Ptr)

open import Agda.Builtin.IO public
    using (IO)

{-# FOREIGN GHC
import qualified System.IO
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaRead, AgdaShow, AgdaEq
    , AgdaOrd, AgdaEnum, AgdaIx
    , AgdaFunctor, AgdaApplicative, AgdaMonad
    , AgdaMonadFail, AgdaMonadFix, AgdaMonadIO
    , AgdaAlternative, AgdaMonadPlus, AgdaSemigroup, AgdaMonoid
    )
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

FilePath : Set
FilePath = List Char

data IOMode : Set where
    ReadMode WriteMode AppendMode ReadWriteMode : IOMode

{-# COMPILE GHC IOMode = data System.IO.IOMode (System.IO.ReadMode | System.IO.WriteMode | System.IO.AppendMode | System.IO.ReadWriteMode) #-}

data BufferMode : Set where
    NoBuffering    : BufferMode
    LineBuffering  : BufferMode
    BlockBuffering : Maybe Int → BufferMode

{-# COMPILE GHC BufferMode = data System.IO.BufferMode (System.IO.NoBuffering | System.IO.LineBuffering | System.IO.BlockBuffering) #-}

data SeekMode : Set where
    AbsoluteSeek : SeekMode
    RelativeSeek : SeekMode
    SeekFromEnd  : SeekMode

{-# COMPILE GHC SeekMode = data System.IO.SeekMode (System.IO.AbsoluteSeek | System.IO.RelativeSeek | System.IO.SeekFromEnd) #-}

data Newline : Set where
    LF CRLF : Newline

{-# COMPILE GHC Newline = data System.IO.Newline (System.IO.LF | System.IO.CRLF) #-}

record NewlineMode : Set where
    constructor mkNewlineMode
    field
        inputNL  : Newline
        outputNL : Newline

{-# COMPILE GHC NewlineMode = data System.IO.NewlineMode (System.IO.NewlineMode) #-}

postulate
    fixIO : (A → IO A) → IO A

    Handle : Set
    stdin  : Handle
    stdout : Handle
    stderr : Handle

    withFile   : FilePath → IOMode → (Handle → IO A) → IO A
    openFile   : FilePath → IOMode → IO Handle
    hClose     : Handle → IO (⊤ {lzero})
    readFile   : FilePath → IO (List Char)
    readFile'  : FilePath → IO (List Char)
    writeFile  : FilePath → List Char → IO (⊤ {lzero})
    appendFile : FilePath → List Char → IO (⊤ {lzero})

    hFileSize    : Handle → IO Integer
    hSetFileSize : Handle → Integer → IO (⊤ {lzero})
    hIsEOF : Handle → IO (⊤ {lzero})
    isEOF  : IO Bool

    hSetBuffering : Handle → BufferMode → IO (⊤ {lzero})
    hGetBuffering : Handle → IO BufferMode
    hFlush        : Handle → IO (⊤ {lzero})

    HandlePosn : Set
    hGetPosn : Handle → IO HandlePosn
    hSetPosn : HandlePosn → IO (⊤ {lzero})
    hSeek    : Handle → SeekMode → Integer → IO (⊤ {lzero})
    hTell    : Handle → IO Integer

    hIsOpen     : Handle → IO Bool
    hIsClosed   : Handle → IO Bool
    hIsReadable : Handle → IO Bool
    hIsWritable : Handle → IO Bool
    hIsSeekable : Handle → IO Bool
    
    hIsTerminalDevice : Handle → IO Bool
    hSetEcho          : Handle → Bool → IO (⊤ {lzero})
    hGetEcho          : Handle → IO Bool
    
    hShow : Handle → IO (List Char)

    hWaitForInput : Handle → Int → IO Bool
    hReady        : Handle → IO Bool
    hGetChar      : Handle → IO Char
    hGetLine      : Handle → IO (List Char)
    hLookAhead    : Handle → IO Char
    hGetContents  : Handle → IO (List Char)
    hGetContents' : Handle → IO (List Char)
    
    hPutChar  : Handle → Char → IO (⊤ {lzero})
    hPutStr   : Handle → List Char → IO (⊤ {lzero})
    hPutStrLn : Handle → List Char → IO (⊤ {lzero})
    hPrint    : ⦃ Show A ⦄ → Handle → A → IO (⊤ {lzero})

    interact     : (List Char → List Char) → IO (⊤ {lzero})
    putChar      : Char → IO (⊤ {lzero})
    putStr       : List Char → IO (⊤ {lzero})
    putStrLn     : List Char → IO (⊤ {lzero})
    print        : ⦃ Show A ⦄ → A → IO (⊤ {lzero})
    getChar      : IO Char
    getLine      : IO (List Char)
    getContents  : IO (List Char)
    getContents' : IO (List Char)
    readIO       : ⦃ Read A ⦄ → List Char → IO A
    readLn       : ⦃ Read A ⦄ → IO A

    withBinaryFile : FilePath → IOMode → (Handle → IO A) → IO A
    openBinaryFile : FilePath → IOMode → IO Handle
    hSetBinaryMode : Handle → Bool → IO (⊤ {lzero})
    hPutBuf : Handle → Ptr A → Int → IO (⊤ {lzero})
    hGetBuf : Handle → Ptr A → Int → IO Int
    hGetBufSome : Handle → Ptr A → Int → IO Int
    hPutBufNonBlocking : Handle → Ptr A → Int → IO Int
    hGetBufNonBlocking : Handle → Ptr A → Int → IO Int

    openTempFile : FilePath → List Char → IO (Tuple2 FilePath Handle)
    openBinaryTempFile : FilePath → List Char → IO (Tuple2 FilePath Handle)
    openTempFileWithDefaultPermissions : FilePath → List Char → IO (Tuple2 FilePath Handle)
    openBinaryTempFileWithDefaultPermissions : FilePath → List Char → IO (Tuple2 FilePath Handle)

    TextEncoding   : Set
    hSetEncoding   : Handle → TextEncoding → IO (⊤ {lzero})
    hGetEncoding   : Handle → IO (Maybe TextEncoding)
    mkTextEncoding : List Char → IO TextEncoding
    
    utf8 utf8_bom               : TextEncoding
    utf16 utf16le utf16be       : TextEncoding
    utf32 utf32le utf32be       : TextEncoding
    latin1 localeEncoding char8 : TextEncoding

    hSetNewlineMode      : Handle → NewlineMode → IO (⊤ {lzero})
    nativeNewline        : Newline
    noNewlineTranslation : NewlineMode
    universalNewlineMode : NewlineMode
    nativeNewlineMode    : NewlineMode

{-# COMPILE GHC fixIO = \ aℓ a -> System.IO.fixIO #-}

{-# COMPILE GHC Handle = type System.IO.Handle #-}
{-# COMPILE GHC stdin  = System.IO.stdin  #-}
{-# COMPILE GHC stdout = System.IO.stdout #-}
{-# COMPILE GHC stderr = System.IO.stderr #-}

{-# COMPILE GHC withFile   = \ aℓ a -> System.IO.withFile   #-}
{-# COMPILE GHC openFile   =           System.IO.openFile   #-}
{-# COMPILE GHC hClose     =           System.IO.hClose     #-}
{-# COMPILE GHC readFile   =           System.IO.readFile   #-}
{-# COMPILE GHC readFile'  =           System.IO.readFile'  #-}
{-# COMPILE GHC writeFile  =           System.IO.writeFile  #-}
{-# COMPILE GHC appendFile =           System.IO.appendFile #-}

{-# COMPILE GHC hFileSize    = System.IO.hFileSize    #-}
{-# COMPILE GHC hSetFileSize = System.IO.hSetFileSize #-}
{-# COMPILE GHC hIsEOF       = System.IO.hIsEOF       #-}
{-# COMPILE GHC isEOF        = System.IO.isEOF        #-}

{-# COMPILE GHC hSetBuffering = System.IO.hSetBuffering #-}
{-# COMPILE GHC hGetBuffering = System.IO.hGetBuffering #-}
{-# COMPILE GHC hFlush        = System.IO.hFlush        #-}

{-# COMPILE GHC HandlePosn = System.IO.HandlePosn #-}
{-# COMPILE GHC hGetPosn   = System.IO.hGetPosn   #-}
{-# COMPILE GHC hSetPosn   = System.IO.hSetPosn   #-}
{-# COMPILE GHC hSeek      = System.IO.hSeek      #-}
{-# COMPILE GHC hTell      = System.IO.hTell      #-}

{-# COMPILE GHC hIsOpen     = System.IO.hIsOpen     #-}
{-# COMPILE GHC hIsClosed   = System.IO.hIsClosed   #-}
{-# COMPILE GHC hIsReadable = System.IO.hIsReadable #-}
{-# COMPILE GHC hIsWritable = System.IO.hIsWritable #-}
{-# COMPILE GHC hIsSeekable = System.IO.hIsSeekable #-}

{-# COMPILE GHC hIsTerminalDevice = System.IO.hIsTerminalDevice #-}
{-# COMPILE GHC hSetEcho          = System.IO.hSetEcho          #-}
{-# COMPILE GHC hGetEcho          = System.IO.hGetEcho          #-}

{-# COMPILE GHC hShow = System.IO.hShow #-}

{-# COMPILE GHC hWaitForInput = System.IO.hWaitForInput #-}
{-# COMPILE GHC hReady        = System.IO.hReady        #-}
{-# COMPILE GHC hGetChar      = System.IO.hGetChar      #-}
{-# COMPILE GHC hGetLine      = System.IO.hGetLine      #-}
{-# COMPILE GHC hLookAhead    = System.IO.hLookAhead    #-}
{-# COMPILE GHC hGetContents  = System.IO.hGetContents  #-}
{-# COMPILE GHC hGetContents' = System.IO.hGetContents' #-}

{-# COMPILE GHC hPutChar  =                    System.IO.hPutChar  #-}
{-# COMPILE GHC hPutStr   =                    System.IO.hPutStr   #-}
{-# COMPILE GHC hPutStrLn =                    System.IO.hPutStrLn #-}
{-# COMPILE GHC hPrint    = \ aℓ a AgdaShow -> System.IO.hPrint    #-}

{-# COMPILE GHC interact     =                    System.IO.interact     #-}
{-# COMPILE GHC putChar      =                    System.IO.putChar      #-}
{-# COMPILE GHC putStr       =                    System.IO.putStr       #-}
{-# COMPILE GHC putStrLn     =                    System.IO.putStrLn     #-}
{-# COMPILE GHC print        = \ aℓ a AgdaShow -> System.IO.print        #-}
{-# COMPILE GHC getChar      =                    System.IO.getChar      #-}
{-# COMPILE GHC getLine      =                    System.IO.getLine      #-}
{-# COMPILE GHC getContents  =                    System.IO.getContents  #-}
{-# COMPILE GHC getContents' =                    System.IO.getContents' #-}
{-# COMPILE GHC readIO       = \ aℓ a AgdaRead -> System.IO.readIO       #-}
{-# COMPILE GHC readLn       = \ aℓ a AgdaRead -> System.IO.readLn       #-}

{-# COMPILE GHC withBinaryFile     = \ aℓ a -> System.IO.withBinaryFile     #-}
{-# COMPILE GHC openBinaryFile     =           System.IO.openBinaryFile     #-}
{-# COMPILE GHC hSetBinaryMode     =           System.IO.hSetBinaryMode     #-}
{-# COMPILE GHC hPutBuf            = \ aℓ a -> System.IO.hPutBuf            #-}
{-# COMPILE GHC hGetBuf            = \ aℓ a -> System.IO.hGetBuf            #-}
{-# COMPILE GHC hGetBufSome        = \ aℓ a -> System.IO.hGetBufSome        #-}
{-# COMPILE GHC hPutBufNonBlocking = \ aℓ a -> System.IO.hPutBufNonBlocking #-}
{-# COMPILE GHC hGetBufNonBlocking = \ aℓ a -> System.IO.hGetBufNonBlocking #-}

{-# COMPILE GHC openTempFile                             = System.IO.openTempFile                             #-}
{-# COMPILE GHC openBinaryTempFile                       = System.IO.openBinaryTempFile                       #-}
{-# COMPILE GHC openTempFileWithDefaultPermissions       = System.IO.openTempFileWithDefaultPermissions       #-}
{-# COMPILE GHC openBinaryTempFileWithDefaultPermissions = System.IO.openBinaryTempFileWithDefaultPermissions #-}

{-# COMPILE GHC TextEncoding   = type TextEncoding        #-}
{-# COMPILE GHC hSetEncoding   = System.IO.hSetEncoding   #-}
{-# COMPILE GHC hGetEncoding   = System.IO.hGetEncoding   #-}
{-# COMPILE GHC mkTextEncoding = System.IO.mkTextEncoding #-}

{-# COMPILE GHC utf8           = System.IO.utf8           #-}
{-# COMPILE GHC utf8_bom       = System.IO.utf8_bom       #-}
{-# COMPILE GHC utf16          = System.IO.utf16          #-}
{-# COMPILE GHC utf16le        = System.IO.utf16le        #-}
{-# COMPILE GHC utf16be        = System.IO.utf16be        #-}
{-# COMPILE GHC utf32          = System.IO.utf32          #-}
{-# COMPILE GHC utf32le        = System.IO.utf32le        #-}
{-# COMPILE GHC utf32be        = System.IO.utf32be        #-}
{-# COMPILE GHC latin1         = System.IO.latin1         #-}
{-# COMPILE GHC localeEncoding = System.IO.localeEncoding #-}
{-# COMPILE GHC char8          = System.IO.char8          #-}

{-# COMPILE GHC hSetNewlineMode      = System.IO.hSetNewlineMode      #-}
{-# COMPILE GHC nativeNewline        = System.IO.nativeNewline        #-}
{-# COMPILE GHC noNewlineTranslation = System.IO.noNewlineTranslation #-}
{-# COMPILE GHC universalNewlineMode = System.IO.universalNewlineMode #-}
{-# COMPILE GHC nativeNewlineMode    = System.IO.nativeNewlineMode    #-}

module Instances where
    postulate
        Functor[IO]      : Functor {aℓ} IO
        Applicative[IO]  : Applicative {aℓ} IO
        Alternative[IO]  : Alternative {aℓ} IO
        Monad[IO]        : Monad {aℓ} IO
        MonadFail[IO]    : MonadFail {aℓ} IO
        MonadFix[IO]     : MonadFix {aℓ} IO
        MonadIO[IO]      : MonadIO {aℓ} IO
        MonadPlus[IO]    : MonadPlus {aℓ} IO
        Semigroup[IO[A]] : ⦃ Semigroup A ⦄ → Semigroup (IO A)
        Monoid[IO[A]]    : ⦃ Monoid A ⦄ → Monoid (IO A)

        Show[Handle] : Show Handle
        Eq[Handle]   : Eq Handle

        Read[BufferMode] : Read BufferMode
        Show[BufferMode] : Show BufferMode
        Eq[BufferMode]   : Eq BufferMode
        Ord[BufferMode]  : Ord BufferMode

        Show[HandlePosn] : Show HandlePosn
        Eq[HandlePosn]   : Eq HandlePosn

        Enum[SeekMode] : Enum SeekMode
        Ix[SeekMode]   : Ix SeekMode
        Read[SeekMode] : Read SeekMode
        Show[SeekMode] : Show SeekMode
        Eq[SeekMode]   : Eq SeekMode
        Ord[SeekMode]  : Ord SeekMode

        Read[Newline] : Read Newline
        Show[Newline] : Show Newline
        Eq[Newline]   : Eq Newline
        Ord[Newline]  : Ord Newline

        Read[NewlineMode] : Read NewlineMode
        Show[NewlineMode] : Show NewlineMode
        Eq[NewlineMode]   : Eq NewlineMode
        Ord[NewlineMode]  : Ord NewlineMode

{-# COMPILE GHC Instances.Functor[IO]      = \ aℓ                 -> AgdaFunctor     #-}
{-# COMPILE GHC Instances.Applicative[IO]  = \ aℓ                 -> AgdaApplicative #-}
{-# COMPILE GHC Instances.Alternative[IO]  = \ aℓ                 -> AgdaAlternative #-}
{-# COMPILE GHC Instances.Monad[IO]        = \ aℓ                 -> AgdaMonad       #-}
{-# COMPILE GHC Instances.MonadFail[IO]    = \ aℓ                 -> AgdaMonadFail   #-}
{-# COMPILE GHC Instances.MonadFix[IO]     = \ aℓ                 -> AgdaMonadFix    #-}
{-# COMPILE GHC Instances.MonadIO[IO]      = \ aℓ                 -> AgdaMonadIO     #-}
{-# COMPILE GHC Instances.MonadPlus[IO]    = \ aℓ                 -> AgdaMonadPlus   #-}
{-# COMPILE GHC Instances.Semigroup[IO[A]] = \ aℓ a AgdaSemigroup -> AgdaSemigroup   #-}
{-# COMPILE GHC Instances.Monoid[IO[A]]    = \ aℓ a AgdaMonoid    -> AgdaMonoid      #-}

{-# COMPILE GHC Instances.Show[Handle] = AgdaShow #-}
{-# COMPILE GHC Instances.Eq[Handle]   = AgdaEq   #-}

{-# COMPILE GHC Instances.Read[BufferMode] = AgdaRead #-}
{-# COMPILE GHC Instances.Show[BufferMode] = AgdaShow #-}
{-# COMPILE GHC Instances.Eq[BufferMode]   = AgdaEq   #-}
{-# COMPILE GHC Instances.Ord[BufferMode]  = AgdaOrd  #-}

{-# COMPILE GHC Instances.Show[HandlePosn] = AgdaShow #-}
{-# COMPILE GHC Instances.Eq[HandlePosn]   = AgdaEq   #-}

{-# COMPILE GHC Instances.Enum[SeekMode] = AgdaEnum #-}
{-# COMPILE GHC Instances.Ix[SeekMode]   = AgdaIx   #-}
{-# COMPILE GHC Instances.Read[SeekMode] = AgdaRead #-}
{-# COMPILE GHC Instances.Show[SeekMode] = AgdaShow #-}
{-# COMPILE GHC Instances.Eq[SeekMode]   = AgdaEq   #-}
{-# COMPILE GHC Instances.Ord[SeekMode]  = AgdaOrd  #-}

{-# COMPILE GHC Instances.Read[Newline] = AgdaRead #-}
{-# COMPILE GHC Instances.Show[Newline] = AgdaShow #-}
{-# COMPILE GHC Instances.Eq[Newline]   = AgdaEq   #-}
{-# COMPILE GHC Instances.Ord[Newline]  = AgdaOrd  #-}

{-# COMPILE GHC Instances.Read[NewlineMode] = AgdaRead #-}
{-# COMPILE GHC Instances.Show[NewlineMode] = AgdaShow #-}
{-# COMPILE GHC Instances.Eq[NewlineMode]   = AgdaEq   #-}
{-# COMPILE GHC Instances.Ord[NewlineMode]  = AgdaOrd  #-}
