{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Posix.Types where

open import Ffi.Hs.-base.Class
open import Ffi.Hs.Foreign.C.Types using (CSize; CClock; CTime; CLong)

{-# FOREIGN GHC
import qualified System.Posix.Types
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaStorable, AgdaBits, AgdaFiniteBits, AgdaBounded
    , AgdaEnum, AgdaIx, AgdaNum, AgdaRead, AgdaIntegral
    , AgdaReal, AgdaShow, AgdaEq, AgdaOrd
    )
#-}

postulate
    CDev      : Set
    CIno      : Set
    CMode     : Set
    COff      : Set
    CPid      : Set
    CSsize    : Set
    CGid      : Set
    CNlink    : Set
    CUid      : Set
    CCc       : Set
    CSpeed    : Set
    CTcflag   : Set
    CRLim     : Set
    CBlkSize  : Set
    CBlkCnt   : Set
    CClockId  : Set
    CFsBlkCnt : Set
    CFsFilCnt : Set
    CId       : Set
    CKey      : Set
    CTimer    : Set
    CSockLen  : Set
    CNfds     : Set
    Fd        : Set

{-# COMPILE GHC CDev      = System.Posix.Types.CDev      #-}
{-# COMPILE GHC CIno      = System.Posix.Types.CIno      #-}
{-# COMPILE GHC CMode     = System.Posix.Types.CMode     #-}
{-# COMPILE GHC COff      = System.Posix.Types.COff      #-}
{-# COMPILE GHC CPid      = System.Posix.Types.CPid      #-}
{-# COMPILE GHC CSsize    = System.Posix.Types.CSsize    #-}
{-# COMPILE GHC CGid      = System.Posix.Types.CGid      #-}
{-# COMPILE GHC CNlink    = System.Posix.Types.CNlink    #-}
{-# COMPILE GHC CUid      = System.Posix.Types.CUid      #-}
{-# COMPILE GHC CCc       = System.Posix.Types.CCc       #-}
{-# COMPILE GHC CSpeed    = System.Posix.Types.CSpeed    #-}
{-# COMPILE GHC CTcflag   = System.Posix.Types.CTcflag   #-}
{-# COMPILE GHC CRLim     = System.Posix.Types.CRLim     #-}
{-# COMPILE GHC CBlkSize  = System.Posix.Types.CBlkSize  #-}
{-# COMPILE GHC CBlkCnt   = System.Posix.Types.CBlkCnt   #-}
{-# COMPILE GHC CClockId  = System.Posix.Types.CClockId  #-}
{-# COMPILE GHC CFsBlkCnt = System.Posix.Types.CFsBlkCnt #-}
{-# COMPILE GHC CFsFilCnt = System.Posix.Types.CFsFilCnt #-}
{-# COMPILE GHC CId       = System.Posix.Types.CId       #-}
{-# COMPILE GHC CKey      = System.Posix.Types.CKey      #-}
{-# COMPILE GHC CTimer    = System.Posix.Types.CTimer    #-}
{-# COMPILE GHC CSockLen  = System.Posix.Types.CSockLen  #-}
{-# COMPILE GHC CNfds     = System.Posix.Types.CNfds     #-}
{-# COMPILE GHC Fd        = System.Posix.Types.Fd        #-}

LinkCount : Set
LinkCount = CNlink

UserID : Set
UserID = CUid

GroupID : Set
GroupID = CGid

ByteCount : Set
ByteCount = CSize

ClockTick : Set
ClockTick = CClock

EpochTime : Set
EpochTime = CTime

FileOffset : Set
FileOffset = COff

ProcessID : Set
ProcessID = CPid

ProcessGroupID : Set
ProcessGroupID = CPid

DeviceID : Set
DeviceID = CDev

FileID : Set
FileID = CIno

FileMode : Set
FileMode = CMode

Limit : Set
Limit = CLong

postulate
    Storable[CDev]   : Storable CDev
    Bits[CDev]       : Bits CDev
    FiniteBits[CDev] : FiniteBits CDev
    Bounded[CDev]    : Bounded CDev
    Enum[CDev]       : Enum CDev
    Ix[CDev]         : Ix CDev
    Num[CDev]        : Num CDev
    Read[CDev]       : Read CDev
    Integral[CDev]   : Integral CDev
    Real[CDev]       : Real CDev
    Show[CDev]       : Show CDev
    Eq[CDev]         : Eq CDev
    Ord[CDev]        : Ord CDev

    Storable[CIno]   : Storable CIno
    Bits[CIno]       : Bits CIno
    FiniteBits[CIno] : FiniteBits CIno
    Bounded[CIno]    : Bounded CIno
    Enum[CIno]       : Enum CIno
    Ix[CIno]         : Ix CIno
    Num[CIno]        : Num CIno
    Read[CIno]       : Read CIno
    Integral[CIno]   : Integral CIno
    Real[CIno]       : Real CIno
    Show[CIno]       : Show CIno
    Eq[CIno]         : Eq CIno
    Ord[CIno]        : Ord CIno

    Storable[CMode]   : Storable CMode
    Bits[CMode]       : Bits CMode
    FiniteBits[CMode] : FiniteBits CMode
    Bounded[CMode]    : Bounded CMode
    Enum[CMode]       : Enum CMode
    Ix[CMode]         : Ix CMode
    Num[CMode]        : Num CMode
    Read[CMode]       : Read CMode
    Integral[CMode]   : Integral CMode
    Real[CMode]       : Real CMode
    Show[CMode]       : Show CMode
    Eq[CMode]         : Eq CMode
    Ord[CMode]        : Ord CMode

    Storable[COff]   : Storable COff
    Bits[COff]       : Bits COff
    FiniteBits[COff] : FiniteBits COff
    Bounded[COff]    : Bounded COff
    Enum[COff]       : Enum COff
    Ix[COff]         : Ix COff
    Num[COff]        : Num COff
    Read[COff]       : Read COff
    Integral[COff]   : Integral COff
    Real[COff]       : Real COff
    Show[COff]       : Show COff
    Eq[COff]         : Eq COff
    Ord[COff]        : Ord COff

    Storable[CPid]   : Storable CPid
    Bits[CPid]       : Bits CPid
    FiniteBits[CPid] : FiniteBits CPid
    Bounded[CPid]    : Bounded CPid
    Enum[CPid]       : Enum CPid
    Ix[CPid]         : Ix CPid
    Num[CPid]        : Num CPid
    Read[CPid]       : Read CPid
    Integral[CPid]   : Integral CPid
    Real[CPid]       : Real CPid
    Show[CPid]       : Show CPid
    Eq[CPid]         : Eq CPid
    Ord[CPid]        : Ord CPid

    Storable[CSsize]   : Storable CSsize
    Bits[CSsize]       : Bits CSsize
    FiniteBits[CSsize] : FiniteBits CSsize
    Bounded[CSsize]    : Bounded CSsize
    Enum[CSsize]       : Enum CSsize
    Ix[CSsize]         : Ix CSsize
    Num[CSsize]        : Num CSsize
    Read[CSsize]       : Read CSsize
    Integral[CSsize]   : Integral CSsize
    Real[CSsize]       : Real CSsize
    Show[CSsize]       : Show CSsize
    Eq[CSsize]         : Eq CSsize
    Ord[CSsize]        : Ord CSsize

    Storable[CGid]   : Storable CGid
    Bits[CGid]       : Bits CGid
    FiniteBits[CGid] : FiniteBits CGid
    Bounded[CGid]    : Bounded CGid
    Enum[CGid]       : Enum CGid
    Ix[CGid]         : Ix CGid
    Num[CGid]        : Num CGid
    Read[CGid]       : Read CGid
    Integral[CGid]   : Integral CGid
    Real[CGid]       : Real CGid
    Show[CGid]       : Show CGid
    Eq[CGid]         : Eq CGid
    Ord[CGid]        : Ord CGid

    Storable[CNlink]   : Storable CNlink
    Bits[CNlink]       : Bits CNlink
    FiniteBits[CNlink] : FiniteBits CNlink
    Bounded[CNlink]    : Bounded CNlink
    Enum[CNlink]       : Enum CNlink
    Ix[CNlink]         : Ix CNlink
    Num[CNlink]        : Num CNlink
    Read[CNlink]       : Read CNlink
    Integral[CNlink]   : Integral CNlink
    Real[CNlink]       : Real CNlink
    Show[CNlink]       : Show CNlink
    Eq[CNlink]         : Eq CNlink
    Ord[CNlink]        : Ord CNlink

    Storable[CUid]   : Storable CUid
    Bits[CUid]       : Bits CUid
    FiniteBits[CUid] : FiniteBits CUid
    Bounded[CUid]    : Bounded CUid
    Enum[CUid]       : Enum CUid
    Ix[CUid]         : Ix CUid
    Num[CUid]        : Num CUid
    Read[CUid]       : Read CUid
    Integral[CUid]   : Integral CUid
    Real[CUid]       : Real CUid
    Show[CUid]       : Show CUid
    Eq[CUid]         : Eq CUid
    Ord[CUid]        : Ord CUid

    Storable[CCc]   : Storable CCc
    Enum[CCc]       : Enum CCc
    Num[CCc]        : Num CCc
    Read[CCc]       : Read CCc
    Real[CCc]       : Real CCc
    Show[CCc]       : Show CCc
    Eq[CCc]         : Eq CCc
    Ord[CCc]        : Ord CCc

    Storable[CSpeed]   : Storable CSpeed
    Enum[CSpeed]       : Enum CSpeed
    Num[CSpeed]        : Num CSpeed
    Read[CSpeed]       : Read CSpeed
    Real[CSpeed]       : Real CSpeed
    Show[CSpeed]       : Show CSpeed
    Eq[CSpeed]         : Eq CSpeed
    Ord[CSpeed]        : Ord CSpeed

    Storable[CTcflag]   : Storable CTcflag
    Bits[CTcflag]       : Bits CTcflag
    FiniteBits[CTcflag] : FiniteBits CTcflag
    Bounded[CTcflag]    : Bounded CTcflag
    Enum[CTcflag]       : Enum CTcflag
    Ix[CTcflag]         : Ix CTcflag
    Num[CTcflag]        : Num CTcflag
    Read[CTcflag]       : Read CTcflag
    Integral[CTcflag]   : Integral CTcflag
    Real[CTcflag]       : Real CTcflag
    Show[CTcflag]       : Show CTcflag
    Eq[CTcflag]         : Eq CTcflag
    Ord[CTcflag]        : Ord CTcflag

    Storable[CRLim]   : Storable CRLim
    Bits[CRLim]       : Bits CRLim
    FiniteBits[CRLim] : FiniteBits CRLim
    Bounded[CRLim]    : Bounded CRLim
    Enum[CRLim]       : Enum CRLim
    Ix[CRLim]         : Ix CRLim
    Num[CRLim]        : Num CRLim
    Read[CRLim]       : Read CRLim
    Integral[CRLim]   : Integral CRLim
    Real[CRLim]       : Real CRLim
    Show[CRLim]       : Show CRLim
    Eq[CRLim]         : Eq CRLim
    Ord[CRLim]        : Ord CRLim

    Storable[CBlkSize]   : Storable CBlkSize
    Bits[CBlkSize]       : Bits CBlkSize
    FiniteBits[CBlkSize] : FiniteBits CBlkSize
    Bounded[CBlkSize]    : Bounded CBlkSize
    Enum[CBlkSize]       : Enum CBlkSize
    Ix[CBlkSize]         : Ix CBlkSize
    Num[CBlkSize]        : Num CBlkSize
    Read[CBlkSize]       : Read CBlkSize
    Integral[CBlkSize]   : Integral CBlkSize
    Real[CBlkSize]       : Real CBlkSize
    Show[CBlkSize]       : Show CBlkSize
    Eq[CBlkSize]         : Eq CBlkSize
    Ord[CBlkSize]        : Ord CBlkSize

    Storable[CBlkCnt]   : Storable CBlkCnt
    Bits[CBlkCnt]       : Bits CBlkCnt
    FiniteBits[CBlkCnt] : FiniteBits CBlkCnt
    Bounded[CBlkCnt]    : Bounded CBlkCnt
    Enum[CBlkCnt]       : Enum CBlkCnt
    Ix[CBlkCnt]         : Ix CBlkCnt
    Num[CBlkCnt]        : Num CBlkCnt
    Read[CBlkCnt]       : Read CBlkCnt
    Integral[CBlkCnt]   : Integral CBlkCnt
    Real[CBlkCnt]       : Real CBlkCnt
    Show[CBlkCnt]       : Show CBlkCnt
    Eq[CBlkCnt]         : Eq CBlkCnt
    Ord[CBlkCnt]        : Ord CBlkCnt

    Storable[CClockId]   : Storable CClockId
    Bits[CClockId]       : Bits CClockId
    FiniteBits[CClockId] : FiniteBits CClockId
    Bounded[CClockId]    : Bounded CClockId
    Enum[CClockId]       : Enum CClockId
    Ix[CClockId]         : Ix CClockId
    Num[CClockId]        : Num CClockId
    Read[CClockId]       : Read CClockId
    Integral[CClockId]   : Integral CClockId
    Real[CClockId]       : Real CClockId
    Show[CClockId]       : Show CClockId
    Eq[CClockId]         : Eq CClockId
    Ord[CClockId]        : Ord CClockId

    Storable[CFsBlkCnt]   : Storable CFsBlkCnt
    Bits[CFsBlkCnt]       : Bits CFsBlkCnt
    FiniteBits[CFsBlkCnt] : FiniteBits CFsBlkCnt
    Bounded[CFsBlkCnt]    : Bounded CFsBlkCnt
    Enum[CFsBlkCnt]       : Enum CFsBlkCnt
    Ix[CFsBlkCnt]         : Ix CFsBlkCnt
    Num[CFsBlkCnt]        : Num CFsBlkCnt
    Read[CFsBlkCnt]       : Read CFsBlkCnt
    Integral[CFsBlkCnt]   : Integral CFsBlkCnt
    Real[CFsBlkCnt]       : Real CFsBlkCnt
    Show[CFsBlkCnt]       : Show CFsBlkCnt
    Eq[CFsBlkCnt]         : Eq CFsBlkCnt
    Ord[CFsBlkCnt]        : Ord CFsBlkCnt

    Storable[CFsFilCnt]   : Storable CFsFilCnt
    Bits[CFsFilCnt]       : Bits CFsFilCnt
    FiniteBits[CFsFilCnt] : FiniteBits CFsFilCnt
    Bounded[CFsFilCnt]    : Bounded CFsFilCnt
    Enum[CFsFilCnt]       : Enum CFsFilCnt
    Ix[CFsFilCnt]         : Ix CFsFilCnt
    Num[CFsFilCnt]        : Num CFsFilCnt
    Read[CFsFilCnt]       : Read CFsFilCnt
    Integral[CFsFilCnt]   : Integral CFsFilCnt
    Real[CFsFilCnt]       : Real CFsFilCnt
    Show[CFsFilCnt]       : Show CFsFilCnt
    Eq[CFsFilCnt]         : Eq CFsFilCnt
    Ord[CFsFilCnt]        : Ord CFsFilCnt

    Storable[CId]   : Storable CId
    Bits[CId]       : Bits CId
    FiniteBits[CId] : FiniteBits CId
    Bounded[CId]    : Bounded CId
    Enum[CId]       : Enum CId
    Ix[CId]         : Ix CId
    Num[CId]        : Num CId
    Read[CId]       : Read CId
    Integral[CId]   : Integral CId
    Real[CId]       : Real CId
    Show[CId]       : Show CId
    Eq[CId]         : Eq CId
    Ord[CId]        : Ord CId

    Storable[CKey]   : Storable CKey
    Bits[CKey]       : Bits CKey
    FiniteBits[CKey] : FiniteBits CKey
    Bounded[CKey]    : Bounded CKey
    Enum[CKey]       : Enum CKey
    Ix[CKey]         : Ix CKey
    Num[CKey]        : Num CKey
    Read[CKey]       : Read CKey
    Integral[CKey]   : Integral CKey
    Real[CKey]       : Real CKey
    Show[CKey]       : Show CKey
    Eq[CKey]         : Eq CKey
    Ord[CKey]        : Ord CKey

    Storable[CTimer]   : Storable CTimer
    Show[CTimer]       : Show CTimer
    Eq[CTimer]         : Eq CTimer
    Ord[CTimer]        : Ord CTimer

    Storable[CSockLen]   : Storable CSockLen
    Bits[CSockLen]       : Bits CSockLen
    FiniteBits[CSockLen] : FiniteBits CSockLen
    Bounded[CSockLen]    : Bounded CSockLen
    Enum[CSockLen]       : Enum CSockLen
    Ix[CSockLen]         : Ix CSockLen
    Num[CSockLen]        : Num CSockLen
    Read[CSockLen]       : Read CSockLen
    Integral[CSockLen]   : Integral CSockLen
    Real[CSockLen]       : Real CSockLen
    Show[CSockLen]       : Show CSockLen
    Eq[CSockLen]         : Eq CSockLen
    Ord[CSockLen]        : Ord CSockLen

    Storable[CNfds]   : Storable CNfds
    Bits[CNfds]       : Bits CNfds
    FiniteBits[CNfds] : FiniteBits CNfds
    Bounded[CNfds]    : Bounded CNfds
    Enum[CNfds]       : Enum CNfds
    Ix[CNfds]         : Ix CNfds
    Num[CNfds]        : Num CNfds
    Read[CNfds]       : Read CNfds
    Integral[CNfds]   : Integral CNfds
    Real[CNfds]       : Real CNfds
    Show[CNfds]       : Show CNfds
    Eq[CNfds]         : Eq CNfds
    Ord[CNfds]        : Ord CNfds

    Storable[Fd]   : Storable Fd
    Bits[Fd]       : Bits Fd
    FiniteBits[Fd] : FiniteBits Fd
    Bounded[Fd]    : Bounded Fd
    Enum[Fd]       : Enum Fd
    Ix[Fd]         : Ix Fd
    Num[Fd]        : Num Fd
    Read[Fd]       : Read Fd
    Integral[Fd]   : Integral Fd
    Real[Fd]       : Real Fd
    Show[Fd]       : Show Fd
    Eq[Fd]         : Eq Fd
    Ord[Fd]        : Ord Fd

{-# COMPILE GHC Storable[CDev]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CDev]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CDev] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CDev]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CDev]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CDev]         = AgdaIx         #-}
{-# COMPILE GHC Num[CDev]        = AgdaNum        #-}
{-# COMPILE GHC Read[CDev]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CDev]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CDev]       = AgdaReal       #-}
{-# COMPILE GHC Show[CDev]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CDev]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CDev]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CIno]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CIno]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CIno] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CIno]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CIno]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CIno]         = AgdaIx         #-}
{-# COMPILE GHC Num[CIno]        = AgdaNum        #-}
{-# COMPILE GHC Read[CIno]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CIno]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CIno]       = AgdaReal       #-}
{-# COMPILE GHC Show[CIno]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CIno]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CIno]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CMode]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CMode]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CMode] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CMode]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CMode]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CMode]         = AgdaIx         #-}
{-# COMPILE GHC Num[CMode]        = AgdaNum        #-}
{-# COMPILE GHC Read[CMode]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CMode]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CMode]       = AgdaReal       #-}
{-# COMPILE GHC Show[CMode]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CMode]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CMode]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[COff]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[COff]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[COff] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[COff]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[COff]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[COff]         = AgdaIx         #-}
{-# COMPILE GHC Num[COff]        = AgdaNum        #-}
{-# COMPILE GHC Read[COff]       = AgdaRead       #-}
{-# COMPILE GHC Integral[COff]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[COff]       = AgdaReal       #-}
{-# COMPILE GHC Show[COff]       = AgdaShow       #-}
{-# COMPILE GHC Eq[COff]         = AgdaEq         #-}
{-# COMPILE GHC Ord[COff]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CPid]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CPid]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CPid] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CPid]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CPid]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CPid]         = AgdaIx         #-}
{-# COMPILE GHC Num[CPid]        = AgdaNum        #-}
{-# COMPILE GHC Read[CPid]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CPid]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CPid]       = AgdaReal       #-}
{-# COMPILE GHC Show[CPid]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CPid]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CPid]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CSsize]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CSsize]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CSsize] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CSsize]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CSsize]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CSsize]         = AgdaIx         #-}
{-# COMPILE GHC Num[CSsize]        = AgdaNum        #-}
{-# COMPILE GHC Read[CSsize]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CSsize]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CSsize]       = AgdaReal       #-}
{-# COMPILE GHC Show[CSsize]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CSsize]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CSsize]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CGid]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CGid]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CGid] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CGid]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CGid]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CGid]         = AgdaIx         #-}
{-# COMPILE GHC Num[CGid]        = AgdaNum        #-}
{-# COMPILE GHC Read[CGid]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CGid]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CGid]       = AgdaReal       #-}
{-# COMPILE GHC Show[CGid]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CGid]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CGid]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CNlink]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CNlink]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CNlink] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CNlink]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CNlink]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CNlink]         = AgdaIx         #-}
{-# COMPILE GHC Num[CNlink]        = AgdaNum        #-}
{-# COMPILE GHC Read[CNlink]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CNlink]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CNlink]       = AgdaReal       #-}
{-# COMPILE GHC Show[CNlink]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CNlink]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CNlink]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CUid]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CUid]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CUid] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CUid]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CUid]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CUid]         = AgdaIx         #-}
{-# COMPILE GHC Num[CUid]        = AgdaNum        #-}
{-# COMPILE GHC Read[CUid]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CUid]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CUid]       = AgdaReal       #-}
{-# COMPILE GHC Show[CUid]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CUid]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CUid]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CCc] = AgdaStorable #-}
{-# COMPILE GHC Enum[CCc]     = AgdaEnum     #-}
{-# COMPILE GHC Num[CCc]      = AgdaNum      #-}
{-# COMPILE GHC Read[CCc]     = AgdaRead     #-}
{-# COMPILE GHC Real[CCc]     = AgdaReal     #-}
{-# COMPILE GHC Show[CCc]     = AgdaShow     #-}
{-# COMPILE GHC Eq[CCc]       = AgdaEq       #-}
{-# COMPILE GHC Ord[CCc]      = AgdaOrd      #-}

{-# COMPILE GHC Storable[CSpeed] = AgdaStorable #-}
{-# COMPILE GHC Enum[CSpeed]     = AgdaEnum     #-}
{-# COMPILE GHC Num[CSpeed]      = AgdaNum      #-}
{-# COMPILE GHC Read[CSpeed]     = AgdaRead     #-}
{-# COMPILE GHC Real[CSpeed]     = AgdaReal     #-}
{-# COMPILE GHC Show[CSpeed]     = AgdaShow     #-}
{-# COMPILE GHC Eq[CSpeed]       = AgdaEq       #-}
{-# COMPILE GHC Ord[CSpeed]      = AgdaOrd      #-}

{-# COMPILE GHC Storable[CTcflag]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CTcflag]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CTcflag] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CTcflag]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CTcflag]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CTcflag]         = AgdaIx         #-}
{-# COMPILE GHC Num[CTcflag]        = AgdaNum        #-}
{-# COMPILE GHC Read[CTcflag]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CTcflag]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CTcflag]       = AgdaReal       #-}
{-# COMPILE GHC Show[CTcflag]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CTcflag]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CTcflag]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CRLim]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CRLim]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CRLim] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CRLim]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CRLim]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CRLim]         = AgdaIx         #-}
{-# COMPILE GHC Num[CRLim]        = AgdaNum        #-}
{-# COMPILE GHC Read[CRLim]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CRLim]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CRLim]       = AgdaReal       #-}
{-# COMPILE GHC Show[CRLim]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CRLim]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CRLim]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CBlkSize]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CBlkSize]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CBlkSize] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CBlkSize]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CBlkSize]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CBlkSize]         = AgdaIx         #-}
{-# COMPILE GHC Num[CBlkSize]        = AgdaNum        #-}
{-# COMPILE GHC Read[CBlkSize]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CBlkSize]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CBlkSize]       = AgdaReal       #-}
{-# COMPILE GHC Show[CBlkSize]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CBlkSize]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CBlkSize]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CBlkCnt]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CBlkCnt]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CBlkCnt] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CBlkCnt]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CBlkCnt]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CBlkCnt]         = AgdaIx         #-}
{-# COMPILE GHC Num[CBlkCnt]        = AgdaNum        #-}
{-# COMPILE GHC Read[CBlkCnt]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CBlkCnt]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CBlkCnt]       = AgdaReal       #-}
{-# COMPILE GHC Show[CBlkCnt]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CBlkCnt]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CBlkCnt]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CClockId]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CClockId]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CClockId] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CClockId]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CClockId]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CClockId]         = AgdaIx         #-}
{-# COMPILE GHC Num[CClockId]        = AgdaNum        #-}
{-# COMPILE GHC Read[CClockId]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CClockId]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CClockId]       = AgdaReal       #-}
{-# COMPILE GHC Show[CClockId]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CClockId]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CClockId]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CFsBlkCnt]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CFsBlkCnt]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CFsBlkCnt] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CFsBlkCnt]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CFsBlkCnt]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CFsBlkCnt]         = AgdaIx         #-}
{-# COMPILE GHC Num[CFsBlkCnt]        = AgdaNum        #-}
{-# COMPILE GHC Read[CFsBlkCnt]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CFsBlkCnt]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CFsBlkCnt]       = AgdaReal       #-}
{-# COMPILE GHC Show[CFsBlkCnt]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CFsBlkCnt]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CFsBlkCnt]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CFsFilCnt]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CFsFilCnt]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CFsFilCnt] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CFsFilCnt]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CFsFilCnt]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CFsFilCnt]         = AgdaIx         #-}
{-# COMPILE GHC Num[CFsFilCnt]        = AgdaNum        #-}
{-# COMPILE GHC Read[CFsFilCnt]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CFsFilCnt]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CFsFilCnt]       = AgdaReal       #-}
{-# COMPILE GHC Show[CFsFilCnt]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CFsFilCnt]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CFsFilCnt]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CId]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CId]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CId] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CId]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CId]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CId]         = AgdaIx         #-}
{-# COMPILE GHC Num[CId]        = AgdaNum        #-}
{-# COMPILE GHC Read[CId]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CId]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CId]       = AgdaReal       #-}
{-# COMPILE GHC Show[CId]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CId]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CId]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CKey]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CKey]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CKey] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CKey]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CKey]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CKey]         = AgdaIx         #-}
{-# COMPILE GHC Num[CKey]        = AgdaNum        #-}
{-# COMPILE GHC Read[CKey]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CKey]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CKey]       = AgdaReal       #-}
{-# COMPILE GHC Show[CKey]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CKey]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CKey]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CTimer] = AgdaStorable #-}
{-# COMPILE GHC Show[CTimer]     = AgdaShow     #-}
{-# COMPILE GHC Eq[CTimer]       = AgdaEq       #-}
{-# COMPILE GHC Ord[CTimer]      = AgdaOrd      #-}

{-# COMPILE GHC Storable[CSockLen]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CSockLen]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CSockLen] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CSockLen]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CSockLen]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CSockLen]         = AgdaIx         #-}
{-# COMPILE GHC Num[CSockLen]        = AgdaNum        #-}
{-# COMPILE GHC Read[CSockLen]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CSockLen]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CSockLen]       = AgdaReal       #-}
{-# COMPILE GHC Show[CSockLen]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CSockLen]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CSockLen]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CNfds]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CNfds]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CNfds] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CNfds]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CNfds]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CNfds]         = AgdaIx         #-}
{-# COMPILE GHC Num[CNfds]        = AgdaNum        #-}
{-# COMPILE GHC Read[CNfds]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CNfds]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CNfds]       = AgdaReal       #-}
{-# COMPILE GHC Show[CNfds]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CNfds]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CNfds]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[Fd]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[Fd]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Fd] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Fd]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[Fd]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[Fd]         = AgdaIx         #-}
{-# COMPILE GHC Num[Fd]        = AgdaNum        #-}
{-# COMPILE GHC Read[Fd]       = AgdaRead       #-}
{-# COMPILE GHC Integral[Fd]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[Fd]       = AgdaReal       #-}
{-# COMPILE GHC Show[Fd]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Fd]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Fd]        = AgdaOrd        #-}
