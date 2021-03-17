{-# language DataKinds    #-}
{-# language GADTs        #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{- |
Module      :  FXPak.Internal
Copyright   :  (c) Christina Wuest 2021
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Internals for FXPak - ensures only valid packets can be encoded.
-}

module FXPak.Internal where

import Prelude

import qualified Data.ByteString.Char8 as BS

import Data.Bits ( (.|.), shiftL )

data Flag = SkipReset | OnlyReset | ClearX | SetX | StreamBurst | NoResponse | Data64Bytes deriving ( Eq, Show, Enum, Bounded )
type Flags = [Flag]

fromFlags :: Flags -> Int
fromFlags = (foldl (.|.) 0) . (fmap ((shiftL 1) . fromEnum))

data Context = File | SNES | MSU | Config deriving ( Eq, Show, Enum, Bounded )
data Context' (c :: Context) where
    File'   :: Context' 'File
    SNES'   :: Context' 'SNES
    MSU'    :: Context' 'MSU
    Config' :: Context' 'Config

context :: Context' c -> Context
context File'   = File
context SNES'   = SNES
context MSU'    = MSU
context Config' = Config

data Opcode = Get | Put | VGet | VPut
            | List | Mkdir | Remove | Move
            | Reset | Boot | PowerCycle | Info | MenuReset | Stream | Time
            | Response
            deriving ( Eq, Show, Enum, Bounded )
data Opcode' (o :: Opcode) where
    Get'        :: Opcode' 'Get
    Put'        :: Opcode' 'Put
    VGet'       :: Opcode' 'VGet
    VPut'       :: Opcode' 'VPut
    List'       :: Opcode' 'List
    Mkdir'      :: Opcode' 'Mkdir
    Remove'     :: Opcode' 'Remove
    Move'       :: Opcode' 'Move
    Reset'      :: Opcode' 'Reset
    Boot'       :: Opcode' 'Boot
    PowerCycle' :: Opcode' 'PowerCycle
    Info'       :: Opcode' 'Info
    MenuReset'  :: Opcode' 'MenuReset
    Stream'     :: Opcode' 'Stream
    Time'       :: Opcode' 'Time
    Response'   :: Opcode' 'Response

opcode :: Opcode' o -> Opcode
opcode Get'        = Get
opcode Put'        = Put
opcode VGet'       = VGet
opcode VPut'       = VPut
opcode List'       = List
opcode Mkdir'      = Mkdir
opcode Remove'     = Remove
opcode Move'       = Move
opcode Reset'      = Reset
opcode Boot'       = Boot
opcode PowerCycle' = PowerCycle
opcode Info'       = Info
opcode MenuReset'  = MenuReset
opcode Stream'     = Stream
opcode Time'       = Time
opcode Response'   = Response

data AddressGet = AddressGet { start      :: Int
                             , dataLength :: Int
                             } deriving ( Show )

data AddressSet = AddressSet { target :: Int
                             , value  :: Int
                             } deriving ( Show )

data Arguments = None
               | Path FilePath
               | PathContents FilePath BS.ByteString
               | PathRename FilePath FilePath
               | GetBytes AddressGet
               | GetBytes2 (AddressGet, AddressGet)
               | GetBytes3 (AddressGet, AddressGet, AddressGet)
               | GetBytes4 (AddressGet, AddressGet, AddressGet, AddressGet)
               | SetByte AddressSet
               | SetByte2 (AddressSet, AddressSet)
               | SetByte3 (AddressSet, AddressSet, AddressSet)
               | SetByte4 (AddressSet, AddressSet, AddressSet, AddressSet)
               deriving ( Show )

data Arguments' (a :: Arguments) where
    None' :: Arguments' 'None
    Path' :: FilePath -> Arguments' ('Path (a :: FilePath))
    PathContents' :: FilePath -> BS.ByteString -> Arguments' ('PathContents (a :: FilePath) (b :: BS.ByteString))
    PathRename' :: FilePath -> FilePath -> Arguments' ('PathRename (a :: FilePath) a)
    GetBytes' :: AddressGet -> Arguments' ('GetBytes (a :: AddressGet))
    GetBytes2' :: AddressGet -> AddressGet -> Arguments' ('GetBytes2 (a :: (AddressGet, AddressGet)))
    GetBytes3' :: AddressGet -> AddressGet -> AddressGet -> Arguments' ('GetBytes3 (a :: (AddressGet, AddressGet, AddressGet)))
    GetBytes4' :: AddressGet -> AddressGet -> AddressGet -> AddressGet -> Arguments' ('GetBytes4 (a :: (AddressGet, AddressGet, AddressGet, AddressGet)))
    SetByte' :: AddressSet -> Arguments' ('SetByte (a :: AddressSet))
    SetByte2' :: AddressSet -> AddressSet -> Arguments' ('SetByte2 (a :: (AddressSet, AddressSet)))
    SetByte3' :: AddressSet -> AddressSet -> AddressSet -> Arguments' ('SetByte3 (a :: (AddressSet, AddressSet, AddressSet)))
    SetByte4' :: AddressSet -> AddressSet -> AddressSet -> AddressSet -> Arguments' ('SetByte4 (a :: (AddressSet, AddressSet, AddressSet, AddressSet)))

arguments :: Arguments' a -> Arguments
arguments None' = None
arguments (Path' a) = Path a
arguments (PathContents' a b) = PathContents a b
arguments (PathRename' a b) = PathRename a b
arguments (GetBytes' a) = GetBytes a
arguments (GetBytes2' a b) = GetBytes2 (a, b)
arguments (GetBytes3' a b c) = GetBytes3 (a, b, c)
arguments (GetBytes4' a b c d) = GetBytes4 (a, b, c, d)
arguments (SetByte' a) = SetByte a
arguments (SetByte2' a b) = SetByte2 (a, b)
arguments (SetByte3' a b c) = SetByte3 (a, b, c)
arguments (SetByte4' a b c d) = SetByte4 (a, b, c, d)

type family ValidPacket (c :: Context) (o :: Opcode) (a :: Arguments) :: Bool where
    ValidPacket 'File 'Get ('Path _) = 'True
    ValidPacket 'File 'Put ('PathContents _ _) = 'True
    ValidPacket 'File 'List ('Path _) = 'True
    ValidPacket 'File 'Mkdir ('Path _) = 'True
    ValidPacket 'File 'Remove ('Path _) = 'True
    ValidPacket 'File 'Move ('PathRename _ _) = 'True
    ValidPacket 'File 'Boot ('Path _) = 'True

    ValidPacket 'File 'Get _ = 'False
    ValidPacket 'File 'Put _ = 'False
    ValidPacket 'File 'VGet _ = 'False
    ValidPacket 'File 'VPut _ = 'False

    ValidPacket _ 'Get ('GetBytes _) = 'True
    ValidPacket _ 'Put ('SetByte _) = 'True
    ValidPacket _ 'VGet ('GetBytes _) = 'True
    ValidPacket _ 'VGet ('GetBytes2 _) = 'True
    ValidPacket _ 'VGet ('GetBytes3 _) = 'True
    ValidPacket _ 'VGet ('GetBytes4 _) = 'True
    ValidPacket _ 'VPut ('SetByte _) = 'True
    ValidPacket _ 'VPut ('SetByte2 _) = 'True
    ValidPacket _ 'VPut ('SetByte3 _) = 'True
    ValidPacket _ 'VPut ('SetByte4 _) = 'True

    ValidPacket _ 'Reset 'None = 'True
    ValidPacket _ 'MenuReset 'None = 'True
    ValidPacket _ 'Info 'None = 'True
    ValidPacket _ 'Stream 'None = 'True
    ValidPacket _ 'PowerCycle 'None = 'True

    ValidPacket _ _ _ = 'False

data Packet = Packet Opcode Context Flags Arguments deriving ( Show )

packet :: (ValidPacket c o a ~ 'True) => (Context' c) -> (Opcode' o) -> Flags -> (Arguments' a) -> Packet
packet (context -> c) (opcode -> o) flags (arguments -> a) = Packet o c flags a
