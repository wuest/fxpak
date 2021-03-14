{-# language DataKinds    #-}
{-# language GADTs        #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module FXPak ( FXPak
             , Opcode(..), Context(..), Arguments(..), FI.Flag(..), Flags
             , open, packet, send
             ) where

import Prelude

import qualified System.Hardware.Serialport as Serial
import qualified Data.ByteString.Char8 as BS

import Data.Bits ( (.&.), shiftR )
import Data.Char ( chr )

import qualified FXPak.Internal as FI

type FXPak = Serial.SerialPort
type Packet = FI.Packet

type Flags = FI.Flags
type AddressGet = (Int, Int)
type AddressSet = (Int, Int)

data Context c where
    File   :: Context (FI.Context' 'FI.File)
    SNES   :: Context (FI.Context' 'FI.SNES)
    MSU    :: Context (FI.Context' 'FI.MSU)
    Config :: Context (FI.Context' 'FI.Config)

data Opcode o where
    Get        :: Opcode (FI.Opcode' 'FI.Get)
    Put        :: Opcode (FI.Opcode' 'FI.Put)
    VGet       :: Opcode (FI.Opcode' 'FI.VGet)
    VPut       :: Opcode (FI.Opcode' 'FI.VPut)
    List       :: Opcode (FI.Opcode' 'FI.List)
    Mkdir      :: Opcode (FI.Opcode' 'FI.Mkdir)
    Remove     :: Opcode (FI.Opcode' 'FI.Remove)
    Move       :: Opcode (FI.Opcode' 'FI.Move)
    Reset      :: Opcode (FI.Opcode' 'FI.Reset)
    Boot       :: Opcode (FI.Opcode' 'FI.Boot)
    PowerCycle :: Opcode (FI.Opcode' 'FI.PowerCycle)
    Info       :: Opcode (FI.Opcode' 'FI.Info)
    MenuReset  :: Opcode (FI.Opcode' 'FI.MenuReset)
    Stream     :: Opcode (FI.Opcode' 'FI.Stream)
    Time       :: Opcode (FI.Opcode' 'FI.Time)
    Response   :: Opcode (FI.Opcode' 'FI.Response)

data Arguments a where
    None         :: Arguments (FI.Arguments' 'FI.None)
    Path         :: FilePath -> Arguments (FI.Arguments' ('FI.Path (a :: FilePath)))
    PathContents :: FilePath -> BS.ByteString -> Arguments (FI.Arguments' ('FI.PathContents (a :: FilePath) (b :: BS.ByteString)))
    PathRename   :: FilePath -> FilePath -> Arguments (FI.Arguments' ('FI.PathRename (a :: FilePath) a))
    GetBytes     :: AddressGet -> Arguments (FI.Arguments' ('FI.GetBytes (a :: AddressGet)))
    GetBytes2    :: AddressGet -> AddressGet -> Arguments (FI.Arguments' ('FI.GetBytes2 (a :: (AddressGet, AddressGet))))
    GetBytes3    :: AddressGet -> AddressGet -> AddressGet -> Arguments (FI.Arguments' ('FI.GetBytes3 (a :: (AddressGet, AddressGet, AddressGet))))
    GetBytes4    :: AddressGet -> AddressGet -> AddressGet -> AddressGet -> Arguments (FI.Arguments' ('FI.GetBytes4 (a :: (AddressGet, AddressGet, AddressGet, AddressGet))))
    SetByte      :: AddressSet -> Arguments (FI.Arguments' ('FI.SetByte (a :: AddressSet)))
    SetByte2     :: AddressSet -> AddressSet -> Arguments (FI.Arguments' ('FI.SetByte2 (a :: (AddressSet, AddressSet))))
    SetByte3     :: AddressSet -> AddressSet -> AddressSet -> Arguments (FI.Arguments' ('FI.SetByte3 (a :: (AddressSet, AddressSet, AddressSet))))
    SetByte4     :: AddressSet -> AddressSet -> AddressSet -> AddressSet -> Arguments (FI.Arguments' ('FI.SetByte4 (a :: (AddressSet, AddressSet, AddressSet, AddressSet))))

open :: FilePath -> IO FXPak
open = flip Serial.openSerial Serial.defaultSerialSettings

send :: FXPak -> Packet -> IO ()
send dev dat = Serial.send dev (pack dat) >> return ()

context :: Context (FI.Context' c) -> FI.Context' c
context File   = FI.File'
context SNES   = FI.SNES'
context MSU    = FI.MSU'
context Config = FI.Config'

opcode :: Opcode (FI.Opcode' o) -> FI.Opcode' o
opcode Get        = FI.Get'
opcode Put        = FI.Put'
opcode VGet       = FI.VGet'
opcode VPut       = FI.VPut'
opcode List       = FI.List'
opcode Mkdir      = FI.Mkdir'
opcode Remove     = FI.Remove'
opcode Move       = FI.Move'
opcode Reset      = FI.Reset'
opcode Boot       = FI.Boot'
opcode PowerCycle = FI.PowerCycle'
opcode Info       = FI.Info'
opcode MenuReset  = FI.MenuReset'
opcode Stream     = FI.Stream'
opcode Time       = FI.Time'
opcode Response   = FI.Response'

arguments :: Arguments (FI.Arguments' a) -> FI.Arguments' a
arguments None                = FI.None'
arguments (Path a)            = FI.Path' a
arguments (PathContents a b)  = FI.PathContents' a b
arguments (PathRename a b)    = FI.PathRename' a b
arguments (GetBytes a)        = FI.GetBytes' a
arguments (GetBytes2 a b)     = FI.GetBytes2' a b
arguments (GetBytes3 a b c)   = FI.GetBytes3' a b c
arguments (GetBytes4 a b c d) = FI.GetBytes4' a b c d
arguments (SetByte a)         = FI.SetByte' a
arguments (SetByte2 a b)      = FI.SetByte2' a b
arguments (SetByte3 a b c)    = FI.SetByte3' a b c
arguments (SetByte4 a b c d)  = FI.SetByte4' a b c d

packet :: (FI.ValidPacket c o a ~ 'True) => Context (FI.Context' c) -> Opcode (FI.Opcode' o) -> Flags -> Arguments (FI.Arguments' a) -> Packet
packet (context -> c) (opcode -> o) flags (arguments -> a) = FI.packet c o flags a

pack :: Packet -> BS.ByteString
pack (FI.Packet o c f a) =
    let op = chr $ fromEnum o
        ctx = chr $ fromEnum c
        flags = chr $ FI.fromFlags f
    in pack' (elem FI.Data64Bytes f) o a ['U', 'S', 'B', 'A', op, ctx, flags]

pack' :: Bool -> FI.Opcode -> FI.Arguments -> [Char] -> BS.ByteString
pack' True  _       = pack64
pack' False FI.VPut = pack64
pack' False FI.VGet = pack64
pack' False _       = pack512

pack64 :: FI.Arguments -> String -> BS.ByteString
pack64 (FI.GetBytes addrGet) tmp = BS.pack $ tmp ++ (nulls 25) ++ (fromAddressGet addrGet) ++ (nulls 24)
pack64 (FI.GetBytes2 (addrGet, addrGet2)) tmp = BS.pack $ tmp ++ (nulls 25) ++ (fromAddressGet addrGet) ++ (fromAddressGet addrGet2) ++ (nulls 16)
pack64 (FI.GetBytes3 (addrGet, addrGet2, addrGet3)) tmp = BS.pack $ tmp ++ (nulls 25) ++ (fromAddressGet addrGet) ++ (fromAddressGet addrGet2) ++ (fromAddressGet addrGet3) ++ (nulls 8)
pack64 (FI.GetBytes4 (addrGet, addrGet2, addrGet3, addrGet4)) tmp = BS.pack $ tmp ++ (nulls 25) ++ (fromAddressGet addrGet) ++ (fromAddressGet addrGet2) ++ (fromAddressGet addrGet3) ++ (fromAddressGet addrGet4)
pack64 (FI.SetByte addrSet) tmp = BS.pack $ tmp ++ (nulls 25) ++ (fromAddressSet addrSet) ++ (nulls 24)
pack64 (FI.SetByte2 (addrSet, addrSet2)) tmp = BS.pack $ tmp ++ (nulls 25) ++ (fromAddressSet addrSet) ++ (fromAddressSet addrSet2) ++ (nulls 16)
pack64 (FI.SetByte3 (addrSet, addrSet2, addrSet3)) tmp = BS.pack $ tmp ++ (nulls 25) ++ (fromAddressSet addrSet) ++ (fromAddressSet addrSet2) ++ (fromAddressSet addrSet3) ++ (nulls 8)
pack64 (FI.SetByte4 (addrSet, addrSet2, addrSet3, addrSet4)) tmp = BS.pack $ tmp ++ (nulls 25) ++ (fromAddressSet addrSet) ++ (fromAddressSet addrSet2) ++ (fromAddressSet addrSet3) ++ (fromAddressSet addrSet4)
pack64 _ _ = undefined

pack512 :: FI.Arguments -> String -> BS.ByteString
pack512 FI.None tmp = BS.pack $ tmp ++ (nulls 505)
pack512 (FI.Path p) tmp =
    let p' = take 255 p
    in BS.pack $ tmp ++ (nulls 249) ++ p' ++ (nulls (256 - (length p')))
pack512 (FI.PathRename s d) tmp =
    let s' = take 255 s
        d' = take 248 d
    in BS.pack $ tmp ++ d' ++ (nulls (249 - (length d'))) ++ s' ++ (nulls (256 - (length s')))
pack512 (FI.GetBytes addrGet) tmp = BS.pack $ tmp ++ (nulls 245) ++ (fromAddressGet addrGet) ++ (nulls 252)
pack512 (FI.SetByte addrSet) tmp = BS.pack $ tmp ++ (nulls 245) ++ (fromAddressSet addrSet) ++ (nulls 252)
pack512 _ _ = undefined

fromAddressGet :: AddressGet -> String
fromAddressGet (addr, size) =
    let validSize = chr $ 0xFF .&. size
        addrhi = chr $ 0xFF .&. (shiftR addr 16)
        addrmid = chr $ 0xFF .&. (shiftR addr 8)
        addrlow = chr $ 0xFF .&. addr
    in (nulls 3) ++ [validSize, chr 0x00, addrhi, addrmid, addrlow]

fromAddressSet :: AddressSet -> String
fromAddressSet (addr, byte) =
    let byte' = chr $ 0xFF .&. byte
        addrhi = chr $ 0xFF .&. (shiftR 16 addr)
        addrmid = chr $ 0xFF .&. (shiftR 8 addr)
        addrlow = chr $ 0xFF .&. addr
    in (nulls 3) ++ [byte', chr 0x00, addrhi, addrmid, addrlow]

nulls :: Int -> String
nulls x = take x $ fmap chr $ repeat 0x00
