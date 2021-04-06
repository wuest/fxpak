{-# language DataKinds         #-}
{-# language GADTs             #-}
{-# language KindSignatures    #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns      #-}
{- |
Module      :  System.Hardware.FXPak
Copyright   :  (c) Christina Wuest 2021
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Interface to working with the FXPak/FXPak Pro flash cart devices for the
SNES/Super Famicom
-}

module System.Hardware.FXPak ( FXPak
                             , Packet, Opcode(..), Context(..), Arguments(..)
                             , FI.AddressGet(..), FI.AddressSet(..)
                             , FI.Flag(..), Flags
                             , open, packet, send
                             ) where

import Prelude

import qualified System.Hardware.Serialport as Serial
import qualified Data.ByteString.Char8 as BS

import Control.Monad.IO.Class ( liftIO )
import Data.Bits ( (.&.), shiftR )
import Data.Char ( chr, ord )

import qualified System.Hardware.FXPak.Internal as FI

-- | An FXPak device (which is exposed as a serial device)
type FXPak = Serial.SerialPort

-- | A packet representing a single complete to send to the FXPak
type Packet = FI.Packet

-- | Represents an address to fetch memory from and a length to read
type AddressGet = FI.AddressGet

-- | Represents a value and the address to which it should be written
type AddressSet = FI.AddressSet

-- | The context in which a packet's command operates
data Context c where
    -- | In the File mode, files can be managed directly, and booted
    File   :: Context (FI.Context' 'FI.File)
    -- | In the SNES context, bytes can be written to and read from memory, and
    -- all system commands (e.g. reset, powercycle, reset to menu, system info,
    -- and stream data) are passed through
    SNES   :: Context (FI.Context' 'FI.SNES)
    MSU    :: Context (FI.Context' 'FI.MSU)
    Config :: Context (FI.Context' 'FI.Config)

-- | The Opcode representing the command to perform
data Opcode o where
    -- | Depending on context, either return the contents of a given file, or
    -- read memory
    Get        :: Opcode (FI.Opcode' 'FI.Get)
    -- | Depending on context, either allow the upload of a file, or write bytes
    -- to memory
    Put        :: Opcode (FI.Opcode' 'FI.Put)
    -- | Retrieve from 1-4 regions of memory
    VGet       :: Opcode (FI.Opcode' 'FI.VGet)
    -- | Write from 1-4 bytes to memory
    VPut       :: Opcode (FI.Opcode' 'FI.VPut)
    -- | List files in a given directory
    List       :: Opcode (FI.Opcode' 'FI.List)
    -- | Make a given directory on the FXPak's filesystem
    Mkdir      :: Opcode (FI.Opcode' 'FI.Mkdir)
    -- | Delete a given path on the FXPak's filesystem
    Delete     :: Opcode (FI.Opcode' 'FI.Delete)
    -- | Move a given path on the FXPak's filesystem to a new location
    Move       :: Opcode (FI.Opcode' 'FI.Move)
    -- | Reset the SNES/SFC
    Reset      :: Opcode (FI.Opcode' 'FI.Reset)
    -- | Boot a given file
    Boot       :: Opcode (FI.Opcode' 'FI.Boot)
    -- | Reset the SNES/SFC
    PowerCycle :: Opcode (FI.Opcode' 'FI.PowerCycle)
    -- | Return information about the running FXPak
    Info       :: Opcode (FI.Opcode' 'FI.Info)
    -- | Reset the SNES/SFC, returning to the FXPak's main menu
    MenuReset  :: Opcode (FI.Opcode' 'FI.MenuReset)
    Stream     :: Opcode (FI.Opcode' 'FI.Stream)
    Time       :: Opcode (FI.Opcode' 'FI.Time)
    -- | Indicates a response packet
    Response   :: Opcode (FI.Opcode' 'FI.Response)

-- | Arguments indicating the desired action taken by a given operation
data Arguments a where
    -- | No arguments - valid only with the Reset, MenuReset, Info, Stream, and
    -- PowerCycle opcodes
    None         :: Arguments (FI.Arguments' 'FI.None)
    -- | Path to a given object - valid for Get, List, Mkdir, Delete, and Boot
    -- in the File context
    Path         :: FilePath -> Arguments (FI.Arguments' ('FI.Path (a :: FilePath)))
    -- | Path with an accompanying ByteString - valid only for Put in the File
    -- context
    PathContents :: FilePath -> BS.ByteString -> Arguments (FI.Arguments' ('FI.PathContents (a :: FilePath) (b :: BS.ByteString)))
    -- | Source and destination paths - valid only for Move in the File context
    PathRename   :: FilePath -> FilePath -> Arguments (FI.Arguments' ('FI.PathRename (a :: FilePath) a))
    -- | An address and length of data to be read for non-File context Get and
    -- VGet opcodes
    GetBytes     :: AddressGet -> Arguments (FI.Arguments' ('FI.GetBytes (a :: AddressGet)))
    -- | Two address/length pairs to be read for non-File context VGet
    GetBytes2    :: AddressGet -> AddressGet -> Arguments (FI.Arguments' ('FI.GetBytes2 (a :: (AddressGet, AddressGet))))
    -- | Three address/length pairs to be read for non-File context VGet
    GetBytes3    :: AddressGet -> AddressGet -> AddressGet -> Arguments (FI.Arguments' ('FI.GetBytes3 (a :: (AddressGet, AddressGet, AddressGet))))
    -- | Four address/length pairs to be read for non-File context VGet
    GetBytes4    :: AddressGet -> AddressGet -> AddressGet -> AddressGet -> Arguments (FI.Arguments' ('FI.GetBytes4 (a :: (AddressGet, AddressGet, AddressGet, AddressGet))))
    -- | A target address and byte to be written for non-File context Put and
    -- VPut opcodes
    SetByte      :: AddressSet -> Arguments (FI.Arguments' ('FI.SetByte (a :: AddressSet)))
    -- | Two address/data pairs to be written for non-File context VPut
    SetByte2     :: AddressSet -> AddressSet -> Arguments (FI.Arguments' ('FI.SetByte2 (a :: (AddressSet, AddressSet))))
    -- | Three address/data pairs to be written for non-File context VPut
    SetByte3     :: AddressSet -> AddressSet -> AddressSet -> Arguments (FI.Arguments' ('FI.SetByte3 (a :: (AddressSet, AddressSet, AddressSet))))
    -- | Four address/data pairs to be written for non-File context VPut
    SetByte4     :: AddressSet -> AddressSet -> AddressSet -> AddressSet -> Arguments (FI.Arguments' ('FI.SetByte4 (a :: (AddressSet, AddressSet, AddressSet, AddressSet))))

-- | List of Flags to be encoded for a given packet
type Flags = FI.Flags

-- | Given a Context (wrapping the internal type-safe Context' construct),
-- produce the internal construct for the purposes of packet construction
context :: Context (FI.Context' c) -> FI.Context' c
context File   = FI.File'
context SNES   = FI.SNES'
context MSU    = FI.MSU'
context Config = FI.Config'

-- | Given an Opcode (wrapping the internal type-safe Opcode' construct),
-- produce the internal construct for the purposes of packet construction
opcode :: Opcode (FI.Opcode' o) -> FI.Opcode' o
opcode Get        = FI.Get'
opcode Put        = FI.Put'
opcode VGet       = FI.VGet'
opcode VPut       = FI.VPut'
opcode List       = FI.List'
opcode Mkdir      = FI.Mkdir'
opcode Delete     = FI.Delete'
opcode Move       = FI.Move'
opcode Reset      = FI.Reset'
opcode Boot       = FI.Boot'
opcode PowerCycle = FI.PowerCycle'
opcode Info       = FI.Info'
opcode MenuReset  = FI.MenuReset'
opcode Stream     = FI.Stream'
opcode Time       = FI.Time'
opcode Response   = FI.Response'

-- | Given an Arguments datum (wrapping the internal type-safe Arguments'
-- construct), -- produce the internal construct for the purposes of packet
-- construction
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

-- | Open a given serial device as an FXPak
open :: FilePath -> IO FXPak
open = flip Serial.openSerial Serial.defaultSerialSettings

-- | Encode a packet to send to an FXPak, preventing encoding of invalid packets
packet :: (FI.ValidPacket c o a ~ 'True) => Context (FI.Context' c) -> Opcode (FI.Opcode' o) -> Flags -> Arguments (FI.Arguments' a) -> Packet
packet (context -> c) (opcode -> o) flags (arguments -> a) = FI.packet c o flags a

-- | Sends a given packet to an FXPak device, returning a ByteString if a
-- response is expected
send :: FXPak -> Packet -> IO (Maybe BS.ByteString)
send dev dat = do
    _ <- Serial.send dev $ pack dat
    let (FI.Packet _ _ flags _) = dat
    if elem FI.NoResponse flags
        then return Nothing
        else do
            resp <- liftIO $ readSerial 512 dev []
            let resp' = BS.unpack resp in
                if (take 5 resp') /= ['U', 'S', 'B', 'A', '\x0F']
                    then return Nothing
                    else fetch dev $ fmap ord $ drop 255 $ resp'

-- | Read a number of bytes from the FXPak designated by a string fro
-- a string read from the serial device
fetch :: FXPak -> [Int] -> IO (Maybe BS.ByteString)
fetch _ []         = return Nothing
fetch _ (0:_)      = return Nothing
fetch dev (size:_) = Just <$> readSerial size dev []

-- | Convert a Packet to a ByteString ready to be sent to the FXPak device,
-- enforcing the appropriate packet size
pack :: Packet -> BS.ByteString
pack (FI.Packet o c f a) =
    let op = chr $ fromEnum o
        ctx = chr $ fromEnum c
        flags = chr $ FI.fromFlags f
    -- "USBA" string is taken from the original usb2snesw application
    in pack' (elem FI.Data64Bytes f) o a ['U', 'S', 'B', 'A', op, ctx, flags]

-- | Select the appropriate size pack function given the opcode and flags given
pack' :: Bool -> FI.Opcode -> FI.Arguments -> [Char] -> BS.ByteString
pack' True  _       = pack64
pack' False FI.VPut = pack64
pack' False FI.VGet = pack64
pack' False _       = pack512

-- | Given a set of GetBytes/SetByte arguments and packet header, generate a 64
-- byte packet
-- Note: 64 byte packets can only be generated for the VGet/VPut opcodes, and
-- therefore any non-GetBytes/SetByte operations are considered undefined in
-- this context
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

-- | Given a set of arguments and packet header, generate a 512 byte packet
-- Note: VGet/VPut-specific arguments (GetBytes2+/SetByte2+) are considered
-- undefined in this context, as VGet/VPut must use 64-byte packet
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

-- | Generate an 8 byte string from an AddressGet
fromAddressGet :: AddressGet -> String
fromAddressGet ag =
    let validSize = chr $ (0xFF .&.) $ FI.dataLength ag
        addr = FI.start ag
        addrhi = chr $ 0xFF .&. (shiftR addr 16)
        addrmid = chr $ 0xFF .&. (shiftR addr 8)
        addrlow = chr $ 0xFF .&. addr
    in (nulls 3) ++ [validSize, chr 0x00, addrhi, addrmid, addrlow]

-- | Generate an 8 byte string from an AddressSet
fromAddressSet :: AddressSet -> String
fromAddressSet as =
    let addr = FI.target as
        byte = chr $ (0xFF .&.) $ FI.value as
        addrhi = chr $ 0xFF .&. (shiftR 16 addr)
        addrmid = chr $ 0xFF .&. (shiftR 8 addr)
        addrlow = chr $ 0xFF .&. addr
    in (nulls 3) ++ [byte, chr 0x00, addrhi, addrmid, addrlow]

-- | Convenience function to generate arbitrary length string of NUL bytes
nulls :: Int -> String
nulls x = take x $ fmap chr $ repeat 0x00

-- | Read a given number of bytes from the FXPak
readSerial :: Int -> FXPak -> [BS.ByteString] -> IO BS.ByteString
readSerial size dev bufs =
    let currSize = sum $ fmap BS.length bufs
    in
        if currSize >= size
           then return $ foldl BS.append "" $ reverse bufs
           else do
               dat <- Serial.recv dev $ size - currSize
               readSerial size dev $ dat:bufs
