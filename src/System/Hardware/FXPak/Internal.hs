{-# language DataKinds    #-}
{-# language GADTs        #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{- |
Module      :  System.Hardware.FXPak.Internal
Copyright   :  (c) Christina Wuest 2021
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Internals for FXPak - ensures only valid packets can be encoded.
-}

module System.Hardware.FXPak.Internal where

import Prelude

import qualified Data.ByteString.Char8 as BS

import Data.Bits ( (.|.), shiftL )

-- | Flags to be encoded as a 1-byte bit map
-- Note: No checking of flag validity is done in the original usb2snesw software
-- and as such research into which combinations produce expected results is
-- still underway
data Flag = SkipReset | OnlyReset | ClearX | SetX | StreamBurst | NoResponse | Data64Bytes deriving ( Eq, Show, Enum, Bounded )

-- | List of Flag data
-- Note: Since this will be reduced to a single bit map, flag duplication is not
-- considered invalid
type Flags = [Flag]

-- | Given a list of Flags, produce the bit map expected by the FXPak
fromFlags :: Flags -> Int
fromFlags = (foldl (.|.) 0) . (fmap $ (shiftL 1) . fromEnum)


-- | The context in which a packet's command operates
data Context = File | SNES | MSU | Config deriving ( Eq, Show, Enum, Bounded )
data Context' (c :: Context) where
    -- | File contexts involve manipulation of the FXPak's filesystem, including
    -- booting files directly
    File'   :: Context' 'File
    -- | SNES contexts involve the system's memory, allowing reading and writing
    -- of RAM
    SNES'   :: Context' 'SNES
    MSU'    :: Context' 'MSU
    Config' :: Context' 'Config

-- | View Pattern to extract a Context from its wrapping Context'
context :: Context' c -> Context
context File'   = File
context SNES'   = SNES
context MSU'    = MSU
context Config' = Config

-- | Represents the operation to be performed by the FXPak
data Opcode = Get | Put | VGet | VPut
            | List | Mkdir | Delete | Move
            | Reset | Boot | PowerCycle | Info | MenuReset | Stream | Time
            | Response
            deriving ( Eq, Show, Enum, Bounded )
data Opcode' (o :: Opcode) where
    -- | Get the contents of a file or target memory
    Get'        :: Opcode' 'Get
    -- | Write a file to the filesystem or a byte to memory
    Put'        :: Opcode' 'Put
    -- | Read and return between 1 and 4 regions of memory
    VGet'       :: Opcode' 'VGet
    -- | Write between 1 and 4 bytes to system memory
    VPut'       :: Opcode' 'VPut
    -- | List files at a given path
    List'       :: Opcode' 'List
    -- | Make a new directory at the given path
    Mkdir'      :: Opcode' 'Mkdir
    -- | Delete a node in the FXPak's filesystem
    Delete'     :: Opcode' 'Delete
    -- | Move a node in the FXPak's filesystem from one location to another
    Move'       :: Opcode' 'Move
    -- | Reset the SNES/SFC
    Reset'      :: Opcode' 'Reset
    -- | Boot a file at the target location
    Boot'       :: Opcode' 'Boot
    -- | Reset the SNES/SFC
    PowerCycle' :: Opcode' 'PowerCycle
    -- | Return information about the running FXPak
    Info'       :: Opcode' 'Info
    -- | Reset the SNES/SFC, returning to the FXPak's main menu
    MenuReset'  :: Opcode' 'MenuReset
    Stream'     :: Opcode' 'Stream
    Time'       :: Opcode' 'Time
    -- | Indicates a response packet
    -- A user should never set this for an outgoing packet.
    Response'   :: Opcode' 'Response

-- | View Pattern to extract a Opcode from its wrapping Opcode'
opcode :: Opcode' o -> Opcode
opcode Get'        = Get
opcode Put'        = Put
opcode VGet'       = VGet
opcode VPut'       = VPut
opcode List'       = List
opcode Mkdir'      = Mkdir
opcode Delete'     = Delete
opcode Move'       = Move
opcode Reset'      = Reset
opcode Boot'       = Boot
opcode PowerCycle' = PowerCycle
opcode Info'       = Info
opcode MenuReset'  = MenuReset
opcode Stream'     = Stream
opcode Time'       = Time
opcode Response'   = Response

-- | Represents an address to fetch memory from and a length to read
data AddressGet = AddressGet { start      :: Int
                             , dataLength :: Int
                             } deriving ( Show )

-- | Represents a value and the address to which it should be written
data AddressSet = AddressSet { target :: Int
                             , value  :: Int
                             } deriving ( Show )

-- | Arguments indicating the desired action taken by a given operation
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
    -- | No arguments - valid only with the Reset, MenuReset, Info, Stream, and
    -- PowerCycle opcodes
    None' :: Arguments' 'None
    -- | Path to a given object - valid for Get, List, Mkdir, Delete, and Boot
    -- in the File context
    Path' :: FilePath -> Arguments' ('Path (a :: FilePath))
    -- | Path with an accompanying ByteString - valid only for Put in the File
    -- context
    PathContents' :: FilePath -> BS.ByteString -> Arguments' ('PathContents (a :: FilePath) (b :: BS.ByteString))
    -- | Source and destination paths - valid only for Move in the File context
    PathRename' :: FilePath -> FilePath -> Arguments' ('PathRename (a :: FilePath) a)
    -- | An address and length of data to be read for non-File context Get and
    -- VGet opcodes
    GetBytes' :: AddressGet -> Arguments' ('GetBytes (a :: AddressGet))
    -- | Two address/length pairs to be read for non-File context VGet
    GetBytes2' :: AddressGet -> AddressGet -> Arguments' ('GetBytes2 (a :: (AddressGet, AddressGet)))
    -- | Three address/length pairs to be read for non-File context VGet
    GetBytes3' :: AddressGet -> AddressGet -> AddressGet -> Arguments' ('GetBytes3 (a :: (AddressGet, AddressGet, AddressGet)))
    -- | Four address/length pairs to be read for non-File context VGet
    GetBytes4' :: AddressGet -> AddressGet -> AddressGet -> AddressGet -> Arguments' ('GetBytes4 (a :: (AddressGet, AddressGet, AddressGet, AddressGet)))
    -- | A target address and byte to be written for non-File context Put and
    -- VPut opcodes
    SetByte' :: AddressSet -> Arguments' ('SetByte (a :: AddressSet))
    -- | Two address/data pairs to be written for non-File context VPut
    SetByte2' :: AddressSet -> AddressSet -> Arguments' ('SetByte2 (a :: (AddressSet, AddressSet)))
    -- | Three address/data pairs to be written for non-File context VPut
    SetByte3' :: AddressSet -> AddressSet -> AddressSet -> Arguments' ('SetByte3 (a :: (AddressSet, AddressSet, AddressSet)))
    -- | Four address/data pairs to be written for non-File context VPut
    SetByte4' :: AddressSet -> AddressSet -> AddressSet -> AddressSet -> Arguments' ('SetByte4 (a :: (AddressSet, AddressSet, AddressSet, AddressSet)))

-- | View Pattern to extract an Arguments datum from its wrapping Arguments'
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

-- | ValidPacket allows a constraint to be added to functions which would create
-- Packet data, guaranteeing that they are only able to generate packets
-- conforming to the expected specifications of the FXPak
type family ValidPacket (c :: Context) (o :: Opcode) (a :: Arguments) :: Bool where
    ValidPacket 'File 'Get ('Path _) = 'True
    ValidPacket 'File 'Put ('PathContents _ _) = 'True
    ValidPacket 'File 'List ('Path _) = 'True
    ValidPacket 'File 'Mkdir ('Path _) = 'True
    ValidPacket 'File 'Delete ('Path _) = 'True
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

-- | Represents a Packet to be sent to the FXPak
data Packet = Packet Opcode Context Flags Arguments deriving ( Show )

-- | Smart Constructor for a Packet, guaranteeing validity via the ValidPacket
-- constraint
packet :: (ValidPacket c o a ~ 'True) => (Context' c) -> (Opcode' o) -> Flags -> (Arguments' a) -> Packet
packet (context -> c) (opcode -> o) flags (arguments -> a) = Packet o c flags a
