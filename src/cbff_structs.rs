// Copyright (c) 2015, David Wood

//! A Compound File is made up of a number of virtual streams. These are collections of data that
//! behave as a linear stream, although their on-disk format may be fragmented. Virtual streams can
//! be user data, or they can be control structures used to maintain the file. Note that the file
//! itself can also be considered a virtual stream.
//!
//! All allocations of space within a Compound File are done in units called sectors. The size of a
//! sector is definable at creation time of a Compound File, but for the purposes of this document
//! will be 512 bytes. A virtual stream is made up of a sequence of sectors.
//!
//! The Compound File uses several different types of sector: Fat, Directory, Minifat, DIF, and
//! Storage. A separate type of 'sector' is a Header, the primary difference being that a Header is
//! always 512 bytes long (regardless of the sector size of the rest of the file) and is always
//! located at offset zero (0). With the exception of the header, sectors of any type can be placed
//! anywhere within the file. The function of the various sector types is discussed below.
//!
//! In the discussion below, the term SECT is used to describe the location of a sector within a
//! virtual stream (in most cases this virtual stream is the file itself). Internally, a SECT is
//! represented as a ULONG.

use std::fmt;
use std::mem;

macro_rules! match_enum {
    ( $val:expr, $cls:ident, $specials:ident, $output:ident, $other:path, [$( $x:ident ),*] ) => ( match $val { $( x if x == $specials::$x as $cls => $output::$x, )* other => $other(other), } );
}

macro_rules! reverse_match_enum {
    ( $val:expr, $cls:ident, $input:ident, $specials:ident, $other:path, [$( $x:ident ),*] ) => ( match $val {
        $( $input::$x => $specials::$x as $cls, )* $other(x) => x, } );
}

#[derive(Eq, PartialEq)]
pub struct Guid {
    contents: [u8; 16],
}

impl fmt::Debug for Guid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{{"));
        try!(write!(f, "{:02x}", self.contents[0]));
        try!(write!(f, "{:02x}", self.contents[1]));
        try!(write!(f, "{:02x}", self.contents[2]));
        try!(write!(f, "{:02x}", self.contents[3]));
        try!(write!(f, "-"));
        try!(write!(f, "{:02x}", self.contents[4]));
        try!(write!(f, "{:02x}", self.contents[5]));
        try!(write!(f, "-"));
        try!(write!(f, "{:02x}", self.contents[6]));
        try!(write!(f, "{:02x}", self.contents[7]));
        try!(write!(f, "-"));
        try!(write!(f, "{:02x}", self.contents[8]));
        try!(write!(f, "{:02x}", self.contents[9]));
        try!(write!(f, "-"));
        try!(write!(f, "{:02x}", self.contents[10]));
        try!(write!(f, "{:02x}", self.contents[11]));
        try!(write!(f, "{:02x}", self.contents[12]));
        try!(write!(f, "{:02x}", self.contents[13]));
        try!(write!(f, "{:02x}", self.contents[14]));
        try!(write!(f, "{:02x}", self.contents[15]));
        try!(write!(f, "}}"));
        Ok(())
    }
}

/// The Header contains vital information for the instantiation of a Compound File. Its total
/// length is 512 bytes. There is exactly one Header in any Compound File, and it is always located
/// beginning at offset zero in the file.
#[repr(packed)]
pub struct FileHeader {
    /// {0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1} for current version,
    /// was {0x0e, 0x11, 0xfc, 0x0d, 0xd0, 0xcf, 0x11, 0xe0} on old, beta 2
    /// files (late ’92) which are also supported by the reference
    /// implementation
    pub sig: [u8; 8],
    /// class id (set with WriteClassStg, retrieved with
    /// GetClassFile/ReadClassStg)
    pub id: Guid,
    /// minor version of the format: 33 is written by reference implementation
    pub minor_version: u16,
    /// major version of the dll/format: 3 is written by reference
    /// implementation
    pub dll_version: u16,
    /// 0xFFFE: indicates Intel byte-ordering
    pub byte_order: u16,
    /// size of sectors in power-of-two (typically 9, indicating 512-byte
    /// sectors)
    pub sector_shift: u16,
    /// size of mini-sectors in power-of-two (typically 6, indicating 64-byte
    /// mini-sectors)
    pub mini_sector_shift: u16,
    /// reserved, must be zero
    pub res: [u8; 6],
    /// FIXME
    pub sect_dir_count: u32,
    /// number of SECTs in the FAT chain
    pub sect_fat_count: u32,
    /// first SECT in the Directory chain
    pub sect_dir_start: u32,
    /// signature used for transactioning: must be zero. The reference
    /// implementation does not support transactioning
    pub signature: u32,
    /// maximum size for mini-streams: typically 4096 bytes
    pub mini_sector_cutoff: u32,
    /// first SECT in the mini-FAT chain
    pub mini_fat_start: u32,
    /// number of SECTs in the mini-FAT chain
    pub mini_fat_count: u32,
    /// first SECT in the DIF chain
    pub sect_dif_start: SectorTypeBack,
    /// number of SECTs in the DIF chain
    pub sect_dif_count: u32,
    /// the SECTs of the first 109 FAT sectors
    pub sect_fat_start: [SectorTypeBack; 109],
}

fn array_to_slice<T>(array: &[T]) -> &[T] {
    array
}

macro_rules! check_eq {
    ( $slf:ident, $other:ident, $field:ident ) => (
        if $slf.$field != $other.$field {
            return false;
        }
    );
}

macro_rules! check_eq_array {
    ( $slf:ident, $other:ident, $field:ident, $len:expr ) => (
        for i in 0..$len {
            if $slf.$field[i] != $other.$field[i] {
                return false;
            }
        }
    );
}

macro_rules! write_item {
    ( $f:ident, $slf:ident, $field:ident ) => (
        try!(writeln!($f, "{}: {:?}", stringify!($field), $slf.$field));
    );
}

macro_rules! write_item_array {
    ( $f:ident, $slf:ident, $field:ident ) => (
        try!(writeln!($f, "{}: {:?}", stringify!($field), array_to_slice(&$slf.$field)));
    );
}

impl Eq for FileHeader { }

impl PartialEq for FileHeader {
    fn eq(&self, other: &Self) -> bool {
        check_eq!(self, other, sig);
        check_eq!(self, other, id);
        check_eq!(self, other, minor_version);
        check_eq!(self, other, dll_version);
        check_eq!(self, other, byte_order);
        check_eq!(self, other, sector_shift);
        check_eq!(self, other, mini_sector_shift);
        check_eq_array!(self, other, res, 6);
        check_eq!(self, other, sect_dir_count);
        check_eq!(self, other, sect_fat_count);
        check_eq!(self, other, sect_dir_start);
        check_eq!(self, other, signature);
        check_eq!(self, other, mini_sector_cutoff);
        check_eq!(self, other, mini_fat_start);
        check_eq!(self, other, mini_fat_count);
        check_eq!(self, other, sect_dif_start);
        check_eq!(self, other, sect_dif_count);
        check_eq_array!(self, other, sect_fat_start, 109);

        true
    }
}

impl fmt::Debug for FileHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_item!(f, self, sig);
        write_item!(f, self, id);
        write_item!(f, self, minor_version);
        write_item!(f, self, dll_version);
        write_item!(f, self, byte_order);
        write_item!(f, self, sector_shift);
        write_item!(f, self, mini_sector_shift);
        write_item_array!(f, self, res);
        write_item!(f, self, sect_dir_count);
        write_item!(f, self, sect_fat_count);
        write_item!(f, self, sect_dir_start);
        write_item!(f, self, signature);
        write_item!(f, self, mini_sector_cutoff);
        write_item!(f, self, mini_fat_start);
        write_item!(f, self, mini_fat_count);
        write_item!(f, self, sect_dif_start);
        write_item!(f, self, sect_dif_count);
        write_item_array!(f, self, sect_fat_start);

        Ok(())
    }
}

#[repr(u32)]
#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum SectorType {
    MaxRegSector,
    Reserved,
    DifSector,
    FatSector,
    EndOfChain,
    Free,
    Pointer(u32),
}

enum SectorTypeSpecials {
    MaxRegSector = 0xfffffffa,
    Reserved = 0xfffffffb,
    DifSector = 0xfffffffc,
    FatSector = 0xfffffffd,
    EndOfChain = 0xfffffffe,
    Free = 0xffffffff,
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct SectorTypeBack {
    backing_val: u32,
}

impl SectorTypeBack {
    pub fn get_enum(&self) -> SectorType {
        match_enum!(self.backing_val,
                    u32,
                    SectorTypeSpecials,
                    SectorType,
                    SectorType::Pointer,
                    [MaxRegSector, Reserved, DifSector, FatSector, EndOfChain, Free])
    }
}

impl fmt::Debug for SectorTypeBack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.get_enum())
    }
}

impl From<SectorType> for SectorTypeBack {
    fn from(val: SectorType) -> SectorTypeBack {
        SectorTypeBack {
            backing_val: reverse_match_enum!(val,
                                             u32,
                                             SectorType,
                                             SectorTypeSpecials,
                                             SectorType::Pointer,
                                             [MaxRegSector,
                                              Reserved,
                                              DifSector,
                                              FatSector,
                                              EndOfChain,
                                              Free]),
        }
    }
}


/// The Fat is the main allocator for space within a Compound File. Every sector in the file is
/// represented within the Fat in some fashion, including those sectors that are unallocated
/// (free). The Fat is a virtual stream made up of one or more Fat Sectors.
///
/// Fat sectors are arrays of SECTs that represent the allocation of space within the file. Each
/// stream is represented in the Fat by a chain, in much the same fashion as a DOS
/// file-allocation-table (FAT). To elaborate, the set of Fat Sectors can be considered together to
/// be a single array -- each cell in that array contains the SECT of the next sector in the chain,
/// and this SECT can be used as an index into the Fat array to continue along the chain.  Special
/// values are reserved for chain terminators (ENDOFCHAIN = 0xFFFFFFFE), free sectors (FREESECT =
/// 0xFFFFFFFF), and sectors that contain storage for Fat Sectors (FATSECT = 0xFFFFFFFD) or DIF
/// Sectors (DIFSECT = 0xFFFFFFC), which are not chained in the same way as the others.
///
/// The locations of Fat Sectors are read from the DIF (Double-indirect Fat), which is described
/// below. The Fat is represented in itself, but not by a chain – a special reserved SECT value
/// (FATSECT = 0xFFFFFFFD) is used to mark sectors allocated to the Fat.
///
/// A SECT can be converted into a byte offset into the file by using the following formula: SECT
/// << ssheader._uSectorShift + sizeof(ssheader). This implies that sector 0 of the file begins at
/// byte offset 512, not at 0.
pub struct FatSector {
    pub fat_sectors: [SectorTypeBack; 128],
}

impl Eq for FatSector { }

impl PartialEq for FatSector {
    fn eq(&self, other: &Self) -> bool {
        check_eq_array!(self, other, fat_sectors, 128);

        true
    }
}

impl fmt::Debug for FatSector {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_item_array!(f, self, fat_sectors);

        Ok(())
    }
}

/// Since space for streams is always allocated in sector-sized blocks, there can be considerable
/// waste when storing objects much smaller than sectors (typically 512 bytes). As a solution to
/// this problem, we introduced the concept of the MiniFat. The MiniFat is structurally equivalent
/// to the Fat, but is used in a different way.  The virtual sector size for objects represented in
/// the Minifat is 1 << ssheader._uMiniSectorShift (typically 64 bytes) instead of 1 <<
/// ssheader._uSectorShift (typically 512 bytes). The storage for these objects comes from a
/// virtual stream within the Multistream (called the Ministream).
///
/// The locations for MiniFat sectors are stored in a standard chain in the Fat, with the beginning
/// of the chain stored in the header.
///
/// A Minifat sector number can be converted into a byte offset into the ministream by using the
/// following formula: SECT << ssheader._uMiniSectorShift. (This formula is different from the
/// formula used to convert a SECT into a byte offset in the file, since no header is stored in the
/// Ministream)
///
/// The Ministream is chained within the Fat in exactly the same fashion as any normal stream. It
/// is referenced by the first Directory Entry (SID 0).
#[derive(Eq, PartialEq, Debug)]
pub struct MiniFatSector {
    pub sector: SectorTypeBack,
}

#[repr(u32)]
#[derive(Eq, PartialEq, Debug)]
pub enum ObjectType {
    Invalid,
    Storage,
    Stream,
    Lockbytes,
    Property,
    Root,
    Other(u8),
}

enum ObjectTypeSpecials {
    Invalid = 0,
    Storage = 1,
    Stream = 2,
    Lockbytes = 3,
    Property = 4,
    Root = 5,
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct ObjectTypeBack {
    backing_val: u8,
}

impl ObjectTypeBack {
    pub fn get_enum(&self) -> ObjectType {
        match_enum!(self.backing_val,
                    u8,
                    ObjectTypeSpecials,
                    ObjectType,
                    ObjectType::Other,
                    [Invalid, Storage, Stream, Lockbytes, Property, Root])
    }
}

impl fmt::Debug for ObjectTypeBack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.get_enum())
    }
}

impl From<ObjectType> for ObjectTypeBack {
    fn from(val: ObjectType) -> ObjectTypeBack {
        ObjectTypeBack {
            backing_val: reverse_match_enum!(val,
                                             u8,
                                             ObjectType,
                                             ObjectTypeSpecials,
                                             ObjectType::Other,
                                             [Invalid, Storage, Stream, Lockbytes, Property, Root]),
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum Color {
    Red,
    Black,
    Other(u8),
}

enum ColorSpecials {
    Red = 0,
    Black = 1,
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct ColorBack {
    backing_val: u8,
}

impl ColorBack {
    pub fn get_enum(&self) -> Color {
        match_enum!(self.backing_val,
                    u8,
                    ColorSpecials,
                    Color,
                    Color::Other,
                    [Red, Black])
    }
}

impl fmt::Debug for ColorBack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.get_enum())
    }
}

impl From<Color> for ColorBack {
    fn from(val: Color) -> ColorBack {
        ColorBack {
            backing_val: reverse_match_enum!(val,
                                             u8,
                                             Color,
                                             ColorSpecials,
                                             Color::Other,
                                             [Red, Black]),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Time {
    value: [u8; 8],
}

#[repr(u32)]
#[derive(Eq, PartialEq, Debug)]
pub enum Sid {
    RegSid(u32),
    MaxRegSid,
    E1,
    E2,
    E3,
    E4,
    NoStream,
}

enum SidSpecials {
    MaxRegSid = 0xfffffffa,
    E1 = 0xfffffffb,
    E2 = 0xfffffffc,
    E3 = 0xfffffffd,
    E4 = 0xfffffffe,
    NoStream = 0xffffffff,
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct SidBack {
    backing_val: u32,
}

impl SidBack {
    pub fn get_enum(&self) -> Sid {
        match_enum!(self.backing_val,
                    u32,
                    SidSpecials,
                    Sid,
                    Sid::RegSid,
                    [MaxRegSid, E1, E2, E3, E4, NoStream])
    }
}

impl fmt::Debug for SidBack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.get_enum())
    }
}

impl From<Sid> for SidBack {
    fn from(val: Sid) -> SidBack {
        SidBack {
            backing_val: reverse_match_enum!(val,
                                             u32,
                                             Sid,
                                             SidSpecials,
                                             Sid::RegSid,
                                             [MaxRegSid, E1, E2, E3, E4, NoStream]),
        }
    }
}


/// The Directory is a structure used to contain per-stream information about the streams in a
/// Compound File, as well as to maintain a tree-styled containment structure. It is a virtual
/// stream made up of one or more Directory Sectors. The Directory is represented as a standard
/// chain of sectors within the Fat. The first sector of the Directory chain (the Root Directory
/// Entry)
///
/// Each level of the containment hierarchy (i.e. each set of siblings) is represented as a
/// red-black tree. The parent of this set of sibilings will have a pointer to the top of this
/// tree. This red-black tree must maintain the following conditions in order for it to be valid:
///
///   1. The root node must always be black. Since the root directory (see below) does not have
///      siblings, it's color is irrelevant and may therefore be either red or black.
///
///   2. No two consecutive nodes may both be red.
///
///   3. The left child must always be less than the right child. This relationship is defined as:
///
///   a. A node with a shorter name is less than a node with a longer name (i.e. compare length of
///      the name)
///
///   b. For nodes with the same length names, compare the two names.
///
/// The simplest implementation of the above invariants would be to mark every node as black, in
/// which case the tree is simply a binary tree.
///
/// A Directory Sector is an array of Directory Entries, a structure represented in the diagram
/// below. Each user stream within a Compound File is represented by a single Directory Entry. The
/// Directory is considered as a large array of Directory Entries. It is useful to note that the
/// Directory Entry for a stream remains at the same index in the Directory array for the life of
/// the stream – thus, this index (called an SID) can be used to readily identify a given stream.
///
/// The directory entry is then padded out with zeros to make a total size of 128 bytes.
///
/// Directory entries are grouped into blocks of four to form Directory Sectors.
///
/// # Root Directory Entry
/// The first sector of the Directory chain (also referred to as the first element of the Directory
/// array, or SID 0) is known as the Root Directory Entry and is reserved for two purposes: First,
/// it provides a root parent for all objects stationed at the root of the multi-stream. Second,
/// its function is overloaded to store the size and starting sector for the Mini-stream.
///
/// The Root Directory Entry behaves as both a stream and a storage. All of the fields in the
/// Directory Entry are valid for the root. The Root Directory Entry’s Name field typically
/// contains the string “RootEntry” in Unicode, although some versions of structured storage
/// (particularly the preliminary reference implementation and the Macintosh version) store only
/// the first letter of this string, “R” in the name. This string is always ignored, since the Root
/// Directory Entry is known by its position at SID 0 rather than by its name, and its name is not
/// otherwise used. New implementations should write “RootEntry” properly in the Root Directory
/// Entry for consistency and support manipulating files created with only the “R” name.
///
/// # Other Directory Entries
/// Non-root directory entries are marked as either stream or storage elements.  Storage elements
/// have a clsid, time, and child values; stream elements may not. Stream elements have valid
/// sect_start and size members, whereas these fields are set to zero for storage elements (except
/// as noted above for the Root Directory Entry).
///
/// To determine the physical file location of actual stream data from a stream directory entry, it
/// is necessary to determine which FAT (normal or mini) the stream exists within. Streams whose
/// size member is less than the mini_sector_cutoff value for the file exist in the ministream, and
/// so the start_sect is used as an index into the MiniFat (which starts at sect_mini_fat_start) to
/// track the chain of mini-sectors through the mini-stream (which is, as noted earlier, the
/// standard (non-mini) stream referred to by the Root Directory Entry’s sect_start value). Streams
/// whose size member is greater than the mini_sector_cutoff value for the file exist as standard
/// streams – their sect_start value is used as an index into the standard FAT which describes the
/// chain of full sectors containing their data).
///
/// # Storage Sectors
/// Storage sectors are simply collections of arbitrary bytes. They are the building blocks of user
/// streams, and no restrictions are imposed on their contents. Storage sectors are represented as
/// chains in the Fat, and each storage chain (stream) will have a single Directory Entry
/// associated with it.
#[derive(Debug, Eq, PartialEq)]
#[repr(packed)]
pub struct StructuredStorageDirectoryEntry {
    /// 64 bytes. The Element name in UTF16/UCS2 (FIXME: which?), padded with zeros to fill this byte array
    pub ab: [u16; 32],
    /// Length of the Element name in characters, not bytes
    pub cb: u16,
    /// Type of object: value taken from the ObjectType enumeration
    pub mse: ObjectTypeBack,
    /// Value taken from Color enumeration.
    pub bflags: ColorBack,
    /// SID of the left-sibling of this entry in the directory tree
    pub left_sibling: SidBack,
    /// SID of the right-sibling of this entry in the directory tree
    pub right_sibling: SidBack,
    /// SID of the child acting as the root of all the children of this element (if mse=ObjectType::Storage)
    pub child: SidBack,
    /// CLSID of this storage (if mse=ObjectType::Storage)
    pub id: Guid,
    /// User flags of this storage (if mse=ObjectType::Storage)
    pub user_flags: u32,
    /// Create time-stamp (if mse=ObjectType::Storage)
    pub create_time: Time,
    /// Create time-stamp (if mse=ObjectType::Storage)
    pub modify_time: Time,
    /// starting SECT of the stream (if mse=ObjectType::Stream)
    pub sect_start: u32,
    /// size of stream in bytes (if mse=ObjectType::Stream)
    pub size: u32,
    /// Reserved for future use. Must be zero.
    pub padding: [u8; 4],
}

pub trait BorrowedFrom<'a, T> {
    fn borrowed_from(val: T) -> &'a Self;
}

impl<'a> BorrowedFrom<'a, &'a [u8]> for FileHeader {
    fn borrowed_from(input: &'a [u8]) -> &'a FileHeader {
        let input_ptr: *const [u8] = input as *const [u8];
        let output_ptr: *const FileHeader = input_ptr as *const FileHeader;
        let output = unsafe { &*output_ptr };
        assert_eq!(mem::size_of_val(input), mem::size_of_val(output));

        output
    }
}

impl<'a> BorrowedFrom<'a, &'a [u8]> for StructuredStorageDirectoryEntry {
    fn borrowed_from(input: &'a [u8]) -> &'a StructuredStorageDirectoryEntry {
        let input_ptr: *const [u8] = input as *const [u8];
        let output_ptr: *const StructuredStorageDirectoryEntry =
            input_ptr as *const StructuredStorageDirectoryEntry;
        let output = unsafe { &*output_ptr };
        assert_eq!(mem::size_of_val(input), mem::size_of_val(output));

        output
    }
}

impl<'a> BorrowedFrom<'a, &'a [u8]> for FatSector {
    fn borrowed_from(input: &'a [u8]) -> &'a FatSector {
        let input_ptr: *const [u8] = input as *const [u8];
        let output_ptr: *const FatSector = input_ptr as *const FatSector;
        let output = unsafe { &*output_ptr };
        assert_eq!(mem::size_of_val(input), mem::size_of_val(output));

        output
    }
}

impl<'a> BorrowedFrom<'a, &'a [u8]> for MiniFatSector {
    fn borrowed_from(input: &'a [u8]) -> &'a MiniFatSector {
        let input_ptr: *const [u8] = input as *const [u8];
        let output_ptr: *const MiniFatSector = input_ptr as *const MiniFatSector;
        let output = unsafe { &*output_ptr };
        assert_eq!(mem::size_of_val(input), mem::size_of_val(output));

        output
    }
}

impl<'a> BorrowedFrom<'a, &'a [u8]> for SectorTypeBack {
    fn borrowed_from(input: &'a [u8]) -> &'a SectorTypeBack {
        let input_ptr: *const [u8] = input as *const [u8];
        let output_ptr: *const SectorTypeBack = input_ptr as *const SectorTypeBack;
        let output = unsafe { &*output_ptr };
        assert_eq!(mem::size_of_val(input), mem::size_of_val(output));

        output
    }
}

#[cfg(test)]
mod tests {
    use std::mem;
    use super::Sid;
    use super::BorrowedFrom;
    use super::FatSector;
    use super::Guid;
    use super::MiniFatSector;
    use super::SectorType;
    use super::ObjectType;
    use super::StructuredStorageDirectoryEntry;
    use super::FileHeader;
    use super::Color;
    use super::Time;

    const DATA: &'static [u8; 3072] = include_bytes!("../assets/test/basic.dat");

    macro_rules! expand_sectors {
        ( [ $( $x:expr ),* ] ) => {
            [ $( $x.into(), )* ]
        };
    }

    #[test]
    fn structured_storage_header_size() {
        assert_eq!(mem::size_of::<FileHeader>(), 512);
    }

    #[test]
    fn structured_storage_directory_entry_size() {
        assert_eq!(mem::size_of::<StructuredStorageDirectoryEntry>(), 128);
    }

    #[test]
    fn unpack_structured_storage_header() {
        let result = FileHeader::borrowed_from(&DATA[0..0x200]);

        let expected = &FileHeader {
            sig: [0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1],
            id: Guid {
                contents: [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                           0x00, 0x00, 0x00, 0x00],
            },
            minor_version: 0x003E,
            dll_version: 0x0003,
            byte_order: 0xFFFE,
            sector_shift: 0x0009,
            mini_sector_shift: 0x0006,
            res: [0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
            sect_dir_count: 0, // FIXME
            sect_fat_count: 0x00000001,
            sect_dir_start: 0x00000001,
            signature: 0x00000000,
            mini_sector_cutoff: 0x00001000,
            mini_fat_start: 0x00000002,
            mini_fat_count: 0x00000001,
            sect_dif_start: SectorType::Pointer(0xFFFFFFFE).into(),
            sect_dif_count: 0x00000000,
            sect_fat_start: expand_sectors!([SectorType::Pointer(0x00000000),
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free,
                                             SectorType::Free]),
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn unpack_fat_sector() {
        let result = FatSector::borrowed_from(&DATA[0x200..0x400]);

        let expected = &FatSector {
            fat_sectors: expand_sectors!([SectorType::FatSector,
                                          SectorType::EndOfChain,
                                          SectorType::EndOfChain,
                                          SectorType::Pointer(0x00000004),
                                          SectorType::EndOfChain,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free,
                                          SectorType::Free]),
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn unpack_root_directory_sector() {
        let result = StructuredStorageDirectoryEntry::borrowed_from(&DATA[0x400..0x480]);

        let expected = &StructuredStorageDirectoryEntry {
            ab: [82, 111, 111, 116, 32, 69, 110, 116 /* "Root Entry" */, 114, 121, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            cb: 0x16,
            mse: ObjectType::Root.into(),
            bflags: Color::Black.into(),
            left_sibling: Sid::NoStream.into(),
            right_sibling: Sid::NoStream.into(),
            child: Sid::RegSid(0x1).into(),
            id: Guid {
                contents: [0x00, 0x67, 0x61, 0x56, 0x54, 0xC1, 0xCE, 0x11, 0x85, 0x53, 0x00, 0xAA,
                           0x00, 0xA1, 0xF9, 0x5B],
            },
            user_flags: 0x00000000,
            create_time: Time { value: [0; 8] },
            modify_time: Time { value: [0x80, 0x1E, 0x92, 0x13, 0x4B, 0xB4, 0xBA, 0x01] }, // FIXME
            sect_start: 0x00000003,
            size: 0x00000240,
            padding: [0; 4],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn unpack_storage1_directory_sector() {
        let result = StructuredStorageDirectoryEntry::borrowed_from(&DATA[0x480..0x500]);

        let expected = &StructuredStorageDirectoryEntry {
            ab: [0x0053, 0x0074, 0x006F, 0x0072, 0x0061, 0x0067, 0x0065, 0x0020, 0x0031, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            cb: 20,
            mse: ObjectType::Storage.into(),
            bflags: Color::Black.into(),
            left_sibling: Sid::NoStream.into(),
            right_sibling: Sid::NoStream.into(),
            child: Sid::RegSid(0x2).into(),
            id: Guid {
                contents: [0x00, 0x61, 0x61, 0x56, 0x54, 0xC1, 0xCE, 0x11, 0x85, 0x53, 0x00, 0xAA,
                           0x00, 0xA1, 0xF9, 0x5B],
            },
            user_flags: 0x00000000,
            create_time: Time { value: [0, 136, 249, 18, 75, 180, 186, 1] }, // FIXME
            modify_time: Time { value: [128, 30, 146, 19, 75, 180, 186, 1] }, // FIXME
            sect_start: 0x00000000,
            size: 0x00000000,
            padding: [0; 4],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn unpack_stream1_directory_sector() {
        let result = StructuredStorageDirectoryEntry::borrowed_from(&DATA[0x500..0x580]);

        let expected = &StructuredStorageDirectoryEntry {
            ab: [0x53, 0x74, 0x72, 0x65, 0x61, 0x6D, 0x20, 0x31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            cb: 18,
            mse: ObjectType::Stream.into(),
            bflags: Color::Black.into(),
            left_sibling: Sid::NoStream.into(),
            right_sibling: Sid::NoStream.into(),
            child: Sid::NoStream.into(),
            id: Guid { contents: [0x00; 16] },
            user_flags: 0x00000000,
            create_time: Time { value: [0; 8] },
            modify_time: Time { value: [0; 8] },
            sect_start: 0x00000000,
            size: 0x00000220,
            padding: [0; 4],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn unpack_unused_directory_sector() {
        let result = StructuredStorageDirectoryEntry::borrowed_from(&DATA[0x580..0x600]);

        let expected = &StructuredStorageDirectoryEntry {
            ab: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0],
            cb: 0,
            mse: ObjectType::Invalid.into(),
            bflags: Color::Red.into(),
            left_sibling: Sid::NoStream.into(),
            right_sibling: Sid::NoStream.into(),
            child: Sid::NoStream.into(),
            id: Guid { contents: [0; 16] },
            user_flags: 0x00000000,
            create_time: Time { value: [0; 8] },
            modify_time: Time { value: [0; 8] },
            sect_start: 0x00000000,
            size: 0x00000000,
            padding: [0; 4],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn unpack_mini_fat_sector() {
        let dummy = &MiniFatSector { sector: SectorType::Pointer(0xDEADBEEF).into() };
        let mut result = [dummy; (0x200 / 4)];

        for i in 0..(0x200 / 4) {
            result[i] = MiniFatSector::borrowed_from(&DATA[(0x600 + i * 4)..(0x600 + i * 4 + 4)]);
        }
        // let result = MiniFatSector::borrowed_from(&DATA[0x600..0x800]);

        let result = result.iter().map(|x| x.sector.get_enum());
        let result: Vec<_> = result.collect();

        let expected = vec![
                SectorType::Pointer(0x00000001),
                SectorType::Pointer(0x00000002),
                SectorType::Pointer(0x00000003),
                SectorType::Pointer(0x00000004),
                SectorType::Pointer(0x00000005),
                SectorType::Pointer(0x00000006),
                SectorType::Pointer(0x00000007),
                SectorType::Pointer(0x00000008),
                SectorType::EndOfChain,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free, SectorType::Free,
                SectorType::Free,
            ];

        assert_eq!(result, expected);
    }
}
