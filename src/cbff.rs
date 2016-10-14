use cbff_structs::*;
use encoding::all::UTF_16LE;
use encoding::{DecoderTrap, Encoding};
use safe_index::SafeIndex;
use std::cmp::min;
use std::collections::{BinaryHeap, HashMap};
use std::mem;
use std::ops::Range;
use std::slice;
use super::sequence::Sequence;
use super::error::CbffResult;
use super::error::CbffErrorEnum::*;

macro_rules! e {
        ($e: expr) => (::std::result::Result::Err(From::from($e)))
}

fn to_bytes<B>(input: &[B]) -> &[u8] {
    let input_len = input.len();
    let input_size = input_len * mem::size_of::<B>();
    let output_size = input_size;
    let output_len = output_size / mem::size_of::<u8>();

    assert_eq!(input_len.checked_mul(mem::size_of::<B>()),
               output_len.checked_mul(mem::size_of::<u8>()));

    let in_ptr: *const B = input.as_ptr() as *const B;
    let out_ptr: *const u8 = in_ptr as *const u8;

    unsafe { slice::from_raw_parts(out_ptr, output_len) }
}

pub struct Cbff<'a> {
    fat_size: u32,
    minifat_size: u32,
    data: &'a [u8],
    dif_chain: Vec<u32>,
    fat_chain: Vec<SectorType>,
    minifat_chain: Vec<SectorType>,
    minifat_start: u32,
    file_header: &'a FileHeader,
}

impl<'a> Cbff<'a> {
    pub fn from_slice(slice: &'a [u8]) -> CbffResult<Cbff<'a>> {
        let file_header = {
            let range = 0..mem::size_of::<FileHeader>();
            let header_slice = try!(slice.get_checked(range).ok_or(IndexOutOfRange));
            FileHeader::borrowed_from(header_slice)
        };

        let fat_size = 1 << file_header.sector_shift;
        let minifat_size = 1 << file_header.mini_sector_shift;

        let mut state = Cbff {
            fat_size: fat_size,
            minifat_size: minifat_size,
            data: slice,
            fat_chain: Vec::new(),
            minifat_chain: Vec::new(),
            dif_chain: Vec::new(),
            minifat_start: 0,
            file_header: file_header,
        };

        state.dif_chain = try!(state.get_dif_chain());
        state.fat_chain = try!(state.get_fat_chain());
        state.minifat_chain = try!(state.get_minifat_chain());
        state.minifat_start = try!(state.get_dir_entry(0)).sect_start;

        assert_eq!(state.fat_chain.len() as u32 % (state.fat_size / 4), 0);

        Ok(state)
    }

    fn get_range(&self, offset: u32, size: u32, index: u32) -> Range<usize> {
        let begin = (offset + 1) * self.fat_size + index * size;
        let end = begin + size;
        begin as usize..end as usize
    }

    fn get_chain_range(&self,
                       offset: u32,
                       step_size: u32,
                       size: u32,
                       start: u32)
                       -> CbffResult<Range<usize>> {
        let mut chain_address = start;
        let required_count = offset / (&self.fat_size / step_size);
        for _ in 0..required_count {
            match try!(self.fat_chain.get_checked(chain_address as usize).ok_or(IndexOutOfRange)) {
                &SectorType::Pointer(offset) => chain_address = offset,
                &other => return e!(InvalidFatEntry(other)),
            }
        }
        let begin = self.fat_size + chain_address * self.fat_size +
                    (offset % (&self.fat_size / step_size)) * step_size;
        let end = begin + size;
        Ok(begin as usize..end as usize)
    }

    fn get_minifat_chain_range(&self,
                               offset: u32,
                               step_size: u32,
                               size: u32,
                               start: u32)
                               -> CbffResult<Range<usize>> {
        assert_eq!(step_size, self.minifat_size);
        let mut minifat_chain_address = start;
        let required_minifat_count = offset / (&self.minifat_size / step_size);
        for _ in 0..required_minifat_count {
            match try!(self.minifat_chain
                .get_checked(minifat_chain_address as usize)
                .ok_or(IndexOutOfRange)) {
                &SectorType::Pointer(minifat_offset) => minifat_chain_address = minifat_offset,
                &other => return e!(InvalidMiniFatEntry(other)),
            }
        }

        self.get_chain_range(minifat_chain_address,
                             self.minifat_size,
                             size,
                             self.minifat_start)
    }

    fn get_dif_chain(&self) -> CbffResult<Vec<u32>> {
        let header = self.file_header.sect_fat_start.iter().map(SectorTypeBack::get_enum);

        let mut output = Vec::new();
        let mut have_seen_free = false;

        for i in header {
            match i {
                SectorType::Pointer(offset) if have_seen_free == false => output.push(offset),
                SectorType::Pointer(_) if have_seen_free == true => {
                    return e!(InvalidDifEntry(output, i))
                }
                SectorType::Free => have_seen_free = true,
                other => return e!(InvalidDifEntry(output, other)),
            }
        }

        let mut offset = self.file_header.sect_dif_start.get_enum();
        let mut count = 0;

        'outer: loop {
            match offset {
                SectorType::Pointer(p) => {
                    for idx in 0..(&self.fat_size / 4) {
                        let range = self.get_range(p, 4, idx);
                        let slice = try!(self.data.get_checked(range).ok_or(IndexOutOfRange));
                        let next_offset = SectorTypeBack::borrowed_from(slice);

                        if idx == self.fat_size / 4 {
                            return e!(DavidCantCount);
                        }

                        if idx != (&self.fat_size / 4 - 1) {
                            match next_offset.get_enum() {
                                SectorType::Pointer(p) => output.push(p),
                                SectorType::EndOfChain => break 'outer,
                                SectorType::Free => {
                                    // FIXME: what's going on?
                                }
                                other => return e!(InvalidDifEntry(output, other)),
                            }
                        } else {
                            count += 1;
                            if count > 10000 {
                                return e!(LoopInDifChain(output, offset));
                            }

                            offset = next_offset.get_enum();
                            continue;
                        }
                    }
                }
                SectorType::EndOfChain => {
                    break;
                }
                other => return e!(InvalidDifEntry(output, other)),
            }
        }

        Ok(output)
    }

    fn get_fat_chain(&self) -> CbffResult<Vec<SectorType>> {
        let mut output = Vec::new();

        for &i in &self.dif_chain {
            for idx in 0..(&self.fat_size / mem::size_of::<FatSector>() as u32) {
                let range = self.get_range(i, mem::size_of::<FatSector>() as u32, idx);
                let slice = try!(self.data.get_checked(range).ok_or(IndexOutOfRange));
                let header = FatSector::borrowed_from(slice);
                for &sector in header.fat_sectors.iter() {
                    output.push(sector.get_enum());
                }
            }
        }

        Ok(output)
    }

    fn get_minifat_chain(&self) -> CbffResult<Vec<SectorType>> {
        let mut output = Vec::new();

        for offset in 0..(self.file_header.mini_fat_count *
                          (&self.fat_size / mem::size_of::<MiniFatSector>() as u32)) {
            let range = try!(self.get_chain_range(offset,
                                                  mem::size_of::<MiniFatSector>() as u32,
                                                  mem::size_of::<MiniFatSector>() as u32,
                                                  self.file_header.mini_fat_start));
            let slice = try!(self.data.get_checked(range).ok_or(IndexOutOfRange));
            let item = MiniFatSector::borrowed_from(slice);
            output.push(item.sector.get_enum());
        }

        Ok(output)
    }

    pub fn get_file_map(&self) -> CbffResult<HashMap<String, Vec<u8>>> {
        let directories = try!(self.get_directory_map(0));

        try!(self.get_files(directories))
            .into_iter()
            .map(|(name, dir)| (name, self.get_data(dir)))
            .sequence()
            .map(|i| i.collect())
    }

    fn get_dir_entry(&self, offset: u32) -> CbffResult<&StructuredStorageDirectoryEntry> {
        let range = try!(self.get_chain_range(offset,
                             mem::size_of::<StructuredStorageDirectoryEntry>() as u32,
                             mem::size_of::<StructuredStorageDirectoryEntry>() as u32,
                             self.file_header.sect_dir_start));
        let slice = try!(self.data.get_checked(range).ok_or(IndexOutOfRange));

        Ok(StructuredStorageDirectoryEntry::borrowed_from(slice))
    }

    fn get_directory_map
        (&self,
         root_offset: u32)
         -> CbffResult<HashMap<u32, (&StructuredStorageDirectoryEntry, Option<u32>)>> {
        let mut output: HashMap<u32, (&StructuredStorageDirectoryEntry, Option<u32>)> =
            HashMap::new();

        let mut to_visit: BinaryHeap<(u32, Option<u32>)> = BinaryHeap::new();
        to_visit.push((root_offset, None));

        loop {
            match to_visit.pop() {
                None => break,
                Some((offset, parent)) => {
                    let header = try!(self.get_dir_entry(offset));

                    match header.mse.get_enum() {
                        ObjectType::Root => {
                            if output.insert(offset, (header, parent)) == None {
                                if let Sid::RegSid(_) = header.left_sibling.get_enum() {
                                    return e!(RootHasSiblings);
                                }
                                if let Sid::RegSid(_) = header.right_sibling.get_enum() {
                                    return e!(RootHasSiblings);
                                }
                                if let Sid::RegSid(new_offset) = header.child.get_enum() {
                                    to_visit.push((new_offset, Some(offset)));
                                }
                            }
                        }
                        ObjectType::Storage => {
                            if output.insert(offset, (header, parent)) == None {
                                if let Sid::RegSid(new_offset) = header.left_sibling.get_enum() {
                                    to_visit.push((new_offset, parent));
                                }
                                if let Sid::RegSid(new_offset) = header.right_sibling.get_enum() {
                                    to_visit.push((new_offset, parent));
                                }
                                if let Sid::RegSid(new_offset) = header.child.get_enum() {
                                    to_visit.push((new_offset, Some(offset)));
                                }
                            }
                        }
                        ObjectType::Stream => {
                            if output.insert(offset, (header, parent)) == None {
                                if let Sid::RegSid(new_offset) = header.left_sibling.get_enum() {
                                    to_visit.push((new_offset, parent));
                                }
                                if let Sid::RegSid(new_offset) = header.right_sibling.get_enum() {
                                    to_visit.push((new_offset, parent));
                                }
                                if let Sid::RegSid(_) = header.child.get_enum() {
                                    return e!(StreamHasChild);
                                }
                            }
                        }
                        ObjectType::Invalid => {
                            // FIXME: log
                        }
                        other => return e!(InvalidDirEntry(other)),
                    }
                }
            }
        }

        Ok(output)
    }

    fn get_files<'b>(&self,
                     directories: HashMap<u32, (&'b StructuredStorageDirectoryEntry, Option<u32>)>)
                     -> CbffResult<HashMap<String, &'b StructuredStorageDirectoryEntry>> {
        let mut output = HashMap::new();

        for (&offset, &(dir, _)) in &directories {
            if dir.mse.get_enum() != ObjectType::Stream {
                continue;
            }

            let mut name: String = "".to_owned();
            let mut loop_offset: u32 = offset;

            loop {
                let &(loop_dir, loop_parent) = try!(directories.get(&loop_offset)
                    .ok_or(InvalidDirectory));
                let loop_name = try!(UTF_16LE.decode(to_bytes(&loop_dir.ab), DecoderTrap::Strict)
                    .map_err(Utf16DecodeError));
                let loop_name: &str = loop_name.trim_right_matches('\u{0}');
                name = "/".to_owned() + loop_name + &name;

                if let Some(loop_offset_new) = loop_parent {
                    loop_offset = loop_offset_new;
                } else {
                    break;
                }
            }

            output.insert(name, dir);
        }

        Ok(output)
    }

    fn get_data(&self, dir: &StructuredStorageDirectoryEntry) -> CbffResult<Vec<u8>> {
        let mut output = Vec::with_capacity(dir.size as usize);
        let mut remaining_size = dir.size;
        let mut current_offset = 0;

        while remaining_size > 0 {
            let range;

            if dir.size >= self.file_header.mini_sector_cutoff {
                range = try!(self.get_chain_range(current_offset,
                                                  self.fat_size,
                                                  min(remaining_size, self.fat_size),
                                                  dir.sect_start));
                remaining_size -= min(remaining_size, self.fat_size);
            } else {
                range = try!(self.get_minifat_chain_range(current_offset,
                                                          self.minifat_size,
                                                          min(remaining_size, self.minifat_size),
                                                          dir.sect_start));
                remaining_size -= min(remaining_size, self.minifat_size);
            }

            let slice = try!(self.data.get_checked(range).ok_or(IndexOutOfRange));
            output.extend(slice.iter().cloned());
            current_offset += 1;
        }

        Ok(output)
    }
}
