use cbff_structs::*;
use encoding::{Encoding, DecoderTrap};
use encoding::all::UTF_16LE;
use std::cmp::min;
use std::collections::{BinaryHeap, HashMap};
use std::convert::AsRef;
use std::mem;
use std::ops::Range;
use std::slice;

macro_rules! p {
    ($e: expr) => (println!("{} = {:?}", stringify!($e), $e))
}

fn to_bytes<B>(input: &[B]) -> &[u8] {
    let input_len = input.len();
    let input_size = input_len * mem::size_of::<B>();
    let output_size = input_size;
    let output_len = output_size / mem::size_of::<u8>();

    assert_eq!(input_len.checked_mul(mem::size_of::<B>()), output_len.checked_mul(mem::size_of::<u8>()));

    let in_ptr: *const B = input.as_ptr() as *const B;
    let out_ptr: *const u8 = in_ptr as *const u8;

    unsafe { slice::from_raw_parts(out_ptr, output_len) }
}

pub struct State<'a> {
    fat_size: u32,
    minifat_size: u32,
    file: &'a [u8],
    dif_chain: Vec<u32>,
    fat_chain: Vec<SectorType>,
    minifat_chain: Vec<SectorType>,
    minifat_start: u32,
}

impl<'a> State<'a> {
    pub fn from_file(file: &'a [u8]) -> State<'a> {
        let temp_fat_chain = Vec::new();
        let temp_minifat_chain = Vec::new();
        let temp_dif_chain = Vec::new();

        let mut state = State {
            fat_size: 0,
            minifat_size: 0,
            file: file,
            fat_chain: temp_fat_chain,
            minifat_chain: temp_minifat_chain,
            dif_chain: temp_dif_chain,
            minifat_start: 0
        };

        state.fat_size = 1 << state.get_file_header().sector_shift;
        state.minifat_size = 1 << state.get_file_header().mini_sector_shift;

        state.dif_chain = state.get_dif_chain();
        state.fat_chain = state.get_fat_chain();
        state.minifat_chain = state.get_minifat_chain();
        state.minifat_start = state.get_dir_entry(0).sect_start;

        assert_eq!(state.fat_chain.len() as u32 % (state.fat_size / 4), 0);

        state
    }

    fn get_range(&self, offset: u32, size: u32, index: u32) -> Range<usize> {
        let begin = (offset + 1) * self.fat_size + index * size;
        let end = begin + size;
        begin as usize..end as usize
    }

    fn get_chain_range(&self, offset: u32, step_size: u32, size: u32, start: u32) -> Range<usize> {
        let mut chain_address = start;
        let required_count = offset / (&self.fat_size / step_size);
        for _ in 0..required_count {
            match self.fat_chain[chain_address as usize] {
                SectorType::Pointer(offset) => chain_address = offset,
                x => panic!("{:?}", x),
            }
        }
        let begin = self.fat_size + chain_address * self.fat_size + (offset % (&self.fat_size / step_size)) * step_size;
        let end = begin + size;
        begin as usize..end as usize
    }

    fn get_minifat_chain_range(&self, offset: u32, step_size: u32, size: u32, start: u32) -> Range<usize> {
        assert_eq!(step_size, self.minifat_size);
        let mut minifat_chain_address = start;
        let required_minifat_count = offset / (&self.minifat_size / step_size);
        for _ in 0..required_minifat_count {
            match self.minifat_chain[minifat_chain_address as usize] {
                SectorType::Pointer(minifat_offset) => minifat_chain_address = minifat_offset,
                x => panic!("{:?}", x),
            }
        }

        self.get_chain_range(minifat_chain_address, self.minifat_size, size, self.minifat_start)
    }

    fn get_file_header(&self) -> &FileHeader {
        let range = 0..mem::size_of::<FileHeader>();
        let slice = &self.file[range];
        FileHeader::borrowed_from(slice)
    }

    fn get_dif_chain(&self) -> Vec<u32> {
        let header = self.get_file_header().sect_fat_start.iter().map(SectorTypeBack::get_enum);

        let mut output = Vec::new();

        for i in header {
            match i {
                SectorType::Pointer(offset) => output.push(offset),
                SectorType::Free => { },
                x => panic!(x),
            }
        }

        let mut offset = self.get_file_header().sect_dif_start.get_enum();
        let mut count = 0;

        'outer: loop {
            match offset {
                SectorType::Pointer(p) => {
                    for idx in 0..(&self.fat_size / 4) {
                        let range = self.get_range(p, 4, idx);
                        let slice = &self.file[range];
                        let next_offset = SectorTypeBack::borrowed_from(slice);

                        if idx == self.fat_size / 4 {
                            panic!("David can't count!");
                        }

                        if idx != (&self.fat_size / 4 - 1) {
                            match next_offset.get_enum() {
                                SectorType::Pointer(p) => output.push(p),
                                SectorType::EndOfChain => break 'outer,
                                SectorType::Free => { /* FIXME: what's going on? */ },
                                x => panic!("{:?}", x),
                            }
                        } else {
                            count += 1;
                            if count > 10000 {
                                panic!("Infinite loop in DIF chain");
                            }

                            println!("updated");
                            offset = next_offset.get_enum();
                            continue;
                        }
                    }
                }
                SectorType::EndOfChain => { break; }
                x => panic!("{:?}", x),
            }
        }

        output
    }

    fn get_fat_chain(&self) -> Vec<SectorType> {
        let mut output = Vec::new();

        for &i in &self.dif_chain {
            for idx in 0..(&self.fat_size / mem::size_of::<FatSector>() as u32) {
                let range = self.get_range(i, mem::size_of::<FatSector>() as u32, idx);
                let slice = &self.file[range];
                let header = FatSector::borrowed_from(slice);
                for &sector in header.fat_sectors.iter() {
                    output.push(sector.get_enum());
                }
            }
        }

        output
    }

    fn get_minifat_chain(&self) -> Vec<SectorType> {
        let mut output = Vec::new();

        for offset in 0..(self.get_file_header().mini_fat_count *
                (&self.fat_size / mem::size_of::<MiniFatSector>() as u32)) {
            let range = self.get_chain_range(offset,
                mem::size_of::<MiniFatSector>() as u32,
                mem::size_of::<MiniFatSector>() as u32,
                self.get_file_header().mini_fat_start);
            let slice = &self.file[range];
            let item = MiniFatSector::borrowed_from(slice);
            output.push(item.sector.get_enum());
        }

        output
    }

    pub fn get_file_map(&self) -> HashMap<String, Vec<u8>> {
        let directories = self.get_directory_map(0);

        self.get_files(directories)
                .into_iter()
                .map(|(name, dir)| (name, self.get_data(dir)))
                .collect()
    }

    fn get_dir_entry(&self, offset: u32) -> &StructuredStorageDirectoryEntry {
            let range = self.get_chain_range(offset,
                mem::size_of::<StructuredStorageDirectoryEntry>() as u32,
                mem::size_of::<StructuredStorageDirectoryEntry>() as u32,
                self.get_file_header().sect_dir_start);
            let slice = &self.file[range];

            StructuredStorageDirectoryEntry::borrowed_from(slice)
    }

    fn get_directory_map(&self, root_offset: u32) -> HashMap<u32, (&StructuredStorageDirectoryEntry, Option<u32>)> {
        let mut output: HashMap<u32, (&StructuredStorageDirectoryEntry, Option<u32>)> = HashMap::new();

        let mut to_visit: BinaryHeap<(u32, Option<u32>)> = BinaryHeap::new();
        to_visit.push((root_offset, None));

        loop {
            match to_visit.pop() {
                None => break,
                Some((offset, parent)) => {
                    let header = self.get_dir_entry(offset);

                    match header.mse.get_enum() {
                        ObjectType::Root => {
                            if output.insert(offset, (header, parent)) == None {
                                if let Sid::RegSid(_) = header.left_sibling.get_enum() {
                                    panic!("Root cannot have siblings");
                                }
                                if let Sid::RegSid(_) = header.right_sibling.get_enum() {
                                    panic!("Root cannot have siblings");
                                }
                                if let Sid::RegSid(new_offset) = header.child.get_enum() {
                                    to_visit.push((new_offset, Some(offset)));
                                }
                            }
                        },
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
                                    panic!("Stream cannot have child");
                                }
                            }
                        },
                        ObjectType::Invalid => { /* FIXME: log */ },
                        x => panic!("{:?}", x),
                    }
                }
            }
        }

        output
    }

    fn get_files<'b>(&self, directories: HashMap<u32, (&'b StructuredStorageDirectoryEntry, Option<u32>)>) -> HashMap<String, &'b StructuredStorageDirectoryEntry> {
        let mut output = HashMap::new();

        for (&offset, &(dir, _)) in &directories {
            if dir.mse.get_enum() != ObjectType::Stream {
                continue;
            }

            let mut name: String = "".to_owned();
            let mut loop_offset: u32 = offset;

            loop {
                let (loop_dir, loop_parent) = directories[&loop_offset];
                let loop_name = UTF_16LE.decode(to_bytes(&loop_dir.ab), DecoderTrap::Strict);
                let loop_name: &str = loop_name.as_ref().map(|x| x.trim_right_matches('\u{0}')).unwrap();
                name = "/".to_owned() + loop_name + &name;

                if loop_parent == None {
                    break;
                }
                loop_offset = loop_parent.unwrap();
            }

            output.insert(name, dir);
        }
        output
    }

    fn get_data(&self, dir: &StructuredStorageDirectoryEntry) -> Vec<u8> {
        let mut output = Vec::with_capacity(dir.size as usize);
        let mut remaining_size = dir.size;
        let mut current_offset = 0;

        if dir.size >= self.get_file_header().mini_sector_cutoff {
            while remaining_size > 0 {
                let range = self.get_chain_range(current_offset, self.fat_size, min(remaining_size, self.fat_size), dir.sect_start);
                let slice = &self.file[range];
                output.extend(slice);
                remaining_size -= min(remaining_size, self.fat_size);
                current_offset += 1;
            }
        } else {
            while remaining_size > 0 {
                let range = self.get_minifat_chain_range(current_offset, self.minifat_size, min(remaining_size, self.minifat_size), dir.sect_start);
                let slice = &self.file[range];
                output.extend(slice);
                remaining_size -= min(remaining_size, self.minifat_size);
                current_offset += 1;
            }
        }

        output
    }
}
