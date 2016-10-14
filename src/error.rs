use super::cbff_structs::{ObjectType, SectorType};
use std::borrow::Cow;

pub type CbffResult<T> = Result<T, CbffError>;

#[derive(Debug, PartialEq)]
pub struct CbffError {
    error: CbffErrorEnum,
}

#[derive(Debug, PartialEq)]
pub enum CbffErrorEnum {
    IndexOutOfRange,
    DavidCantCount,
    InvalidDirectory,
    Utf16DecodeError(Cow<'static, str>),
    RootHasSiblings,
    StreamHasChild,
    InvalidDifEntry(Vec<u32>, SectorType),
    InvalidFatEntry(SectorType),
    InvalidMiniFatEntry(SectorType),
    InvalidDirEntry(ObjectType),
    LoopInDifChain(Vec<u32>, SectorType),
}

impl From<CbffErrorEnum> for CbffError {
    fn from(t: CbffErrorEnum) -> Self {
        CbffError { error: t }
    }
}
