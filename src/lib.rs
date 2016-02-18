// Copyright (c) 2015-2016, David Wood

#![feature(type_ascription)]

extern crate encoding;

mod cbff;
mod cbff_structs;
mod error;
mod safe_index;
mod sequence;

pub use cbff::Cbff;
pub use error::CbffError;
pub use error::CbffResult;
