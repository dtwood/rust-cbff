use std::ops::{Range, RangeFrom, RangeTo};

pub trait SafeIndex<Idx: ?Sized> {
    type Output: ?Sized;

    fn get_checked(&self, index: Idx) -> Option<&Self::Output>;
}

impl<T> SafeIndex<usize> for [T] {
    type Output = T;

    fn get_checked(&self, index: usize) -> Option<&Self::Output> {
        if index >= self.len() {
            None
        } else {
            Some(&self[index])
        }
    }
}

impl<T> SafeIndex<RangeTo<usize>> for [T] {
    type Output = [T];

    fn get_checked(&self, index: RangeTo<usize>) -> Option<&Self::Output> {
        if index.end >= self.len() {
            None
        } else {
            Some(&self[index])
        }
    }
}

impl<T> SafeIndex<RangeFrom<usize>> for [T] {
    type Output = [T];

    fn get_checked(&self, index: RangeFrom<usize>) -> Option<&Self::Output> {
        if index.start >= self.len() {
            None
        } else {
            Some(&self[index])
        }
    }
}

impl<T> SafeIndex<Range<usize>> for [T] {
    type Output = [T];

    fn get_checked(&self, index: Range<usize>) -> Option<&Self::Output> {
        if index.end < index.start {
            None
        } else if index.end >= self.len() {
            None
        } else {
            Some(&self[index])
        }
    }
}

impl<T> SafeIndex<usize> for Vec<T> {
    type Output = T;

    fn get_checked(&self, index: usize) -> Option<&Self::Output> {
        self.get(index)
    }
}

impl<T> SafeIndex<RangeTo<usize>> for Vec<T> {
    type Output = [T];

    fn get_checked(&self, index: RangeTo<usize>) -> Option<&Self::Output> {
        if index.end >= self.len() {
            None
        } else {
            Some(&self[index])
        }
    }
}

impl<T> SafeIndex<RangeFrom<usize>> for Vec<T> {
    type Output = [T];

    fn get_checked(&self, index: RangeFrom<usize>) -> Option<&Self::Output> {
        if index.start >= self.len() {
            None
        } else {
            Some(&self[index])
        }
    }
}

impl<T> SafeIndex<Range<usize>> for Vec<T> {
    type Output = [T];

    fn get_checked(&self, index: Range<usize>) -> Option<&Self::Output> {
        if index.end < index.start {
            None
        } else if index.end >= self.len() {
            None
        } else {
            Some(&self[index])
        }
    }
}
