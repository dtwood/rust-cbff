use std::collections::hash_map;
use std::iter::Map;
use std::ops::FnMut;
use std::vec;

pub trait Sequence {
    type Output;

    fn sequence(self) -> Self::Output;
}

impl<'a, KP, VP, K, V, E, F> Sequence for Map<hash_map::IntoIter<KP, VP>, F>
    where F: FnMut((KP, VP)) -> (K, Result<V, E>)
{
    type Output = Result<vec::IntoIter<(K, V)>, E>;

    fn sequence(self) -> Self::Output {
        let mut output = Vec::new();

        for (k, v) in self {
            output.push((k, try!(v)));
        }

        Ok(output.into_iter())
    }
}
