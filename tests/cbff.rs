extern crate cbff;

use cbff::Cbff;
use std::collections::HashMap;

macro_rules! hash_map {
    ( ) => ( HashSet::new() );
    ( $( $k: expr => $v: expr ),* ) => ( {
        let mut output = HashMap::new();
        $(
            output.insert($k, $v);
        )*
        output
    } );
    ( $( $k: expr => $v: expr, )* ) => ( hash_map![ $( $k => $v ),* ] );
}

const DATA: &'static [u8; 3072] = include_bytes!("../assets/test/basic.dat");

#[test]
fn test_from_slice() {
    let result = Cbff::from_slice(DATA).and_then(|c| c.get_file_map());
    let expected = hash_map![
        "/Root Entry/Storage 1/Stream 1".to_owned()
        => b"Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1Data for stream 1"
            .to_vec()
    ];

    assert_eq!(result, Ok(expected));
}
