use std::env;
use std::fs::File;
use std::io::{Error, ErrorKind};

extern crate byte;
use byte::BytesExt;

extern crate stdf;
use stdf::records::{Header, V4};

extern crate memmap;
use memmap::MmapOptions;

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() <= 1 {
        println!("Usage: {} <stdf file>", args[0]);
        return;
    }
    match dump_stdf(&args[1]) {
        Ok(_) => {}
        Err(e) => println!("{:?}", e),
    };
}

fn dump_stdf(filename: &str) -> Result<(), Error> {
    let f = File::open(filename)?;
    let m = unsafe { MmapOptions::new().map(&f)? };
    let bytes = &m[..];
    let endian = Header::detect_endian(bytes)
        .map_err(|x| Error::new(ErrorKind::Other, format!("{:?}", x)))?;
    let offset = &mut 0;
    loop {
        match bytes.read_with::<V4>(offset, endian) {
            Ok(v4) => println!("{:?}", v4),
            Err(byte::Error::BadOffset(x)) => {
                if x == bytes.len() {
                    return Ok(());
                } else {
                    return Err(Error::new(
                        ErrorKind::Other,
                        format!("bad offset {} before EOF", x),
                    ));
                }
            }
            Err(e) => return Err(Error::new(ErrorKind::Other, format!("{:?}", e))),
        };
    }
}
