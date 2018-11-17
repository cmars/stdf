extern crate byte;
use byte::ctx;
use byte::{BytesExt, TryRead, TryWrite};

use types::*;

#[derive(Debug, Eq, PartialEq)]
pub struct Header {
    pub rec_len: U2,
    pub rec_typ: U1,
    pub rec_sub: U1,
}

impl<'a> TryRead<'a, ctx::Endian> for Header {
    fn try_read(bytes: &'a [u8], endian: ctx::Endian) -> byte::Result<(Self, usize)> {
        let offset = &mut 0;
        Ok((
            Header {
                rec_len: bytes.read_with::<U2>(offset, endian)?,
                rec_typ: bytes.read_with::<U1>(offset, ())?,
                rec_sub: bytes.read_with::<U1>(offset, ())?,
            },
            *offset,
        ))
    }
}

impl<'a> TryWrite<ctx::Endian> for Header {
    fn try_write(self, bytes: &mut [u8], endian: ctx::Endian) -> byte::Result<usize> {
        let offset = &mut 0;
        bytes.write_with::<U2>(offset, self.rec_len, endian)?;
        bytes.write_with::<U1>(offset, self.rec_typ, ())?;
        bytes.write_with::<U1>(offset, self.rec_sub, ())?;
        Ok(*offset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use byte::{BytesExt, BE, LE};

    #[test]
    fn test_header() {
        let b: &[u8] = &[0x00, 0x01, 0xa5, 0x5a];
        let offset = &mut 0;
        let header = b.read_with::<Header>(offset, BE).unwrap();
        assert_eq!(
            header,
            Header {
                rec_len: U2::from(1),
                rec_typ: U1::from(0xa5),
                rec_sub: U1::from(0x5a)
            }
        );
        let mut out = vec![0; b.len()];
        out.write_with(&mut 0, header, BE).unwrap();
        assert_eq!(b, out.as_slice());

        *offset = 0;
        let header = b.read_with::<Header>(offset, LE).unwrap();
        assert_eq!(
            header,
            Header {
                rec_len: U2::from(256),
                rec_typ: U1::from(0xa5),
                rec_sub: U1::from(0x5a)
            }
        );
        let mut out = vec![0; b.len()];
        out.write_with(&mut 0, header, LE).unwrap();
        assert_eq!(b, out.as_slice());
    }
}
