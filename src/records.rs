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
                rec_typ: bytes.read_with::<U1>(offset, endian)?,
                rec_sub: bytes.read_with::<U1>(offset, endian)?,
            },
            *offset,
        ))
    }
}

impl<'a> TryWrite<ctx::Endian> for Header {
    fn try_write(self, bytes: &mut [u8], endian: ctx::Endian) -> byte::Result<usize> {
        let offset = &mut 0;
        bytes.write_with::<U2>(offset, self.rec_len, endian)?;
        bytes.write_with::<U1>(offset, self.rec_typ, endian)?;
        bytes.write_with::<U1>(offset, self.rec_sub, endian)?;
        Ok(*offset)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FAR {
    pub header: Header,
    pub cpu_type: U1,
    pub stdf_ver: U1,
}

impl FAR {
    pub fn detect_endian(bytes: &[u8]) -> byte::Result<ctx::Endian> {
        byte::check_len(bytes, 2)?;
        let far = bytes.read_with::<FAR>(&mut 0, byte::BE)?;
        if u8::from(far.header.rec_typ) != 0 || u8::from(far.header.rec_sub) != 10 {
            return Err(byte::Error::BadInput {
                err: "refusing to detect endian-ness with a non-FAR record",
            });
        }
        if far.header.rec_len == U2::from(2) {
            Ok(byte::BE)
        } else if far.header.rec_len == U2::from(512) {
            Ok(byte::LE)
        } else {
            Err(byte::Error::BadInput {
                err: "invalid or unrecognized FAR record header length",
            })
        }
    }
}

impl<'a> TryRead<'a, ctx::Endian> for FAR {
    fn try_read(bytes: &'a [u8], endian: ctx::Endian) -> byte::Result<(Self, usize)> {
        let offset = &mut 0;
        Ok((
            FAR {
                header: bytes.read_with::<Header>(offset, endian)?,
                cpu_type: bytes.read_with::<U1>(offset, endian)?,
                stdf_ver: bytes.read_with::<U1>(offset, endian)?,
            },
            *offset,
        ))
    }
}

impl<'a> TryWrite<ctx::Endian> for FAR {
    fn try_write(self, bytes: &mut [u8], endian: ctx::Endian) -> byte::Result<usize> {
        let offset = &mut 0;
        bytes.write_with::<Header>(offset, self.header, endian)?;
        bytes.write_with::<U1>(offset, self.cpu_type, endian)?;
        bytes.write_with::<U1>(offset, self.stdf_ver, endian)?;
        Ok(*offset)
    }
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct ATR<'a> {
    pub header: Header,
    pub mod_tim: U4,
    pub cmd_line: Cn<'a>,
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct MIR<'a> {
    pub header: Header,
    pub setup_t: U4,
    pub start_t: U4,
    pub stat_num: U1,
    pub mode_cod: C1,
    pub rtst_cod: C1,
    pub prot_cod: C1,
    pub burn_tim: U2,
    pub cmod_cod: C1,
    pub lot_id: Cn<'a>,
    pub part_typ: Cn<'a>,
    pub node_nam: Cn<'a>,
    pub tstr_typ: Cn<'a>,
    pub job_nam: Cn<'a>,
    pub job_rev: Cn<'a>,
    pub sblot_id: Cn<'a>,
    pub oper_nam: Cn<'a>,
    pub exec_typ: Cn<'a>,
    pub exec_ver: Cn<'a>,
    pub test_cod: Cn<'a>,
    pub tst_temp: Cn<'a>,
    pub user_txt: Cn<'a>,
    pub aux_file: Cn<'a>,
    pub pkg_typ: Cn<'a>,
    pub famly_id: Cn<'a>,
    pub date_cod: Cn<'a>,
    pub facil_id: Cn<'a>,
    pub floor_id: Cn<'a>,
    pub proc_id: Cn<'a>,
    pub oper_frq: Cn<'a>,
    pub spec_nam: Cn<'a>,
    pub spec_ver: Cn<'a>,
    pub flow_id: Cn<'a>,
    pub setup_id: Cn<'a>,
    pub dsgn_rev: Cn<'a>,
    pub eng_id: Cn<'a>,
    pub rom_cod: Cn<'a>,
    pub serl_num: Cn<'a>,
    pub supr_nam: Cn<'a>,
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

    #[test]
    fn test_far() {
        let b: &[u8] = &[0x02, 0x00, 0u8, 10u8, 2u8, 4u8];
        let offset = &mut 0;
        let endian = FAR::detect_endian(b).unwrap();
        assert_eq!(endian, LE);
        let far = b.read_with::<FAR>(offset, endian).unwrap();
        assert_eq!(
            far,
            FAR {
                header: Header {
                    rec_len: U2::from(2),
                    rec_typ: U1::from(0),
                    rec_sub: U1::from(10)
                },
                cpu_type: U1::from(2),
                stdf_ver: U1::from(4),
            }
        );
        let mut out = vec![0; b.len()];
        out.write_with(&mut 0, far, endian).unwrap();
        assert_eq!(b, out.as_slice());

        let b: &[u8] = &[0x00, 0x02, 0u8, 10u8, 1u8, 4u8];
        assert_eq!(FAR::detect_endian(b).unwrap(), BE);
    }
}
