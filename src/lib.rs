extern crate byte;

use byte::ctx;
use byte::{check_len, BytesExt, TryRead};

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct B1(u8);
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct C1(u8);
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct U1(u8);
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct U2(u16);
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct U4(u32);
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct U8(u64);
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct I1(i8);
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct I2(i16);
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct I4(i32);
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct I8(i64);
#[derive(Debug, PartialEq, PartialOrd)]
pub struct R4(f32);
#[derive(Debug, PartialEq, PartialOrd)]
pub struct R8(f64);
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Cn<'a>(&'a str);
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Bn<'a>(&'a [u8]);
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Dn<'a>(u16, &'a [u8]);

macro_rules! single_byte_type {
    ($field_type:ident, $internal_type:ident) => {
        impl<'a> TryRead<'a> for $field_type {
            fn try_read(bytes: &'a [u8], _ctx: ()) -> byte::Result<(Self, usize)> {
                check_len(bytes, 1)?;
                Ok(($field_type(bytes[0] as $internal_type), 1))
            }
        }
    };
}

single_byte_type!(B1, u8);
single_byte_type!(C1, u8);
single_byte_type!(U1, u8);
single_byte_type!(I1, i8);

macro_rules! fixed_multi_byte_type {
    ($field_type:ident, $internal_type:ident, $byte_length:expr) => {
        impl<'a> TryRead<'a, ctx::Endian> for $field_type {
            fn try_read(bytes: &'a [u8], endian: ctx::Endian) -> byte::Result<(Self, usize)> {
                check_len(bytes, $byte_length)?;
                Ok((
                    $field_type(bytes.read_with::<$internal_type>(&mut 0, endian)?),
                    $byte_length,
                ))
            }
        }
    };
}

fixed_multi_byte_type!(U2, u16, 2);
fixed_multi_byte_type!(U4, u32, 4);
fixed_multi_byte_type!(U8, u64, 8);
fixed_multi_byte_type!(I2, i16, 2);
fixed_multi_byte_type!(I4, i32, 4);
fixed_multi_byte_type!(I8, i64, 8);
fixed_multi_byte_type!(R4, f32, 4);
fixed_multi_byte_type!(R8, f64, 8);

macro_rules! variable_length_type {
    ($field_type:ident, $internal_type:ty, $read_context:expr) => {
        impl<'a> TryRead<'a> for $field_type<'a> {
            fn try_read(bytes: &'a [u8], _ctx: ()) -> byte::Result<(Self, usize)> {
                let offset = &mut 0;
                let len = bytes.read_with::<U1>(offset, ())?;
                Ok((
                    $field_type(
                        bytes.read_with::<$internal_type>(offset, $read_context(len.0 as usize))?,
                    ),
                    *offset,
                ))
            }
        }
    };
}

variable_length_type!(Cn, &str, ctx::Str::Len);
variable_length_type!(Bn, &[u8], ctx::Bytes::Len);

impl<'a> TryRead<'a, ctx::Endian> for Dn<'a> {
    fn try_read(bytes: &'a [u8], endian: ctx::Endian) -> byte::Result<(Self, usize)> {
        let offset = &mut 0;
        let d_len = bytes.read_with::<U2>(offset, endian)?.0;
        let b_len = d_len / 8 + (if d_len % 8 > 0 { 1 } else { 0 });
        Ok((
            Dn(
                d_len,
                bytes.read_with::<&'a [u8]>(offset, ctx::Bytes::Len(b_len as usize))?,
            ),
            *offset,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use byte::{BytesExt, BE, LE};

    macro_rules! test_single_byte {
        ($name:ident, $field_type:ident, $internal_type:ident, $byte_value:expr, $expect_value:expr) => {
            #[test]
            fn $name() {
                let b: &[u8] = &[$byte_value];
                let offset = &mut 0;
                let v = b.read_with::<$field_type>(offset, ()).unwrap();
                assert_eq!(v, $field_type($expect_value));
            }
        };
    }

    test_single_byte!(test_u1_a5, U1, u8, 0xa5, 0xa5);
    test_single_byte!(test_u1_5a, U1, u8, 0x5a, 0x5a);
    test_single_byte!(test_b1_a5, B1, u8, 0xa5, 0xa5);
    test_single_byte!(test_b1_5a, B1, u8, 0x5a, 0x5a);
    test_single_byte!(test_c1_a5, C1, u8, 0xa5, 0xa5);
    test_single_byte!(test_c1_5a, C1, u8, 0x5a, 0x5a);
    test_single_byte!(test_i1_1, I1, i8, 0x01, 1);
    test_single_byte!(test_i1_n1, I1, i8, 0xff, -1);
    test_single_byte!(test_i1_127, I1, i8, 0x7f, 127);
    test_single_byte!(test_i1_n128, I1, i8, 0x80, -128);

    macro_rules! test_multi_byte {
        ($name:ident, $field_type:ident, $internal_type:ident, $bytes_value:expr, $expect_be:expr, $expect_le:expr) => {
            #[test]
            fn $name() {
                let b: &[u8] = $bytes_value;
                let offset = &mut 0;
                let v = b.read_with::<$field_type>(offset, BE).unwrap();
                assert_eq!(v, $field_type($expect_be));
                *offset = 0;
                let v = b.read_with::<$field_type>(offset, LE).unwrap();
                assert_eq!(v, $field_type($expect_le));
            }
        };
    }

    test_multi_byte!(test_u2, U2, u16, &[0xde, 0xad], 0xdead, 0xadde);
    test_multi_byte!(
        test_u4,
        U4,
        u32,
        &[0xde, 0xad, 0xbe, 0xef],
        0xdeadbeef,
        0xefbeadde
    );
    test_multi_byte!(
        test_u8,
        U8,
        u64,
        &[0xba, 0xbe, 0xfe, 0xed, 0xde, 0xad, 0xbe, 0xef],
        0xbabefeeddeadbeef,
        0xefbeaddeedfebeba
    );
    test_multi_byte!(test_i2, I2, i16, &[0xde, 0xad], -8531, -21026);
    test_multi_byte!(
        test_i4,
        I4,
        i32,
        &[0xde, 0xad, 0xbe, 0xef],
        -559038737,
        -272716322
    );
    test_multi_byte!(
        test_i8,
        I8,
        i64,
        &[0xba, 0xbe, 0xfe, 0xed, 0xde, 0xad, 0xbe, 0xef],
        -4990271039483298065,
        -1171307680082510150
    );
    test_multi_byte!(test_r4, R4, f32, &[0x3f, 0x80, 0x00, 0x00], 1.0, 4.6006e-41);
    test_multi_byte!(
        test_r8,
        R8,
        f64,
        &[0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        1.0,
        3.03865e-319
    );

    #[test]
    fn test_cn() {
        let b: &[u8] = &[0x05, 0x68, 0x65, 0x6c, 0x6c, 0x6f];
        let offset = &mut 0;
        let v = b.read_with::<Cn>(offset, ()).unwrap();
        assert_eq!(v, Cn("hello"));
    }

    #[test]
    fn test_bn() {
        let b: &[u8] = &[0x05, 0x68, 0x65, 0x6c, 0x6c, 0x6f];
        let offset = &mut 0;
        let v = b.read_with::<Bn>(offset, ()).unwrap();
        assert_eq!(v, Bn(&[0x68, 0x65, 0x6c, 0x6c, 0x6f]));
    }

    #[test]
    fn test_dn() {
        let b: &[u8] = &[0x00, 0x0d, 0x68, 0x65, 0xa5];
        let offset = &mut 0;
        let v = b.read_with::<Dn>(offset, BE).unwrap();
        assert_eq!(v, Dn(13, &[0x68, 0x65]));
    }
}
