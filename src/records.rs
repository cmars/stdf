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

#[derive(Debug, Eq, PartialEq, STDFRecord)]
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

/*
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

impl TryWrite<ctx::Endian> for FAR {
    fn try_write(self, bytes: &mut [u8], endian: ctx::Endian) -> byte::Result<usize> {
        let offset = &mut 0;
        bytes.write_with::<Header>(offset, self.header, endian)?;
        bytes.write_with::<U1>(offset, self.cpu_type, endian)?;
        bytes.write_with::<U1>(offset, self.stdf_ver, endian)?;
        Ok(*offset)
    }
}
*/

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

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct MRR<'a> {
    pub header: Header,
    pub finish_t: U4,
    pub disp_cod: C1,
    pub usr_desc: Cn<'a>,
    pub exc_desc: Cn<'a>,
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct PCR {
    pub header: Header,
    pub head_num: U1,
    pub site_num: U1,
    pub part_cnt: U1,
    pub rtst_cnt: U1,
    pub abrt_cnt: U1,
    pub good_cnt: U1,
    pub func_cnt: U1,
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct HBR<'a> {
    pub header: Header,
    pub head_num: U1,
    pub site_num: U1,
    pub hbin_num: U2,
    pub hbin_cnt: U4,
    pub hbin_pf: C1,
    pub hbin_nam: Cn<'a>,
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct SBR<'a> {
    pub header: Header,
    pub head_num: U1,
    pub site_num: U1,
    pub sbin_num: U2,
    pub sbin_cnt: U4,
    pub sbin_pf: C1,
    pub sbin_nam: Cn<'a>,
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct PMR<'a> {
    pub header: Header,
    pub pmr_index: U2,
    pub chan_typ: U2,
    pub chan_nam: Cn<'a>,
    pub phy_nam: Cn<'a>,
    pub log_nam: Cn<'a>,
    pub head_num: U1,
    pub site_num: U1,
}

/*
#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct PGR<'a> {
    pub header: Header,
    pub grp_indx: U2,
    pub grp_nam: Cn<'a>,
    pub indx_cnt: U2,
    #[array_length(indx_cnt)]
    pub pmr_indx: Vec<U2>,
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct PLR<'a> {
    pub header: Header,
    pub grp_cnt: U2,
    #[array_length(grp_cnt)]
    pub grp_indx: Vec<U2>,
    #[array_length(grp_cnt)]
    pub grp_mode: Vec<U2>,
    #[array_length(grp_cnt)]
    pub grp_radx: Vec<U1>,
    #[array_length(grp_cnt)]
    pub pgm_char: Vec<Cn<'a>>,
    #[array_length(grp_cnt)]
    pub rtn_char: Vec<Cn<'a>>,
    #[array_length(grp_cnt)]
    pub pgm_chal: Vec<Cn<'a>>,
    #[array_length(grp_cnt)]
    pub rtn_chal: Vec<Cn<'a>>,
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct RDR {
    pub header: Header,
    pub num_bins: U2,
    #[array_length(num_bins)]
    pub rtst_bin: Vec<U2>,
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct SDR<'a> {
    pub header: Header,
    pub head_num: U1,
    pub site_grp: U1,
    pub site_cnt: U1,
    #[array_length(site_cnt)]
    pub site_num: Vec<U1>,
    pub hand_typ: Cn<'a>,
    pub hand_id: Cn<'a>,
    pub card_typ: Cn<'a>,
    pub card_id: Cn<'a>,
    pub load_typ: Cn<'a>,
    pub load_id: Cn<'a>,
    pub dib_typ: Cn<'a>,
    pub dib_id: Cn<'a>,
    pub cabl_typ: Cn<'a>,
    pub cabl_id: Cn<'a>,
    pub cont_typ: Cn<'a>,
    pub cont_id: Cn<'a>,
    pub lasr_typ: Cn<'a>,
    pub lasr_id: Cn<'a>,
    pub extr_typ: Cn<'a>,
    pub extr_id: Cn<'a>,
}
*/

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct WIR<'a> {
    pub header: Header,
    pub head_num: U1,
    pub site_grp: U1,
    pub start_t: U4,
    pub wafer_id: Cn<'a>,
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct WRR<'a> {
    pub header: Header,
    pub head_num: U1,
    pub site_grp: U1,
    pub finish_t: U4,
    pub part_cnt: U4,
    pub rtst_cnt: U4,
    pub abrt_cnt: U4,
    pub good_cnt: U4,
    pub func_cnt: U4,
    pub wafer_id: Cn<'a>,
    pub fabwf_id: Cn<'a>,
    pub frame_id: Cn<'a>,
    pub mask_id: Cn<'a>,
    pub usr_desc: Cn<'a>,
    pub exc_desc: Cn<'a>,
}

#[derive(Debug, PartialEq, STDFRecord)]
pub struct WCR {
    pub header: Header,
    pub wafr_siz: R4,
    pub die_ht: R4,
    pub die_wid: R4,
    pub wf_units: U1,
    pub wf_flat: C1,
    pub center_x: I2,
    pub center_y: I2,
    pub pos_x: C1,
    pub pos_y: C1,
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct PIR {
    pub header: Header,
    pub head_num: U1,
    pub site_num: U1,
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct PRR<'a> {
    pub header: Header,
    pub head_num: U1,
    pub site_num: U1,
    pub part_flg: B1,
    pub num_test: U2,
    pub hard_bin: U2,
    pub soft_bin: U2,
    pub x_coord: I2,
    pub y_coord: I2,
    pub test_t: U4,
    pub part_id: Cn<'a>,
    pub part_txt: Cn<'a>,
    pub part_fix: Bn<'a>,
}

#[derive(Debug, PartialEq, STDFRecord)]
pub struct TSR<'a> {
    pub header: Header,
    pub head_num: U1,
    pub site_num: U1,
    pub test_typ: C1,
    pub test_num: U4,
    pub exec_cnt: U4,
    pub fail_cnt: U4,
    pub alrm_cnt: U4,
    pub test_nam: Cn<'a>,
    pub seq_name: Cn<'a>,
    pub test_lbl: Cn<'a>,
    pub opt_flag: B1,
    pub test_tim: R4,
    pub test_min: R4,
    pub test_max: R4,
    pub tst_sums: R4,
    pub tst_sqrs: R4,
}

#[derive(Debug, PartialEq, STDFRecord)]
pub struct PTR<'a> {
    pub header: Header,
    pub head_num: U1,
    pub site_num: U1,
    pub test_flg: B1,
    pub parm_flg: B1,
    pub result: R4,
    pub test_txt: Cn<'a>,
    pub alarm_id: Cn<'a>,
    pub opt_flag: B1,
    pub res_scal: I1,
    pub llm_scal: I1,
    pub hlm_scal: I1,
    pub lo_limit: R4,
    pub hi_limit: R4,
    pub units: Cn<'a>,
    pub c_resfmt: Cn<'a>,
    pub c_llmfmt: Cn<'a>,
    pub c_hlmfmt: Cn<'a>,
    pub lo_spec: R4,
    pub hi_spec: R4,
}

/*
#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct MPR<'a> {
    pub header: Header,
    pub head_num: U1,
    pub site_num: U1,
    pub test_flg: B1,
    pub parm_flg: B1,
    pub rtn_icnt: U2,
    pub rslt_cnt: U2,
    #[nibble_array_length(rtn_icnt)]
    pub rtn_stat: Vec<U1>,
    #[array_length(rtn_rslt)]
    pub rtn_rslt: Vec<R4>,
    pub test_txt: Cn<'a>,
    pub alarm_id: Cn<'a>,
    pub opt_flag: B1,
    pub res_scal: I1,
    pub llm_scal: I1,
    pub hlm_scal: I1,
    pub lo_limit: R4,
    pub hi_limit: R4,
    pub start_in: R4,
    pub incr_in: R4,
    #[array_length(rtn_icnt)]
    pub rtn_indx: Vec<U2>,
    pub units: Cn<'a>,
    pub units_in: Cn<'a>,
    pub c_resfmt: Cn<'a>,
    pub c_llmfmt: Cn<'a>,
    pub c_hlmfmt: Cn<'a>,
    pub lo_spec: R4,
    pub hi_spec: R4,
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct FTR<'a> {
    pub header: Header,
    pub head_num: U1,
    pub site_num: U1,
    pub test_flg: B1,
    pub opt_flag: B1,
    pub cycl_cnt: U4,
    pub rel_vadr: U4,
    pub rept_cnt: U4,
    pub num_fail: U4,
    pub xfail_ad: I4,
    pub yfail_ad: I4,
    pub vect_off: I2,
    pub rtn_icnt: U2,
    pub pgm_icnt: U2,
    #[array_length(rtn_icnt)]
    pub rtn_indx: Vec<U2>,
    #[nibble_array_length(rtn_icnt)]
    pub rtn_stat: Vec<U1>,
    #[array_length(pgm_icnt)]
    pub pgm_indx: Vec<U2>,
    #[nibble_array_length(pgm_icnt)]
    pub pgm_stat: Vec<U1>,
    pub fail_pin: Dn<'a>,
    pub vect_nam: Cn<'a>,
    pub time_set: Cn<'a>,
    pub op_code: Cn<'a>,
    pub test_txt: Cn<'a>,
    pub alarm_id: Cn<'a>,
    pub prog_txt: Cn<'a>,
    pub rslt_txt: Cn<'a>,
    pub patg_num: U1,
    pub spin_map: Dn<'a>,
}
*/

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct BPS<'a> {
    pub header: Header,
    pub seq_name: Cn<'a>,
}

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct EPS {
    pub header: Header,
}

/*
#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct GDR<'a> {
    pub header: Header,
    pub fld_cnt: U2,
    #[array_length(fld_cnt)]
    pub gen_data: Vec<Vn<'a>>,
}
*/

#[derive(Debug, Eq, PartialEq, STDFRecord)]
pub struct DTR<'a> {
    pub header: Header,
    pub text_dat: Cn<'a>,
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
