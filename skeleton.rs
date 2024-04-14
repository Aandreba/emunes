use std::mem::MaybeUninit;

#[repr(C)]
pub struct AdcOutput {
    res: u8,
    carry: bool,
    overflow: bool,
    zero: bool,
    negative: bool,
    updated_overflow: bool,
}

#[no_mangle]
pub extern "C" fn adc(lhs: u8, rhs: u8, carry: bool, decimal: bool, output: &mut AdcOutput) {
    let out_carry: bool;
    let mut out_overflow: Option<bool> = None;

    let res = if decimal {
        // Decimal
        let mut res = bcd_to_u8(lhs)
            .wrapping_add(bcd_to_u8(rhs))
            .wrapping_add(carry as u8);

        if res > 99 {
            res -= 100;
            out_carry = true;
        } else {
            out_carry = false;
        }

        u8_to_bcd(res) as u16
    } else {
        // Binary
        let op = rhs as u16;
        let acc = lhs as u16;
        let res = acc.wrapping_add(op).wrapping_add(carry as u16);

        out_overflow = Some((acc ^ res) & (op ^ res) & 0x0080 != 0);
        out_carry = res & 0xff00 != 0;
        res
    };

    *output = AdcOutput {
        res: res as u8,
        zero: res & 0xff == 0,
        negative: res & 0x80 != 0,
        carry: out_carry,
        overflow: out_overflow.unwrap_or_else(uninit_bool),
        updated_overflow: out_overflow.is_some(),
    };
}

#[no_mangle]
pub extern "C" fn sbc(lhs: u8, rhs: u8, carry: bool, decimal: bool, output: &mut AdcOutput) {
    let borrow = !carry;

    let out_carry: bool;
    let mut out_overflow: Option<bool> = None;

    let res = if decimal {
        // Decimal
        let mut res = bcd_to_u8(lhs)
            .wrapping_sub(bcd_to_u8(rhs))
            .wrapping_sub(borrow as u8) as i8;

        if res.is_negative() {
            res += 100;
            out_carry = false;
        } else {
            out_carry = true;
        }

        u8_to_bcd(res as u8) as u16
    } else {
        // Binary
        let op = rhs as u16;
        let acc = lhs as u16;
        let res = acc.wrapping_sub(op).wrapping_sub(borrow as u16);

        out_overflow = Some((acc ^ res) & (!op ^ res) & 0x0080 != 0);
        out_carry = res & 0xff00 == 0;
        res
    };

    *output = AdcOutput {
        res: res as u8,
        zero: res & 0xff == 0,
        negative: res & 0x80 != 0,
        carry: out_carry,
        overflow: out_overflow.unwrap_or_else(uninit_bool),
        updated_overflow: out_overflow.is_some(),
    };
}

// Taken from https://github.com/kromych/yamos6502/blob/main/src/bcd.rs
pub extern "C" fn u8_to_bcd(u: u8) -> u8 {
    if u < 100 {
        ((u / 10) << 4) | (u % 10)
    } else {
        0x00
    }
}

pub extern "C" fn bcd_to_u8(bcd: u8) -> u8 {
    (bcd >> 4) * 10 + (bcd & 0x0f)
}

#[inline(always)]
fn uninit_bool() -> bool {
    unsafe { MaybeUninit::uninit().assume_init() }
}
