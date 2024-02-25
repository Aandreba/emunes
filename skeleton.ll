%UserData = type {
    ptr, ; tick
    ptr, ; read_u8
    ptr, ; read_u16
    ptr, ; write_u8
    ptr  ; write_u16
}

%BinaryResult = type {
    i8, ; result
    i1, ; next carry
    i1  ; next overflow
}

%DecimalResult = type {
    i8, ; result
    i1  ; next carry
}

; Binary ops
define internal fastcc %BinaryResult @binary_adc(i8 %lhs, i8 %rhs, i1 %carry) {
entry:
    ; Zero-extend arguments
    %lhs_ext = zext i8 %lhs to i16
    %rhs_ext = zext i8 %rhs to i16
    %carry_ext = zext i1 %carry to i16

    ; Calculate result
    %0 = add i16 %lhs_ext, %rhs_ext
    %res_ext = add i16 %0, %carry_ext
    %res = trunc i16 %res_ext to i8

    ; Calculate next overflow
    %1 = xor i16 %lhs_ext, %res_ext
    %2 = xor i16 %rhs_ext, %res_ext
    %3 = and i16 %1, %2
    %4 = and i16 %3, u0x0080
    %next_carry = icmp ne i16 %4, 0

    ; Calculate next carry
    %5 = and i16 %res_ext, u0xff00
    %next_overflow = icmp ne i16 %5, 0

    ; Return result
    %6 = insertvalue %BinaryResult undef, i8 %res, 0
    %7 = insertvalue %BinaryResult %6, i1 %next_carry, 1
    %8 = insertvalue %BinaryResult %7, i1 %next_overflow, 2
    ret %BinaryResult %8
}

define internal fastcc %BinaryResult @binary_sbc(i8 %lhs, i8 %rhs, i1 %borrow) {
entry:
    %carry = xor i1 %borrow, true

    ; Zero-extend arguments
    %lhs_ext = zext i8 %lhs to i16
    %rhs_ext = zext i8 %rhs to i16
    %rhs_inv = xor i16 %rhs_ext, -1
    %carry_ext = zext i1 %carry to i16

    ; Calculate result
    %0 = sub i16 %lhs_ext, %rhs_ext
    %res_ext = sub i16 %0, %carry_ext
    %res = trunc i16 %res_ext to i8

    ; Calculate next overflow
    %1 = xor i16 %lhs_ext, %res_ext
    %2 = xor i16 %rhs_inv, %res_ext
    %3 = and i16 %1, %2
    %4 = and i16 %3, u0x0080
    %next_carry = icmp ne i16 %4, 0

    ; Calculate next carry
    %5 = and i16 %res_ext, u0xff00
    %next_overflow = icmp eq i16 %5, 0

    ; Return result
    %6 = insertvalue %BinaryResult undef, i8 %res, 0
    %7 = insertvalue %BinaryResult %6, i1 %next_carry, 1
    %8 = insertvalue %BinaryResult %7, i1 %next_overflow, 2
    ret %BinaryResult %8
}

; Decimal ops
define internal fastcc %DecimalResult @decimal_adc(i8 %lhs, i8 %rhs, i1 %carry) {
entry:
    ; Transform arguments
    %lhs_binary = call i8 @bcd_to_u8(i8 %lhs)
    %rhs_binary = call i8 @bcd_to_u8(i8 %rhs)
    %carry_ext = zext i1 %carry to i8

    ; Calculate result & next carry
    %0 = add i8 %lhs_binary, %rhs_binary
    %1 = add i8 %0, %carry_ext
    %next_carry = icmp ugt i8 %1, 99
    %delta = select i1 %next_carry, i8 100, i8 0
    %2 = sub i8 %1, %delta
    %res = call i8 @u8_to_bcd(i8 %2)

    ; Return result
    %3 = insertvalue %DecimalResult undef, i8 %res, 0
    %4 = insertvalue %DecimalResult %3, i1 %next_carry, 1
    ret %DecimalResult %4
}

define internal fastcc %DecimalResult @decimal_sbc(i8 %lhs, i8 %rhs, i1 %borrow) {
entry:
    %carry = xor i1 %borrow, true

    ; Transform arguments
    %lhs_binary = call i8 @bcd_to_u8(i8 %lhs)
    %rhs_binary = call i8 @bcd_to_u8(i8 %rhs)
    %carry_ext = zext i1 %carry to i8

    ; Calculate result & next carry
    %0 = sub i8 %lhs_binary, %rhs_binary
    %1 = sub i8 %0, %carry_ext
    %next_carry = icmp sge i8 %1, 0
    %delta = select i1 %next_carry, i8 0, i8 100
    %2 = add i8 %1, %delta
    %res = call i8 @u8_to_bcd(i8 %2)

    ; Return result
    %3 = insertvalue %DecimalResult undef, i8 %res, 0
    %4 = insertvalue %DecimalResult %3, i1 %next_carry, 1
    ret %DecimalResult %4
}

; Binary Coded Decimal
define internal fastcc i8 @u8_to_bcd(i8 %u) inlinehint {
entry:
    %has_overflow = icmp ult i8 %u, 100
    br i1 %has_overflow, label %overflow, label %transform
transform:
    %0 = udiv i8 %u, 10
    %1 = urem i8 %u, 10
    %2 = shl i8 %0, 4
    %3 = or i8 %1, %2
    ret i8 %3
overflow:
    ret i8 u0x00
}

define internal fastcc i8 @bcd_to_u8(i8 %bcd) inlinehint {
entry:
    %0 = lshr i8 %bcd, 4
    %1 = mul i8 %0, 10
    %2 = and i8 %bcd, u0x0f
    %3 = add i8 %1, %2
    ret i8 %3
}

; Imported functions
declare void @_tick(i8, ptr)
declare i16 @_read_u8(i16, ptr)
declare i32 @_read_u16(i16, ptr)
declare i8 @_write_u8(i16, i8, ptr)
declare i8 @_write_u16(i16, i16, ptr)
