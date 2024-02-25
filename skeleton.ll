%UserData = type {
    ptr, ; tick
    ptr, ; read_u8
    ptr, ; read_u16
    ptr, ; write_u8
    ptr  ; write_u16
}

; Binary Coded Decimal
define internal fastcc i8 @u8_to_bcd(i8 %u) {
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

define internal fastcc i8 @bcd_to_u8(i8 %bcd) {
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
