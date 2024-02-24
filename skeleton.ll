%ExternCpu = type {
    i8,         ; accumulator
    i8,         ; x
    i8,         ; y
    i8,         ; stack pointer
    i8,         ; flags
    %UserData
}

%Cpu = type {
    i8,         ; accumulator
    i8,         ; x
    i8,         ; y
    i8,         ; stack pointer
    <8 x i1>,   ; flags
    %UserData
}

%UserData = type {
    ptr, ; tick
    ptr, ; read_u8
    ptr, ; read_u16
    ptr  ; write_u8
    ptr, ; write_u16
}

; Stack Ops
define internal fastcc %Cpu @push(i8 %op, %Cpu %cpu) {
entry:
    ; Write value
    %stack_ptr = extractvalue %Cpu %cpu, 3
    %stack_ptr_ext = zext i8 %stack_ptr to i16
    %stack_addr = add i16 u0x100, %stack_ptr_ext
    call void @write_u8(i16 %stack_addr, i8 %op, %Cpu %cpu)

    ; Update stack pointer
    %new_stack_ptr = sub i8 %stack_ptr, 1
    %new_cpu = insertvalue %Cpu %cpu, i8 %new_stack_ptr, 3
    ret %Cpu %new_cpu
}

define internal fastcc { i8, %Cpu } @pop(%Cpu %cpu) {
entry:
    ; Update stack pointer
    %stack_ptr = extractvalue %Cpu %cpu, 3
    %new_stack_ptr = add i8 %stack_ptr, 1
    %new_cpu = insertvalue %Cpu %cpu, i8 %new_stack_ptr, 3

    ; Read value
    %stack_ptr_ext = zext i8 %new_stack_ptr to i16
    %stack_addr = add i16 u0x100, %stack_ptr_ext
    %op = call i8 @read_u8(i16 %stack_addr, %Cpu %new_cpu)

    ; Set up result
    %res1 = insertvalue { i8, %Cpu } undef, i8 %op, 0
    %res2 = insertvalue { i8, %Cpu } %res1, %Cpu %new_cpu, 1
    ret { i8, %Cpu } %res2
}

; Flag Utils
define internal fastcc %Cpu @set_nz(i8 %op, %Cpu %cpu) alwaysinline {
entry:
    %is_neg = icmp slt i8 %op, 0
    %is_zero = icmp eq i8 %op, 0

    %old_flags = extractvalue %Cpu %cpu, 4
    %updated_neg = insertelement <8 x i1> %old_flags, i1 %is_neg, i8 7
    %updated_zero = insertelement <8 x i1> %updated_neg, i1 %is_zero, i8 1

    %new_cpu = insertvalue %Cpu %cpu, <8 x i1> %updated_zero, 4
    ret %Cpu %new_cpu
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

; Imported function helpers
define internal fastcc void @tick(i8 %cycles, %Cpu %cpu) alwaysinline {
entry:
    %user_data = extractvalue %Cpu %cpu, 5
    %user_data_ptr = extractvalue %UserData %user_data, 0
    call void @_tick(i8 %cycles, ptr %user_data_ptr)
    ret void
}

define internal fastcc i8 @read_u8(i16 %addr, %Cpu %cpu) alwaysinline {
entry:
    %user_data = extractvalue %Cpu %cpu, 5
    %user_data_ptr = extractvalue %UserData %user_data, 1
    %result = call i8 @_read_u8(i16 %addr, ptr %user_data_ptr)
    ret i8 %result
}

define internal fastcc void @write_u8(i16 %addr, i8 %op, %Cpu %cpu) alwaysinline {
entry:
    %user_data = extractvalue %Cpu %cpu, 5
    %user_data_ptr = extractvalue %UserData %user_data, 2
    call void @_write_u8(i16 %addr, i8 %op, ptr %user_data_ptr)
    ret void
}

; Imported functions
declare void @_tick(i8, ptr)
declare i8 @_read_u8(i16, ptr)
declare i16 @_read_u16(i16, ptr)
declare void @_write_u8(i16, i8, ptr)
declare void @_write_u16(i16, i16, ptr)
