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
    ptr  ; write_u8
}

; Stack Ops
define internal fastcc %Cpu @push(i8 %op, %Cpu %cpu) {
entry:
    ; Write value
    %stack_ptr = extractvalue %Cpu %cpu, 3
    $stack_ptr_ext = zext i8 %stack_ptr to i16
    %stack_addr = add i16 0x100, %stack_ptr_ext
    call void @write_u8(%stack_addr, %op, %cpu)

    ; Update stack pointer
    %new_stack_ptr = sub i8 %stack_ptr, 1
    %new_cpu = insertvalue %Cpu %cpu, %new_stack_ptr, 3
    ret %Cpu %new_cpu
}

define internal fastcc { i8, %Cpu } @pop(%Cpu %cpu) {
entry:
    ; Update stack pointer
    %stack_ptr = extractvalue %Cpu %cpu, 3
    %new_stack_ptr = add i8 %stack_ptr, 1
    %new_cpu = insertvalue %Cpu %cpu, %new_stack_ptr, 3

    ; Read value
    $stack_ptr_ext = zext i8 %new_stack_ptr to i16
    %stack_addr = add i16 0x100, %stack_ptr_ext
    %op = call i8 @read_u8(%new_stack_ptr, %new_cpu)

    ; Set up result
    %res1 = insertvalue { i8, %Cpu } undef, i8 %op, 0
    %res2 = insertvalue { i8, %Cpu } %res1, %Cpu %new_cpu, 1
    ret { i8, %Cpu } %res2
}

; Flag Utils
define internal fastcc alwaysinline %Cpu @set_nz(i8 %op, %Cpu %cpu) {
entry:
    %is_neg = icmp slt i8 %op, i8 0
    %is_zero = icmp eq i8 %op, i8 0

    %old_flags = extractvalue %Cpu %cpu, 4
    %updated_neg = insertelement <8 x i1> %old_flags, i1 $is_neg, 7
    %updated_zero = insertelement <8 x i1> %updated_neg, i1 $is_zero, 1

    %new_cpu = insertvalue %Cpu %cpu, <8 x i1> %updated_zero, 4
    ret %new_cpu
}

; Binary Coded Decimal
define internal fastcc i8 @u8_to_bcd(i8 %u) {
entry:
    %has_overflow = icmp ult i8 %u, 100
    br i1 %has_overflow, label %overflow, label %transform
transform:
    %1 = udiv i8 %u, 10
    %2 = urem i8 %u, 10
    %3 = shl i8 %1, 4
    %4 = or i8 %2, %3
    ret i8 %4
overflow:
    ret i8 0x00
}

define internal fastcc i8 @bcd_to_u8(i8 %bcd) {
entry:
    %1 = lshr i8 %bcd, 4
    %2 = mul i8 %1, 10
    %3 = and i8 %bcd, 0x0f
    %4 = add i8 %2, %3
    ret i8 %4
}

; Imported function helpers
define internal fastcc alwaysinline void @tick(i8 %cycles, %Cpu %cpu) {
entry:
    %user_data = extractvalue %Cpu %cpu, 5
    %user_data_ptr = extractvalue %UserData %user_data, 0
    call void @_tick(%cycles, %user_data_ptr)
    ret void
}

define internal fastcc alwaysinline i8 @read_u8(i16 %addr, %Cpu %cpu) {
entry:
    %user_data = extractvalue %Cpu %cpu, 5
    %user_data_ptr = extractvalue %UserData %user_data, 1
    %result = call i8 @_read_u8(%addr, %user_data_ptr)
    ret i8 %result
}

define internal fastcc alwaysinline void @write_u8(i16 %addr, i8 %op, %Cpu %cpu) {
entry:
    %user_data = extractvalue %Cpu %cpu, 5
    %user_data_ptr = extractvalue %UserData %user_data, 2
    call void @_write_u8(%addr, %op, %user_data_ptr)
    ret void
}

; Imported functions
declare void @_tick(i8, ptr)
declare i8 @_read_u8(i16, ptr)
declare void @_write_u8(i16, i8, ptr)
