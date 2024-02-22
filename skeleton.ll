%Cpu = type {
    i8,         ; accumulator
    i8,         ; x
    i8,         ; y
    i8,         ; stack pointer
    <8 x i1>,   ; flags
    %UserData
}

%UserData = type {
    ptr nonnull ; read_u8
    ptr nonnull ; write_u8
}

define internal fastcc alwaysinline %Cpu @lda(i8 %op, %Cpu %cpu) {
entry:
    %updated_acc = insertvalue %Cpu %cpu, i8 %op, 0
    %new_cpu = call %Cpu @set_nz(%op, %updated_acc)
    ret %Cpu %new_cpu
}

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

declare i8 @read_u8(i16, ptr nonnull)
declare void @write_u8(i16, i8, ptr nonnull)
