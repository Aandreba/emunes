%AdcOutput = type {
    i8, ; res
    i8, ; carry
    i8, ; overflow
    i8, ; zero
    i8, ; negative
    i8  ; updated_overflow
}

define void @adc(i8 noundef zeroext %lhs, i8 noundef zeroext %rhs, i1 noundef zeroext %carry, i1 noundef zeroext %decimal, ptr noalias nocapture noundef writeonly align 1 dereferenceable(6) %output) unnamed_addr {
start:
  br i1 %decimal, label %bb1, label %bb6

bb1:
  %_50 = lshr i8 %lhs, 4
  %_51 = and i8 %lhs, 15
  %_53 = lshr i8 %rhs, 4
  %_54 = and i8 %rhs, 15
  %rhs8 = zext i1 %carry to i8
  %reass.add = add nuw nsw i8 %_53, %_50
  %reass.mul = mul i8 %reass.add, 10
  %self5 = add nuw nsw i8 %_54, %_51
  %self7 = add nuw nsw i8 %self5, %rhs8
  %0 = add i8 %self7, %reass.mul
  %_14 = icmp ugt i8 %0, 99
  %1 = add i8 %0, -100
  %res1.0 = select i1 %_14, i8 %1, i8 %0
  %_55 = icmp ult i8 %res1.0, 100
  br i1 %_55, label %bb7, label %bb12

bb6:
  %op = zext i8 %rhs to i16
  %acc = zext i8 %lhs to i16
  %self2 = add nuw nsw i16 %op, %acc
  %rhs3 = zext i1 %carry to i16
  %res4 = add nuw nsw i16 %self2, %rhs3
  %_27 = xor i16 %res4, %acc
  %_28 = xor i16 %res4, %op
  %_26 = and i16 %_28, 128
  %_25 = and i16 %_26, %_27
  %_24 = icmp ne i16 %_25, 0
  %2 = zext i1 %_24 to i8
  %3 = icmp ugt i16 %res4, 255
  br label %bb12

bb7:
  %_57 = udiv i8 %res1.0, 10
  %_56 = shl nuw i8 %_57, 4
  %_58 = urem i8 %res1.0, 10
  %4 = or i8 %_56, %_58
  %5 = zext i8 %4 to i16
  br label %bb12

bb12:
  %out_carry.020 = phi i1 [ %3, %bb6 ], [ %_14, %bb1 ], [ %_14, %bb7 ]
  %res.018 = phi i16 [ %res4, %bb6 ], [ 0, %bb1 ], [ %5, %bb7 ]
  %_44.0 = phi i8 [ %2, %bb6 ], [ 0, %bb1 ], [ 0, %bb7 ]
  %6 = xor i1 %decimal, true
  %_41 = zext i1 %out_carry.020 to i8
  %_39 = and i16 %res.018, 128
  %_38 = icmp ne i16 %_39, 0
  %_37 = zext i1 %_38 to i8
  %_35 = and i16 %res.018, 255
  %_34 = icmp eq i16 %_35, 0
  %_33 = zext i1 %_34 to i8
  %_31 = trunc i16 %res.018 to i8
  %_46 = zext i1 %6 to i8
  store i8 %_31, ptr %output, align 1
  %_30.sroa.4.0.output.sroa_idx = getelementptr inbounds i8, ptr %output, i64 1
  store i8 %_41, ptr %_30.sroa.4.0.output.sroa_idx, align 1
  %_30.sroa.5.0.output.sroa_idx = getelementptr inbounds i8, ptr %output, i64 2
  store i8 %_44.0, ptr %_30.sroa.5.0.output.sroa_idx, align 1
  %_30.sroa.6.0.output.sroa_idx = getelementptr inbounds i8, ptr %output, i64 3
  store i8 %_33, ptr %_30.sroa.6.0.output.sroa_idx, align 1
  %_30.sroa.7.0.output.sroa_idx = getelementptr inbounds i8, ptr %output, i64 4
  store i8 %_37, ptr %_30.sroa.7.0.output.sroa_idx, align 1
  %_30.sroa.8.0.output.sroa_idx = getelementptr inbounds i8, ptr %output, i64 5
  store i8 %_46, ptr %_30.sroa.8.0.output.sroa_idx, align 1
  ret void
}

define void @sbc(i8 noundef zeroext %lhs, i8 noundef zeroext %rhs, i1 noundef zeroext %carry, i1 noundef zeroext %decimal, ptr noalias nocapture noundef writeonly align 1 dereferenceable(6) %output) unnamed_addr {
start:
  %borrow = xor i1 %carry, true
  br i1 %decimal, label %bb1, label %bb6

bb1:
  %_54 = lshr i8 %lhs, 4
  %_55 = and i8 %lhs, 15
  %_57 = lshr i8 %rhs, 4
  %_58 = and i8 %rhs, 15
  %rhs8.neg = sext i1 %borrow to i8
  %reass.add = sub nsw i8 %_54, %_57
  %reass.mul = mul i8 %reass.add, 10
  %self5 = sub nsw i8 %_55, %_58
  %self7 = add nsw i8 %self5, %rhs8.neg
  %_11 = add i8 %self7, %reass.mul
  %_16 = icmp sgt i8 %_11, -1
  %0 = add nsw i8 %_11, 100
  %res1.0 = select i1 %_16, i8 %_11, i8 %0
  %_59 = icmp ult i8 %res1.0, 100
  br i1 %_59, label %bb7, label %bb12

bb6:
  %op = zext i8 %rhs to i16
  %acc = zext i8 %lhs to i16
  %self2 = sub nsw i16 %acc, %op
  %rhs3.neg = sext i1 %borrow to i16
  %res4 = add nsw i16 %self2, %rhs3.neg
  %_30 = xor i16 %res4, %acc
  %1 = xor i16 %res4, %op
  %_31 = and i16 %1, 128
  %_29 = xor i16 %_31, 128
  %_28 = and i16 %_29, %_30
  %_27 = icmp ne i16 %_28, 0
  %2 = zext i1 %_27 to i8
  %3 = icmp ult i16 %res4, 256
  br label %bb12

bb7:
  %_61 = udiv i8 %res1.0, 10
  %_60 = shl nuw i8 %_61, 4
  %_62 = urem i8 %res1.0, 10
  %4 = or i8 %_60, %_62
  %5 = zext i8 %4 to i16
  br label %bb12

bb12:
  %out_carry.021 = phi i1 [ %3, %bb6 ], [ %_16, %bb1 ], [ %_16, %bb7 ]
  %res.019 = phi i16 [ %res4, %bb6 ], [ 0, %bb1 ], [ %5, %bb7 ]
  %_48.0 = phi i8 [ %2, %bb6 ], [ 0, %bb1 ], [ 0, %bb7 ]
  %6 = xor i1 %decimal, true
  %_45 = zext i1 %out_carry.021 to i8
  %_43 = and i16 %res.019, 128
  %_42 = icmp ne i16 %_43, 0
  %_41 = zext i1 %_42 to i8
  %_39 = and i16 %res.019, 255
  %_38 = icmp eq i16 %_39, 0
  %_37 = zext i1 %_38 to i8
  %_35 = trunc i16 %res.019 to i8
  %_50 = zext i1 %6 to i8
  store i8 %_35, ptr %output, align 1
  %_34.sroa.4.0.output.sroa_idx = getelementptr inbounds i8, ptr %output, i64 1
  store i8 %_45, ptr %_34.sroa.4.0.output.sroa_idx, align 1
  %_34.sroa.5.0.output.sroa_idx = getelementptr inbounds i8, ptr %output, i64 2
  store i8 %_48.0, ptr %_34.sroa.5.0.output.sroa_idx, align 1
  %_34.sroa.6.0.output.sroa_idx = getelementptr inbounds i8, ptr %output, i64 3
  store i8 %_37, ptr %_34.sroa.6.0.output.sroa_idx, align 1
  %_34.sroa.7.0.output.sroa_idx = getelementptr inbounds i8, ptr %output, i64 4
  store i8 %_41, ptr %_34.sroa.7.0.output.sroa_idx, align 1
  %_34.sroa.8.0.output.sroa_idx = getelementptr inbounds i8, ptr %output, i64 5
  store i8 %_50, ptr %_34.sroa.8.0.output.sroa_idx, align 1
  ret void
}
