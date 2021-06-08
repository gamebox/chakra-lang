target datalayout="e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple="x86_64-pc-linux-gnu"

@.const.0 = private unnamed_addr constant [13 x i8] c"Hello World!\00", align 1


define noalias %struct.Envelope* @init({ i64 }* %0) {
entry:
    %1 = getelementptr inbounds { i64 }, { i64 }* %0, i64 0, i32 0
    %2 = load i64, i64* %1, align 8
    %3 = getelementptr inbounds { %struct.Envelope* (i64, i8*)* }, { %struct.Envelope* (i64, i8*)* }* @Chakra_stdlib__io, i64 0, i32 0
    %4 = load %struct.Envelope* (i64, i8*)*, %struct.Envelope* (i64, i8*)** %3, align 8
    %5 = tail call %struct.Envelope* %4(i64 %2, i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.const.0, i64 0, i64 0))
    ret %struct.Envelope* %5
}