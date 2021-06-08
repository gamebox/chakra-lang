target datalayout=""
target triple=""

@.str.1 = private unnamed_addr constant [13 x i8] c"Hello World!\00", align 1

%struct.Actor = type { %struct.TurnResult* (i8**)*, %struct.TurnResult* (i8*, %struct.Msg*)* }
%struct.TurnResult = type { i8*, %struct.Envelope* }
%struct.Envelope = type { %struct.ActorId, %struct.Msg }
%struct.ActorId = type { i64, i64 }
%struct.Msg = type { i8*, i8* }

; Function Attrs: nofree nounwind
declare dso_local noalias i8* @malloc(i64) local_unnamed_addr
declare dso_local %struct.Envelope* @Chakra_stdlib__print(i64, i8*) local_unnamed_addr
declare dso_local %struct.Envelope* @Chakra_stdlib__commands__none() local_unnamed_addr

define noalias %struct.Envelope* @test__main__init({ i64 }* %0) {
    %2 = getelementptr inbounds { i64 }, { i64 }* %0, i64 0, i32 0
    %3 = load i64, i64* %2, align 8
    %4 = tail call %struct.Envelope* @Chakra_stdlib__print(i64 %3, i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.1, i64 0, i64 0)) #6
    ret %struct.Envelope* %4
}

@Chakra_stdlib__io = dso_local global { %struct.Envelope* (i64, i8*)* } { %struct.Envelope* (i64, i8*)*  @Chakra_stdlib__print }, align 8

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

; %( commands ) = /stdlib
;
; ; Type is { (<#on|#off>, <#toggle>) -> (<#on|#off>, !) }
; receive(state, msg) =
;     (state, msg) ?
;     | ( #on, #toggle ) -> (#off, commands.none())
;     | ( #off, #toggle ) -> (#on, commands.none())
;
define { i64, %struct.Envelope* }* @test_other__receive(i64 %0, i64 %1) {
    switch i64 %0, label %12 [
        i64 100, label %3
        i64 101, label %4
    ]
3:
    br label %5
4:
    br label %5
5:
    %6 = phi i64 [ 101, %3 ], [100, %4 ]
    %7 = tail call %struct.Envelope* @Chakra_stdlib__commands__none()
    %8 = tail call dereferenceable_or_null(16) i8* @malloc(i64 16)
    %9 = bitcast i8* %8 to { i64, %struct.Envelope* }*
    %10 = getelementptr { i64, %struct.Envelope* }, { i64, %struct.Envelope* }* %9, i64 0, i32 0 ; pointer to result state
    store i64 %6, i64* %10, align 8
    %11 = getelementptr { i64, %struct.Envelope* }, { i64, %struct.Envelope* }* %9, i64 0, i32 1 ; pointer to result msg
    store %struct.Envelope* %7, %struct.Envelope** %11, align 8
    br label %12
12:
    %13 = phi { i64, %struct.Envelope* }* [ %9, %5], [ null, %2]
    ret { i64, %struct.Envelope* }* %13
}

define %struct.TurnResult* @test_other__receive_ACTOR(i8* %0, i8* %1) {
    %3 = bitcast i8* %0 to i64*
    %4 = load i64, i64* %3
    %5 = bitcast i8* %0 to i64*
    %6 = load i64, i64* %3
    %7 = tail call { i64, %struct.Envelope* }* @test_other__receive(i64 %4, i64 %6)
    %8 = tail call dereferenceable_or_null(16) i8* @malloc(i64 16)
    %9 = bitcast i8* %8 to %struct.TurnResult*
    ; lower into generic type
    ret %struct.TurnResult* %9
}

; define fastcc %struct.List__str* @Chakra_stdlib__list__map.num.str(i8* (i64) %arg0, %struct.List__num* %arg1) nounwind {
;     %1 = icmp i1 0, %struct.List__num* %arg1, %struct.List__num* null
;     br i1 %1, label %2, label %?
; 2:
;     br label %return
; 3:
    
; return:
;     %? = phi %struct.List__num* [%2, null], []
; }