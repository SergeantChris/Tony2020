String Pù£oþ
; ModuleID = 'tony program'
source_filename = "tony program"

declare void @puti(i32)

declare void @putb(i1)

declare void @putc(i8)

declare void @puts(i8*)

declare i32 @geti()

declare i1 @getb()

declare i8 @getc()

declare void @gets(i32, i8*)

declare i32 @abs(i32)

declare i32 @ord(i8)

declare i8 @chr(i32)

declare i32 @strlen(i8*)

declare i32 @strcmp(i8*, i8*)

declare i32 @strcpy(i8*, i8*)

declare i32 @strcat(i8*, i8*)

define i32 @main() {
program_entry:
  call void @jackthecutestdoggo()
  ret i32 0
}

define void @jackthecutestdoggo() {
entry:
  %n = alloca i32, align 4
  br label %continue

continue:                                         ; preds = %entry
  %limit = alloca i32, align 4
  %number = alloca i32, align 4
  %counter = alloca i32, align 4
  store i32 0, i32* %counter
  store i32 0, i32* %n
  call void @puti(i32 111)
  call void @puts(<null operand!>)
  ret void
}

define i1 @"prime?"(i32 %n) {
entry:
  %i = alloca i32, align 4
  %slttmp = icmp slt i32 %n, 0
  %if_cond = icmp ne i1 %slttmp, false
  br i1 %if_cond, label %then, label %ifelse2

then:                                             ; preds = %entry
  %negsign = sub i32 0, %n
  %calltmp = call i1 @"prime?"(i32 %negsign)
  ret i1 %calltmp

ifelse:                                           ; preds = %endif13
  %modtmp = srem i32 %n, 2
  %eqtmp14 = icmp eq i32 %modtmp, 0
  %if_cond15 = icmp ne i1 %eqtmp14, false
  br i1 %if_cond15, label %then16, label %endif19

ifelse1:                                          ; preds = %endif8
  %eqtmp = icmp eq i32 %n, 2
  %if_cond9 = icmp ne i1 %eqtmp, false
  br i1 %if_cond9, label %then10, label %endif13

ifelse2:                                          ; preds = %entry
  %slttmp3 = icmp slt i32 %n, 2
  %if_cond4 = icmp ne i1 %slttmp3, false
  br i1 %if_cond4, label %then5, label %endif8

else:                                             ; preds = %endif19
  br i1 true, label %then20, label %endif23

endif:                                            ; preds = %endif23
  ret i1 true

then5:                                            ; preds = %ifelse2
  ret i1 false

endif8:                                           ; preds = %ifelse2
  br label %ifelse1

then10:                                           ; preds = %ifelse1
  ret i1 true

endif13:                                          ; preds = %ifelse1
  br label %ifelse

then16:                                           ; preds = %ifelse
  ret i1 false

endif19:                                          ; preds = %ifelse
  br label %else

then20:                                           ; preds = %else
  store i32 3, i32* %i
  %i24 = load i32, i32* %i
  %divtmp = sdiv i32 %n, 2
  %sletmp = icmp sle i32 %i24, %divtmp
  br label %loop

endif23:                                          ; preds = %else
  br label %endif

loop:                                             ; preds = %endif32, %then20
  %iter = phi i1 [ %sletmp, %then20 ], [ %sletmp36, %endif32 ]
  br i1 %iter, label %body, label %endfor

body:                                             ; preds = %loop
  %i25 = load i32, i32* %i
  %modtmp26 = srem i32 %n, %i25
  %eqtmp27 = icmp eq i32 %modtmp26, 0
  %if_cond28 = icmp ne i1 %eqtmp27, false
  br i1 %if_cond28, label %then29, label %ifelse30

endfor:                                           ; preds = %loop
  ret i1 true

then29:                                           ; preds = %body
  ret i1 false

ifelse30:                                         ; preds = %body
  br label %else31

else31:                                           ; preds = %ifelse30
  br label %endif32

endif32:                                          ; preds = %else31
  %i33 = load i32, i32* %i
  %addtmp = add i32 %i33, 2
  store i32 %addtmp, i32* %i
  %i34 = load i32, i32* %i
  %divtmp35 = sdiv i32 %n, 2
  %sletmp36 = icmp sle i32 %i34, %divtmp35
  br label %loop
}
