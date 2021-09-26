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
  %number = alloca i32, align 4
  %calltmp = call i32 @geti()
  store i32 %calltmp, i32* %number
  %number1 = load i32, i32* %number
  %calltmp2 = call i1 @"prime?"(i32 %number1, i32* %number)
  call void @putb(i1 %calltmp2)
  ret void
}

define i1 @"prime?"(i32 %n, i32* %number) {
entry:
  %i = alloca i32, align 4
  %slttmp = icmp slt i32 %n, 0
  %if_cond = icmp ne i1 %slttmp, false
  br i1 %if_cond, label %then, label %ifelse2

then:                                             ; preds = %entry
  %negsign = sub i32 0, %n
  %number3 = alloca i32*, align 8
  store i32* %number, i32** %number3
  %number4 = load i32*, i32** %number3
  %calltmp = call i1 @"prime?"(i32 %negsign, i32* %number4)
  ret i1 %calltmp

ifelse:                                           ; preds = %endif15
  %modtmp = srem i32 %n, 2
  %eqtmp16 = icmp eq i32 %modtmp, 0
  %if_cond17 = icmp ne i1 %eqtmp16, false
  br i1 %if_cond17, label %then18, label %endif21

ifelse1:                                          ; preds = %endif10
  %eqtmp = icmp eq i32 %n, 2
  %if_cond11 = icmp ne i1 %eqtmp, false
  br i1 %if_cond11, label %then12, label %endif15

ifelse2:                                          ; preds = %entry
  %slttmp5 = icmp slt i32 %n, 2
  %if_cond6 = icmp ne i1 %slttmp5, false
  br i1 %if_cond6, label %then7, label %endif10

else:                                             ; preds = %endif21
  br i1 true, label %then22, label %endif25

endif:                                            ; preds = %endif25
  ret i1 false

then7:                                            ; preds = %ifelse2
  ret i1 false

endif10:                                          ; preds = %ifelse2
  br label %ifelse1

then12:                                           ; preds = %ifelse1
  ret i1 true

endif15:                                          ; preds = %ifelse1
  br label %ifelse

then18:                                           ; preds = %ifelse
  ret i1 false

endif21:                                          ; preds = %ifelse
  br label %else

then22:                                           ; preds = %else
  store i32 3, i32* %i
  %i26 = load i32, i32* %i
  %divtmp = sdiv i32 %n, 2
  %sletmp = icmp sle i32 %i26, %divtmp
  br label %loop

endif25:                                          ; preds = %else
  br label %endif

loop:                                             ; preds = %endif34, %then22
  %iter = phi i1 [ %sletmp, %then22 ], [ %sletmp38, %endif34 ]
  br i1 %iter, label %body, label %endfor

body:                                             ; preds = %loop
  %i27 = load i32, i32* %i
  %modtmp28 = srem i32 %n, %i27
  %eqtmp29 = icmp eq i32 %modtmp28, 0
  %if_cond30 = icmp ne i1 %eqtmp29, false
  br i1 %if_cond30, label %then31, label %ifelse32

endfor:                                           ; preds = %loop
  ret i1 true

then31:                                           ; preds = %body
  ret i1 false

ifelse32:                                         ; preds = %body
  br label %else33

else33:                                           ; preds = %ifelse32
  br label %endif34

endif34:                                          ; preds = %else33
  %i35 = load i32, i32* %i
  %addtmp = add i32 %i35, 2
  store i32 %addtmp, i32* %i
  %i36 = load i32, i32* %i
  %divtmp37 = sdiv i32 %n, 2
  %sletmp38 = icmp sle i32 %i36, %divtmp37
  br label %loop
}
