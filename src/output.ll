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
  %len = alloca i32, align 4
  %s = alloca [5 x i8]
  %elemalloc = getelementptr inbounds [5 x i8], [5 x i8]* %s, i32 0, i32 0
  store i8 84, i8* %elemalloc
  %elemalloc1 = getelementptr inbounds [5 x i8], [5 x i8]* %s, i32 0, i32 1
  store i8 101, i8* %elemalloc1
  %elemalloc2 = getelementptr inbounds [5 x i8], [5 x i8]* %s, i32 0, i32 2
  store i8 115, i8* %elemalloc2
  %elemalloc3 = getelementptr inbounds [5 x i8], [5 x i8]* %s, i32 0, i32 3
  store i8 116, i8* %elemalloc3
  %s4 = load [5 x i8], [5 x i8]* %s
  call void @puts([5 x i8] %s4)
  %ConstStringArray = alloca [5 x i8]
  %elemalloc5 = getelementptr inbounds [5 x i8], [5 x i8]* %ConstStringArray, i32 0, i32 0
  store i8 84, i8* %elemalloc5
  %elemalloc6 = getelementptr inbounds [5 x i8], [5 x i8]* %ConstStringArray, i32 0, i32 1
  store i8 101, i8* %elemalloc6
  %elemalloc7 = getelementptr inbounds [5 x i8], [5 x i8]* %ConstStringArray, i32 0, i32 2
  store i8 120, i8* %elemalloc7
  %elemalloc8 = getelementptr inbounds [5 x i8], [5 x i8]* %ConstStringArray, i32 0, i32 3
  store i8 116, i8* %elemalloc8
  %StrArrayPtr = bitcast [5 x i8]* %ConstStringArray to i8*
  %retarray = bitcast i8* %StrArrayPtr to [5 x i8]*
  %callval = load [5 x i8], [5 x i8]* %retarray
  store [5 x i8] %callval, [5 x i8]* %s
  %s9 = load [5 x i8], [5 x i8]* %s
  call void @puts([5 x i8] %s9)
  %ConstStringArray10 = alloca [16 x i8]
  %elemalloc11 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 0
  store i8 103, i8* %elemalloc11
  %elemalloc12 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 1
  store i8 105, i8* %elemalloc12
  %elemalloc13 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 2
  store i8 118, i8* %elemalloc13
  %elemalloc14 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 3
  store i8 101, i8* %elemalloc14
  %elemalloc15 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 4
  store i8 32, i8* %elemalloc15
  %elemalloc16 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 5
  store i8 97, i8* %elemalloc16
  %elemalloc17 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 6
  store i8 32, i8* %elemalloc17
  %elemalloc18 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 7
  store i8 110, i8* %elemalloc18
  %elemalloc19 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 8
  store i8 117, i8* %elemalloc19
  %elemalloc20 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 9
  store i8 109, i8* %elemalloc20
  %elemalloc21 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 10
  store i8 98, i8* %elemalloc21
  %elemalloc22 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 11
  store i8 101, i8* %elemalloc22
  %elemalloc23 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 12
  store i8 114, i8* %elemalloc23
  %elemalloc24 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 13
  store i8 58, i8* %elemalloc24
  %elemalloc25 = getelementptr inbounds [16 x i8], [16 x i8]* %ConstStringArray10, i32 0, i32 14
  store i8 32, i8* %elemalloc25
  %StrArrayPtr26 = bitcast [16 x i8]* %ConstStringArray10 to i8*
  call void @puts(i8* %StrArrayPtr26)
  %calltmp = call i32 @geti()
  store i32 %calltmp, i32* %n
  %n27 = load i32, i32* %n
  call void @getit(i32 %n27, i32* %len, i32* %n)
  %ConstStringArray28 = alloca [5 x i8]
  %elemalloc29 = getelementptr inbounds [5 x i8], [5 x i8]* %ConstStringArray28, i32 0, i32 0
  store i8 67, i8* %elemalloc29
  %elemalloc30 = getelementptr inbounds [5 x i8], [5 x i8]* %ConstStringArray28, i32 0, i32 1
  store i8 105, i8* %elemalloc30
  %elemalloc31 = getelementptr inbounds [5 x i8], [5 x i8]* %ConstStringArray28, i32 0, i32 2
  store i8 97, i8* %elemalloc31
  %elemalloc32 = getelementptr inbounds [5 x i8], [5 x i8]* %ConstStringArray28, i32 0, i32 3
  store i8 111, i8* %elemalloc32
  %StrArrayPtr33 = bitcast [5 x i8]* %ConstStringArray28 to i8*
  %calltmp34 = call i32 @strlen(i8* %StrArrayPtr33)
  store i32 %calltmp34, i32* %len
  %ConstStringArray35 = alloca [12 x i8]
  %elemalloc36 = getelementptr inbounds [12 x i8], [12 x i8]* %ConstStringArray35, i32 0, i32 0
  store i8 109, i8* %elemalloc36
  %elemalloc37 = getelementptr inbounds [12 x i8], [12 x i8]* %ConstStringArray35, i32 0, i32 1
  store i8 32, i8* %elemalloc37
  %elemalloc38 = getelementptr inbounds [12 x i8], [12 x i8]* %ConstStringArray35, i32 0, i32 2
  store i8 97, i8* %elemalloc38
  %elemalloc39 = getelementptr inbounds [12 x i8], [12 x i8]* %ConstStringArray35, i32 0, i32 3
  store i8 32, i8* %elemalloc39
  %elemalloc40 = getelementptr inbounds [12 x i8], [12 x i8]* %ConstStringArray35, i32 0, i32 4
  store i8 103, i8* %elemalloc40
  %elemalloc41 = getelementptr inbounds [12 x i8], [12 x i8]* %ConstStringArray35, i32 0, i32 5
  store i8 32, i8* %elemalloc41
  %elemalloc42 = getelementptr inbounds [12 x i8], [12 x i8]* %ConstStringArray35, i32 0, i32 6
  store i8 105, i8* %elemalloc42
  %elemalloc43 = getelementptr inbounds [12 x i8], [12 x i8]* %ConstStringArray35, i32 0, i32 7
  store i8 32, i8* %elemalloc43
  %elemalloc44 = getelementptr inbounds [12 x i8], [12 x i8]* %ConstStringArray35, i32 0, i32 8
  store i8 99, i8* %elemalloc44
  %elemalloc45 = getelementptr inbounds [12 x i8], [12 x i8]* %ConstStringArray35, i32 0, i32 9
  store i8 58, i8* %elemalloc45
  %elemalloc46 = getelementptr inbounds [12 x i8], [12 x i8]* %ConstStringArray35, i32 0, i32 10
  store i8 32, i8* %elemalloc46
  %StrArrayPtr47 = bitcast [12 x i8]* %ConstStringArray35 to i8*
  call void @puts(i8* %StrArrayPtr47)
  %len48 = load i32, i32* %len
  call void @puti(i32 %len48)
  ret void
}

define void @myprint(i1 %b, i32* %len, i32* %n) {
entry:
  call void @putb(i1 %b)
  ret void
}

define i1 @"prime?"(i32 %n, i32* %len) {
entry:
  %i = alloca i32, align 4
  %slttmp = icmp slt i32 %n, 0
  %if_cond = icmp ne i1 %slttmp, false
  br i1 %if_cond, label %then, label %ifelse2

then:                                             ; preds = %entry
  %negsign = sub i32 0, %n
  %len3 = alloca i32*, align 8
  store i32* %len, i32** %len3
  %len4 = load i32*, i32** %len3
  %calltmp = call i1 @"prime?"(i32 %negsign, i32* %len4)
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
  ret i1 true

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

define void @getit(i32 %ka, i32* %len, i32* %n) {
entry:
  %len1 = alloca i32*, align 8
  store i32* %len, i32** %len1
  %len2 = load i32*, i32** %len1
  %calltmp = call i1 @"prime?"(i32 %ka, i32* %len2)
  %len3 = alloca i32*, align 8
  store i32* %len, i32** %len3
  %len4 = load i32*, i32** %len3
  %n5 = alloca i32*, align 8
  store i32* %n, i32** %n5
  %n6 = load i32*, i32** %n5
  call void @myprint(i1 %calltmp, i32* %len4, i32* %n6)
  ret void
}
