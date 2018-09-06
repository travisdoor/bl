; ModuleID = 'main'
source_filename = "main"

%Foo.0 = type { i32 }

define void @test(%Foo.0*) {
init:
  %foo = alloca %Foo.0*
  store %Foo.0* %0, %Foo.0** %foo
  br label %entry

entry:                                            ; preds = %init
  br label %exit

exit:                                             ; preds = %entry
  ret void
}

define i32 @main() {
init:
  %ret = alloca i32
  %f = alloca %Foo.0
  br label %entry

entry:                                            ; preds = %init
  %tmp = getelementptr %Foo.0, %Foo.0* %f, i32 0
  call void @test(%Foo.0* %tmp)
  store i32 0, i32* %ret
  br label %exit

exit:                                             ; preds = %entry
  %tmp1 = load i32, i32* %ret
  ret i32 %tmp1
}

