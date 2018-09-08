; ModuleID = 'main'
source_filename = "main"

define i32 @main() {
init:
  %ret = alloca i32
  %arr = alloca [32 x i32]
  %arr2 = alloca [800 x i32]
  br label %entry

entry:                                            ; preds = %init
  store i32 0, i32* %ret
  br label %exit

exit:                                             ; preds = %entry
  %tmp = load i32, i32* %ret
  ret i32 %tmp
}

