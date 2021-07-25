 AREA .text, CODE, ARM64
 EXPORT dcCallbackThunkEntry
dcCallbackThunkEntry PROC
 mov x10, sp
 stp x29, x30, [sp, #-208 ]!
 mov x29, sp
 add x11, x29 , #16
 stp x0, x1, [x11, #0 ]
 stp x2, x3, [x11, #16]
 stp x4, x5, [x11, #32]
        stp x6, x7, [x11, #48]
 stp d0, d1, [x11, #64]
 stp d2, d3, [x11, #80]
      stp d4, d5, [x11, #96]
        stp d6, d7, [x11, #112]
 eor x12, x12, x12
 stp x10,x12,[x11, #128]
 str x12, [x11, #144]
 mov x0 , x9
 add x1 , x29 , #16
 add x2 , x29 , #184
 ldr x3 , [x9 , #40]
 ldr x11, [x9 , #32]
 blr x11
 and w0, w0, #255
 cmp w0, 'f'
 b.eq dcCall_arm64_retf
 cmp w0, 'd'
 b.eq dcCall_arm64_retf
dcCall_arm64_reti
 ldr x0, [x29, #184]
 b dcCall_arm64_ret
dcCall_arm64_retf
 ldr d0, [x29, #184]
dcCall_arm64_ret
 ldp x29, x30, [sp], #208
 ret
 ENDP
 END
