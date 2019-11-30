    .section	 __TEXT,__text
    .global 	_start

_start:
    xorl 	%ebp,%ebp   
    movq 	0(%rsp),%rdi 
    lea 	8(%rsp),%rsi  
    call	__os_start
    movq 	%rax,%rdi       
    movl 	$60,%eax        
    syscall
    int3
