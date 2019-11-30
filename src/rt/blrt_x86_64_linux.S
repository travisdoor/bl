    .globl 	_start

_start:
    xorl 	%ebp,%ebp   
    movq 	0(%rsp),%rdi 
    leaq 	8(%rsp),%rsi  
    leaq 	8(%rsp,%rdi,8),%rdx  
    call	__os_start
    movq 	%rax,%rdi       
    movl 	$60,%eax        
    syscall
    int3
