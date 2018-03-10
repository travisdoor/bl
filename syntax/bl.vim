" Vim syntax file
" Language: Biscuit 
" Maintainer: Martin Dorazil 
" Latest Revision: 22 Feb 2018

if exists("b:current_syntax")
  finish
endif

syn keyword blKey public struct as module behavior impl use extern return if else while loop break continue enum
syn keyword blType i32 i64 u32 u64 f32 f64 void char string bool ptr u8 i8
syn keyword blBool true false 
syn keyword blConst self 
syn keyword blTodo contained TODO FIXME NOTE TEST HACK

syn region blString start='"' end='"'

syn region blComment start='/\*' end='\*/' contains=blTodo
syn match blComment "//.*$" contains=blTodo

syn match blPreproc "#run"

syntax match blNumber "\v<\d+>"
syntax match blNumber "\v<\d+\.\d+>"

" Operators
syn match blOperator	"\(<<\|>>\|[-+*/%&^|<>!=]\)="
syn match blOperator	"<<\|>>\|&&\|||\|++\|--\|->"
syn match blOperator	"[.!~*&%<>^|=,+-]"
syn match blOperator	"/[^/*=]"me=e-1
syn match blOperator	"/$"
syn match blOperator "&&\|||"
syn match blOperator	"[][]"

syn match blCustomParen transparent "(" contains=cParen contains=cCppParen
syn match blCustomFunc "\w\+\s*(\@=" contains=cCustomParen

syn match blCustomDot    "\." contained
syn match blCustomMemVar "\(\.\|->\)\h\w*" contains=blCustomDot


let b:current_syntax = "bl"

hi def link blKey Keyword 
hi def link blType Type 
hi def link blNumber Constant 
hi def link blString Constant 
hi def link blConst Constant 
hi def link blBool Boolean
hi def link blComment Comment 
hi def link blOperator Operator 
hi def link blCustomFunc Function
hi def link blCustomMemVar Function
hi def link blPreproc PreProc 
