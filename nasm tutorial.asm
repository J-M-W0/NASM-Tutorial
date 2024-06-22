———————————————————————————————————————————————
; Chapter 1: Introduction
; 1.1 What is NASM?
; 	The Netwide Assembler, NASM, is an 80x86 and x86-64 assembler designed for portability and modularity.
;	Its syntax is designed to be simple and easy to understand, similar to the syntax in the Intel Software Developer Manual with minimal complexity.	
———————————————————————————————————————————————
; Chapter 2: Running NASM 
; 2.1 NASM Command-Line Syntax
;  To assemble a file, you issue a command of the form:
	nasm -f <format> <filename> [-o <output>]
;
; For example:
 	nasm -f elf myfile.asm
; will assemble 'myfile.asm' into an ELF object file 'myfile.o'
; 	
; And 
 	nasm -f bin myfile.asm -o myfile.com
; wil assemble 'myfile.asm' into a raw binary file 'myfile.com'
; 
; To produce a listing file, 
 	nasm -f coff myfile.asm -l myfile.lst
; with the hex codes output from NASM displayed on the left of the original sources
; 
; To get further usage instructions:
 	nasm -h
; 
; If you use Linux, but aren't sure whether your system is 'a.out' or ELF:
 	file nasm
; if your system is ELF, then it shows below
;	nasm: ELF 32-bit LSB executable i386 (386 and up) Version 1
; and you shall use the option `-f elf` when produce object files.
; 		
; if your system is a.out, then it shows below
; 	nasm: Linux/i386 demand-paged executable (QMAGIC)
; and you shall use the option `-f aout` instead.
;
; To pre-include a file
	nasm myfile.asm -p myinc.inc
; same as placing the directive %include "myinc.inc" at the start of the file.
;
; To pre-define a macro
	nasm myfile.asm -dFOO=100
; same as %define FOO 100
; To un-define a macro
 	nasm myfile.asm -dFOO=100 -uFOO
; 	
; Display version info
	nasm -v
; 
; Specifying Multipass Optimization
; Using the -O option (capital O), you can tell NASM to carry out different levels of optimization. 
; 	-O0		No optimization. All operands take their long forms, if a short form is not specified, except conditional jumps.	
;
; 	-O1		Minimal optimization. As above, but immediate operands which will fit in a signed byte are optimized, unless the long form is specified. 
;			Conditional jumps default to the long form unless otherwise specified. 
;
; 	-Ox		Multipass optimization. (NOTE: here is the actual letter 'x').
;			Minimize branch offsets and signed immediate bytes, 
;			overriding size specification unless the `strict` keyword has been used.
;
; 	-Ov 	At the end of assembly, print the number of passes actually executed.
;
; Enable(-w) or Disable(-W) Assembly Warnings.
;

; 2.2 Quick Start for NASM Users
; 2.2.1 NASM is Case-Sensitive.

; 2.2.2 NASM requires square brackets for memory references.
; if you want acces to the contents of a memory location then it requires square brackets around the address.
; but if you want access to the address of a variable then it doesn't.
; So the NASM code `mov ax, bar` means exactly in MASM `mov ax, offset bar`.
; And 'bar', 'var' here means exactly same, they're all labels!!
	bar		dw	2
	var:	dw	0
	mov		ax, [bar]
	mov		ax, [bar + var]
	mov		ax, [es:di]
	mov		ax, bar

; 2.2.3 NASM doesn't store variable types.
	bar		dw	0
	mov		word [bar], 2			; So you must specify that `bar` is a WORD.
———————————————————————————————————————————————
; Chapter 3: The NASM Language
; 3.1 Layout of a NASM Source Line
;
; 		label:	instruction operands	; comment
;
; NASM uses backslash (\) as the line continuation character, sp if a line ends with backslash, 
; the next line is considered to be a part of the backslash-ended line.
; 
; NASM places no restrictions on white space within a line: labels may have white space before them, 
; or instructions may have no space before them, or anything. The colon after a label is also optional.
; 
; Valid characters in labels are letters, numbers, _, $, #, @, ~, ., and ?. The only characters which 
; may be used as the first character of an identifier are letters, ., _ and ?
; An identifier may also be prefixed with a $ to indicate that it is intended to be read as an identifier 
; and not a reserved word. Maximum length of an identifier is 4095 characters.
; 
; The instruction field may contain any machine instruction: Pentium and P6 instructions, FPU instructions, 
; MMX instructions and even undocumented instructions are all supported.
; 
; NASM does not use the gas–style syntax in which register names must be prefixed by a % sign.
; 
; For x87 floating-point instructions, NASM accepts a wide range of syntaxes
	fadd 		st1				; this sets st0 := st0 + st1
	fadd		st0, st1		; so does this
	fadd		st1, st0		; this sets st1 := st1 + st0
	fadd		to st1			; so does this


; 3.2 Pseudo-Instruction
; 3.2.1 Dx: Declaring Initialized Data 
; `DB`, `DW`, `DD`, `DQ`, `DT`, `DO`, `DY`, `DZ` (collectively "Dx" in this documentation) are used.
;   1		 2	    4		8	   16     32	  64	    128	bytes.
; which corresponds to 'BYTE', 'WORD', 'DWORD', 'QWORD', 'TWORD', 'OWORD', ...
; and DT, DO, DY and DZ do not accept integer numeric constants as operands
	db		0x55					; just one byte 0x55, one bye == 2 hexadecimal numbers.
	db		0x55, 0x56, 0x57		; three bytes in succession.
	db		'a', 0x55 				; character constants are OK.
	db		'hello', 13, 10, '$'	; so are string constants.
	dw		0x1234					; 0x34 0x12
	dw		'a'						; 0x61 0x00
	dw		'ab'					; 0x61 0x62
	dw		'abc'					; 0x61 0x62 0x63 0x00
	dd		0x12345678				; 0x78 0x56 0x34 0x12
	dd		1.234567e20				; floating-point constant
	dq		0x123456789abcdef0		; 8-byte constant
	dq		1.234567e20				; double-precision float
	dt		1.234567e20				; extended-precision float
; A `?` argument to declare uninitialized storage:
	db		?						; uninitialized

; 3.2.2 RESB and Friends: Declaring Uninitialized Data
; `RESB`, `RESW`, `RESD`, `RESQ`, `REST`, `RESO`, `RESY`, `RESZ` are designed to be used in \
; BSS section of a module, they declare uninitialized storage space.
	buffer:			resb		64		; reserve 64 bytes.
	wordvar:		resw		1		; reserve 1 word (2 bytes).
	realarray:		resq		10		; array of ten reals.
; Since NASM 2.15, the MASM syntax of using ? and DUP in the Dx directives is also supported. 
	buffer:			db		64 dup (?)
	wordvar:		dw		?
	realarray:		dq		10 dup (?)

;3.2.3 INCBIN: Including External Binary Files
; This can be handy for (for example) including graphics and sound data directly into a game executable file.
; It can be called in one of these three ways"
	incbin	"file.dat"				; include the whole file.
	incbin	"file.dat", 1024		; skip the first 1024 bytes.
	incbin	"file.dat", 1024, 512	; skip the first 1024 bytes and include maximal 512 bytes.
; INCBIN is both a directive and a standard macro, the standard macro version searches for the file \
; in the include file search path and adds the file to the dependency lists. 
; This macro can be overridden if desired.

; 3.2.4 EQU: Defining Constants
; It defines a symbol to a given constant value: when EQU is used, the source line must contain a label.
; This definition is absolute, and cannot change later.
	message		db		'hello, world'
	msglen		equ		$ - message	; 12

; 3.2.5 TIMES: Repeating Instructions or Data
; The TIMES prefix causes the instruction to be assembled multiple times.
	zerobuf:	times 64 db 0
	buffer:		db 'hello, world'
				times 64 - $ + buffer db ' '		; times 64-12	db ' '
				; which will store exactly enough spaces to make the total length of buffer up to 64.
	times 100	movsb		; trick for trivial unrolled loops.


; 3.3 Effective Addresses
; effective addresses in NASM are enclosed in square brackets.
	wordvar	dw	123
			mov	ax, [wordvar]
			mov	ax, [wordvar + 1]
			mov ax, [es:wordvar + bx]
			mov	ax, [ebx * 5]					; [ebx + ebx + ebx + ebx + ebx]
			mov	ax, [label1 * 2 - label2 ]		; [label1 + label1 - label2]


; 3.4 Contants
; NASM understands four different types of constant: numeric, character, string and floating-point.
; 3.4.1 Numeric Constants
; ASM allows you to specify numbers in a variety of number bases
; decimal
	mov	ax, 200
	mov	ax, 0200
	mov	ax, 0200d
	mov	ax, 0d200
; hex
	mov	ax, 0c8h
	mov	ax, 0xc8
	mov	ax, 0hc8
	mov	ax, $0c8		; $0 meas hex, too
; octal
	mov	ax, 310q
	mov	ax, 310o
	mov	ax, 0o310
	mov	ax, 0q310
; binary
	mov	ax, 11001000b
	mov	ax, 1100_1000b
	mov	ax, 1100_1000y
	mov	ax, 0b1100_1000
	mov	ax, 0y1100_1000

; 3.4.2 Character Strings
; A character string consists of up to eight characters enclosed in either single quotes ('...'), \ 
; double quotes ("...") or backquotes (`...`). Single or double quotes are equivalent to NASM \
; (except of course that surrounding the constant with single quotes allows double quotes to \ 
; appear within it and vice versa).
; The following escape sequences are recognized by backquoted(`...`) strings:
	\'				; single quote 
	\"				; double quote
	\`				; backquote
	\\				; backslash
	\?				; question mark
	\0				; NUL (ASCII 0)
	\a				; BEL (ASCII 7)
	\b				; BS (ASCII 8)
	\t				; TAB (ASCII 9)
	\n				; LF (ASCII 10)
	\v				; VT (ASCII 11)
	\f				; FF (ASCII 12)
	\r				; CR (ASCII 13)
	\e				; ESC (ASCII 27)
	\xFF			; Up to 2 hexadecimal digits	;not case-sensitve
	\u1234			; 4 hexadecimal digits - unicode character (UTF-8)
	\u12345678		; 8 hexadecimal digits - unicode character (UTF-8)
	; https://www.compart.com/en/unicode/U+263A
	db				`\u263a`			; UTF-8 white smiley face (U+263A)
	db				`\xE2\x98\xBA`		; UTF-8 white smiley face
	db				0xe2, 0x98, 0xba	; UTF-8 white smiley face (UTF-8 Encoding)

; 3.4.3 Character Constants
; A character constant consists of a string up to eight bytes long, used in an expression context. 
; It is treated as if it was an integer.
; A character constant with more than one byte will be arranged with `little-endian order`:
	mov		eax, 'abcd'		; stored as 0x64636261

; 3.4.4 String Constants
; String constants are character strings used in the context of some pseudo-instruction.
; namely the DB family and INCBIN (where it represents a filename).
; A string constant looks like a character constant, only longer. It is treated as a \ 
; concatenation of maximum-size character constants for the conditions. 
	db		'hello'					; string constant
	db		'h', 'e', 'l', 'l', 'o'	; equivalent character constants
;
	dd		'ninechars'				; doubleword string constant
	db		'ninechars', 0, 0, 0	; and really looks like this

; 3.4.5 Unicode Strings
; The special operators __?utf16?__, __?utf16le?__, ... allows definition of Unicode strings.
; They take a string in UTF-8 format and converts it to UTF-16 or UTF-32, respectively.
; Unless the be forms are specified, the output is littleendian.
	%define u(x) __?utf16?__(x)
	%define w(x) __?utf32?__(x)
;
	dw	u('C:\WINDOWS'), 0		; pathname in UTF-16
	dd	w(`A+B = \u206a`), 0	; string in UTF-32

; 3.4.6 Floating-Point Constants
; Floating-point constants are acceptable only as arguments to DB, DW, DD, DQ, DT, and DO, 
; 	or as arguments to the special operators __?float8?__, __?float16?__, __?bfloat16?__, ...
;
; Floating-point constants are expressed in the traditional form: digits, then a period, then  
; 	optionally more digits, then optionally an E followed by an exponent.
;
; The period is mandatory, so that NASM can distinguish between dd 1, which declares an
; 	integer constant, and dd 1.0 which declares a floating-point constant.
;
; NASM also support C99-style hexadecimal floating-point: 
;	0x, hexadecimal digits, period, optionally more hexadeximal digits, 
;	then optionally a P followed by a binary (not hexadecimal) exponent in decimal notation. 
; As an extension, NASM additionally supports the 0h and $ prefixes for hexadecimal,  
; as well binary and octal floating-point, using the 0b or 0y and 0o or 0q prefixes, respectively.
;
; Underscores to break up groups of digits are permitted in floating-point constants as well.
	db	-0.2						; "quarter precision".
	dw	-0.5						; IEEE 745r half precision.
	dd	1.2							; an easy one.
	dd	1.222_222_222				; underscores are permitted.
	dd	0x1p+2						; 1.0 * 2^2 = 4.0 
	dq	0x1p+32						; 1.0 * 2^32 = 4294967296.0
	dq	1.e10						; 10 000 000 000.0
	dq	1.e+10						; synonymous with 1.e10
	dq	1.e-10						; 0.000 000 000 1
	dt	3.141592653589793238462 	; pi
	do	1.e+400						; IEEE 754r quad precision.
; The 8-bit "quarter-precision" floating-point format is sign:exponent:mantissa = 1:4:3 with \ 
; an exponent bias of 7.This appears to be the most frequently used 8-bit floating-point format (minifloat).
; NASM cannot do compile-time arithmetic on floating-point constants.
	mov		rax, __?float64?__(3.141592653589793238462)
	mov 	rax, 0x400921fb54442d18
	%define Inf __?Infinity?__
	%define NaN	__?QNaN?__
			dq	+1.5, -Inf, NaN		; double-precision constants.


; 3.5 Expressions
; Expressions in NASM are similar in syntax to those in C.
; Expressions are evaluated as 64-bit integers which are then adjusted to the appropriate size.
; NASM supports two special tokens in expressions, allowing calculations to involve the current assembly position: the `$` and `$$` tokens:
 	`$`  ; - evaluates to the assembly position at the beginning of the line containing the expression, 
		; 	 so you can code an infinite loop using JMP $.
	`$$` ; - evaluates to the beginning of the current section, so you can tell how far into the section you are by using ($-$$).
; 3.5.1 Conditional Operator
; A boolean value is true if nonzero and false if zero. 
; The operators which return a boolean value always return 1 for true and 0 for false.
	boolean ? trueval : falseval

; 3.5.2 Boolean OR ||
; 3.5.3 Boolean XOR ^^
; 3.5.4 Boolean AND &&

; 3.5.5 Comparison Operators 
 	=、== 	; - compare for equality.
 	!=、<> 	; - compare for inequality.
 	< 		; - compares less than.
 	<= 		; - compares less than or equal.
 	> 		; - compares greater than.
 	>= 		; - compares greater than or equal.
	<=> 	; - evaluates -1 for less than, 0 for equal, 1 for greater than.

; 3.5.6 Bitwise OR |
; 3.5.7 Bitwise XOR ^
; 3.5.8 Bitwise AND &

; 3.5.9 Bit Shift Operators
	<< 		; - left-shift (x2).
	>> 		; -	unsigned right-shift(/2) filled with zeroes.
	<<<		; -	left-shift.
	>>>		; -	signed right-shift filled with copies of the most significant(sign) bit.

; 3.5.10 Addition and Subtraction Operators: + -
; 3.5.11 Multiplication, Division and Modulo
 	* 		; - for multiplication.
 	/ 		; - for unsigned division.
	// 		; - for signed division.
 	% 		; - for unsigned modulo.
	%% 		; - for signed modulo.

; 3.5.12 Unary Operators
 	- 		; - negates (2's complement) its operand.
 	+ 		; - does nothing.
 	~ 		; - bitwise negation (1's complement) of its operand.
 	! 		; - boolean negation.
 	SEG 	; - provides segment address of its operand.


; 3.6 SEG and WRT
; When writing large 16-bit programs, which must be split into multiple segments,  
; it is often necessary to be able to refer to the segment part of the address of a symbol.
; The SEG operator evaluates to the 'preferred' segment base of a symbol,  
; defined as the segment base relative to which the offset of the symbol makes sense.
	mov		ax, seg symbolx
	mov		es, ax
	mov		bx, symbolx
; So the code above will load ES:BX with a valid pointer to the symbol `symbol`.
; 
; Things can be more complex than this: since 16-bit segments and groups may overlap, 
; you might occasionally want to refer to some symbol using a different segment base from the preferred one. 
; NASM lets you do this, by the use of the WRT (With Reference To) keyword.
	mov		ax, weird_seg				; weird_seg is a segment base
	mov		es, ax					
	mov		bx, symbol wrt weird_seg	
; this code above will load ES:BX with a different, but functionally equivalent pointer to the symbol `symbol`.
; 
; NASM supports far calls (inter-segment) and jumps by means of the syntax call segment:offset, 
; where segment and offset both represent immediate values.
	call	(seg procedure) : procedure
	call	weird_seg : (procedure wrt weird_seg)
; the parentheses are just included for clarity. And NASM supports the syntax `call far procedure` 
; as a synonym for the first of the above usages. JMP works identically to CALL in these examples.
; 
; To declare a far pointer to a data item in a data segment, you must code
    dw     	symbol, seg symbol


; 3.7 STRICT: Inhibiting Optimization
; When assembling with the optimizer set to level 2 or higher, NASM will use size specifiers (BYTE, WORD, DWORD, ...) but will give them the smallest possible size. 
; The keyword `restrict` can be used to inhibit optimization and force a particular operand to be emitted in the specified size.
; for example with the optimization on, and in BITS 16 mode,
	push 	dword 0x21		; is encoded in three bytes 0x66 0x6a 0x21
	push	strict dword	; whereas is encoded in six bytes, with a full dword immediate operand 0x66 0x68 0x21 0x00 0x00 0x00
; With the optimizer off, the same code (six bytes) is generated whether the STRICT keyword was used or not.


; 3.8 Critical Expressions
; Although NASM has an optional multi-pass optimizer, there are some expressions which must be resolvable on the first pass. 
; These are called `Critical Expressions`.
; So one thing NASM can't handle is code whose size depends on the value of a symbol declared after the code in question. 
; For example:
	times (var - $) 	db 0
	var:				db 'where am I?'
; NASM will reject this example because it cannot tell the size of the TIMES line when it first sees it. 
; (error: non-constant argument supplied to TIMES)
; NASM rejects these examples by means of a concept called a critical expression, 
; which is defined to be an expression whose value is required to be computable in the first pass, 
; and which must therefore depend only on symbols defined before it. 
; The argument to the TIMES prefix is a critical expression.

; 3.9 Local Labels
; A label beginning with a single period is treated as a local label, 
; which means that it is associated with the previous non-local label.
	label1		; some code, btw, a colon `:` after the label is totally optional.
	.loop
				; some more code.
				jne		.loop
				ret
	label2		; some code.
	.loop			
				; some more code.
				jne		.loop
				ret
	label3		; continued from label1, label2 above.
				; some more code.
				jmp		 label1.loop
; if a label begins with the special prefix `..@`, then it does nothing to the local label mechanism. 
	label1:							; a non-local label.
	.local:							; this is really `label1.local`.
	..@foo:							; this a special symbol, which is neither local nor non-local, \
									; thus can't have any references to any local labels.
	label2:							; another non-local label.
	.local:							; this is really `label2.local`.
				jmp 	..@foo		; this will jump three lines above.
;
; NASM has the capacity to define other special symbols beginning with a double period, for example: \
 		`..start` 		; - is used to specify the entry point in the obj output format, 
 		`..imagebase` 	; - is used to find out the offset from a base address of the current image in the win64 output format.
; So just keep in mind that symbols beginning with a double period are special.
———————————————————————————————————————————————
; Chapter 4: The NASM Preprocessor
; 4.1 Preprocessor Expansions
; 4.1.1 Continuation Line Collapsing
; The preprocessor first collapses all lines which end with a backslash (\) character into a single line.
; Thus:
	%define THIS_VERY_LONG_MACRO_NAME_IS_DEFINED_TO \
			THIS_VALUE
; will work like a single-line macro without the backslash-newline sequence.

; The Normal Way: %define
; Example1:
	%define ctrl 0x1f & 
	%define param(a, b) ((a) + (a) * (b))
; Same:
	mov		byte [(2) + (2) * (ebx)], 0x1f & 'D'
	mov		byte [param(2, ebx)], ctrl 'D'
;
; Example2, if this happens, the preprocessor will only expand the first occurrence of the macro:
	%define a(x) 1 + a(x)
	mov		ax, a(3)
; the macro will only expand once, becoming 1 + a(3)
; 
; Example3, you can also overload single-line macros:
	%define foo(x) 1 + x
	%define foo(x, y) 1 + x * y
; 
; example4, when the expansion of a single-line macro contains tokens which invoke another macro, 
; 			the expansion is performed at invocation time, not at definition time:
	%define a(x) 1 + b(x)
	%define b(x) 2 * x
; thus the code above is legal.
; 
; example5:
	%define isTrue 1
	%define isFalse isTrue
	%define isTrue 0
	var1: 	db isFalse
	%define isTrue 1
	var2:	db isFalse
; in the code above, `var1` is equal to 0 and `var2` is equal to 1.
; This is because, when a single-line macro is defined using %define, it is expanded only when it is called. 
; As isFalse expands to isTrue, the expansion will be the current value of isTrue -- The first time it is called that is 0, and the second time it is 1.
; 
; example6:
	%xdefine isTrue 1
	%xdefine isFalse isTrue
	%xdefine isTrue 0
	var1:	db isFalse
	%xdefine isTrue 1
	var2:	db isFalse
; is this code now `var1` and `var2` both are 1, because now isFalse is expanded at the time that it was defined, rather than when it's called.
; 
; example7, undefine a macro:
	%define foo bar
	%undef foo

; Preprocessor Variables: %assign 
; example1, %assign is used for simple textual substitution of assigning a value to a symbol or macro, so it's a not so powerful macro:
	%assign my_string "Hello, World!"
	%assign my_number 42
	section .data
		message 	db my_string, 0
	section .text
		mov 		eax, my_number
; also you can do %define with the code above.

; Defining Strings: %defstr
; example1, these two are equivalent:
	%defstr t TestString
	%define t 'TestString'

; Defining Tokens: %deftok
;  these two are equivalent:
	%deftok t 'Ttt'
	%define t Ttt

; Concatenating Strings: %strcat
	%strcat alpha "Alpha: ", '12" screen'
	%strcat beta '"foo"\', "'bar'"
; alpha := Alpha: 12" screen 
; beta := "foo"\\'bar'

; String Length: %strlen
	%strlen charcnt 'my string'
; charcnt := 9

; Extracting Substrings: %substr
	%substr s1 'abcd' 1			; 'a'
	%substr s2 'abcd' 2			; 'b'
	%substr s3 'abcd' 3			; 'c'
	%substr s4 'abcd' 2, 2		; 'bc'
	%substr s5 'abcd', 2, -1	; -1 means until the end of the string, 'bcd'
	%substr s6 'abcd', 2, -2	; positive number means length, negative number means position, -2 means one character before the end, 'bc'

; Multi-Line Macros: %macro
	%macro ahha 1				; 1 means take one parameter, multi-parameters are separated through comma `,`
		push 	ebp
		mov		ebp, %1			; %1 means the first parameter. %0 denotes the number of parameters received.
		pop		ebp
	%endmacro
	
	%macro lala 2
		%2: db 	%1
	%endmacro

	lala 'a', letter_a			; letter_a: 	db 'a'
	lala 'ab', string_ab		; string_ab: 	db 'ab'
	lala {13, 10}, crlf			; crlf: 		db 13, 10
;
; Overload machine instructions:
	%macro push 2
		push %1
		push %2
	%endmacro

	push eax			; this is not macro, just simple machine instruction.
	push ebx, ecx		; this is macro
;
; Macro-Local Labels:
	%macro retz 0
		jnz		%%skip
		ret
		%%skip:
	%endmacro
;
; Greedy Macro Parameters:
	%macro writefile 2+
		jmp 	%%endstr
		%%str:	db	%2
		%%endstr:
		mov		dx, %%str
		mov		cx, %%endstr - %%str
		mov		bx, %1
		mov		ah, 0x40
		int 	0x21
	%endmacro

	writefile [filehandle], "hello, world!", 13, 10, 0		; so the `"hello, world!", 13, 10, 0` will be treated whole as %2
; 
; Macro Parameters Range:
	%macro multiparameters1 1-*			; takes 1 to multiple parameters
		db %{ 3 : 5 }
	%endmacro
	%macro multiparameters2 1-*
		db %{ 5 : 3 }					; small trick, %{1:1} gives you the last argument.
	%endmacro

	multiparameters1 1, 2, 3, 4, 5, 6	; db 3, 4, 5
	multiparameters2 1, 2, 3, 4, 5, 6	; db 5, 4, 3
; 
; Default Macro Parameters:
	%macro die 0-1+ "Painful Program Death"
		writefile 	2, %1
		mov			ax, 0x4c01
		int 		0x21
	%endmacro
; 
; Rotating Macro Parameters:
	%macro RotateTest 1-*
		push		%1		; push the first parameter.
		%rotate		1		; positive number rotates leftwards, negative number rotates rightwards.
		push		%1		; push the `second` parameter, the `first` parameter is now the last parameter.
	%endmacro
; 
; Concatenating Macro Parameters:
	%macro key_entry 2
		keypos%1	equ % - keytab
					db 	%2
	%endmacro
	keytab:
		key_entry 	F1, 128 + 1
		key_entry	F2, 128 + 2
; 
; Undefining Multi-Line Macros:
	%macro foo 1-3
		; TODO
	%endmacro
	%unmacro foo 1-3		; must include the correct parameter list, otherwise error.

; Conditional Assembly
	%if <condition1>
		; TODO1
	%elif <condition2>
		; TODO2
	%else 
		; TODO3
	%endif

	%ifdef DEBUG
	%endif 

	%ifndef DEBUG
	%endif

	%ifdef F1
	%elifdef F2
	%else
		%warning "Neither F1 or F2 was defined, asssuming F1"
	%endif

	%ifmacro foo 1-3
	%else
		%error "foo is not defined!"
	%endif
;
; Testing Token Types: %ifid, %ifnum, %ifstr, %iftoken, %ifntoken, %ifempty, %ifenv
	%macro writefile 2-3+
		%ifstr %2
			jmp	%%endstr
			%if %0 = 3					; equality can be represented by `=`
				%%str:	db %2, %3
			%else
				%%str:	db %2
			%endif
			%%endstr:
				mov		dx, %%str
				mov		cx, %%endstr - %%str
		%else
			mov			dx, %2
			mov			cx, %3
		%endif
		mov				bx, %1
		mov				ah, 0x40
		int				0x21
	%endmacro

; Preprocessor Loops: %rep
	Fibonacci:
		%assign i 0
		%assign j 1
		%rep 100
			%if j > 65535
				%exitrep
			%endif
			dw j
			%assign k j+i
			%assign i j
			%assign j k
		%endrep

		fib_numbers	equ	($ - Fibonacci) / 2		; `$ - Fibonacci` calculates the number of bytes, divides by 2 make it a word, it calculates how many Fibonacci numbers were generated.

; Include other files: %include
; Search the Include Path:	%pathsearch
	%pathsearch FOO "foo.bin"
; with `I "bins/"` in the include path it may end up as FOO := "bins/foo.bin"
; 
; Include Standard Macro Package: %use
	%use altreg
	%use 'altreg'
; when a standard macro package is used, a testable single-line macro of the form __USE_package__ is also defined. 
; By far not so useful at all.

; Context Stack is a stack only belongs to the macros, you can create context labels there, which is powerful.
; `%push` and `%pop` will create / remove contexts on the top of the context stack.
; `%$` will define a context label which is only visible to context stack elements.
	%macro repeat 0
		%push	repeat			; usually after calling the macro, it automatically removes itself from the context stack, but with %push now it stays until %pop
		%$begin:
	%endmacro

	%macro until 1
		j%-1	%$begin			; %-1 refers to `ne` if %1 is `e`.
		%pop					; removes the top of the context stack.
	%endmacro

	mov		si, string
	repeat
	add		si, 3
	scasb
	until	e
; the code above would scan every fourth byte of a string in search of the byte in AL, until AL == ES:[E]DI.

; Read an Environmental Variable: %!
	%defstr env	%!'FOO'
; assuming that you haev an environment variable called FOO, now the content of FOO will be a string in `env`

; File Name (String) and Line Number (Numeric Constant) Macros: __FILE__, __LINE__
; Current BITS Mode (Number): __BITS__
; Current Output Format:
	; %ifidn - if two macro identifiers are equal
	%ifidn __OUTPUT_FORMAT__, win32
		%define NEWLINE 13, 10
	%elifidn __OUTPUT_FORMAT__, elf32
		%define NEWLINE 10
	%endif
; __DATE__ and __TIME__ are both strings in ISO 8601 format: "YYYY-MM-DD" and "HH:MM:SS"
; __DATE_NUM__ and __TIME_NUM__ are numeric form: YYYYMMDD and HHMMSS
; __POSIX_TIME__ is a number containing the number of seconds since POSIX epoch, 1 January 1970 00:00:00 UTC, excluding any leap seconds.

; Declaring Structure Data Types (unique, no duplicates in memory)
section .data
	struc example
	    .a resd 1  ; int a - 4 bytes
	    .b resd 1  ; int b - 4 bytes
	    .c resb 1  ; char c - 1 byte
	endstruc
	; Create two instances of 'example'
	my_struct1 resb example_size  			; NASM Assembler automatically creates a variable <struc_name>_size, here example_size := 9
	my_struct2 resb example_size 

section .text
    mov eax, 5    ; Set 'a' to 5
    mov dword [my_struct1 + example.a], eax
    mov eax, 10   ; Set 'b' to 10
    mov dword [my_struct1 + example.b], eax
    mov al, 0x41  ; Set 'c' to 'A'
    mov byte [my_struct1 + example.c], al
    ; Initialize 'my_struct2'
    mov eax, 15    ; Set 'a' to 15
    mov dword [my_struct2 + example.a], eax
    mov eax, 20    ; Set 'b' to 20
    mov dword [my_struct2 + example.b], eax
    mov al, 0x42   ; Set 'c' to 'B'
    mov byte [my_struct2 + example.c], al
    ; Load 'a' from 'my_struct1' into eax
    mov eax, dword [my_struct1 + example.a]
    ; Add 'a' from 'my_struct2' to eax
    add eax, dword[my_struct2 + example.a]
    ; Store the sum in 'a' of 'my_struct1'
    mov dword [my_struct1 + example.a], eax



section .data
	struc example
	    .a:
			resd 1  ; int a - 4 bytes
	    .b:
			resd 1  ; int b - 4 bytes
	    .c resb 1  ; char c - 1 byte
	endstruc
	; Create two instances of 'example'
	my_struct1: 
		istruc example
			; both defining all okay, depends on your taste, one with comma, one without.
		    at example.a
						dd 5
		    at example.b
						dd 10
		    at example.c, db 0x41
		iend
	my_struct2: 
		istruc example
		    at example.a, dd 15
		    at example.b, dd 20
		    at example.c, db 0x42
		iend

section .text
    ; Load 'a' from 'my_struct1' into eax
    mov eax, [my_struct1 + example.a]
    ; Add 'a' from 'my_struct2' to eax
    add eax, [my_struct2 + example.a]
    ; Store the sum in 'a' of 'my_struct1'
    mov [my_struct1 + example.a], eax



section .data
	struc point3D   ; Define a struct called point3D
   	 	.x resd 1   ; Reserve a 4-byte integer for x
    	.y resd 1   ; Reserve a 4-byte integer for y
    	.z resd 1   ; Reserve a 4-byte integer for z
	endstruc
	; Create instances of the struct
	origin: resb point3D_size
	my_point: resb point3D_size

section .text
	; Initialize origin to (0, 0, 0)
	mov dword [origin + point3D.x], 0
	mov dword [origin + point3D.y], 0
	mov dword [origin + point3D.z], 0
	; Initialize my_point to (5, 10, 15)
	mov dword [my_point + point3D.x], 5
	mov dword [my_point + point3D.y], 10
	mov dword [my_point + point3D.z], 15
	; Now, add my_point and origin and store the result in my_point
	mov eax, dword [origin + point3D.x]   ; Load x of origin into eax
	add eax, dword [my_point + point3D.x]  ; Add x of my_point to eax
	mov dword [my_point + point3D.x], eax  ; Store the result in x of my_point
	mov eax, dword [origin + point3D.y]   ; Load y of origin into eax
	add eax, dword [my_point + point3D.y]  ; Add y of my_point to eax
	mov dword [my_point + point3D.y], eax  ; Store the result in y of my_point
	mov eax, dword [origin + point3D.z]   ; Load z of origin into eax
	add eax, dword [my_point + point3D.z]  ; Add z of my_point to eax
	mov dword [my_point + point3D.z], eax  ; Store the result in z of my_point


; ALIGN (padding with NOPs, often used in code section) and ALIGNB (padding with 0, often used in data section): data alignment
section .data
	; An unaligned byte of data
	unalign_byte 	db 0x1
	; Align the next data on a 4-byte boundary
	align 4
	aligned_dword 	dw 0x1234

section .text
	; An unaligned instruction
	unalign_instruction:
	    mov eax, 0
	; Align the next instruction on a 16-byte boundary
	align 16
	aligned_instruction:
	    mov ebx, 0

section .data
	; An unaligned byte of data
	unalign_byte_b 	db 0x1
	; Align the next data on a 4-byte boundary
	; It does this by inserting padding bytes (0s) until the next 4-byte boundary. 
	alignb 4
	aligned_dword_b dw 0x1234

———————————————————————————————————————————————
; Chapter 5: Standard Macro Package
	%use altreg

	process1:
		mov		r0l, r3h		; mov al, bh
		ret
; not so useful, after using this macro package, 
; R[0-3]H stands for AH, CH, DH, BH, R[0-3]L stands for AL, CL, DL, BL
; and R[8-15]L stands for the old R[8-15]B.

———————————————————————————————————————————————
; Chapter 6: Assembler Directives
; Specifying Target Processor Mode
	[BITS 16]
	[BITS 32]
	[BITS 64]
; the `bin` output format defaults to 16-bit mode.

; Defining a section named `haha` (both are equivalent):
	section haha
	[section haha]

; Defining absolute labelsa
section .data
    absolute_data absolute 0x12345678
    ; Defines a label called "absolute_data" (no colons after it!) with an absolute address of 0x12345678
	; this address will be physical address if in real mode and virtual address in protected mode.
	; However, just be reminded, it does not set value for the memory, so if you add `db 0` next line, 
	; it won't set the value of address 0x12345678 to be `0`, rather on the next address of the .data section.

section .text
        mov eax, [absolute_data]
        ; Access the data represented by the absolute address of the "absolute_data" label

; Importing Symbols from Other Modules: extern
; You can declare the same variable as EXTERN more than once: NASM will quietly ignore the second and later redeclarations. 
	extern _printfa						; single argument is expanded in assembly stage.
	extern _sscanf, _fscanf				; multiple arguments are expanded in preprocessing stage.
	extern _variable : wrt dgroup

; Exporting Symbols to Other Modules: global
; The GLOBAL directive applying to a symbol must appear before the definition of the symbol. 
	global _main
	_main:
		; TODOs
; The elf object format, lets you specify whether global data items are functions or data: 	
	global hashlookup:function, hashtable:data
; you can also add visiblity keywords
	global hashtable:data hidden		; symbols cannot be seen outside of its module.
	global hashtable:data internal		; temporarily consider it the same as hidden.
	global hashtable:data default		; symbols can be seen and used from everywhere.
	global hashtable:data protected		; symbols can be seen everywhere but cannot be overridden by symbols with the same name in other modules, but doesnot prevent from being modified.
; we can also control the size of the data associated with the symbol:
	global hashtable:data (hashtable.end - hashtable)
	hashtable:
		db 1, 2, 3
	.end:
; make a constant symbol which cannot be changed
	global c
	section .data
		c 	equ 42
; or
	global c
	section constant_section nowrite
		c	db 42


; Defining Common Data Areas: common
	common intvar 4
; Same as:
	global intvar
	section .bss
		intvar 	resb 	4

	common array 100:4
; Same as:
	global array
	second .bss
		align	4
		array	resb	100

———————————————————————————————————————————————
; Output Formats
; Binary File Program Origin: ORG
	org		0x0100
	dd		label1
label1:
; it is allowed to use a label in your assembly code before it's defined, this is called `forward reference`.
; the code above will define the value in address 0x0100-0x0104 to be the address of `label1`.

; bin Extensions to the SECTION Directive:
	section .data align=16
; this code above specifies that the section .data must be aligned on a 16-byte boundary, must be the power of 2.
; it ensures that the starting address of the .data section is a multiple of 16. 

; Multisection Support for the `bin` format:
;	- Sections may be designated `progbits` or `nobits`. Default is `progbits` (except .bss, which defaults to `nobits`, of course).
;	- Sections can be aligned at a specified boundary following the previous section with `align=`, or at an arbitrary byte-granular position with `start=`.
;	- Sections can be given a virtual start address, which will be used for the calculation of all memory references within that section with `vstart=`.
;	- Sections can be ordered using `follows=<section>` or `vfollows=<section>` as an alternative to specifying an explicit start address.
;	- Any code which comes before an explicit SECTION directive is directed by default into the `.text` section.
; 	- If an ORG statement is not given, `ORG 0` is used by default.
; 	- The .bss section will be placed after the last progbits section, unless start=, vstart=, follows=, or vfollows= has been specified.
; 	- All sections are aligned on `dword`(align=4) boundaries, unless a different alignment has been specified.
; 	- NASM creates the `section.<secname>.start` for each section, which may be used in your code.
	section A1 start=0x1000
; all labels start in section A1 will have the address of `their offsets + 0x1000`, but it does not indicate where the section will be loaded.
	section A2 vstart=0x2000
; all labels start in section A2 will have the address of `their offsets + 0`, but it asssumes that the section will be loaded into address 0x2000.
; 
; The Usgae of `vstart`:
	section A1 vstart=0x7C00
	l1: db 0x34
	    mov ax, 0x7C00
	    mov al, [ax]
 
	section A2
	l2: db 0x34
	    mov ax, 0x0000
	    mov al, [ax]

; `-f elf32` or `-f elf64` generates a default output file of extension of `.o`, and `-f elf` is synonym for `-f elf32`.

; `elf` Extensions to the SECTION Directive
; available qualifiers are:
;   - `alloc` defines the section to be one which is loaded into memory when the program is run. 
;	- `noalloc` defines it to be one which is not, such as an informational or comment section.
;
;   - `exec` defines the section to be one which should have execute permission when the program is run. 
;	- `noexec` defines it as one which should not.
;
;  	- `write` defines the section to be one which should be writable when the program is run. 
;	- `nowrite` defines it as one which should not (read-only).
;
; 	- `progbits` defines the section to be one with explicit contents stored in the object file: an ordinary code or data section, 
;	- for example, `nobits` defines the section to be one with no explicit contents given, such as a BSS section.
;
;	- `align=`, used with a trailing number as in obj, gives the alignment requirements of the section.
;	- `tls` defines the section to be one which contains thread local variables.
;
;The defaults assumed by NASM if you do not specify the above qualifiers are:
	section .text    progbits  alloc   exec    nowrite  align=16 
	section .rodata  progbits  alloc   noexec  nowrite  align=4 
	section .lrodata progbits  alloc   noexec  nowrite  align=4 
	section .data    progbits  alloc   noexec  write    align=4 
	section .ldata   progbits  alloc   noexec  write    align=4 
	section .bss     nobits    alloc   noexec  write    align=4 
	section .lbss    nobits    alloc   noexec  write    align=4 
	section .tdata   progbits  alloc   noexec  write    align=4    tls 
	section .tbss    nobits    alloc   noexec  write    align=4    tls 
	section .comment progbits  noalloc noexec  nowrite  align=1 
	section other    progbits  alloc   noexec  nowrite  align=1

———————————————————————————————————————————————
; Mixed-Size Instructions
; a - address, o - operand.
; `a16`, `a32`, `a64` are prefixes to specify an operation's mode.
	a32 lodsb
; would loads from [DS:ESI] instead of [DS:SI] in 16-bit mode.
; These prefixes can be used with CMPSx, SCASx, LODSx, STOSx, MOVSx, INSx, OUTSx, etc.
; it can also force the particular one of SP, ESP, RSP to be the satck pointer.

; PUSH and POP, when applied to segment registers in 32-bit mode,
	o16 push ss
	o16 push ds
; this will saves a doubleword of stack space by fitting two segment registers into the space which would normally be consumed by pushinhg one.
; you can also use `o32` prefix to force the 32-bit behavior in 16-bit mode.
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
———————————————————————————————————————————————
;    ORG 0x100:
;
; Imagine you are writing a book, but for some reason, you decided to start writing from page 100 instead of page 1. 
; Any reference you make to the pages in your book would be relative to page 100. 
; So if you say, "Go forward 10 pages," you would end up on page 110, not page 11. 
; That's essentially what ORG 0x100 does. It tells the assembler, "Hey, let's start counting from 100 (in hexadecimal) rather than 0." 
; So if your code references a label, it thinks that label is positioned relative to address 0x100.
;
; If you wrote org 0x100 in your assembly code and then loaded that code at memory address 0x500, 
; the first instruction after the org directive will be at memory address 0x600, not 0x100.
;
; Remember, the org directive tells the assembler to assemble the following instructions as if they start from the specified address. 
; So, if you use org 0x100, the assembler will calculate all offsets as if the section starts at 0x100.
;
; However, when you actually load the code into memory, it will go where you put it. 
; So, if you load your code at 0x500, that's the real, physical address where your code starts in memory. 
; The org 0x100 doesn't change this.
;
; This means that if you had a label immediately after org 0x100 and you referenced that label in your code, the assembler would use an offset of 0x100 for that label, 
; because of the org directive. But if you then load your code at 0x500, the actual, physical address of that label in memory would be 0x600 (because 0x500 + 0x100 = 0x600).;
;
;
;    section .text vstart=0x100:
;
; Now let's imagine you're organizing a large party at a hall. 
; The hall is your computer's memory, and each section (.text, .data, .bss, etc.) is a different group of people at the party (coders, data, uninitialized data, etc.). 
; You as an organizer (the linker/loader) decide where each group should be located in the hall. 
; If you say, "Coders, please go to the area starting at position 100," that's essentially what vstart=0x100 does. 
; It's telling the linker or loader, "Please put this section in the memory at address 0x100."
;
; So, the key difference is ORG changes where the assembler thinks it is in the program while assembling, 
; whereas vstart tells the linker/loader where to put the code or data in memory when it's being loaded for execution.
;
; The vstart directive in NASM, which sets the virtual start address of a section, 
; is generally used when creating object files that will be linked together to form an executable such as an ELF file. 
; The linker or loader takes care of placing the section at the correct virtual address in memory when the program is loaded for execution. 


———————————————————————————————————————————————
———————————————————————————————————————————————
———————————————————————————————————————————————

