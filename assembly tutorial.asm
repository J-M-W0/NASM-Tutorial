——————————————————————————————————————————————————————
	A Simply Guide
——————————————————————————————————————————————————————
; What is Assembly Language
; 	Each personal computer has a microprocessor that manages the computer's arithmetical, logical, and control activities.
; 	Each family of processors has its own set of instructions for handling various operations 
;	such as getting input from keyboard, displaying information on screen and performing various other jobs. 
;	These set of instructions are called 'machine language instructions'.
; 	A processor understands only machine language instructions, which are strings of 1's and 0's.
;	However, machine language is too obscure and complex for using in software development.
;	So, the low-level assembly language is designed for a specific family of processors that represents 
;	various instructions in symbolic code

; Basic Features of PC Hardware
; 	The main internal hardware of a PC consists of `processor`, `memory`, and `registers`. 
; 	Registers are processor components that hold data and address. 
;	To execute a program, the system copies it from the external device into the internal memory. 
;	The processor executes the program instructions.

; Addressing Data in Memory
; 	The process through which the processor controls the execution of instructions is referred as 
;	the `fetch-decode-execute cycle` or the `execution cycle`. 
;	It consists of three continuous steps: 
;		· Fetching the instruction from memory.	
;		· Decoding or identifying the instruction.	
;		· Executing the instruction.
;
; 	The processor may access one or more bytes of memory at a time. 
;	Let us consider a hexadecimal number 0x0725. This number will require two bytes of memory. 
;	The most significant byte is 0x07 and least significant byte is 0x25. 
;	The processor brings the value 0x0725 from register to memory, 
;	it will transfer 0x25 first to the lower memory address and 0x07 to the next memory address.
;
; 	There are two kinds of memory addresses
; 		absolute address			- a direct reference of specific location.
; 		segment address (or offset)	- starting address of a memory segment with the offset value.

; Download NASM on Linux 
; 	1. Check The netwide assembler (NASM) website for the latest version.
; 	2. Download the Linux source archive `nasm-X.XX.ta.gz`.
; 	3. Unpack the archive into a directory which creates a subdirectory nasm-X. XX.
; 	4. cd to nasm-X.XX and type `./configure`. 
;		This shell script will find the best C compiler to use and set up Makefiles accordingly.
; 	5. Type `make` to build the nasm and ndisasm binaries.
; 	6. Type `make install` to install nasm and ndisasm in /usr/local/bin and to install the man pages.
——————————————————————————————————————————————————————
; Assembly - Basic Syntax
; An assembly program can be divided into three sections − 
; 	the data section,
; 	the bss section and
; 	the text section.
; 
; The `data` Section
; 	The data section is used for declaring initialized data or constants.
;	This data does not change at runtime.
; 	You can declare various constant values, file names, or buffer size, etc., in this section.
; 	The syntax for declaring data section is -
		section .data	
; The `bss` Section
; 	The bss section is used for declaring variables. 
;	The syntax for declaring bss section is −
		section .bss
; The `text` Section
; 	The text section is used for keeping the actual code.
; 	This section must begin with the declaration `global _start`, which tells the kernel where the program execution begins.
; 	The syntax for declaring text section is −
		section .text
			global _start
		_start:

; The Hello World Program in Assembly
;
	section .text
		global _start		; must be declared for linker (ld)
	_start:					; tells the linker entry point
		mov		edx, len	
		mov		ecx, msg	
		mov		ebx, 1		; file descriptor (stdout)
		mov		eax, 4		; system call number (sys_write)
		int 	0x80		; call kernel
		
		mov		eax, 1		; system call number (sys_exit)
		int 	0x80		; call kernel
	
	section .data
		msg		db	'Hello, World!', 0x0a
		len		equ	$ - msg
;
; firstly, after `nasm -f elf hello.asm` you'll have an object file called 'hello.o', 
; to link the object file and create an executable file named 'hello', type `ld -m elf_i386 -s -o hello hello.o`,
; finally execute the program by `./hello`.
——————————————————————————————————————————————————————
; Assembly - Registers
; To speed up the processor operations, the processor includes some internal memory storage locations, which are called `registers`.
; The registers store data elements for processing without having to access the memory. \
;	A limited number of registers are built into the processor chip.
;
; Groups of registers
;	· general registers
;			- data registers
;			- pointer registers
;			- index registers
; 	· control registers
; 	· segment registers

; 32-bit vs. 64-bit registers
;		with a 32-bit register one can address 2^32 memory addresses (about 4 GB of RAM), \
;		while 64-bit registers can address 2^64 addresses (17.2 billion GB of RAM). 

; General Registers

			64-bit		Lower 32-bit		Lower 16-bit	Higher 8-bit	Lower 8-bit
			rax			eax					ax				ah				al		(Accumulator)
			rbx			ebx					bx				bh				bl		(Base)
			rcx			ecx					cx				ch 				cl		(Counter)
			rdx			edx					dx				dh				dl		(Data)
			rsi			esi					si			
			rdi			edi					di
			rbp			ebp					bp
			rsp			esp					sp						
			r8			r8d					r8w				r8b
			r9			r9d					r9w				r9b
			r10			r10d				r10w			r10b
			r11			r11d				r11w			r11b
			r12			r12d				r12w			r12b
			r13			r13d				r13w			r13b
			r14			r14d				r14w			r14b
			r15			r15d				r15w			r15b
			  
; The instruction pointer `eip` and `flags` register have been extended to 64 bits (`rip` and `rflags`).
; The x64 processor also provides several sets of floating-point registers:
; 		eight 80-bit x87 registers.
; 		eight 64-bit MMX registers (These registers overlap with the x87 registers).
; 		The original set of eight 128-bit SSE registers is increased to sixteen.

; Data Registers
; AX is the primary accumulator, it is used in input/output and most arithmetic instructions. \
;	For example, in multiplication operation, one operand is stored in EAX or AX or AL register \
;	according to the size of the operand.
; BX is known as the base register, as it could be used in indexed addressing.
; CX is known as the count register, as the ECX, CX registers store the loop count in iterative operations.
; DX is known as the data register, it is also used in input/output operations with AX for \
;	multiply and divide operations involving large values.
;
; Pointer Registers
; IP (Instruction Pointer) 
;	- The 16-bit IP register stores the offset address of the next instruction to be executed. \
;	IP in association with the CS register (as CS:IP) gives the complete address of the current instruction \
;	in the code segment.
;
; SP (Stack Pointer)
; 	− The 16-bit SP register provides the offset value within the program stack.
;	SP in association with the SS register (SS:SP) refers to be current position of data or address within the program stack.
;
; BP (Base Pointer)
; 	− The 16-bit BP register mainly helps in referencing the parameter variables passed to a subroutine. \
; 	The address in SS register is combined with the offset in BP to get the location of the parameter. \
;	BP can also be combined with DI and SI as base register for special addressing.
; 
; Index Registers
; SI (Source Index)
; 	− It is used as source index for string operations.
; DI (Destination Index)
;	 − It is used as destination index for string operations.
; 
; Control Registers
; The 32-bit instruction pointer register and the 32-bit flags register combined are considered as the control registers.
		16-bit Flags Register	
		Flags:					O 	D 	I 	T 	S 	Z 		A 		P 		C
		Bit-no.:	15	14	13	12	11	10	9	8	7	6	5	4	3	2	1	0
; The common flag bits are:
; 		Overflow Flag (OF)
; 			- It indicates the overflow of the leftmost bit of data after a signed arithmetic operation.
; 		Direction Flag (DF)
; 			− It determines left or right direction for moving or comparing string data. 
;				0 for left-to-right, 1 for right-to-left.
; 		Interrupt Flag (IF)
; 			- It determines whether the external interrupts like keyboard entry, etc., 
;				are to be ignored or processed. 
;				0 for disabling external interrupt, 1 for enabling.
; 		Trap Flag (TF)
; 			− It allows setting the operation of the processor in single-step mode. 
;				The DEBUG program we used sets the trap flag, 
;				so we could step through the execution one instruction at a time.
; 		Sign Flag (SF)
; 			- It shows the sign of the result of an arithmetic operation. 
;				0 for positive value, 1 for negative value.
; 		Zero Flag (ZF)
; 			- It indicates the result of an arithmetic or comparison operation. 
;				0 for non-zero result, 1 for zero result.
; 		Auxiliary Carry Flag (AF)
; 			- Used for specialized arithmetic. 
;				The AF is set when a 1-byte arithmetic operation causes a carry from bit 3 into bit 4. 
; 		Parity Flag (PF)
; 			- It indicates the total number of 1-bits in the result from an arithmetic operation. 
;				0 for even numbers of 'ones', 1 for odd numbers of 'ones'.
; 		Carry Flag (CF)
; 			- It contains the carry of 0 or 1 from the leftmost bit after an arithmetic operation. 
;				It also stores the contents of last bit of a shift or rotate operation.
;
; Segment Registers
; Code Segment 
;	− It contains all the instructions to be executed. 
;		A 16-bit Code Segment register or CS register stores the starting address of the code segment.
; Data Segment 
;	− It contains data, constants and work areas. 
;		A 16-bit Data Segment register or DS register stores the starting address of the data segment.
; Stack Segment 
; 	− It contains data and return addresses of procedures or subroutines. 
;		It is implemented as a 'stack' data structure. 
;		The Stack Segment register or SS register stores the starting address of the stack. 
; Apart from the DS, CS and SS registers, 
;	there are other extra segment registers - ES (extra segment), FS and GS, 
;	which provide additional segments for storing data.
——————————————————————————————————————————————————————

============================================================================
	Real Things with Deepness
============================================================================
; A 1.44MB floppy disk has 80 tracks, 2 heads, and 18 sectors per track, 
; resulting in a total of 2880 sectors. 
; The cylinder count is not applicable for floppy disks as they do not have a physical cylinder structure.

; BCD - Binary-Coded Decimal, in BCD each decimal is represented by a 4-bit binary number.
; For example, the decimal number 1234 can be represented in BCD as 0x12, 0x34, because 
; 0x12 (18) represents 8-bit hexadecimal of which 0010 0001.
; 
;
; ASCII Table
;		DEC  	HEX		Symbol
; 		48		0x30	0
; 		49		0x31	1
;		50		0x32	2
;		.		.		.
; 		.		.		.
;		.		.		.
; 		65		0x41	A
; 		66		0x42	B
;		67		0x43	C
;		.		.		.
; 		.		.		.
;		.		.		.
;		97		0x61	a
; 		98		0x62	b
;		99		0x63	c
;		.		.		.
; 		.		.		.
;		.		.		.
; 
; 
; After `mov AX, 0x1234`, AL := 0x34, AH := 0x12		
; 
;
; What is unpacked BCD value?
; the decimal number 1234 can be representedas the unpacked BCD value {0x01, 0x02, 0x03, 0x04},
; where each byte represents a single digit. Or as the packed BCD value {0x12, 0x34}.
; 
; Are table1, table2 and table3 equivalent?
	table1 	dw 	0x0010, 0x0010, 0x0010
	table2 	dw 	0x10, 0x10, 0x10
	table3 	dw 	0x00101010
; answer: table1 and table2 are equivalent, just table1 uses leading zeroes for each value, and table2 omitted the leader zeroes,
;		however, table3 is not.
; 
;
; What is the difference between EBP and ESP?
; In x86 assembly language, EBP and ESP are two important registers used to manage the call stack and access data stored on the stack.
; EBP is used as a frame pointer to access parameters and local variables within a function or subroutine,
; 	when a function is called, the current value of EBP is pushed onto the stack, and EBP := ESP, 
; 	this creates a frame pointer that points to the base of the current stack frame, allowing access to parameters and local variables.
; ESP is used to point to the top of the stack, which is the memory location of the most recently pushed value,
;	ESP is updated whenever a value is pushed or poped from the stack.
	my_function:
		push ebp
		mov	ebp, esp
		; ,,,
		mov esp, ebp
		pop	ebp
		ret
; 
; Difference between `dword [mem_loc]` and `dword ptr [mem_loc]`?
		mov		byte ptr [mem_loc], 0x01
		mov		dword [mem_loc], 0x01
; both above are the same, 
; however, `mov 	dword ptr [mem_loc], 0x01` is not correct, because ptr explicitly specifies the size of the data.
; shoud rather be `mov	dword ptr [mem_loc], 0x00000001`
; 
; Difference between es:di and es:edi ? 
; es:di is used for addressing 16-bit words, while es:edi is used for addressing 32-bit doublewords.
; for 8-bit bytes please always add 'byte [location]'. 
;
; Protected System Mode & User Mode ?
; 
; What is protection mode?
; 
; What is 64-bit mode?
============================================================================
; 								x86 Assembly
; 
; When referring to x86 we address the complete range of x86-based processors (since the original Intel 8086 in 1978), 
; which includes: IA-32 assembly, x86-64 and Intel 64.
; 
; Note, that there is a separate 64-bit instruction set, the IA-64 (Itanium). 
; It was meant to replace the x86 line, but did not gain as much popularity as anticipated, 
; so this replacement did not occur. 
============================================================================
; 								x86 Basics
============================================================================
; x86 Family
; 
; The name x86 is derived from the fact that many of Intel's early processors had names ending in "86". 
; 
; As of 2009, x86 primarily refers to IA-32 (Intel Architecture, 32-bit) and/or x86-64, the extension to 64-bit computing. 
; 
; Intel x86 Microprocessors
; 		8086 / 8087 	(1978)
; 		8088			(1979)
;  		80186 / 80187	(1982)
; 		80286 / 80287	(1982)
; 		80386			(1985)
; 		80486			(1989)
; 		Pentium			(1993)
; 		Pentium Pro		(1995)
; 		Pentium II		(1997)
; 		Pentium III		(1999)
; 		Pentium 4		(2000)
; 		Core			(2006)
; 		Core 2			(2006)
; 		i Series		(2008)
; 		Celeron			(first model 1998)
; 		Xeon			(first model 1998)
;
; AMD x86 Compatible Microprocessors
; 		Athlon
; 		Turion
;  		Duron
; 		Sempron
; 		Opteron
; ============================================================================
; x86 Architecture and Register Description


; In total, the x86 architecture has 8 General-Purpose Registers (GPR), 6 Segment Registers,
; 	1 Flags Register and 1 Instruction Pointer. (16 registers)
; 64-bit x86 has additional registers.
; 
; GPR (16-bit naming conventions)
; 	1. Accumulator register (AX)
; 	2. Counter register (CX)
; 	3. Data register (DX)
; 	4. Base register (BX)  
; 	5. Stack Pointer register (SP)
; 	6. Stack Base Pointer register (BP)
; 	7. Source Index register (SI)
; 	8. Destination Index register (DI)
; The order in which they are listed here is for a reason: 
; 	it is the same order that is used in a `push-to-stack` operation, which will be covered later.
; 
; All registers can be accessed in 16- and 32-bit mode. 
; In 32-bit mode, a prefix `E`(extended) is added, e.g. (EAX) is the 32-bit accumulator.
; Similarly, in the 64-bit version, the `E` is replaced with `R`(register), e.g. (RAX) the 64-bit register now.
; 
; Segment Registers
; 	1. Stack Segment 	(SS)
;	2. Code Segment 	(CS)
; 	3. Data Segment 	(DS)
; 	4. Extra Segment	(ES)
; 	5. F Segment 		(FS)	- pointer to more extra data
; 	6. G Segment 		(GS)	- pointer to still more extra data
; 
; EFLAGS Register
; It is a 32-bit register used as a collection of bits representing Boolean values 
; to store the results of operations and the state of the processor. 
;	+ —————————————————————————————————————————————————————————————————————————————	+
;	|	Flags:		0	0	0	0	0 	0 	0 	0 	0 	0 	ID	VIP	VIF	AC	VM	RF 	|
;	|	Bit-no.:	31	30	29	28	27	26	25	24	23	22	21	20	19	18	17	16	|
;	+ —————————————————————————————————————————————————————————————————————————————	+
;	| 	Flags:		0	NT	`IOPL`	OF 	DF 	IF 	TF 	SF 	ZF 	0	AF 	0	PF 	1	CF	|
;	| 	Bit-no.:	15	14	13	12	11	10	9	8	7	6	5	4	3	2	1	0	|
;	+ —————————————————————————————————————————————————————————————————————————————	+
;
; 0. Carry Flag, CF : 
;		set if the last arithmetic operation carried (addition) or borrowed (subtraction) a bit beyond the size of the register.
;
; 2. Parity Flag, PF : 
;		set if number of '1' bits in the least significant byte is a multiple of 2.
;
; 4. Adjust Flag, AF : 
;		carry bit of BCD arithmetic operations.
;
; 6. Zero Flag, ZF : 
;		set if the result of an operation is zero.
;
; 7. Sign Flag, SF : 
;		set if the result of an operation is negative.
;
; 8. Trap Flag, TF : 
;		set if step by step debugging.
;
; 9. Interruption Flag, IF : 
;		set if the processor could accept hardware interrupts.
;
; 10. Direction Flag, DF : 
;		'1' - decrementing pointer, i.e. reading memory backwards, '0' - reversed.
;
; 11. Overflow Flag, OF : 
;		set if signed arithmetic operations result in a value too large for the register to contain.
;
; 12. - 13. I/O Privilege Level Field, IOPL : 
;		I/O privilege level of the current process.
;
; 14. Nested Task Flag, NT : 
;		controls chaining of interrupts, set if the current process is linked to the next process.
;
; 16. Resume Flag, RF : 
;		response to debug execeptions.
;
; 17. Virtual-8086 Mode. VM : 
;		set if 8086 compatibility mode.
;
; 18. Alignment Check, AC : 
;		set if alignment checking of a memory reference is done.
;
; 19. Virtual Interrupt Flag, VIF : 
;		virtual image of IF.
;
; 20. Virtual Interrupt Pending Flag, VIP : 
;		set if an interrupt is pending.
;
; 21. Identification Flag, ID : 
;		support CPUID instruction if can be set.
; 
; Instruction Pointer
; 	the EIP register contains the address of the next instruction to be executed if no branching is done.
; 	EIP can only be read through the stack after a call instruction.



; Memory
; The x86 architecture is little-endian, meaning that multi-byte values are written least significant byte first.
; e.g., the 32-bit double word 0x12345678 would be written as 0x78 0x56 0x34 0x12 when doing a memory dump. 


; Addressing modes
; Register addressing
		mov			ax, bx
		mov			ax, 1
		mov			ax, 0x001F
; Direct memory addressing
		.data
			my_var	dw	0xABCD
		.code
			mov		ax,  word [my_var]				; AX := 0xABCD
; Direct offset addressing
		byte_table	db	0x11, 0x19, 0x23, 0x47
		mov			al, byte [byte_table + 2]		; AL := 0x23
; Register Indirect
		mov			ax, word [di] 					; The registers used for indirect addressing are BX, BP, SI, DI


; General-purpose registers (64-bit naming conventions)
; 64-bit x86 adds 8 more general-purpose registers, named R8, R9, R10 and so on up to R15. 
;
; 	R8 - R15 are the new 64-bit registers.
; 	R8D - R15D are the lowermost 32 bits of each register.
;	R8W - R15W are the lowermost 16 bits of each register.
;	R8B - R15B are the lowermost 8 bits of each register.
;
; As well, 64-bit x86 includes SSE2, so each 64-bit x86 CPU has at least 8 registers (named XMM0–XMM7) 
; that are 128 bits wide, but only accessible through SSE instructions.
; If the processor supports AVX, as newer Intel and AMD desktop CPUs do, 
; then each of these registers is actually the lower half of a 256-bit register (named YMM0–YMM7), 
; the whole of which can be accessed with AVX instructions for further parallelization. 


; Stack
; the stack is a Last In First Out (LIFO) data structure.
	mov	ax, 0x0001
	mov	bx, 0x0002
	mov	cx, 0x0003
	
	push ax
	push bx
	push cx
	
	call some_func_requires_arguments
	
	pop cx
	pop bx
	pop ax
; it is usually used to pass arguments to functions or procedures and also to keep track of control flow when the call instruction is used.
; another common use of the Stack is temporarily saving registers. 


; CPU operation modes
; Real Mode
; a holdover from the original Intel 8086. 
; You generally won't need to know anything about it (unless you are programming for a DOS-based system or, more likely, writing a boot loader that is directly called by the BIOS). 
; The Intel 8086 accessed memory using 20-bit addresses. 
; But, as the processor itself was 16-bit, Intel invented an addressing scheme that provided a way of mapping a 20-bit addressing space into 16-bit words.
; In Real Mode, a segment and an offset register are used together to yield a final memory address. 
; The value in the segment register is multiplied by 16 (shifted 4 bits to the left) and the offset is added to the result. This provides a usable address space of 1 MB (0xFFFFF). 
; 
; Protected Mode
;1. flat memory model
; 	normally used in modern 32-bit operating systems (such as Windows, Linux, ...),
; 	also any register can be used in addressing, and segment registers are generally unused in flat mode.
;2. multi-segment memory model
; 	using a 32-bit register to address memory, the program can access (almost) all of the memory in a modern computer, 
; 	the `CS`, `DS`, and `ES` registers are used to point to the difference 'chunks' of memory.
;
; Long Mode 
; 	The term "Long Mode" refers to the 64-bit mode. 
; ============================================================================
; Comments
; In assembly, comments are usually denoted by a semicolon `;` 
; although GAS uses `#` for single line comments and `/* ... */` for block comments.
; ============================================================================
; 128-bit, 256-bit and 512-bit (SSE/AVX)
; 64-bit x86 includes SSE2 (an extension to 32-bit x86), which provides 128-bit registers for specific instructions. 
; Most CPUs made since 2011 also have AVX, a further extension that lengthens these registers to 256 bits. 
; Some also have AVX-512, which lengthens them to 512 bits and adds 16 more registers. 
; 		XMM0 – 7			: SSE2 and newer.
; 		XMM8 - 15			: SSE3 and newer and AMD (but not Intel) SSE2.
; 		YMM0 - 15			; AVX, each inclues the corresponding XMM as its lower half.
; 		ZMM0 - 15			; AVX-512F, each includes the corresponding YMM as its lower half.
; 		ZMM16 - 31			; AVX-512F, not addressable in narrower modes unless AVX-512VL is implemented.
;		XMM16 - 31			; AVX-512VL, each is the lower quarter of the corresponding ZMM. 
; 		YMM16 - 31			; AVX-512VL, each is the lower half of the corresponding ZMM.


; Addressing memory
; 8086 and 80186
; CS:IP (Code Segment: Instruction Pointer) represents the 20-bit address of the physical memory from where the next instruction for execution will be picked up. 
; Likewise, SS:SP (Stack Segment: Stack Pointer) points to a 20-bit absolute address which will be treated as stack top (8086 uses this for pushing/popping values). 
; 
; Protected Mode (80286+)
; As ugly as this may seem, it was in fact a step towards the protected addressing scheme used in later chips.
; The 80286 had a protected mode of operation, in which all 24 of its address lines were available, allowing for addressing of up to 16 MiB of memory. 
; In protected mode, the CS, DS, ES, and SS registers were not segments but selectors, 
; pointing into a table that provided information about the blocks of physical memory that the program was then using.
; In this mode, the pointer value CS:IP = 0x0010:0x2400 is used as follows: 
; 	CS := 0x0010 is an offset into the selector table, pointer at a specific selector, which would have a 
;	24-bit value to indicate the start of a memory block, a 16-bit value to indicate how long the blcok is, 
; 	and flags to specify whether the block can be written or is currently physically in memory and other info.
;	Let's say that the memory block pointed to actually starts at the 24-bit address 0x164400, 
;	the actual address referred to then is 0x164400 + 0x2400 = 0x166800, if the selector also includes
; 	that the blcok is 0x2400 bytes long, the reference would be to the byte immediately following that block,
; 	which would cause an exeception: the operating system should not allow a program to read memory that it doesnot own.
; 
; 32-Bit Addressing
; 32-bit addresses can cover memory up to 4 GiB in size.
; This means that we don't need to use offset addresses in 32-bit processors.
; Instead, we use what is called the "Flat addressing" scheme, where the address in the register directly points to a physical memory location.
; The segment registers are used to define different segments, so that programs don't try to execute the stack section, 
; and they don't try to perform stack operations on the data section accidentally.  
============================================================================
; 								x86 Instruction Set
============================================================================
; x86 Instructions
; 
; Conventions
; 1. template for instructions that take no operands:
; 			lnstr
; 2. template for instructions that take 1 operand:
; 			lnstr arg
; 3. template for instructions that take 2 operands:
; 			lnstr dest, src						(Intel Syntax)
; 			lnstr src, dest						(GAS Syntax)
; 4. template for instructions that take 3 operands:
; 			lnstr dest, src, aux					(Intel Syntax)
; 			lnstr aux, src, dest					(GAS Syntax)
; 
; Suffixes
; some instructions, especially when built for non-Windows platforms (i.e. Unix, Linux, etc.), 
; require the use of suffixes to specify the size of the data which will be the subject of the operation.
; 	b (byte) = 8 bits
; 	w (word) = 16 bits
; 	l (long) = 32 bits
; 	q (quad) = 64 bits
; An example of the usage with the `mov` instruction on a 32-bit architecture, GAS syntax:
; 		movl $0x000F, %eax
; However GREAT NEWS: on Intel syntax you DON'T have to use the suffix. 
; Based on the register name and the used immediate value the compiler knows which data size to use.
; ============================================================================
; Data Transfer Instructions (Intel Syntax)

; Move data
; 1. syntax
	`mov 	dest, src`
; 2. operands
; 	dest : register, memory
; 	src	: register, memory, immediate value 
; 3. modified flags : None
; 

; Data swap
; 1. syntax
 	`xchg	dest, src`
; 2. operands
; 	any combination of register and memory operands, except that you cannot exchange two memory blocks.
; 3. modified flags : None
; 4. miscellaneous : 
; 	(1) if one of the operands is a memory address, then the operation has an implicit 'lock' prefix,
; 		that is, the exchange operation is atomic.
; 	(2) another thing worth noting is that the common `nop` do-nothing instruction, 0x90, 
;		is the opcode for `xchg  eax, eax`

; Data swap based on comparison
; 1. syntax
	`cmpxchg	  arg1, arg2`
; 2. operands
; 	arg1 : register, memory
; 	arg2 : register
; 3. modified flags : ZF(set if arg1 is equal to AL/AX/EAX), CF, PF, AF, SF, OF
; 4. miscellaneous :
; 	(1) this instruction has an implicit operand: the AL/AX/EAX depending on the size of arg1.
; 		firstly, campares arg1 to AL/AX/EAX
; 		secondly, if they are equal, arg1 := arg2, otherwise AL/AX/EAX := arg1
; 	(2) unlike `xchg`, there's no implicit 'lock' prefix, if required to be atomic, 'lock' has to be prefixed.

; Move with zero extension
; 1. syntax
	`movzx	dest, src`
; 2. operands
; 	dest : register
; 	src	: register, memory
; 	ALSO 'dest' has to be larger than 'src'.
; 3. modified flags : None
; 4. miscellaneous : 
; 	(1) the remaining bits in 'dest' that are not provided by 'src' are filled with zeroes.

; Move with sign extension
; 1. syntax
	`movsx	dest, src`
; 2. operands
; 	the same as `movzx`.
; 3. modified flags : None
; 4. miscellaneous : 
; 	(1) pads the remaining bits not provided by 'src' with the sign-bit (MSB) of 'src'.

; Move string
; 1. syntax
 	`movsb`	
; 2. operands 
; 	No explicit operands, but 
; 	· ECX determines the number of iterations.
; 	· ESI specifies the source address.
; 	· EDI specifies the destination address.
; 	· DF determines the direction, which could be altered by `cld` and `std`.
; 3. modified flags : None
; 4. miscellaneous : 
; 	(1) moving byte-by-byte until ECX is zero.
;	(2) if DF is cleared, then ESI and EDI are both incremented after operation, otherwise decremented.

; Move word
; 1. syntax
	`movsw`
; 2. operands
; 	the same as `movsb`.
; 3. modified flags : None
; 4. miscellaneous : 
; 	(1) moving word-by-word (2 bytes each time).
; 	(2) the rest is the same as `movsb`.

; Load effective address
; 1. syntax
	`lea		dest, src`
; 2. operands
; 	dest : register
; 	src	 : register, memory, immediate
; 3. modified flags : None
; 4. miscellaneous : 
; 	(1) it does basically the same thing as `mov`, just rather than loading the contents of the address 
; 		it loads the address itself.
; 	(2) it is also general-purpose unsigned integer arithmetic because no FLAGS will be modified via this way.
; 	(3) here's a small example
		my_array		db	1, 2, 3, 4, 5
		lea			eax, [my_array + 2]		; compute the address of the third element.
		
; Conditional move
; 1. syntax
	`cmov{suffix} 	dest, src`
; Suffixes
;	a 	- above							CF = 0 and ZF = 0
;	ae 	- above or equal				CF = 0
;	b 	- below							CF = 1
;	be 	- below or equal				CF = 1 and ZF = 1
; 	c 	- carry							CF = 1
; 	e 	- equal							ZF = 1
; 	g 	- greater than					ZF = 0 and SF = OF 		
; 	ge 	- greater than or equal			SF = OF
; 	l 	- less than						SF != OF	
; 	le 	- less than or equal			ZF = 1 or SF != OF
; 	na 	- not above						CF = 1 or ZF = 1
;	nae	- not above nor equal			CF = 1
; 	nb 	- not below						CF = 0
; 	nbe	- not below nor equal			CF = O and ZF = 0
;	nc 	- not carry 					CF = 0
; 	ne 	- not equal						ZF = 0
; 	ng 	- no greater than				ZF = 1 or SF != OF
; 	nge	- no greater than nor equal		SF != OF
; 	nl 	- no less than					SF = OF
; 	nle	- no less than nor equal		ZF = 0  and SF = OF
; 	no 	- not overflow					OF = 0
; 	np 	- not parity					PF = 0
; 	ns 	- not sign						SF = 0
; 	nz 	- not zero						ZF = 0
; 	o 	- overflow						OF = 1
; 	p 	- parity						PF = 1
; 	pe	- parity even					PF  = 1		
; 	po 	- parity odd					PF = 0
; 	s 	- sign							SF = 1
; 	z 	- zero							ZF = 1
;
; 2. operands
;	dest : register
;	src	 : register, memory
; 3. modified flags : None
; 4. miscellaneous : 
; 	(1) The cmov instruction needs to be available on the platform 
;		which can be checked by using the `cpuid` instruction. 

; Data transfer instructions of 8086 microprocessor
; General : mov, push, pop, pusha, popa, xchg, xlat
; Input / Output : in, out
; Address transfer instruction : lea, lds, les
; Flags : lahf, sahf, pushf, popf
; ============================================================================
; Control Flow Instructions

; The instruction pointer (EIP) register contains the address of the next instruction to be executed. 
; To change the flow of control, the programmer must be able to modify the value of EIP.

; Test instructions
; 1. syntax
	`test	accumulator, reference`
; 2. operands
; 	accumulator : register(recommend AL/AX/EAX if reference is an immediate), memory
; 	reference	   : register, immediate
; 3. modified flags : SF, ZF, PF, CF, OF
; 4. miscellaneous : 
; 	(1) performs a bit-wise logical 'and' operation of which the result will refer to as the 'commonBits'.
; 	(2) SF = most significant bit (MSB) of the 'commonBits'.
; 	(3) ZF = 1 if 'commonBits' == 0, otherwise 0.
; 	(4) PF = 1 if 'commonBits' has an even number of 1-Bit.
; 	(5) CF, OF automatically cleared to 0

; Compare instructions
; 1. syntax
	`cmp 	minuend, subtrahend`
; 2. operands
; 	minuend 	  : register(only AL/AX/EAX if subtrahend is an immediate), memory
; 	subtrahend : register, memory, immediate
; 3. modified flags : SF, ZF, PF, CF, OF
; 4. miscellaneous : 
; 	(1) performs a comparison operation `minuend - subtrahend`, the result which is called 'difference'.
; 	(2) the EFLAGS register is set in the same manner as a `sub` instruction.

; Jump instructions
; The jump instructions allow the programmer to (indirectly) set the value of the EIP register.
; All of the jump instructions, with the exception of `jmp`, are conditional jumps.
; 
; 1. syntax
	`jmp			destination`
	`j{suffix}		destination`
; 2. operands
; 	destination : register, memory, immediate, label
; 3. modified flags : None
; 4. miscellaneous : 
;
; 	(1) short jump	is within a range of [-128, 127] bytes from the current instruction,
;		it takes less memory and is faster than near or far jump.
		loop1:
			dec rcx
			jmp short 	loop1		
; 	notice that here `short` is optional, because the assembler will automatically choose the shortest jump instruction available based on the distance between the source and destination addresses.
;
;	(2) near jump allows you to transfer control to a location within the same code segment or module.
		loop2:
			dec rcx
			jmp near 		loop2	;`near` is also optional there.
;
; 	(3) far jump allows you to transfer control to a location in a difference code segment or module,
; 		implemented with 'ptr16:16' or 'ptr16:32' operand, which specifies a segment and an offset address.
			jmp far 		[0x1234 : 0x5678]	; far jump to the address 0x1234 : 0x5678	
; 		In this case the operand consists of a segment selector(0x1234) and an offset(0x5678).
; 		Also the `far` is optional.
; 
; 	(4) indirect jump
			section	.data
				jump_table	dq		func1, func2, func3		; define a jump table with pointers to functions.
			section 	.text
				global _start
			_start:
				mov			rcx, 1
				jmp 		qword [jump_table + rcx*8]		; call func2
				
			func1:
				; code for func1 here.
				ret
			func2:
				; code for func2 here.
				ret
			func3:
				; code for func3 here.
				ret

; Jump if counter register is zero
; 1. syntax
	`jcxz		destination`
	`jecxz		destination`
	`jrcxz		destination`

; Function calls (`call` and `ret`)
; 1. syntax
	`call	subroutine_name`
	`ret`
	`ret 	arg`
; 2. operand
; 	subroutine_name : the name of a subroutine or function to be called.
;	arg : immediate, which removes 'arg' bytes from the stack after returning.
; 3. modified flags : None
; 4. miscellaneous : 
; 	(1) when `call` instruction is executed, the CPU pushes the address of the next instruction(EIP) onto the stack
; 		and then jumps to the address of the specified subroutine or function,
; 	(2) the `ret` instruction pops the return address from the top of the stack and jumps to that address.
; 
; Loop instructions
; 1. syntax
	`loop		arg`
	`loope		arg`
	`loopne		arg`
; 2. operand
;	arg : register, memory, immediate, label
; 3. modified flags : None
; 4. miscellaneous : 
; 	(1) decrements ECX and jumps to the address specified by 'arg' unless ECX becomes 0

; Enter and Leave
; 1. syntax
	`enter		arg1, arg2`
	`leave`
; 2. operand
; 	arg1 : immediate16 
;	arg2 : 0, 1, immediate8
; 3. modified flags : None
; 4. miscellaneous : 
;	(1) a stack frame is a block of memory on the call stack that is allocated for a single function call. 
;		It contains all of the information needed to restore the calling environment after the function has completed execution, 
;		including local variables, function parameters, and the return address.
;		When a function is called, a new stack frame is created on top of the current stack frame.
;	(2) arg1 represents the size of the local data to be allocated on the stack in bytes.
;	(3) arg2 represents the nesting level, if a main program calls subroutine A, which then calls subroutine B,
;		the A will haev a nesting level of 0 and B a nesting level of 1.
; 	(4) `enter` creates a stack frame with the specified amount of space allocated on the stack.
; 	(5) `leave` destroys the current stack frame, and restores the previous frame.
; 		i.e. set SP/ESP/RSP := BP/EBP/RBP then pop BP/EBP/RBP.
; 	(6) example
	my_function:
		enter	8, 0		; allocate 8 bytes of local data and set the nesting level to 0
		
		mov		dword [ebp-4], eax	; store a function parameter in a local variable.
		mov		dword [ebp-8], ebx	; store another function parameter in a local variable.
		
		; do some work here, using the local variables.
		
		leave			; deallocate the stack frame.
		ret				; return from the function.

; Retrieve information about the processor
; 1. syntax
	`cpuid`
; 2. initial EAX value
;	EAX := 0x01
;	EAX := 0x02

; Other control instructions
1、`hlt`		
;		halts the processor, execution will be resumed after processing next hardware interrupt, unless 'IF' is cleared.
2、`nop`
;		do nothing, just wastes an instruction cycle each time.
3、`lock`
;		a prefix enforces atomicity when accessing shared memory locations. examples are:
		lock cmpxchg 	dword [mem_loc], ebx
;		along from that, `lock` can also be used with `add, sub, inc, dec, and, or, xor, not, xadd, bts` and so on.
4、`wait` 
; 		synchronize the processor with the floating-point unit (FPU).
;		it waits for any outstanding FPU operations to complete before allowing the processor to continue 
;		executing instructions, typically used in conjunction with `fwait` or `fnop` instructions.
;		`wait` waits for all the FPU operations to complete, but `fwait` rather only waits for the one it follows.
	section .data
		value dd 25.0         ; A floating-point value to compute the square root of

	section .text
		global _start
	
	_start:
		finit                 	; Initialize the FPU
		fld dword [value] 	; Load the value from memory onto the FPU stack
		fsqrt                 	; Compute the square root of the value on top of the stack
		wait                  	; Wait for any outstanding FPU operations to complete

		; Do something with the square root value, such as store it in memory or print it out

		mov 		eax, 1 	; Set the exit code to 1
		xor 		ebx, ebx  ; Clear the ebx register
		int 0x80              	; Invoke the system call to exit the program
; ============================================================================
; Arithmetic Instructions
; For the operands will appear below -
; 	destination : register, memory
; 	source	  : register, memory, immediate
; However, at most could one operand be a memory location.

; Addition and Subtraction
	`add 	destination, addend`
	`sub 	destination, subtrahend`
	
; Unsigned Multiplication
; 1. syntax
	`mul 	multiplicand`
; 2. operand
; 	register, memory
; 3. modified flags : CF, OF
; 4. miscellaneous : 
;	(1) CF is set if the result is too large even to fit in the higher part of product.
;	(2) OF is set if the result is too large to fit in the destination operand.
;	(3) result by mul
		+ ——————————————————————————————————————————————————————————————+
		| width of multiplicand			1-byte	2-byte	4-byte	8-byte	|
		+ ——————————————————————————————————————————————————————————————+
		| corresponding multiplier		AL 		AX		EAX		RAX		|
		| higher part of product		AH		DH		EDX		RDX		|
		| lower  part of product		AL 		AX		EAX		RAX		|
		+ ——————————————————————————————————————————————————————————————+
; Signed Multiplication
; 1. synatx
	`imul 	multiplicand`
	`imul	destination, multiplicand`
	`imul	destination, multiplier, multiplicand`
; 2. operands
; 	already explained.
; 3. modified flags : CF, OF, ZF, SF, PF
; 4. miscellaneous :
; 	(1) CF is the same.
; 	(2) OF is set if the result is too large or too small to fit in the destination operand.
; 	(3) ZF is set if the result is zero.
; 	(4) SF is set if the MSB of the result is set, indicating a negative result.
; 	(5) PF is set if the lower eight bits of the result contain an even number of 1 bits.

; Division
; 1. synatx
	`div	divisor`		; unsigned
	`idiv 	divisor`		; signed
; 2. operand
;	register, memory
; 3. modified flags : CF, ZF, SF, PF
; 4. miscellaneous : 
;	(1) CF := 0 if the quotient fits into the destination operand, otherwise 1
;	(2) ZF is set if the quotient is zero, otherwise 0
;	(3) SF is set according to the quotient.
;	(4) PF is set according to the quotient.
;	(5) results
		+-----------------------------------------------------------------------+
		| wid-th of the divisor		1-byte	2-byte	  4-byte		8-byte	    |
		+-----------------------------------------------------------------------+
		| corresponding dividend	AX		DX:AX	  EDX:EAX		RDX:RAX	    |
		| remainder					AH		DX		  EDX			RDX		    |
		| quotient					AL 		AX		  EAX			RAX		    |
		+-----------------------------------------------------------------------+

; Sign inversion
; 1. syntax
	`neg 	arg`	
; 2. operand
;	arg : register, memory
; 3. modified flags : CF
; 4. miscellaneous : 
; 	(1) arthmetically negates the argument, which gives two's complement neagtion out.
;	(2) CF := 0 if the source operand is 0, otherwise 1

; Carry arithmetic instructions
; 1. syntax
	`adc 	dest, src`	; dest := dest + src + CF
	`sbb 	dest, src`	; dest := dest - src - CF

; In- / Decrement
; 1. synatx
	`inc 	augend`
	`dec 	minuend`
; 2. operands
; 	augend, minuend : register, memory
; 3. modified flags : ZF, SF, OF
; 4. miscellaneous : 
; 	(1) ZF := 1 if the result is zero.
; 	(2) SF is set according to the result.
; 	(3) OF is set if the result causes a signed overflow.
; ============================================================================
; Logic Instructions

; Binary operations
; 1. syntax
	`and	destination, mask_operand`
	`or 	destination, addend`
	`xor	destination, flip`
; 2. operands
; 	destination : register, memory
; 	others : register, memory, immediate
; 3, modified flags : SF, ZF, PF
; 4. miscellaneous : 
; 	(1) SF := MSB
; 	(2) ZF := 1 if result is zero.
; 	(3) PF depends on the result.

; Unary operation
	`not	argument`		; performs a bit-wise inversion of argument.
; ============================================================================
; Shift and Rotate Instructions 
; operands could be -
; 	dest : register, memory
;	cnt	: register (usually ECX), memory, immediate

; Logical Shift instructions (the spaces are always filled with zeros, and the last goes into the carry flag)
; 1. synatx
	`shr	dest, cnt`		; shift right
	`shl	dest, cnt`		; shift left

; Arithmetic Shift instructions (same, but if MSB was originally 1 then for sar it will be filled with 1)
; 1. syntax
	`sar	dest, cnt`		; shift arithmeticcally right, spaces are filled with sign bit.
	`sal	dest, cnt`		; shift arithmeticcally left, same as `shl`

; Extended shift instructions (easily understandable)
;  1. syntax
	`shrd	dest, src, cnt`
	`shld	dest, src, cnt`

; Rotate instructions
; 1. syntax
	`ror	dest, cnt`
	`rol	dest, cnt`

; Rotate with carry instructions
; 1. synatx
	`rcr	dest, cnt`
	`rcl	dest, cnt`
; ============================================================================
; Other Instructions

; Stack instructions
; 1. syntax
	`push	arg`
	`pop	arg`
	`pusha`			; push in this order : AX, CX, DX, BX, SP, BP, SI, DI
	`popa`			; pop in this order : DI, SI, BP, SP, BX, DX, CX, AX
	`popad`			; push all the 32-bit registers rather than 16-bit.
	`popad`			; pop ...
	`pushf`			; retrieve the flags to the stack.
	`popf`			; store the flags from the stack.

; Flags instructions
; 1. syntax
	`sti`			; set IF
	`cli`			; clear IF
	`std`			; set DF
	`cld`			; clear DF
	`stc`			; set CF
	`clc`			; clear CF
	`cmc`			; invert CF
	`sahf`			; stores the content of AH register into the lower byte of the flag register.
	`lahf`			; loads the AH register with the content of the lower byte of the flag register.

; I / O instructions
; 1. syntax
	`in		dest, src`
	`out	dest, src`
; 2. operands
; 	dest	: ususally AX / EAX
; 	src 	: usually DX / EDX

; System instructions (there were added with the Pentium II)
; 1. syntax
	`sysenter`	; causes the processor to enter protected system mode (supervisor mode or "kernel mode"). 
	`sysexit`	; causes the processor to leave protected system mode, and enter user mode.

; Miscellaneous(Misc) instructions
; 1. syntax
	`rdtsc`		; read time stamp counter.
; 2. explanation
;	This instruction reads the value of the time-stamp counter (TSC), which is a 64-bit register 
; 	that keeps track of the number of clock cycles since the processor was reset. 
; 	After invoking this instruction, the value is stored in EDX:EAX
; ============================================================================
; x86 Interrupts
; Interrupts are special routines that are defined on a per-system basis.

; Interrupt instructions
	`int	interrupt_number`

; Types of interrupts
; 1. Hardware interrupts
; 	triggered by hardware devices. For instance, when you type on your keyboard, the keyboard triggers a hardware interrupt.
; 	Hardware interrupts are typically asynchronous - their occurrence is unrelated to the instructions being executed at the time they are raiseed.
; 2. Software interrupts
; 	triggered by the instruction `int`. For example, `int 0x14` triggers interrupt 0x14, 
; 	the processor then stops the current program, and jumps to the code to handle interrupt 0x14
; 3. Exceptions
; 	caused by execeptional conditions in the code which is executing. Like an attempt to divide by zero.
============================================================================
; 								Instruction Extensions
============================================================================
; Instruction extensions
; 
; The instruction set of the original x86 processor, the 8086, has been expanded upon and improved many times since its introduction.
; Among the new features added by these extensions are new registers, more instructions. 
; They have also facilitated the move from 16- to 32-bit (and now from 32- to 64-bit).
; These chapters will be covering the following extensions : 
;	· MMX - by Intel in 1996
;	· SSE - by Intel in 1999
;	· 3D Now! - by AMD in 1998
; ============================================================================
; Floating point
; The ALU(arithmetic logic unit) is only capable of dealing with integer values.
; A highly specialized coprocessor, all part of the FPU(floating-point unit) will allow you to manipulate decimals.
; 
; x87 Coprocessor
; The original x86 family members had a separate math coprocessor that handled floating point arithmetic. 
; The original coprocessor was the 8087, and all FPUs since have been dubbed “x87” chips. 
; Later variants integrated the FPU into the microprocessor itself.
; Having the capability to manage floating point numbers means a few things: 
; 	1. The microprocessor must have space to store floating point numbers.
; 	2. The microprocessor must have instructions to manipulate floating point numbers.
; The FPU, even when it is integrated into an x86 chip, is still sometimes called the “x87” section.
; 
; The FPU has an array of eight registers that can be accessed as a stack.
; `st1` or simply `st` refers to the register that is currently at the top of the stack.
; If eight values were stored on the stack, `st7` refers to last element on the stack (i.e. the bottom). 
; Numbers are pushed onto the stack from memory, and are popped off the stack back to memory.
; There is no instruction allowing to transfer values directly to or from ALU registers.
; The x87 stack can only be accessed by FPU instructions.
; FPU instructions generally will pop the first two items off the stack, act on them, and push the answer back on to the top of the stack. 
; In order to reduce round-off errors, the FPU stack registers are all 80 bits wide. 
; 
; Examples with NASM syntax (calculating the square root of 123.45)
	global _start
	
	section .data
		val:	dq	123.45	; define a quadword (double-precision) floating-point.
	
	section .bss
		res:	resq	1		; reserve 1 quadword for the result.

	section .text
	
	[org	0x7c00]
	
	_start:
		fld		qword [val]	; load value into st(0)
		fsqrt				; compute sqrt of st(0) and store the result in st(0)
		fstp	qword [res]	; store st(0) value
		

; Floating-Point instruction set
	fld, fild, fbld, ……
; ============================================================================
; MMX
; Most of the new instructions are "single instruction, multiple data" (SIMD), meaning that single instructions work with multiple pieces of data in parallel. 
; MMX has a few problems, though: 
;	instructions run slightly slower than the regular arithmetic instructions.
;	the Floating Point Unit (FPU) can't be used when the MMX registers are in use.
;	MMX registers use saturation arithmetic. 

; Saturation Arithmetic
; 	255 + 100 = 255	(never rolls back to 0)
;	200 + 100 = 255
; 	0 - 100 = 0
; 	99 - 100 = 0

; Single Instruction Multiple Data (SIMD) Instructions
; The MMX registers are 64 bits wide, but can be broken down as follows: 
; 	two 32-bit values
; 	four 16-bit values
; 	eight 8-bit values
; The MMX registers cannot easily be used for 64 bit arithmetic. 
; Because let's say that we have 4 bytes loaded in an MMX register: 10, 25, 128, 255. 
; We have them arranged as such: 
; 	MM0:	| 10 | 25 | 128 | 255 |
; And we do the following pseudo code operation:
; 	MM0 + 10
; We would get the following result:
; 	MM0:	| 10+10 | 25+10 | 128+10 | 255+10| = | 20 | 35 | 138 | 255 (Saturation) |
; Using MMX, we are essentially performing 4 additions in the time it takes to perform 1 addition using the regular registers, 
; using 4 times fewer instructions. 

; There are 8 64-bit MMX registers(MM0-7). 
; To avoid having to add new registers, they were made to overlap with the FPU stack register. 
; This means that the MMX instructions and the FPU instructions cannot be used simultaneously.

; MMX instruction set
; Several suffixes are used to indicate what data size the instruction operates on: 
; 	· 'B'yte
; 	· 'W'ord
; 	· 'D'ouble word
; 	· 'Q'uad word
; 	· 'US' - unsigned
; 	· 'S' - signed
	EMMS, MOVD, MOVQ, PACKSSDW, PACKSSWB, PACKUSWB, PADDB, PADDD, PADDSB, 
	PADDSW, PADDUSB, PADDUSW, PADDW, PAND, PANDN, PCMPEQB, PCMPEQD, PCMPEQW, 
	PCMPGTB, PCMPGTD, PCMPGTW, PMADDWD, PMULHW, PMULLW, POR, PSLLD, PSLLQ, 
	PSLLW, PSRAD, PSRAW, PSRLD, PSRLQ, PSRLW, PSUBB, PSUBD, PSUBSB, PSUBSW, PSUBUSB, 
	PSUBUSW, PSUBW, PUNPCKHBW, PUNPCKHDQ, PUNPCKHWD, PUNPCKLBW, PUNPCKLDQ, 
	PUNPCKLWD, PXOR 
; ============================================================================
; SSE (Streaming SIMD Extensions)
; It is essentially the floating-point equivalent of the MMX instructions.
; The SSE registers are 128 bits (XMM0-7), and can be used to perform operations on a variety of data sizes and types. 
; Unlike MMX, the SSE registers do not overlap with the floating point stack. 
; And SSE2 expanded the capabilities of the XMM registers, so they can now be used as: 
; 	· two 64-bit floating points (double-precision)
; 	· two 64-bit integers
; 	· four 32-bit floating points (single-precision)
; 	· four 32-bit integers
; 	· eight 16-bit integers
; 	· sixteen 8-bit characters (bytes)

; Data movement examples
	global _start
	
	section .data
		align 	16							; here `align 16` affects all the lines until next alignment directive or section declaration, only in code segment it affects one line.
		v1:		dd	1.1, 2.2, 3.3, 4.4		; single precision
		v1dp:	dq	1.1, 2.2				; double precision
		v2:		dd	5.5, 6.6, 7.7, 8.8
		v2s1:	dd	5.5, 6.6, 7.7, -8.8
		v2s2:	dd	5.5, 6.6, -7.7, -8.8
		v2s3:	dd	5.5, -6.6, -7.7, -8.8
		v2s4:	dd	-5.5, 6.6, -7.7, -8.8
		num1:	dd	1.2
		v3:		dd	1.2, 2.3, 4.5, 6.7		; no longer 16-byte aligned
		v3dp:	dq	1.2, 2.3				; no longer 16-byte aligned
	
	section .bss
		mask1:	resd	1
		mask2:	resd	1
		mask3:	resd	1
		mask4:	resd	1
	
	section .text
		_start:
			movaps		xmm0, [v1]		; move four 32-bit floats(single-precision) to xmm0
			movaps		xmm1, [v2]		;
			movups		xmm2, [v3]		; need to use `movups` since v3 is not 16-byte aligned, otherwise would be seg fault.
			movss		xmm3, [num1]	; move 32-bit float num1 to the least significant element of xmm3
			movlps		xmm4, [v3]		; mov 64 bits from memory to the lower 64-bit element of xmm4
			movhps		xmm4, [v2]
			movhlps		xmm5, xmm4		; transfer the higher 64 bits of xmm4 to lower 64 bits of xmm5
			movlhps		xmm5, xmm4		; transfer the lower 64 bits of xmm4 to higher 64 bits of xmm5
			
			movaps		xmm6, [v2s1]
			movmskps	eax, xmm6		; extract the sign bits from four 32-bit floats in xmm6 and create 4 bit mask in eax
			mov			[mask1], eax	; should be 8 	- (1000)
			movaps		xmm6, [v2s2]
			mov			[mask2], eax	; should be 12	- (1100)
			movaps		xmm6, [v2s3]
			mov			[mask3], eax	; should be 14	- (1110)
			movaps		xmm6, [v2s4]
			mov			[mask4], eax	; should be 15	- (1111)
			
			movapd		xmm6, [v1dp]	; move two 64-bit floats(double-precision) to xmm6
			movhpd		xmm6, [v1dp+8]	; move a 64-bit float(double-precision) into the higher 64-bit element of xmm6
			movlpd		xmm6, [v1dp]	; move a 64-bit float(double-precision) into the lower 64-bit element of xmm6
			movupd		xmm6, [v3dp]	; Move two 64-bit floats to xmm6 (since v3dp isn't 16-byte aligned)

; Arithmetic example using packed singles
	global _start
	
	section	.data
		v1:		dd	1.1, 2.2, 3.3, 4.4
		v2:		dd	5.5, 6.6, 7.7, 8.8
	
	section	.bss
		v3:		resd	4
	
	section	.text
	_start:
		
		movups	xmm0, [v1]	; load v1 into xmm0
		movups	xmm1, [v2]	; load v2 into xmm1
		
		addps	xmm0, xmm1	; add the 4 numbers in xmm1(from v2)
		mulps	xmm0, xmm1	; multiply the four numbers in xmm1 with the previous results
		subps	xmm0, xmm1	; subtract the four numbers in xmm1 with the previous results
		movups	[v3], xmm0	; store v1 in v3
		ret
; The result values should be:	30.800	51.480	77.000	107.360


; SSE Instruction Set
; 	· SSE 	: added with Pentium III
	Floating-point Instructions:
		ADDPS, ADDSS, CMPPS, CMPSS, COMISS, CVTPI2PS, CVTPS2PI, CVTSI2SS, CVTSS2SI, 
		CVTTPS2PI, CVTTSS2SI, DIVPS, DIVSS, LDMXCSR, MAXPS, MAXSS, MINPS, MINSS, MOVAPS, 
		MOVHLPS, MOVHPS, MOVLHPS, MOVLPS, MOVMSKPS, MOVNTPS, MOVSS, MOVUPS, MULPS, 
		MULSS, RCPPS, RCPSS, RSQRTPS, RSQRTSS, SHUFPS, SQRTPS, SQRTSS, STMXCSR, SUBPS, 
		SUBSS, UCOMISS, UNPCKHPS, UNPCKLPS
	Integer Instructions:
		ANDNPS, ANDPS, ORPS, PAVGB, PAVGW, PEXTRW, PINSRW, PMAXSW, PMAXUB, PMINSW, 
		PMINUB, PMOVMSKB, PMULHUW, PSADBW, PSHUFW, XORPS 
; 	· SSE2 	: added with Pentium 4 
	Floating-point Instructions:
		ADDPD, ADDSD, ANDNPD, ANDPD, CMPPD, CMPSD, COMISD, CVTDQ2PD, CVTDQ2PS, 
		CVTPD2DQ, CVTPD2PI, CVTPD2PS, CVTPI2PD, CVTPS2DQ, CVTPS2PD, CVTSD2SI, CVTSD2SS, 
		CVTSI2SD, CVTSS2SD, CVTTPD2DQ, CVTTPD2PI, CVTTPS2DQ, CVTTSD2SI, DIVPD, DIVSD, 
		MAXPD, MAXSD, MINPD, MINSD, MOVAPD, MOVHPD, MOVLPD, MOVMSKPD, MOVSD, 
		MOVUPD, MULPD, MULSD, ORPD, SHUFPD, SQRTPD, SQRTSD, SUBPD, SUBSD, UCOMISD, 
		UNPCKHPD, UNPCKLPD, XORPD 
	Integer Instructions:
		MOVDQ2Q, MOVDQA, MOVDQU, MOVQ2DQ, PADDQ, PSUBQ, PMULUDQ, PSHUFHW, 
		PSHUFLW, PSHUFD, PSLLDQ, PSRLDQ, PUNPCKHQDQ, PUNPCKLQDQ 
;	· SSE3	: added with later Pentium 4
	ADDSUBPD, ADDSUBPS, HADDPD, HADDPS, HSUBPD, HSUBPS, MOVDDUP, MOVSHDUP, MOVSLDUP 
; 	· SSSE3	: added with Xeon 5100 and early Core 2
	PSIGNW, PSIGND, PSIGNB, PSHUFB, PMULHRSW, PMADDUBSW, PHSUBW, PHSUBSW, PHSUBD, 
	PHADDW, PHADDSW, PHADDD, PALIGNR, PABSW, PABSD, PABSB 
; 	· SSE4.1	: added with later Core 2
	MPSADBW, PHMINPOSUW, PMULLD, PMULDQ, DPPS, DPPD, BLENDPS, BLENDPD, BLENDVPS, 
	BLENDVPD, PBLENDVB, PBLENDW, PMINSB, PMAXSB, PMINUW, PMAXUW, PMINUD, PMAXUD, 
	PMINSD, PMAXSD, ROUNDPS, ROUNDSS, ROUNDPD, ROUNDSD, INSERTPS, PINSRB, PINSRD, 
	PINSRQ, EXTRACTPS, PEXTRB, PEXTRW, PEXTRD, PEXTRQ, PMOVSXBW, PMOVZXBW, PMOVSXBD, 
	PMOVZXBD, PMOVSXBQ, PMOVZXBQ, PMOVSXWD, PMOVZXWD, PMOVSXWQ, PMOVZXWQ, 
	PMOVSXDQ, PMOVZXDQ, PTEST, PCMPEQQ, PACKUSDW, MOVNTDQA 
; 	· SSE4a	: added with Phenom
	LZCNT, POPCNT, EXTRQ, INSERTQ, MOVNTSD, MOVNTSS 
; 	· SSE4.2	: added with Nehalem
	CRC32, PCMPESTRI, PCMPESTRM, PCMPISTRI, PCMPISTRM, PCMPGTQ 
; ============================================================================
; AVX, AVX2, FMA3, FMA4
; ============================================================================
; 3DNow!
; 3DNow! is AMD's extension of the MMX instruction set (K6-2 and more recent) for with floating-point instruction.
; The instruction set NEVER  gained much popularity, 
; and AMD announced on August 2010 that support for 3DNow! will be dropped in future AMD processors, 
; except for two instructions. 
============================================================================
; 								Advanced x86 
; ============================================================================
; Protected Mode
; Modern Operating Systems (Windows, Unix, Linux, BSD, etc...) all operate in protected mode.
; This information will be particularly useful to people who are trying to program kernels or bootloaders. 


; Real Mode Operation
; When an x86 processor is powered up or reset, it is in real mode. 
; In real mode, the x86 processor essentially acts like a very fast 8086. (Only the base instruction set of the processor can be used. )
;
; Real mode memory address space is limited to 1MiB of addressable memory, and each memory segment is limited to 64KiB.
; Real Mode is provided essentially for backwards-compatibility with 8086 and 80186 programs. 


; Protected Mode Operation
; In protected mode operation, the x86 can address 4 GB of address space.
; This may map directly onto the physical RAM (in which case, if there is less than 4 GB of RAM, some address space is unused), 
; or paging may be used to arbitrarily translate between virtual addresses and physical addresses.
; 
; In Protected mode, the segments in memory can be assigned protection, 
; and attempts to violate this protection cause a "General Protection" exception. 
; Protected mode in the 386, amongst other things, is controlled by the Control Registers, which are labelled CR0, CR2, CR3, and CR4. 
; Protected mode in the 286 is controlled by the Machine Status Word. 


; Long Mode
; Long mode was introduced by AMD with the advent of the Athlon64 processor.
; Long mode allows the microprocessor to access 64-bit memory space, and access 64-bit long registers.
; Many 16 and 32-bit instructions do not work (or work correctly) in Long Mode. 
; To unlock the 64-bit capabilities of the chip, the chip must be switched into Long Mode. 


; Entering Protected Mode
; The lowest 5 bits of the control register CR0 contain 5 flags that determine how the system is going to function.
; This register has 1 flag that we are particularly interested in: the "Protected Mode Enable" flag (PE).
; Here are the general steps to entering protected mode: 
; 	1. Create a valid GDT (Global Descriptor Table).
; 	2. Create a 6-bit pseudo-descriptor to point to the GDT
; 	3.	
;		.1 If paging is going to be used, load CR3 with a valid page table, PDBR or PML4.
; 		.2 If PAE (Physical Address Extension) is going to be used, set CR4.PAE = 1.
; 		.3 If switching to long mode, set IA32_EFER.LME = 1.
; 	4. Disable Interrupts (CLI).
; 	5. Load an IDT (Interrupt Descriptor Table) pseudo-descriptor that has a null limit.
;		(this prevents the real mode IDT from being used in protected mode)
; 	6. Set the PE bit (and the PG bit if paging is going to be enabled) of the MSW(Machine Status Word) or CR0 register.
; 	7. Execute a far jump.
;		(in case of switching to long mode, even if the destination code segment is a 64-bit code segment,
;		the offset must not exceed 32-bit since the far jump instruction is executed in compatibility mode)
; 	8. Load data segment registers with valid selector(s) to prevents GF exceptions when interrupts happen.
;	9. Load SS:(E)SP with a valid stack. 
; 	10. Load an IDT pseudo-descriptor that points to the IDT.
; 	11. Enable interrupts.


; Entering Long Mode on a 64-bit x86 processor (x86-64)
; 	1. If paging is enabled, disable paging.
; 	2. If CR4.PAE is not already set, set it.
; 	3. Set IA32_EFER.LME = 1.
; 	4. Load CR3 with a valid PML4 table.
; 	5. Enabling paging.
; 	6. At this point you'll be in compatibility mode. A far jump may be executed to switch to long mode.
; 		However, the offset must not exceed 32-bit.


; Using the CR 32-bit Registers (Many bits of the CR registers only influence behavior in protected mode. )
; 
; CR0
;	+ —31———————————————————————5————4————3————2————1————0— +
;	| PG | ---- RESERVED ---- | NE | ET | TS | EM | MP | PE |
;	+ ————————————————————————————————————————————————————— +
; PE (Protected Environment Flag) - bit 0, puts the system into protected mode when set.
; MP (Monitor Coprocessor Flag) - bit 1, controls the operation of the "WAIT" instruction.
; EM (Emulate Flag) - bit 2, coprocessor instructions will generate an exception when set.
; TS (Task Switch Flag) - bit 3, is set automatically when the processor switches to a new task.
; ET (Extension Type Flag) - bit 4, tells us which type of coprocessor is installed (0 for 80287, 1 for 80387).
; NE (New Exceptions) - bit 5, if clear, FPU exceptions arrive as interrupts; if set, as exceptions.
; PG (Paging Flag) - bit 31, memory paging is enabled when set.
; 
; CR2
; contains a value called the Page Fault Linear Address (PFLA). 
; When a page fault occurs, the address that access was attempted on is stored in CR2. 
; 
; CR3
; The upper 20 bits of it are called the Page Directory Base Register (PDBR).
; Which holds the physical address of the page directory.
; 
; CR4
; contains several flags controlling advanced features of the processor. 


; Paging
; Paging is a special job that microprocessors can perform to make the available amount of memory in a system appear larger and more dynamic than it actually is.
; In a paging system, a certain amount of space may be laid aside on the hard drive (or on any secondary storage) called the 'swap file' or 'swap partition'.
; The virtual memory of the system is everything a program can access like memory, and includes physical RAM and the swap space. 
; The total virtual memory is broken down into chunks(pages) of memory, each usually being 4096 bytes.
; These pages can then be moved around throughout the virtual memory, 
; and all pointers inside those pages will be automatically directed to point to the new locations by referencing them to a global paging directory that the microprocessor maintains.
; The pointer to the current paging directory is stored in the CR3 register. 
; 
; A 'page fault' occurs when the system attempts to read from a page that is marked as "not present" in the paging directory/table,
; or when the system attempts to write data beyond the boundaries of a currently available page,
; or when any number of other errors occur in the paging system.
; When a page fault occurs, the accessed memory address is stored in the CR2 register. 


; Other Modes
; virtual 8086 mode : This is a mode in which application software that was written to run in real mode is executed under the supervision of a protected-mode, multi-tasking OS.
; system management mode : This mode enables the processor to perform system tasks, like power management, without disrupting the operating system or other software.
; ============================================================================
; Global Descriptor Table
; It is a table in memory that defines the processor's memory segments.
; The GDT sets the behavior of the segment registers and helps to ensure that protected mode operates smoothly. 

; GDTR
; The GDT is pointed to by a special register in the x86 chip, the GDT Register, which 48-bit long.
; The lower 16 bits tell the size of the GDT (which is called LIMIT), and SizeOf(GDT) := LIMIT+1 (bytes).
; and the upper 32 bits tell the location of the GDT in memory (which is called BASE), and BASE := starting address.
; 
; To load the GDTR
	lgdt	[gdtr]
; where the parameter [gdtr] is a pointer to 6 bytes(48 bits) of memory containning the desired GDTR value. 
; NOTE that to complete the process of loading a new GDT, the segment registers need to be reloaded (the CS register must be reloaded using a far jump).
	flush_gdt:
		lgdt		[gdtr]
		jmp		0x08 :complete_flushing
	
	complete_flushing:
		mov ax, 0x10
		mov ds, ax
		mov es, ax
		mov fs, ax
		mov gs, ax
		mov ss, ax
		ret

; GDT
; The GDT contains a number of entries called Segment Descriptors.
; Each is 8 bytes long and contains information on the starting point of the segment, the length of the segment, and the access rights of the segment. 
; The following NASM-syntax code represents a single GDT entry: 
	struc gdt_entry
		.limit 		dw 0        ; Segment limit (lower 16 bits)
		.base_low 	dw 0		; Base address (lower 16 bits)
		.base_mid 	db 0        ; Base address (middle 8 bits)
		.access 	db 0        ; Access byte, including descriptor type, privilege level, and segment present flag
		.flags 		db 0        ; Flags byte, including segment size, granularity, and other attributes
		.base_high 	db 0        ; Base address (upper 8 bits)
	endstruc
; ; Example of a GDT entry for a code segment with a base address of 0x00000000, a limit of 0xFFFFF (1MB), and access permissions allowing code execution in ring 0 and 3
	gdt_code:
		gdt_entry .limit 0x000FFFFF, .base 0x00000000, .access 0b1001_1010, .flags 0b0000_1100
; the granularity is a flag which determines whether the segment size should be interpreted in byte units or in page units.
; if it is set, then the segment size is interpreted in units of 4KB pages, otherwise in units of bytes.


; LDT
; Each separate program will receive, from the operating system, a number of different memory segments for use.
; he characteristics of each local memory segment are stored in a data structure called the Local Descriptor Table (LDT). 
; The GDT contains pointers to each LDT. 
; ============================================================================
; Advanced Interrupts

; Interrupt Service Routines (ISR)
; The actual code that is invoked when an interrupt occurs is called the Interrupt Service Routine (ISR).
; At minimum, FLAGS and CS:IP are saved and the ISR's CS:IP loaded; however, some mechanisms cause a full task switch to occur before the ISR begins (and another task switch when it ends).


; The Interrupt Vector Table (IVT)
; In the original 8086 processor (and all x86 processors in Real Mode), the IVT controlled the flow into an ISR.
; The IVT started at memory address 0x00, and could go as high as 0x3FF, for a maximum number of 256 ISRs (ranging from interrupt 0 to 255).
; Each entry in the IVT contained 2 words of data: A value for IP and a value for CS (in that order).
; FOR INSTANCE, let's say we have the following interrupt:
	int 0x14
; when we trigger the interrupt, the processor goes to the 20th(0x14 = 20, and indices start at 0) location in IVT.
; Since each table entry is 4 bytes (2-byte IP, 2-byte CS), the microprocessor goes to [4*0x14] = [0x50], 
; at [0x50] there is the new IP value, and [0x52] is the new CS value.
; In newer x86 models, the IVT was replaced with the Interrupt Descriptor Table. 
; 
; When interrupts occur in real mode, the FLAGS register is pushed onto the stack, followed by CS, then IP.
; The `iret` instruction restores CS:IP and FLAGS, allowing the interrupted program to continue unaffected.
; For hardware interrupts, all other registers must be explicitly preserved (e.g. if an interrupt routine makes use of AX, it should push AX when it begins and pop AX when it ends).


; The Interrupt Descriptor Table (IDT)
; Since the 286 (but extended on the 386), interrupts may be managed by a table in memory called the Interrupt Descriptor Table (IDT).
; The IDT only comes into play when the processor is in protected mode.
; Much like the IVT, the IDT contains a listing of pointers to the ISR routine; however, there are now three ways to invoke ISRs: 
;
; 	1. Task Gates: caused by a task switch, allowing ISR to run in its own context (with its own LDT, etc.).
; 	2. Interrupt Gates: similar to the original interrupt mechanism, placing EFLAGS, CS and EIP on the stack.
; 		The ISR may be located in a segment of equal or higher privilege to the currently executing segment, but not of lower privilege.
; 		(higher privileges are numerically lower, with level 0 being the highest privilege).
; 	3. Trap Gates: identical to interrupt gates, except they do not clear the interrupt flag.
; 
; IDT entry(8-bit) described by NASM-syntax code
	struc idt_entry
		.offset_low 	dw 0    ; Offset of the interrupt handler (lower 16 bits)
		.selector 		dw 0   	; Selector of the interrupt handler code segment
		.reserved 		db 0    ; Reserved byte, always set to 0
		.type 			db 0    ; Type byte, including the type of interrupt gate, the descriptor privilege level, and other attributes
		.offset_high 	dw 0    ; Offset of the interrupt handler (upper 16 bits)
	endstruc
	

; IDT Register (IDTR)
; is 48-bit long. 
; The lower 16 bits are the LIMIT section, and the upper 32 bits are the BASE section.
; 
; To load the IDTR, use
	lidt	[idtr]
; To store the IDTR, use
	sub		esp, 6
	sidt	[esp]		; store the IDTR to the stack.

 
; Interrupt instructions
	`int 	arg`		; calls the specific interrupt.
	`into	arg`		; ... if the Overflow Flag is set.
	`iret`				; returns from an interrupt service routine (ISR). 
	
	
; Disabling Interrupts
; Sometimes it is important that a routine is not interrupted unexpectedly. For this reason, the x86 allows hardware interrupts to be disabled if necessary. 
; The x86 has an interrupt flag (IF) in the FLAGS register. When this flag is set to 0, hardware interrupts are disabled, otherwise they are enabled.
	`cli`				; clear the IF
	`sti`				; set the IF
; Instructions that load values into the FLAGS register (such as `popf` and `iret`) may also modify IF. 
; Also note that this flag does not affect the int instruction or processor exceptions; only hardware-generated interrupts.

; ============================================================================
; Bootloaders
; What is a Bootloader?
; Bootloaders are small pieces of software that play a role in getting an operating system loaded and ready for execution when a computer is turned on.
; To the processor, a bootloader is just another piece of code that it blindly executes.
; 
; On IBM PC compatibles, the first program to load is the Basic Input/Output System (BIOS). 
; The BIOS performs many tests and initialisations, and if everything is OK, the BIOS's boot loader begins.
; Its purpose is to load another boot loader! It selects a disk (or some other storage media) from which it loads a secondary boot loader. 


; The Bootsector
; The first 512 bytes of a disk are known as the 'bootsector' or 'Master Boot Record'(MDR).
; If the bootsector of a disk contains a valid boot sector (the last word of the sector must contain the signature 0xAA55), then the disk is treated by the BIOS as bootable. 


; The Boot Process
; When switched on or reset, an x86 processor begins executing the instructions it finds at address FFFF:0000 (at this stage it is operating in Real Mode)
; 
; Eventually the actual boot loading begins. 
; First the BIOS searches for and initialises available storage media (such as floppy drives, hard disks, CD drives), 
; then it decides which of these it will attempt to boot from. It checks each device for availability (e.g. ensuring a floppy drive contains a disk), 
; then the 0xAA55 signature, in some predefined order (often the order is configurable using the BIOS setup tool). 
; It loads the first sector of the first bootable device it comes across into RAM, and initiates execution. 
; Ideally, this will be another boot loader, and it will continue the job, making a few preparations, then passing control to something else. 


; Technical Details
; A bootloader runs under certain conditions that the programmer must appreciate in order to make a successful bootloader.
; The following pertains to bootloaders initiated by the PC BIOS: 
; 	1. The first sector of a drive contains its boot loader.
; 	2. One sector is 512 bytes — the last two bytes of which must be 0xAA55 (i.e. 0x55 followed by 0xAA), or else the BIOS will treat the drive as unbootable.
; 	3. If everything is in order, said first sector will be placed at RAM address 0000:7C00, and the BIOS's role is over as it transfers control to 0000:7C00 (that is, it JMPs to that address).
; 	4. The DL will contain the drive number that is being booted from, useful if you want to read more data from elsewhere on the drive.
; 	5. The BIOS leaves behind a lot of code, 
;		both to handle hardware interrupts (such as a keypress) and to provide services to the bootloader and OS (such as keyboard input, disk read, and writing to the screen). 
;		You must understand the purpose of the Interrupt Vector Table (IVT), and be careful not to interfere with the parts of the BIOS that you depend on. 
;		Most operating systems replace the BIOS code with their own code, but the boot loader can't use anything but its own code and what the BIOS provides. 
;		Useful BIOS services include int 0x10 (for displaying text/graphics), int 0x13 (disk functions) and int 0x16 (keyboard input).
; 	6. This means that any code or data that the boot loader needs must either be included in the first sector (be careful not to accidentally execute data) 
;			or manually loaded from another sector of the disk to somewhere in RAM. Because the OS is not running yet, most of the RAM will be unused. 
;			However, you must take care not to interfere with the RAM that is required by the BIOS interrupt handlers and services mentioned above.
; 	7. The OS code itself (or the next bootloader) will need to be loaded into RAM as well.
; 	8. The BIOS places the stack pointer 512 bytes beyond the end of the boot sector, meaning that the stack cannot exceed 512 bytes. 
;		It may be necessary to move the stack to a larger area.
; 	9. There are some conventions that need to be respected if the disk is to be readable under mainstream operating systems.
;		For instance you may wish to include a BIOS Parameter Block on a floppy disk to render the disk readable under most PC operating systems.
; 
; Most assemblers will have a command or directive similar to `org 0x7C00` that informs the assembler that the code will be loaded starting at offset 0x7C00.
; The assembler will take this into account when calculating instruction and data addresses.
; If you leave this out, the assembler assumes the code is loaded at address 0 and this must be compensated for manually in the code. 
; 
; Usually, the bootloader will load the kernel into memory, and then jump to the kernel. The kernel will then be able to reclaim the memory used by the bootloader (because it has already performed its job).
; However it is possible to include OS code within the boot sector and keep it resident after the OS begins. 
; 
; Simple bootloader designed for NASM:
	org	0x7c00
	
	jmp short	Start		; jump over the data(the 'short' keyword makes the jmp instruction smaller).
	
	msg:		db	'Hello World! '
	end_msg:
	
	Start:
		mov		bx, 0x000f	; Page 0, colour attribute 15(white) for the int 0x10 calls below.
		mov		cx, 1		; We will want to write 1 character
		xor		dx, dx		; Start at top left corner
		mov		ds, dx		; Ensures ds = 0 (to let us load the message)
		cld					; Ensures DF is cleared (for LODSB)
		
	Print:
		mov		si, msg		; Loads the address of the first byte of the message, 0x7C02 in this case.
	Char:
		; PC BIOS Interrupt 0x10 subfunction 2 (AH := 2) - Set cursor position.
		; BH = page, DH = row, DL = column.
		mov		ah, 2
		int		0x10
		lodsb
		; PC BIOS Interrupt 0x10 subfunction 9 (AH := 9) - Write character and colour.
		; BH = page, AL = character, BL = attribute, CX = character count.
		mov		ah, 9
		int 	0x10
		inc		dl			; advance cursor.
		cmp		dl, 80		; Wrap around edge of screen if necessary.
		jne		Skip
		xor		dl, dl
		inc		dh
		cmp		dh, 25		; Wrap around bottom of screen if necessary.
		jne		Skip
		xor		dh, dh
		
	Skip:
		cmp		si, end_msg	; If we're not at end of message,
		jne		Char			; continue loading characters,
		jmp		Print			; otherwise restart from the beginning of the message.
		
	times 0x0200-2 - ($ - $$)	db 0				; zerofill up to 510 bytes.
	;OPTIONAL:
	;To zerofill up to the size of a standard 1.44MB, 3.5" floppy disk
	;times 1474560 - ($ - $$) db 0
; nasm -f bin -o floppy.img floppy.asm
; While strictly speaking this is not a bootloader, it is bootable, and demonstrates several things: 
; 	· How to include and access data in the boot sector
; 	· How to skip over included data (this is required for a BIOS Parameter Block)
; 	· How to place the 0xAA55 signature at the end of the sector (NASM will issue an error if there is too much code to fit in a sector)
; 	· The use of BIOS interrupts
; ============================================================================
;								Other Commonly Used Instructions
; ============================================================================
; Some common x86-64 instructions categories:
; 	1. Arithmetic and logical instructions.
; 	2. Control flow instructions.
; 	3. Data transfer instructions.
; 	4. String instructions.
; 	5. System instructions.
; 	6. SIMD instructions.
; 	7. Floating-point instructions.


; Bit test
; 1. synatx
	`bt		dest, src`
; 2. operands
; 	dest	: register, memory
; 	src	: register, immediate
; 3. modified flags : ZF, CF
; 4. miscellaneous : 
; 	(1) 'src' represents the bit index (0-31) of the bit to be tested, with can be either a register or an immediate value.
; 		However, if the bit index is in a register, it must be in the lower 8 bits of the register.
; 	(2) ZF := bit tested, CF := ~ZF.

; Bit test and complement
; 1. syntax
	`btc	dest, src`
; 2. operands
;	same as `bt`.
; 3. modified flags : ZF, CF
; 4. miscellaneous : 
;	(1) basically the same as the `bt` instruction, but it will also flip the bit if it is set.

; Bit test and reset
; 1. syntax
	`btr	dest, src`
; 2. operands
;	same as `bt`.
; 3. modified flags : ZF, CF
; 4. miscellaneous : 
;	(1) basically the same as the `bt` instruction, but it will clear the bit.
;	(2) notice that even though `btc` and `btr` basically do the same thing, the logic behind them is different - 
;		that `btc` will leave the bit unchanged if it is already 0, whereas `btr` will always clear the bit to 0 even if it is already 0.

; Bit test and set
; 1. syntax
	`bts	dest, src`
; 2. operand
;	same as `bt`.
; 3. modified flags : ZF, CF
; 4. miscellaneous :
;	(1) basically the same as the `bt` instruction, but it will set the bit if it was previously 0.

; Repeat (prefix)
; 1. syntax (the 'b' could be replaced by w,d)
; NOTE, in legacy mode, [ESI] is [DS:(E)SI] and [EDI] is [ES:(E)DI]
	`rep{suffix} 	instructions`
	`rep movsb`		; move string byte, from [ESI] to [EDI]
	`rep stosb`		; store string byte, from AL to [EDI]
	`rep lodsb`		; load string byte, from [ESI] to AL
	`rep scasb`		; scan string byte, compare AL with [EDI], if same, ZF := 1 otherwise 0.
	`rep cmpsb`		; compare string byte from [ESI] and [EDI], if same, ZF := 1 otherwise 0.
; 2. operand
; 3. modified flags : 
; 4. miscellaneous :
;	(1) repeating the operation ECX times.

; Set byte on condition
; 1. syntax
	`set{suffix}	dest`
; 2. operand
;	dest : reg8 / mem8
; 3. modified flags : No need to know.
; 4. miscellaneous :
;	(1) set 'dest' to 1 if condition was met, otherwise 0.

; Byte swap
; 1. syntax
	`bswap 	dest`
; 2. operand
;	dest : reg32, reg64, mem32, mem64
; 3. modified flags : 
; 4. miscellaneous : 
;	(1) swaps the byte order of a 32-bit or 64-bit integer. 
;	(2) one example : 
		mov		eax, 0x12345678		; eax is a 32-bit register.
		bswap	eax					; EAX := 0x78563412 because it swaps byte-by-byte.

; Data transfer (variety of IN and OUT)
; 1. syntax
	; read a byte/word/dword from I/O port.
	`in 	al/ax/eax, imm8`
	`in		al/ax/eax, dx`
	; write a byte/word/dword to I/O port.
	`out 	imm8, al/ax/eax`
	`out 	dx, al/ax/eax`
	; read a byte/word/dword from port specified by DX into [es:(e)di]
	`insb / insw / insd`
	;write a byte/word/dword from [es:(e)di] into port specified by DX
	`outsb / outsw / outsd`


; 
; 1. syntax

; 2. operand
; 3. modified flags : 
; 4. miscellaneous :

; 
; 
; 
; 
; 
; 
; 
; ============================================================================
; 								WindowsXP debug
; ============================================================================
; R
; input - `r` : view all the register values. 
; input - `r register_name` : view the register value first, then could change it.
; 
; D
; input - `d segment:offset` : view the content in memory.
; input - `d segment:start_offset end_offset` : view the memory in specific range.
; 
; E
; input - `e segment:offset data_array` : change memory content, for multiple data use space to limit each byte.
; 
; U
; input - `u segment:offset` : translate the content in memory into assembly instructions.
; 
; A
; input - `a segment:offset` : write assembly instructions into specific memory.
; 
; T
; input - `t` : execute the code in CS:IP
; 
; 
; 
; 
; 
;============================================================================
; 								x86 Chipset
; ============================================================================
; x86 Chipset
; 
; 
; 
; ============================================================================
; Direct Memory Access
; 
; 
; 
; 
; ============================================================================
; Programmable Interrupt Controller
; 
; 
; 
; 
; 
; ============================================================================
; Programmable Interval Timer
; 
; 
; 
; 
; 
; ============================================================================
; Programmable Parallel Interface
; 
; 
; 
; 
; 
; ============================================================================
; 								Syntaxes and Assembles
; ============================================================================
; x86 Assemblers
; 1. GNU Assembler (GAS)
; 2. Microsoft Macro Assembler (MASM)
; 3. JWASM
; 4. Netwide Assembler (NASM)
; 5. Flat Assembler (FASM)
; 6. YASM Assembler
; 7. HLA
; ============================================================================
; GNU assembly syntax (GAS)
; ============================================================================
; MASM Syntax
; ============================================================================
; HLA Syntax
; ============================================================================
; FASM Syntax
; ============================================================================
; NASM Syntax
; ============================================================================
; 
; 
; 
; 
; 
; 
; 
; ============================================================================







