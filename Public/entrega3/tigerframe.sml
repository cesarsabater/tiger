(*
	Frames para el 80386 (sin displays ni registers).

		|    argn    |	fp+4*(n+1)
		|    ...     |
		|    arg2    |	fp+16
		|    arg1    |	fp+12
		|	fp level |  fp+8
		|  retorno   |	fp+4
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-4
		|   local2   |	fp-8
		|    ...     |
		|   localn   |	fp-4*n
*)

structure tigerframe :> tigerframe = struct

open tigertree
open tigerassem

type level = int

val fp = "fp"				(* frame pointer *)
val rv = "r0"
val sp = "r13"				(* stack pointer *)
val lr = "r14" 
val pc = "r15"
val rv = "r0"				(* return value  *) 
val ov = "OV"				(* overflow value (edx en el 386) *)
val wSz = 4					(* word size in bytes *)
val log2WSz = 2				(* base two logarithm of word size in bytes *)
val fpPrev = 0				(* offset (bytes) *)
(* arreglar static link!!! *)
val fpPrevLev = 0 (* 8	*)	(* offset (bytes) *)
val argsInicial = 0			(* words *)
val argsOffInicial = 0		(* words *)
val argsGap = wSz			(* bytes *)
val regInicial = 1			(* reg *)
val localsInicial = 0		(* words *)
val localsGap = ~4 			(* bytes *)
val calldefs = [rv]
val specialregs = [rv, fp, sp, lr, pc]
val argregs = [rv, "r1", "r2", "r3"]
val callersaves = []
val calleesaves = ["r4","r5","r6","r7","r8","r9","r10","r11"]

type frame = {
	name: string,
	formals: bool list,
	locals: bool list,
	actualArg: int ref,
	actualLocal: int ref,
	actualReg: int ref
}

type register = string
datatype access = InFrame of int | InReg of tigertemp.label
datatype frag = PROC of {body: tigertree.stm, frame: frame}
	| STRING of tigertemp.label * string
datatype canonfrag = 
    CANONPROC of {body: tigertree.stm list, frame: frame}
    | CANONSTRING of tigertemp.label * string

fun newFrame{name, formals} = {
	name=name,
	formals=formals,
	locals=[],
	actualArg=ref argsInicial,
	actualLocal=ref localsInicial,
	actualReg=ref regInicial
}
fun name(f: frame) = #name f
fun string(l, s) = l^tigertemp.makeString(s)^"\n"(* PARA QUE ESTA ESTO *)
fun formals({formals=f, ...}: frame) = 
	let	fun aux(n, []) = []
		| aux(n, h::t) = InFrame(n)::aux(n+argsGap, t)
	in aux(argsInicial, f) end
fun maxRegFrame(f: frame) = !(#actualReg f)
fun allocArg (f: frame) b = 
	case b of
	true =>
		let	val ret = (!(#actualArg f)+argsOffInicial)*wSz
			val _ = #actualArg f := !(#actualArg f)+1
		in	InFrame ret end
	| false => InReg(tigertemp.newtemp())
fun allocLocal (f: frame) b = 
	case b of
	true =>
		let	val ret = InFrame(!(#actualLocal f)+localsGap)
		in	#actualLocal f:=(!(#actualLocal f)+localsGap); ret end (* esto está modificado, hay que verificar que esté bien! *)
	| false => InReg(tigertemp.newtemp())
fun exp(InFrame k) e = MEM(BINOP(PLUS, e, CONST k))
	| exp(InReg l) _ = TEMP l
fun externalCall(s, l) = CALL(NAME s, l)

fun procEntryExit1 (frame,body) = body

fun procEntryExit2 (frame, body) = 
    body @ [ OPER{assem="", src=[], dst=[sp, pc]@calleesaves, jump=SOME []} ] (* checkear mejor *)  
end
