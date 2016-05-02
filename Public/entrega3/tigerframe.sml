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

type level = int

val fp = "fp"				(* frame pointer *)
val rv = "r0"				(* return value  *) 
val sp = "r13"				(* stack pointer *)
val lr = "r14"				(* link register *) 
val pc = "r15" 				(* program counter *)
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
val numLocalsInicial = 0    (* sreg *)
val localsGap = ~4 			(* bytes *)
val calldefs = [rv]
val specialregs = [fp, sp, lr, pc]
val argregs = [rv, "r1", "r2", "r3"]
val callersaves = ["r0","r1","r2","r3"]
val calleesaves = ["r4","r5","r6","r7","r8","r9","r10","r11"]
val usable = ["r0","r1","r2","r3","r4","r5","r6","r7","r8","r9","r10","r11"]
datatype access = InFrame of int | InReg of tigertemp.label

(*
val usable = ["r0", "r1"];
*)

type frame = {
	name : string,
	formals : bool list,
	locals : bool list,
	actualArg : int ref,
	actualLocal : int ref,
	actualReg : int ref, 
    localsInFrame : int ref, 
	argsAcc : (access list) ref
}

type register = string

datatype frag = PROC of {body: tigertree.stm, frame: frame}
	| STRING of tigertemp.label * string
datatype canonfrag = 
    CANONPROC of {body: tigertree.stm list, frame: frame}
    | CANONSTRING of tigertemp.label * string

fun allocArg (f: frame) b = 
	case b of
	true =>
		let	val ret = (!(#actualArg f)+argsOffInicial)*wSz
			val _ = #actualArg f := !(#actualArg f)+1
		in	InFrame ret end
	
	| false =>  InReg(tigertemp.newtemp())

fun newFrame{name, formals} = 
let
	val newframe = {	name=name,
				formals=formals,
				locals=[],
				actualArg=ref argsInicial,
				actualLocal=ref localsInicial,
				actualReg=ref regInicial,
                localsInFrame = ref numLocalsInicial,
				argsAcc = ref ([] : (access list)) 
			}
            
     val _ = (#argsAcc newframe := (List.map (fn b => allocArg newframe b) formals))
in
	newframe
end

fun name(f: frame) = #name f
fun string(l, s) = l^tigertemp.makeString(s)^"\n"(* PARA QUE ESTÁ ESTO? *)
fun formals({argsAcc, ...}: frame) = !argsAcc

(*
	let	fun aux(n, []) = []
		| aux(n, h::t) = InFrame(n)::aux(n+argsGap, t)
	in aux(argsInicial, f) end
*)

fun maxRegFrame(f: frame) = !(#actualReg f)

fun allocLocal (f: frame) b = 
    case b of
        true =>
            let	val ret = InFrame(!(#actualLocal f)+localsGap)
            in	
                #localsInFrame f := !(#localsInFrame f)+1;
                #actualLocal f:=(!(#actualLocal f)+localsGap); 
                ret
            end (* esto está modificado, hay que verificar que esté bien! *)
        | false => InReg(tigertemp.newtemp())
    
fun exp(InFrame k) e = MEM(BINOP(PLUS, e, CONST k))
	| exp(InReg l) _ = TEMP l
fun externalCall(s, l) = CALL(NAME s, l)


(*
fun procEntryExit1 (frame,body) = body
*)

fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)

fun mkpushlist [] = "{}"
|   mkpushlist (x::xs) = 
    let
        fun mkpushlist1 [] = "" 
        |   mkpushlist1 (t::ts) = ","^t^(mkpushlist1 ts)
    in 
        "{"^x^(mkpushlist1 xs)^"}" 
    end



fun procEntryExit1 (fr : frame, body) =  
let 
	val argsAcc = #argsAcc fr
	fun aux [] _ = []
	|   aux (acc::accs) n = if n < List.length argregs 
			then tigertree.MOVE(exp acc (TEMP fp), 
							TEMP (List.nth(argregs,n))) :: aux accs (n+1) 
			else if not (List.nth ((#formals fr), n)) then (* si no escapa la copiamos a un temp *) 
			let 
				val varoffset = ((n - List.length argregs)+localsGap)*wSz
				val src = (if (varoffset >= 0) then MEM(BINOP(PLUS,CONST varoffset, TEMP fp)) 
				                              else MEM(BINOP(MINUS,CONST (~varoffset), TEMP fp))) 
			in
				tigertree.MOVE(exp acc (TEMP fp), src) :: aux accs (n+1)
			end
			else aux accs (n+1) (* si escapa y no esta en registro, no hacemos nada *) 

	val moveargs = aux (!argsAcc) 0 (*Instrucciones para mover de los 
										argumentos a los locals donde la 
										función ve internamente las cosas *)
	val freshtmps = List.tabulate (List.length calleesaves, 
									fn _ => TEMP (tigertemp.newtemp()))
	val moves = ListPair.zip(freshtmps, List.map TEMP calleesaves)
	val saveregs = List.map MOVE moves (* Instrucciones para salvar en temporarios los callee saves *)
	
	val revmoves = List.map (fn(x,y) => (y,x)) moves
	val restoreregs = List.map MOVE revmoves (* Restaurar los callee saves *)
in 
	seq( (*saveregs @ *) moveargs @ [body] (* @ restoreregs*) ) 
end

fun procEntryExit2 (frame, body) = 
	body @ [ tigerassem.OPER{assem="", src=[rv,sp] @ calleesaves, dst=[], jump=SOME[]}]


fun procEntryExit3 (frame:frame,instrs) = {prolog = ".global " ^ #name frame ^ "\n" ^
                                                   "\t" ^ #name frame ^ ":\n" ^  
                                                   "\t#prologo:\n"^
                                                   "\tpush "^mkpushlist (calleesaves@[lr])^"\n"^
                                                   "\taddq $"^(Int.toString (!(#localsInFrame frame) * wSz * (~1)))^", sp\n\n",
                                    body = instrs,
                                    epilog = "\t#END "^(#name frame)^"\n"^
                                             "\taddq $"^(Int.toString (!(#localsInFrame frame) * wSz))^", sp\n\n"^
                                             "\tpop "^mkpushlist (pc::(rev calleesaves))^"\n"
                                             }


end


