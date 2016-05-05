structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs

exception breakexc
exception divCero

type level = {parent:frame option , frame: frame, level: int}
type access = tigerframe.access

type frag = tigerframe.frag
val fraglist = ref ([]: frag list)

val actualLevel = ref ~1 (* _tigermain debe tener level = 0. *)
fun getActualLev() = !actualLevel

val outermost: level = {parent=NONE,
	frame=newFrame{name="_tigermain", formals=[]}, level=getActualLev()}
fun newLevel{parent={parent, frame, level}, name, formals} =
	{
		(* consultar si esto está bien! como hacer newLevel!! *)
		parent=SOME frame,
		frame=newFrame{name=name, formals=true::formals}, (* acá se agrega el static link *)
		level=level+1
	}
fun allocArg{parent, frame, level} b = tigerframe.allocArg frame b
fun allocLocal{parent, frame, level} b = tigerframe.allocLocal frame b
(* CUIDADO: formals no es usable para funciones externas, porque usa el tail *)
fun formals{parent, frame, level} = tl (tigerframe.formals frame) (* le sacamos el static link *)

datatype exp =
	Ex of tigertree.exp
	| Nx of tigertree.stm
	| Cx of label * label -> tigertree.stm

fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)

fun unEx (Ex e) = e
	| unEx (Nx s) = ESEQ(s, CONST 0)
	| unEx (Cx cf) =
	let
		val r = newtemp()
		val t = newlabel()
		val f = newlabel()
	in
		ESEQ(seq [MOVE(TEMP r, CONST 1),
			cf (t, f),
			LABEL f,
			MOVE(TEMP r, CONST 0),
			LABEL t],
			TEMP r)
	end

fun unNx (Ex e) = EXP e
	| unNx (Nx s) = s
	| unNx (Cx cf) =
	let
		val t = newlabel()
		val f = newlabel()
	in
		seq [cf(t,f),
			LABEL t,
			LABEL f]
	end

fun unCx (Nx s) = raise Fail ("Error (UnCx(Nx..))")
	| unCx (Cx cf) = cf
	| unCx (Ex (CONST 0)) =
	(fn (_,f) => JUMP(NAME f, [f]))
	| unCx (Ex (CONST _)) =
	(fn (t,_) => JUMP(NAME t, [t]))
	| unCx (Ex e) =
	(fn (t,f) => CJUMP(NE, e, CONST 0, t, f))

fun Ir(e) =
	let	fun aux(Ex e) = tigerit.tree(EXP e)
		| aux(Nx s) = tigerit.tree(s)
		| aux _ = raise Fail "bueno, a completar!"
		fun aux2(PROC{body, frame}) = aux(Nx body)
		| aux2(STRING(l, "")) = l^":\n"
		| aux2(STRING("", s)) = "\t"^s^"\n"
		| aux2(STRING(l, s)) = l^":\t"^s^"\n"
		fun aux3 [] = ""
		| aux3(h::t) = (aux2 h)^(aux3 t)
	in	aux3 e end
	
fun nombreFrame frame = print(".globl " ^ tigerframe.name frame ^ "\n")

(* While y for necesitan la u'ltima etiqueta para un break *)
local
	val salidas: label option tigerpila.Pila = tigerpila.nuevaPila1 NONE
in
	val pushSalida = tigerpila.pushPila salidas
	fun popSalida() = tigerpila.popPila salidas
	fun topSalida() =
		case tigerpila.topPila salidas of
		SOME l => l
		| NONE => raise Fail "break incorrecto!"			
end

val datosGlobs = ref ([]: frag list)
fun procEntryExit{level: level, body} =
	let	
(*
        val label = STRING(name(#frame level), "")
*)
		val body' = PROC{frame= #frame level, body=unNx body}
(*
		val final = STRING(";;-------", "")
*)
(*
	in	datosGlobs:=(!datosGlobs@[label, body', final]) end
*)

    in
        datosGlobs:=(!datosGlobs@[body']) 
    end
fun getResult() = !datosGlobs

fun stringLen s =
	let	fun aux[] = 0
		| aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
		| aux(_::t) = 1+aux(t)
	in	aux(explode s) end

fun stringExp(s: string) =
	let	val l = newlabel()
(*
		val len = ".long "^makestring(stringLen s)
*)
(*
		val str = ".string \""^s^"\""
*)
(*
		val _ = datosGlobs:=(!datosGlobs @ [STRING(l, len), STRING("", str)])
*)
        val _ = datosGlobs:=(!datosGlobs @ [STRING(l, s)])
	in	Ex(NAME l) end
	
fun preFunctionDec() =
	(pushSalida(NONE);
	actualLevel := !actualLevel+1)

fun functionDec(e, l, proc) =
	let	val body =
				if proc then unNx e
				else MOVE(TEMP rv, unEx e)
		val body' = procEntryExit1(#frame l, body)
		val () = procEntryExit{body=Nx body', level=l}
	in	Ex(CONST 0) end
	
fun postFunctionDec() =
	(popSalida(); actualLevel := !actualLevel-1)

fun unitExp() = Ex (CONST 0)

fun nilExp() = Ex (CONST 0)

fun intExp i = Ex (CONST i)

fun genDiff 0 = TEMP fp
    | genDiff x = MEM (BINOP (PLUS, CONST fpPrevLev, genDiff (x-1)) )

fun simpleVar(acc, nivel) =
	Ex ( tigerframe.exp acc (genDiff (getActualLev() - nivel)) ) (*COMPLETAR*)

fun varDec(acc) = simpleVar(acc, getActualLev())

fun fieldVar(var, field) = 
let
	val r = unEx var
	val i = CONST field
	val rr = newtemp()
	val ri = newtemp()
in
	Ex( ESEQ(seq[MOVE(TEMP rr, r),
				 MOVE(TEMP ri, i),
				 EXP(externalCall("_checkNil", [TEMP rr])) ] ,
				 MEM(BINOP(PLUS, TEMP rr,
			            BINOP(MUL, TEMP ri, CONST tigerframe.wSz)))))
end (*COMPLETAR*)

fun subscriptVar(arr, ind) =
let
	val a = unEx arr
	val i = unEx ind
	val ra = newtemp()
in
	Ex( ESEQ(seq[MOVE(TEMP ra, a),
				 EXP(externalCall("_checkIndexArray", [TEMP ra, i]))], 
		     MEM(BINOP(PLUS, TEMP ra,
						case i of 
						 CONST k => CONST (k * tigerframe.wSz)
						 | _ => BINOP(LSHIFT, i, CONST(tigerframe.log2WSz ))))))
			           
			           (* BINOP(MUL, TEMP ri, CONST tigerframe.wSz))))) *)(* si hay tiempo, hacer la optimizacion de los shifts *)
end


fun recordExp l =
let
	val ret = newtemp() 
	fun gentemps 0 = [] | gentemps n = newtemp()::gentemps(n-1)
	val s = length l	
	fun aux ((e,s), t) = (MOVE(TEMP t, unEx e), s, TEMP t)
	val lexps = map aux (ListPair.zip(l, gentemps s))
	val lexps' = map #1 lexps
	val l' = Listsort.sort (fn ((_,m,_),(_,n,_)) => Int.compare(m,n)) lexps 
in
	Ex ( ESEQ (seq (lexps'@[EXP (externalCall("_allocRecord", 
												(CONST s)::(map #3 l'))),
							MOVE (TEMP ret, TEMP rv)]), 
				 TEMP ret))
end (* Ex (CONST 0) *) (*COMPLETAR*)

fun arrayExp{size, init} =
let
	val s = unEx size
	val i = unEx init
in
	Ex (externalCall("_initArray", [s, i]))
end

fun callExp (name,ext,isproc,lev:level,ls) = 
		let
			val slx = genDiff ((getActualLev() - (#level lev))+1) (* agrega el acceso a static links *)
			(* pasamos los argumentos en registros para respetar convenciones de c.. *)
			fun prepArgs [] (rt, re) = (rt, re)
			  | prepArgs (h::t) (rt, re) = 
					case h of 
					  Ex (CONST s) => (prepArgs t ((CONST s)::rt, re))
					| Ex (NAME n) => (prepArgs t ((NAME n)::rt, re))
					| _ =>  let 
								val tmp = TEMP(newtemp())
							in 
								prepArgs t (tmp::rt, (MOVE(tmp, unEx h))::re)
							end
			(* hayq ue agregar el reverse para la convencion de llamada de c *)
			val (rt, re) = prepArgs ((* rev *)ls) ([],[])
			val rt' = if ext then rt else slx::rt
			(* en el caso de funciones externas, encapsulamos la implementacion en <tigerframe> *)
			(* NOTA: la convencion de llamada de C tambien deberia encapsularse! *)
			val cexp = if ext then externalCall(name, rt')
							else CALL(NAME name, rt')
		in 
			if isproc then 
				Nx (seq (re@[EXP cexp]))
			else
				let 
					val tmp = TEMP(newtemp());
				in 
					Ex (ESEQ( seq (re@[EXP cexp, 
										MOVE(tmp, TEMP rv)]), 
								tmp))
				end
		end	(*COMPLETAR*)

fun letExp ([], body) = Ex (unEx body)
 |  letExp (inits, body) = Ex (ESEQ(seq inits, unEx body))

fun breakExp() = 
let
	val s = topSalida()
in 
	Nx (JUMP(NAME s, [s]))
end
	
	
fun seqExp ([]:exp list) = Nx (EXP(CONST 0))	
	| seqExp (exps:exp list) =
		let
			fun unx [e] = []
				| unx (s::ss) = (unNx s)::(unx ss)
				| unx[] = []
		in
			case List.last exps of
				Nx s =>
					let val unexps = map unNx exps
					in Nx (seq unexps) end
				| Ex e => Ex (ESEQ(seq(unx exps), e))
				| cond => Ex (ESEQ(seq(unx exps), unEx cond))
		end

fun preWhileForExp() = pushSalida(SOME(newlabel()))

fun postWhileForExp() = (popSalida(); ())

fun whileExp {test: exp, body: exp, lev:level} =
let
	val cf = unCx test
	val expb = unNx body
	val (l1, l2, l3) = (newlabel(), newlabel(), topSalida())
in
	Nx (seq[LABEL l1,
		cf(l2,l3),
		LABEL l2,
		expb,
		JUMP(NAME l1, [l1]),
		LABEL l3])
end


fun forExp {lo, hi, var, body} =
let
	val expb = unNx body
	val expv = unEx var
	val explo = unEx lo
	val exphi = unEx hi
	val (l1, l2, l3, l4) = (newlabel(), newlabel(), newlabel(), topSalida())
	val hit = newtemp()
in 
	Nx (seq[ MOVE(TEMP hit, exphi),  
			MOVE(expv, explo),
			JUMP(NAME l1, [l1]),
			LABEL l3,
			MOVE(expv, BINOP (PLUS, expv, CONST 1)),
			LABEL l1,
			CJUMP(LE, expv, TEMP hit, l2, l4),
			LABEL l2, 
			expb, 
			CJUMP(EQ, expv, TEMP hit, l4, l3), 
			LABEL l4])
end	 (*COMPLETAR*)

(* está bien esto así???? *)
fun ifThenExp{test, then'} =
let
	val test' = unCx test
	val	then'' = unNx then'
	val t = newlabel()
	val join = newlabel()
in
	Nx ( seq[test'(t, join), 
					LABEL t, 
					then'', 
					LABEL join] )
end
(* Ex (CONST 0) *) (*COMPLETAR*)

(* está bien esto así???? *)
fun ifThenElseExpUnit {test,then',else'} =
let
	val test' = unCx test
	val then'' = unNx then' 
	val else'' = unNx else'
	val t = newlabel() and f = newlabel()
	val join = newlabel()
in
	Nx ( seq [test'(t,f),
				LABEL t,  
				then'', 
				JUMP(NAME join, [join]),
				LABEL f,
				else'', 
				JUMP(NAME join, [join]), 
				LABEL join] )
end (* Ex (CONST 0) *) (*COMPLETAR*)

(* está bien esto así???? *)
fun
  (* caso en el que aparecen 2 condicionales *)
  ifThenElseExp {test, then'=(Cx c1), else'=(Cx c2)} = 
	let 
		val test' = unCx test
		val t = newlabel() and f = newlabel() 
		fun mkstm (t',f') = 
				seq [ test'(t, f), 
					LABEL t, 
					c1(t', f'),
					LABEL f,
					c2(t', f') ] 
	in 
		Cx mkstm 
	end
	(* caso en el que aparece un condicional en then *)
  | ifThenElseExp {test, then'=(Cx c), else'= Ex (CONST 0) } =
	let 
		val test' = unCx test
		val t = newlabel()
		fun mkstm (t',f') = seq [ test'(t, f'), LABEL t, c(t', f') ]
	in
		Cx mkstm
	end
  | ifThenElseExp {test, then'=(Cx c), else'} = 
	let 
		val test' = unCx test
		val else'' = unEx else'
		val t = newlabel() and f = newlabel()
		val z = newlabel() and j = newlabel()
		val r = newtemp()
	in 
		Ex ( ESEQ (  seq [ MOVE(TEMP r, CONST 1),
							test'(t,f), 
							LABEL t, 
							c(j,z), 
							LABEL f, 
							MOVE(TEMP r, else''),
							JUMP(NAME j, [j]), 
							LABEL z,
							MOVE(TEMP r, CONST 0),
							LABEL j],
						TEMP r))
	end
	(* caso en el que aparece un condicional en else, para este caso
		rearmamos test para que sea su negacion, y reutilizamos el caso
		anterior para un condicional, intercambiando then y else *)
  | ifThenElseExp {test, then', else'=(Cx c)} = 
	let
		fun test'' (t,f) = unCx test (f,t)
	in
		ifThenElseExp{test=(Cx test''), then'=(Cx c), else'=then'}
	end 
  (* caso generico *) 
  | ifThenElseExp {test,then',else'} =
	let
		val test' = unCx test
		val then'' = unEx then' 
		val else'' = unEx else'
		val t = newlabel() and f = newlabel()
		val join = newlabel()
		val r = newtemp() 
	in
		Ex ( ESEQ (seq [test'(t,f),
							LABEL t, 
							MOVE (TEMP r, then''), 
							JUMP(NAME join, [join]),
							LABEL f,
							MOVE (TEMP r, else''), 
							JUMP(NAME join, [join]), 
							LABEL join],
					TEMP r) ) 
	end
	(* Ex (CONST 0) *)(*COMPLETAR*)

fun assignExp{var, exp} =
let
	val v = unEx var
	val vl = unEx exp
in
	Nx (MOVE(v,vl))
end

fun binOpIntExp {left, oper, right} = 
let
	val l = unEx left
	val r = unEx right
in 
	case oper of
		PlusOp => Ex (BINOP(PLUS,l,r))
	  | MinusOp => Ex (BINOP(MINUS,l,r))
	  | TimesOp => Ex (BINOP(MUL,l,r))
	  | DivideOp => Ex (BINOP(DIV,l,r))
	  | _ => raise Fail "Esto no deberia pasar (6)"
end (* Ex (CONST 0) *) (*COMPLETAR*)

fun binOpIntRelExp {left,oper,right} =
let 
	val l = unEx left
	val r = unEx right
in 
	 case oper of 
		EqOp => Cx (fn (t, f) => CJUMP(EQ, l, r, t, f))
	  | NeqOp => Cx (fn (t, f) => CJUMP(NE, l, r, t, f))
	  | LtOp => Cx (fn (t, f) => CJUMP(LT, l, r, t, f))
	  | GtOp => Cx (fn (t, f) => CJUMP(GT, l, r, t, f))
	  | LeOp => Cx (fn (t, f) => CJUMP(LE, l, r, t, f))
	  | GeOp => Cx (fn (t, f) => CJUMP(GE, l, r, t, f))
	  | _ => raise Fail "Esto no  deberia pasar (7)" 
end (* Ex (CONST 0) (*COMPLETAR*) *)

fun binOpStrExp {left,oper,right} =
let
	val l = unEx left
	val r = unEx right
	val cmp = externalCall("_stringcmp", [l, r])
in 
	case oper of
		EqOp => Cx (fn (t, f) => CJUMP(EQ, CONST 0, cmp, t, f))
	  | NeqOp => Cx (fn (t, f) => CJUMP(NE, CONST 0, cmp, t, f))
	  | LtOp => Cx (fn (t, f) => CJUMP(EQ, CONST ~1, cmp, t, f))
	  | GtOp => Cx (fn (t, f) => CJUMP(EQ, CONST 1, cmp, t, f))
	  | LeOp => Cx (fn (t, f) => CJUMP(NE, CONST 1, cmp, t, f))
	  | GeOp => Cx (fn (t, f) => CJUMP(NE, CONST ~1, cmp, t, f))
	  | _ => raise Fail "Esto no deberia pasar (8)" 
end (*  Ex (CONST 0)  *) (*COMPLETAR*)

(*
fun  cfexp (MEM e) = MEM (cfexp e)
   | cfexp (CALL (e, l)) = CALL (cfexp e, (map cfexp l))
   | cfexp (ESEQ (st,ex)) = ESEQ (cfstm st, cfexp ex)
   (* BINOP CON PLUS *) 
   | cfexp (BINOP (PLUS, CONST c1, CONST c2)) = CONST (c1+c2)
   | cfexp (BINOP (PLUS, CONST c1, BINOP (PLUS, CONST c2, t))) = BINOP (PLUS, CONST (c1+c2), cfexp t)
   | cfexp (BINOP (PLUS, t, CONST c)) = BINOP (PLUS, CONST c, cfexp t)
   (* BINOP CON MINUS *)
   | cfexp (BINOP (PLUS, CONST c1, BINOP (MINUS, CONST c2, t))) = BINOP (MINUS, CONST (c1+c2), cfexp t)
   | cfexp (BINOP (MINUS, CONST c1, BINOP (PLUS, CONST c2, t))) = BINOP (MINUS, CONST (c1-c2), cfexp t)
   | cfexp (BINOP (MINUS, CONST c1, BINOP (MINUS, CONST c2, t))) = BINOP (PLUS, CONST (c1-c2), cfexp t)
   (* BINOP CON MUL *) 
   | cfexp (BINOP (MUL, CONST c1, CONST c2)) = CONST (c1*c2)
   | cfexp (BINOP (MUL, CONST e, CONST c)) = (BINOP (MUL, CONST c, CONST e))
   | cfexp (BINOP (MUL, CONST c1, (BINOP (PLUS, CONST c2, t)))) = (* DISTRIB suma *)  
		(BINOP (PLUS, CONST (c1*c2), (BINOP (MUL, CONST c1, t))))
   | cfexp (BINOP (MUL, CONST c1, (BINOP (MINUS, CONST c2, t)))) = (* DISTRIB resta *)  
		(BINOP (MINUS, CONST (c1*c2), (BINOP (MUL, CONST c1, t))))
   | cfexp (BINOP (MUL, CONST c1, (BINOP (MUL, CONST c2, t)))) =   (* ASOCIATIVA multiplicacion *) 
		(BINOP (MUL, CONST (c1*c2), t))
   | cfexp e = e

and cfstm (MOVE (e1,e2)) 	= (MOVE (cfexp e1, cfexp e2)) 
		| cfstm (EXP e) 	= (EXP (cfexp e))
		| cfstm (JUMP (e, l)) 	= JUMP ((cfexp e), l)
		| cfstm (CJUMP (ro, e1, e2, l1, l2)) = CJUMP (ro, cfexp e1, cfexp e2, l1, l2)
		| cfstm (SEQ (s1, s2)) 	= SEQ (cfstm s1, cfstm s2) 
		| cfstm stm = stm
*)

end

