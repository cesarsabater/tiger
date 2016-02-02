structure tigercodegen :> tigercodegen = struct

open tigerassem
open tigertree
open tigerframe

fun codegen frame stm = 
let 
	val ilist = ref (nil: instr list)
	fun emit x = ilist := x :: !ilist
	
	fun munchStm (SEQ(a,b)) = (munchStm a; munchStm b)
		| munchStm (MOVE(TEMP t1, MEM (BINOP (PLUS, CONST i, TEMP t2)))) =
				emit(OPER {assem="mov `d0, M["^Int.toString(i)^"+`s0]\n", 
							dst=[t1], src=[t2], jump=NONE })
		| munchStm _ = emit(OPER {assem="", src=[], dst=[], jump=NONE })
				
		
	and result gen = let val t = tigertemp.newtemp() in gen t; t end
	
	(* fun munchExp( *) 
in 	
	munchStm stm; 
	rev(!ilist)
end


end


