structure tigercodegen :> tigercodegen = struct

open tigerassem
open tigertree
open tigerframe




fun codegen frame stm = 
let 
	val ilist = ref (nil: instr list)
	fun emit x = ilist := x :: !ilist
	
	
	(*fun munchArgs (i,  []) = [] 
	  | munchArgs (i,h::t) = emit (OPER {assem = "STMDB SP!, {s0}\n",
	                                     src = [munchExp i],
	                                     dst = [],
	                                     jump = NONE }) ; munchArgs(i+1,t)
	 *)
	(**)
	
	
	
	
	fun munchStm (SEQ(a,b)) = (munchStm a ; munchStm b)
	  
	  
	  | munchStm (EXP(CALL (NAME lf,args))) =
        emit (OPER {assem = "bl s0\n",
                    src = munchArgs(0,args),
                    dst = calldefs,
                    jump = lf}) (* O jump = NONE ?*)	  
	  
	  
	  
	  
	  | munchStm (MOVE(TEMP t1, e2)) = 
	     emit(OPER {assem= "mov 'd0,s0\n",
	                src = [munchExp e2],
	                dst = [t1],
	                jump = NONE })
	                 
	  | munchStm (MOVE(MEM e1, e2)) =
	     emit(OPER {assem= "str 's1,['s0]\n"
	                src = [munchExp e1,munchExp e2], 
	                dst = [],
	                jump = NONE }) 
	  
	  | munchStm (tigertree.LABEL lab) = 
	     emit( tigerassem.LABEL { assem = lab ^ ": \n" , lab = lab} )
	     
	                
	
	(*fun munchStm (SEQ(a,b)) = (munchStm a; munchStm b)
		| munchStm (MOVE(TEMP t1, MEM (BINOP (PLUS, CONST i, TEMP t2)))) =
				emit(OPER {assem="mov `d0, M["^Int.toString(i)^"+`s0]\n", 
							dst=[t1], src=[t2], JUMP=NONE })
		| munchStm _ = (OPER {assem="", src=[], dst=[], JUMP=NONE })
	*)			
		
	and result gen = let val t = tigertemp.newtemp() in gen t; t end
	
	
	and munchExp (MEM (BINOP (PLUS, CONST i, e1)))
	      
	      result(fn r => emit(OPER
	            {assem = "LDR d0,[s0,#" ^ int i ^ "]\n",
	             src = [munchExp e1], dst = [r],
	             jump = NONE }))
	  
	
	  | munchExp (BINOP(PLUS,e1,e2)) = 
	         
	      result(fn r => emit(OPER 
	            {assem = "ADD d0, s0, s1\n",
	             src = [munchExp e1, munchExp e2], dst = [r],
	             jump = NONE }))
	  
	  | munchExp (BINOP(MINUS,e1,e2)) = 
	      result(fn r => emit(OPER 
	            {assem = "SUB d0, s0, s1\n",
	             src = [munchExp e1, munchExp e2], dst = [r],
	             jump = NONE }))              	             
	             
	  | munchExp (MEM e1) =
	      
	      result(fn r => emit(OPER
	            {assem = "LDR d0, [s0]\n",
	             src = [munchExp e1], dst = [r],
	             jump = NONE }))            
	             
	  | munchExp (CONST i) =
	      let val i32 = if (i < 65536) then "32" else ""
	      in 
	         result(fn r => emit(OPER
	            {assem = "MOV" ^ i32 ^ " d0, #" ^ int i ^ "\n",
	             src = [] , dst = [r],
	             jump = NONE }))
	      end                
	             
	  | munchExp (TEMP t) = t         
	             
	
	
	
	(* fun munchExp( *) 
in 	
	muchStm stm; 
	rev(!ilist)
end





