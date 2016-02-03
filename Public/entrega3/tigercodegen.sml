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
	
	

	and munchStm (SEQ(a,b)) = (munchStm a ; munchStm b)
	  
	  | munchStm (CJUMP (relop,e1,e2,lab1,lab2)) =
	    
	    let instr  = case relop of EQ => "BEQ"
	                             | GT => "BGT"
	                             | LT => "BLT"
	                             | GE => "BGE"
	                             | LE => "BLE"
	                             | NE => "BNE"
	                           
	    in emit (OPER {assem = "CMP s0, s1\n" ^ instr ^ lab1,
	                   src = [muchExp e1, munchExp e2], dst = [],
	                   jump = [lab1,lab2] })
	                   
	    end               
	  

	  | munchStm (JUMP (NAME (lab), _)) =
	    emit (OPER {assem = "b lab\n",
	                src = [], dst = [],
	                jump = [lab] })
	  
	  
	  | munchStm (JUMP ( e1 , labels) ) =
	    emit (OPER {assem = "bx s0\n",
	                src = [munchExp e1], dst = [],
	                jump = labels })
	     
	  
	  

   (* | munchStm (EXP(CALL (NAME lf,args))) =
        emit (OPER {(ssem = "bl s0\n",

                    src = munchArgs(0,args),
                    dst = calldefs,
                    jump = lf})   *)(* O jump = NONE ?*)	  
	    
    
	  | munchStm (MOVE(TEMP t1, e2)) = 
	     emit(OPER {assem= "mov 'd0,s0\n",
	                src = [munchExp e2],
	                dst = [t1],
	                jump = NONE })
	                 
	  | munchStm (MOVE(MEM e1, e2)) =
	     emit(OPER {assem= "str 's1,['s0]\n",
	                src = [munchExp e1,munchExp e2], 
	                dst = [],
	                jump = NONE }) 
	  
	  | munchStm (tigertree.LABEL lab) = 
	     emit( tigerassem.LABEL { assem = lab ^ ": \n" , lab = lab} )
	     
	                
      | munchStm _ = emit(OPER {assem="", src=[], dst=[], jump=NONE }) 
				
		
	and result gen = let val t = tigertemp.newtemp() in gen t; t end
	
	

	and munchExp (MEM (BINOP (PLUS, CONST i, e1))) = 
	      
	     result(fn r => emit(OPER
	            {assem = "LDR d0,[s0,#" ^ Int.toString i ^ "]\n",
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
	            {assem = "MOV" ^ i32 ^ " d0, #" ^ Int.toString i ^ "\n",
	             src = [] , dst = [r],
	             jump = NONE }))
	      end                
	             
	  | munchExp (TEMP t) = t         
	             
      | munchExp _ = tigertemp.newtemp()
	
	
	(* fun munchExp( *) 
in 	
	munchStm stm; 
	rev(!ilist)
end


end


