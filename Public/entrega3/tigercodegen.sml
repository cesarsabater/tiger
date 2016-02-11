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
	  
	  | munchStm (EXP e) = let val _ = (munchExp e) in () end 
 	  
	  
	  | munchStm (CJUMP (relop,e1,e2,lab1,lab2)) =
	    
	    let val instr  = case relop of EQ => "BEQ"
	                             | GT => "BGT"
	                             | LT => "BLT"
	                             | GE => "BGE"
	                             | LE => "BLE"
	                             | NE => "BNE"
	                           
	    in emit (OPER {assem = "CMP 's0, 's1\n" ^ instr ^ lab1,
	                   src = [munchExp e1, munchExp e2], dst = [],
	                   jump = SOME [lab1,lab2] })
	                   
	    end               
	  

	  | munchStm (JUMP (NAME (lab), _)) =
	    emit (OPER {assem = "b "^lab^"\n",
	                src = [], dst = [],
	                jump = SOME [lab] })
	  
	  
	  | munchStm (JUMP ( e1 , labels) ) =
	    emit (OPER {assem = "bx 's0\n",
	                src = [munchExp e1], dst = [],
	                jump = SOME labels })
	     
	  
	  

     | munchStm (EXP(CALL (NAME lf,args))) =
       let val argtemps = munchArgs(0,args) 
           fun genPush [] = () 
             | genPush (h::t) = (emit(OPER{assem = "PUSH {'s0}\n", src=[h] , dst=[], jump=NONE}) ; (genPush t)) 
       in
        genPush argtemps ;
        emit (OPER {assem = "bl " ^ lf ^ "\n",
					src =  argtemps,
                    dst = calldefs,
                    jump = NONE})   (* O jump = lf ?*)	  
	    end
    
	  | munchStm (MOVE(TEMP t1, e2)) = 
	     emit(OPER {assem= "mov 'd0,'s0\n",
	                src = [munchExp e2],
	                dst = [t1],
	                jump = NONE })
	  | munchStm (MOVE(MEM e1, BINOP(PLUS, CONST i, TEMP sp) )) =  (* i mult de 4 entre 0 y 1020*)
	     emit(OPER {assem= "str 's0, [sp,#" ^ Int.toString i ^ "]\n",
	                src = [munchExp e1,sp], 
	                dst = [],
	                jump = NONE })
	  
	  | munchStm (MOVE(MEM e1, BINOP(PLUS, CONST i, e2))) =
	     emit(OPER {assem="str 's0, ['s1, #" ^ Int.toString i ^ "]\n",
	                src = [munchExp e1,munchExp e2],
	                dst = [],
	                jump= NONE })                
	                              
	                 
	  | munchStm (MOVE(MEM e1, BINOP(PLUS, e2, e3))) =
	     emit(OPER {assem="str 's0, ['s1, 's2]\n",
	                src = [munchExp e1,munchExp e2,munchExp e3],
	                dst = [],
	                jump= NONE })  
	    
	                 
	                 
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
	            {assem = "LDR 'd0,['s0,#" ^ Int.toString i ^ "]\n",
	             src = [munchExp e1], dst = [r],
	             jump = NONE }))
	  
	
	  | munchExp (BINOP(PLUS,e1,e2)) = 
	         
	      result(fn r => emit(OPER 
	            {assem = "ADD 'd0, 's0, 's1\n",
	             src = [munchExp e1, munchExp e2], dst = [r],
	             jump = NONE }))
	  
	  | munchExp (BINOP(MINUS,e1,e2)) = 
	      result(fn r => emit(OPER 
	            {assem = "SUB 'd0, 's0,'s1\n",
	             src = [munchExp e1, munchExp e2], dst = [r],
	             jump = NONE }))              	             
	             
	  | munchExp (MEM e1) =
	      
	      result(fn r => emit(OPER
	            {assem = "LDR 'd0, ['s0]\n",
	             src = [munchExp e1], dst = [r],
	             jump = NONE }))            
	             
	  | munchExp (CONST i) =
	      let val i32 = if (i >= 65536) then "32" else ""
	      in 
	         result(fn r => emit(OPER
	            {assem = "MOV" ^ i32 ^ " 'd0, #" ^ Int.toString i ^ "\n",
	             src = [] , dst = [r],
	             jump = NONE }))
	      end                
	             
	  
	  | munchExp (NAME lab) =
	      result (fn r => emit(OPER
	            {assem = "LDR ^ 'd0, =" ^ lab ^ "\n",
	             src = [], dst = [r],
	             jump = NONE }))
	  
	  | munchExp (TEMP t) = t         
	             
      | munchExp _ = tigertemp.newtemp()
	
	
	and munchArgs ( _ , []) = []
	 |  munchArgs (n, h::t) = (munchExp h) :: munchArgs(n+1,t)   
	   
	 (*  emit(OPER{assem = "PUSH {'s0}\n", src=[munchExp h] , dst=[], jump=NONE}) ; munchArgs(n+1,t) *)
	
	
	(* fun munchExp( *) 
in 	
	munchStm stm; 
	rev(!ilist)
end


end


