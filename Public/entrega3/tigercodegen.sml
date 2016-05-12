structure tigercodegen :> tigercodegen = struct

open tigerassem
open tigertree
open tigerframe

      exception Multiplode4 

fun codegen frame stm = 
let 
	val ilist = ref (nil: instr list)
	fun emit x = ilist := x :: !ilist

	
	fun imm12 x = ((x >= 0) andalso (x <= 4095)) (*then true else false*)   
    
    val immConst = 65536
    
    fun negoffset x = (x < 256)          (*Puede ser 4095 en ARM. Si es Thumb es 256, pongo 256 por las dudas *)


	
    (*fun munchArgs (i,  []) = [] 
	  | munchArgs (i,h::t) = emit (OPER {assem = "STMDB SP!, {s0}\n",
	                                     src = [munchExp i],
	                                     dst = [],
	                                     jump = NONE }) ; munchArgs(i+1,t)
	 *)
	(**)
	
	
   (*-------------Munchea SEQs-------------------*)

	and munchStm (SEQ(a,b)) = (munchStm a ; munchStm b)
	  
	  
  (*-------------Munchea JUMPSs-------------------*)  
	  
	  | munchStm (CJUMP (relop,e1,e2,lab1,lab2)) =
	    
	    let val instr  = case relop of 
	                               EQ   => "beq"
	                             | GT   => "bgt"
	                             | LT   => "blt"
	                             | GE   => "bge"
	                             | LE   => "ble"
	                             | NE   => "bne"
	                             | ULT  => "blo" 
	                             | ULE  => "bls"
	                             | UGT  => "bhi"
	                             | UGE  => "bhs"
 	                           
	    in emit (OPER {assem = "cmp     's0, 's1\n" ^ instr ^ " " ^ lab1 ^ "\n",
	                   src = [munchExp e1, munchExp e2], dst = [],
	                   jump = SOME [lab1,lab2] })
	                   
	    end               

	  | munchStm (JUMP (NAME (lab), _)) =
	    emit (OPER {assem = "b       "^lab^"\n",
	                src = [], dst = [],
	                jump = SOME [lab] })
	  
	  
	  | munchStm (JUMP ( e1 , labels) ) =
	    emit (OPER {assem = "bx      's0\n",  (* ojo con bx *)
	                src = [munchExp e1], dst = [],
	                jump = SOME labels })
	 
	 
	 (**************************)
	 | munchStm (MOVE(TEMP x,CALL a)) = munchStm(SEQ(EXP(CALL a),MOVE(TEMP x,TEMP tigerframe.rv)))
	     
	  
    (*-------------Munchea CALLs-------------------*)  

     | munchStm (EXP(CALL (NAME lf,args))) =
     
       
       let 
     (*      fun munchArgs (_ , [])     = []
             | munchArgs (n,(h :: t)) =  if n < List.length argregs then  
                                          let val argreg = List.nth(argregs,n) in  
                                          (emit(tigerassem.MOVE{assem = "mov     'd0, 's0\n",src =munchExp h,dst = argreg}) ; argreg :: munchArgs(n+1,t)) end
                                         else                                        
                                          (emit(OPER{assem = "push    {'s0}\n", src=[h], dst=[], jump=NONE}) ; munchArgs (n+1),t)
                                   
       
     *)           
       
       
           val argtemps = munchArgs(0,args) 
      
           fun genPush _ [] = () 
             | genPush n (h::t) =(if n < List.length argregs then
                                     (emit(tigerassem.MOVE{assem = "mov     'd0, 's0\n",src = h,dst = List.nth(argregs,n)}) ; genPush (n+1) t) 
                                  else              
                                     (List.app (fn x => (emit(OPER{assem = "push    {'s0}\n", src=[x], dst=[], jump=NONE}))) (List.rev(h::t))  
                                     )
                                   (*   (emit(OPER{assem = "push    {'s0}\n", src=[h], dst=[], jump=NONE}) ; genPush (n+1) t)  *)
                                  )  
       in
        (if (List.length(argtemps) > 0) then genPush 0 argtemps else () );
        emit (OPER {assem = "bl      " ^ lf ^ "\n",
					src = List.take(argregs, Int.min(List.length argregs,List.length argtemps)),
                    dst = calldefs,   (*original calldefs*)
                    jump = NONE}) ;  (* O jump = lf ?*)
              let val offset = (List.length args - List.length tigerframe.argregs)*tigerframe.wSz in
                    if (offset > 0) then (munchStm (MOVE(TEMP sp,BINOP(PLUS,CONST offset,TEMP sp))))
                    else ()            
              end   
                    	  
	   end
	   
	   
	 (*-------------Munchea MOVE a TEMP de una suma -------------------*)  
	   
	  | munchStm (MOVE(TEMP x,BINOP(PLUS, CONST i,TEMP y))) = 
	       if (imm12 i) then (
	           emit(OPER {assem = (if (String.compare(x,y) = EQUAL) then 
	                                  "add    'd0, #" ^ Int.toString i ^ "\n" 
	                               else 
	                                  "add    'd0, 's0, #" ^ Int.toString i ^ "\n"),
	                      src = [y],
	                      dst = [x], 
	                      jump = NONE })
	           
	       ) else (
	           emit(OPER {assem = "add    'd0, 's0, 's1\n",
	                      src = [y,munchExp(CONST i)],
	                      dst = [x], 
	                      jump = NONE })
	           
	       
	       )
      

      (*----------------Accesos a Memoria--------------*)   
 
      | munchStm (MOVE(MEM(BINOP(PLUS, CONST i, TEMP sp)), e1  )) =  (*V*) (* i mult de 4 entre 0 y 1020*)
	     
	     ((if (i mod 4 = 0) then () else raise Fail "offset del sp tiene que ser multiplo de 4\n") ;  
	    
	     (if ((i >= 0) andalso (i <= 1020)) then 
	       (emit(OPER {assem= "str     's0, [sp, #" ^ Int.toString i ^ "]\n",
	                src = [munchExp e1,sp], 
	                dst = [],
	                jump = NONE }))
	     else 
	       (emit(OPER {assem= "str     's0,[sp, 's1]\n",
	                src = [munchExp e1,munchExp(CONST i),sp], 
	                dst = [],
	                jump = NONE })))
	      )            
	  
	  | munchStm (MOVE(MEM (BINOP(PLUS, CONST i, e2)), e1)) =
	  if imm12 i then
	     emit(OPER {assem="str     's0, ['s1, #" ^ Int.toString i ^ "]\n",
	                src = [munchExp e1,munchExp e2],
	                dst = [],
	                jump= NONE })
	  else 
	     emit(OPER {assem="str     's0, ['s2, 's1]\n",
	                src = [munchExp e1,munchExp (CONST i),munchExp e2],
	                dst = [],
	                jump= NONE })
	                                  
     | munchStm (MOVE(MEM (BINOP(PLUS, e2, CONST i)), e1 )) =
	  if imm12 i then
	     emit(OPER {assem="str     's0, ['s1, #" ^ Int.toString i ^ "]\n",
	                src = [munchExp e1,munchExp e2],
	                dst = [],
	                jump= NONE })
	  else 
	     emit(OPER {assem="str     's0, ['s2, 's1]\n",
	                src = [munchExp e1,munchExp (CONST i),munchExp e2],
	                dst = [],
	                jump= NONE })
	                                     	 
	                              
	                 
	  | munchStm (MOVE(MEM (BINOP(PLUS, e2, e3)), e1)) =
	     emit(OPER {assem="str     's0, ['s1, 's2]\n",
	                src = [munchExp e1,munchExp e2,munchExp e3],
	                dst = [],
	                jump= NONE })  
	    
	  
	  | munchStm (MOVE(MEM (BINOP(MINUS, CONST i, e2)),e1 )) =
	  if (negoffset i) then
	     emit(OPER {assem="str     's0, ['s1, #-" ^ Int.toString i ^ "]\n",
	                src = [munchExp e1,munchExp e2],
	                dst = [],
	                jump= NONE })
	  else 
	     emit(OPER {assem="str     's0, ['s1]\n",
	                src = [munchExp e1,munchExp(BINOP(MINUS, CONST i, e2))],
	                dst = [],
	                jump= NONE })
	  
	 | munchStm (MOVE(MEM (BINOP(MINUS, e2, CONST i)), e1)) =
	  if (negoffset i) then
	     emit(OPER {assem="str     's0, ['s1, #-" ^ Int.toString i ^ "]\n",
	                src = [munchExp e1,munchExp e2],
	                dst = [],
	                jump= NONE })
	  else 
	     emit(OPER {assem="str     's0, ['s1]\n",
	                src = [munchExp e1,munchExp(BINOP(MINUS, e2, CONST i))],
	                dst = [],
	                jump= NONE })
	  
	  
	  | munchStm (MOVE(MEM e1, e2)) =
	     emit(OPER {assem= "str     's1,['s0]\n",
	                src = [munchExp e1,munchExp e2], 
	                dst = [],
	                jump = NONE }) 
	                
	   | munchStm (MOVE(TEMP t1, e2)) = 
	     emit(tigerassem.MOVE {assem= "mov     'd0, 's0\n",
	                src = munchExp e2,
	                dst = t1})
	  
	  
	  | munchStm (tigertree.LABEL lab) = 
	     emit( tigerassem.LABEL { assem = lab ^ ": \n" , lab = lab} )
	     
	  
	  | munchStm (EXP e) = let val _ = (munchExp e) in () end 
	  
	  
	  | munchStm _ = raise Fail "Casos no cubiertos en tigercodegen.munchStm"               
      (*| munchStm _ = emit(OPER {assem="", src=[], dst=[], jump=NONE }) *) 
					
	and result gen = let val t = tigertemp.newtemp() in gen t; t end
	
	and 
       
       munchExp (ESEQ _) = raise Fail "Este caso ESEQ no debería aparecer por el canonizar"   
  	
	 | munchExp (CALL _) = raise Fail "Este caso CALL no debería aparecer por el canonizar" 
	
	 | munchExp (MEM (BINOP (PLUS, CONST i, e1))) = 
	    if (imm12 i) then  
	     result(fn r => emit(OPER
	            {assem = "ldr     'd0, ['s0, #" ^ Int.toString i ^ "]\n",
	             src = [munchExp e1], dst = [r],
	             jump = NONE }))
	    else 
	     result(fn r => emit(OPER
	            {assem = "ldr     'd0, ['s1, 's0]\n",
	             src = [munchExp (CONST i),munchExp e1], dst = [r],
	             jump = NONE }))      
	 
	| munchExp (MEM (BINOP (PLUS, e1, CONST i))) = munchExp(MEM (BINOP (PLUS, CONST i, e1)))
 
	| munchExp (MEM (BINOP (MINUS, CONST i, e1))) = 
	    if (negoffset i) then  
	     result(fn r => emit(OPER
	            {assem = "ldr     'd0, ['s0, #-" ^ Int.toString i ^ "]\n",
	             src = [munchExp e1], dst = [r],
	             jump = NONE }))
	    else 
	     result(fn r => emit(OPER
	            {assem = "ldr     'd0, ['s0]\n",
	             src = [munchExp (BINOP (MINUS, CONST i, e1))], dst = [r],
	             jump = NONE }))                   
	  
	  | munchExp (MEM (BINOP (MINUS, e1, CONST i))) = munchExp (MEM (BINOP (MINUS, CONST i, e1)))  
	
	  | munchExp (BINOP (oper,e1,e2)) =
	  
	      let val op_instr = case oper of
	                            PLUS    => "add "
	                          | MINUS   => "sub "
	                          | MUL     => "mul "
	                          | DIV     => "udiv"
	                          | AND     => "and "
	                          | OR      => "orr "
	                          | XOR     => "eor "
	                          | LSHIFT  => "lsl "
	                          | RSHIFT  => "lsr "
	                          | ARSHIFT => "asr "
	      in
	         
	      result(fn r => emit(OPER 
	            {assem = op_instr ^ "    'd0, 's0, 's1\n",
	             src = [munchExp e1, munchExp e2], dst = [r],
	             jump = NONE }))
	  
	      end              
	  | munchExp (MEM e1) =
	      
	      result(fn r => emit(OPER
	            {assem = "ldr    'd0, ['s0]\n",
	             src = [munchExp e1], dst = [r],
	             jump = NONE }))            
	             
	  | munchExp (CONST i) =
	     let 
				val bound8 = 256
				val bound16 = 65536
				val assm = if i > 0 andalso i < bound16 then
							(if i < bound8 then 
								"mov 'd0, #" ^ Int.toString i ^ "\n"
								else
								"movw 'd0, #" ^ Int.toString i ^ "\n")
						   else 
							"movw   'd0, #:lower16:" ^ Int.toString i ^ "\n" ^
							"movt   'd0, #:upper16:" ^ Int.toString i ^ "\n"
	      in 
			result(fn r => emit(OPER {assem = assm, src = [] , dst = [r], jump = NONE }))

	      end                
	  
	  | munchExp (NAME lab) =
	      result (fn r => emit(OPER
	            {assem = "movw    'd0, #:lower16:" ^ lab ^ "\n" ^
                         "movt    'd0, #:upper16:" ^ lab ^ "\n",
	             src = [], dst = [r],
	             jump = NONE }))
	  
	  | munchExp (TEMP t) = t          (**)
	             
      | munchExp _ = tigertemp.newtemp() (*ok?*)
	
	
	and munchArgs ( _ , []) = []
	 |  munchArgs (n, h::t) = (munchExp h) :: munchArgs(n+1,t)   
	   
	 (*  emit(OPER{assem = "PUSH {'s0}\n", src=[munchExp h] , dst=[], jump=NONE}) ; munchArgs(n+1,t) *)
	
	
	(* fun munchExp( *) 
in 	
	munchStm stm; 
	rev(!ilist)
end


end


