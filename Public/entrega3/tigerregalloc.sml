structure tigerregalloc :> tigerregalloc =
struct
open tigerassem
  
   exception ErrorOffset
   exception ErrorAccess

   
   (*Code gen Store and fetch instr*)
   fun storeTemp(temp, mempos) =
			let
				val desp = if mempos<0 then " - " ^ Int.toString(~mempos) else if mempos>0 then " + " ^ Int.toString(mempos) else ""
			in
				OPER {assem="str 's0 [falta fp,#" ^ desp ^ "]\n", src=[temp,tigerframe.fp], dst=[], jump=NONE}
			end
   fun fetchTemp(temp, mempos) =
			let
				val desp = if mempos<0 then " - " ^ Int.toString(~mempos) else if mempos>0 then " + " ^ Int.toString(mempos) else ""
			in
				OPER {assem="ldr 'd0[falta fp,#" ^ desp ^ "]\n", src=[tigerframe.fp], dst=[temp], jump=NONE}
			end
   
   (*Spill *)
   fun spill spilledNodes frame instrList = 
      
      let
          val ilist = ref ([]:(instr list))
      
          fun accToInt a = case a of tigerframe.InFrame i => i
                                    |tigerframe.InReg => raise ErrorAccess
          
          val offset : (tigertemp.temp,int) Polyhash.hash_table = Polyhash.mkTable(Polyhash.hash,(fn (a,b) => String.compare(a,b) = EQUAL)) (100,ErrorOffset)  
     
          fun allocate t = Polyhash.insert offset (t,accToInt(tigerframe.allocLocal frame true)) 
        
          val newTemps = tigerset.newEmpty(String.compare) ; 
           
          fun emitNew fetchStore mempos = let t = tigertemp.newtemp() in
                                              tigerset.add (newTemps,t) ;
                                              emit(fetchStore (t,mempos)) 
                                          end
          
          fun procInstruction (OPER {assem = assem,src = src,dst = dst, jump = jump}) =  let
                           fun gensrc t = case (Polyhash.peek offset t) of SOME mempos => emitNew fetchTemp mempos
                                                                          |NONE        => ()
                           fun gendst t = case (Polyhash.peef offset t) of SOME mempos => emitNew storeTemp mempos
                                                                          |NONE        => ()                                                                 
            in
              List.app gensrc src ;
              emit(OPER {assem = assem,src = src,dst = dst, jump = jump}) ;
              List.app gendst dst 
            end    
      in    
          List.app allocate spilledNodes ;
          
          List.app procInstruction instrList ;
          
          (!ilist , newTemps)
      end
end      
