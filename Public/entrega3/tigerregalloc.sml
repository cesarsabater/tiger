structure tigerregalloc :> tigerregalloc =
struct
open tigerassem
  
   exception ErrorOffset
   exception ErrorAccess

  type allocation = (tigertemp.temp, tigerframe.register) Polyhash.hash_table
   

   
   (*Spill *)
   fun spill (spilledNodes,frame,instrList) = 
      let
          (*Lista de instrucciones nueva*)
          val ilist = ref ([]:(instr list))
          fun emit x = ilist := x::(!ilist)
          
          
         (*Code gen Store and fetch instr*)
          fun storeTemp(temp, mempos) =
			 let
				val desp = if mempos<0 then " - " ^ Int.toString(~mempos) else if mempos>0 then " + " ^ Int.toString(mempos) else ""
			 in
				emit(OPER {assem="str 's0 [falta fp,#" ^ desp ^ "]\n", src=[temp,tigerframe.fp], dst=[], jump=NONE}) 
			 end
          fun fetchTemp(temp, mempos) =
	         let
		 		val desp = if mempos<0 then " - " ^ Int.toString(~mempos) else if mempos>0 then " + " ^ Int.toString(mempos) else ""
	  	     in
				emit(OPER {assem="ldr 'd0[falta fp,#" ^ desp ^ "]\n", src=[tigerframe.fp], dst=[temp], jump=NONE})
			 end
          
          
          (*Conjunto de temporarios nuevos*)
          val newTemps = tigerset.newEmpty(String.compare) 
          (*Tabla de temp spilleados a su mempos en el frame*)
          val offset : (tigertemp.temp,int) Polyhash.hash_table = Polyhash.mkTable(Polyhash.hash,(fn (a,b) => String.compare(a,b) = EQUAL)) (100,ErrorOffset)  
          
          (* access->int ,saca el constructor InFrame*)
          fun accToInt a = case a of tigerframe.InFrame i => i
                                    |tigerframe.InReg _ => raise ErrorAccess
          
          
          (* Crea espacio en frame para el temp t y lo agrega a la tabla offset *)
          fun allocate t = Polyhash.insert offset (t,accToInt(tigerframe.allocLocal frame true)) 
        
     
          (* para usar con fold. retorna la lista de temps con modificaciones 
                      y la lista de pares (tempnuevo,mempos) para nuevas instrucciones *)      
          fun genNewTemps (t,(ts,newtmem))  = case (Polyhash.peek offset t) of SOME mempos => 
                                                                                          (let val t' = tigertemp.newtemp() in 
                                                                                             (tigerset.add (newTemps,t) ; 
                                                                                             ((t'::ts),(t',mempos)::newtmem)) 
                                                                                           end)
                                                                              |NONE        => (t::ts,newtmem)                                  
                                          
          (*Procesa cada instruccion*)
          fun procInstr (OPER  {assem = assem,src = src,dst = dst, jump = jump})  = 
             let   
                           
                           val (src',news) = foldr genNewTemps ([],[]) src
                           val (dst',newd) = foldr genNewTemps ([],[]) dst                                                                                                                 
             in
                List.app fetchTemp news ;
                emit(OPER {assem = assem,src = src',dst = dst', jump = jump}) ;
                List.app storeTemp newd
             end
          
          | procInstr (MOVE {assem = assem,src = src, dst = dst}) = 
             let
                           val (src',news) = foldr genNewTemps ([],[]) [src]
                           val (dst',newd) = foldr genNewTemps ([],[]) [dst]     
             in
                List.app fetchTemp news ;
                emit(MOVE {assem = assem,src = List.hd src',dst = List.hd dst'}) ;
                List.app storeTemp newd
            end
          | procInstr a = emit(a)   
            
   
      in    
          tigerset.app allocate spilledNodes ;
          
          List.app procInstr instrList ;
          
          (rev(!ilist) , newTemps)
      end
end      
