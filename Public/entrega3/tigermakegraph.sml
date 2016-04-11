structure tigermakegraph :> tigermakegraph = struct

   
    open tigerassem
    open tigergraph
 
     
    
   fun instrs2graph instr_list = 
    
    let 
      
      val control = newGraph() 
      (*val def = Graph.newTable()
      val use = Graph.newTable()
      val ismove = Graph.newTable()
      val ttab = Graph.newTable() 
       *)
      
      (* obtengo un nodo por instruccion *)
      fun nodeFromInstr _ = newNode (control)
  
      val node_list = List.map nodeFromInstr instr_list
  
      (*val useFromInstr : (node * instr * ((tigertemp.temp list) table)) -> ((tigertemp.temp list) table)*)
      fun useFromInstr (node, OPER { src, ... }, tabla)  = Splaymap.insert (tabla,node,src)  
      |   useFromInstr (node, MOVE { src, ... }, tabla) =  Splaymap.insert (tabla,node,[src])
      |   useFromInstr (node, LABEL { ... }, tabla) = tabla
  
      fun defFromInstr (node, OPER { dst, ... }, tabla)  = Splaymap.insert (tabla,node,dst)  
      |   defFromInstr (node, MOVE { dst, ... }, tabla) =  Splaymap.insert (tabla,node,[dst])
      |   defFromInstr (node, LABEL { ... }, tabla) = tabla
 
      fun moveFromInstr (node, OPER {... }, tabla)  = Splaymap.insert (tabla,node,false)  
      |   moveFromInstr (node, MOVE { ... }, tabla) =  Splaymap.insert (tabla,node,true)
      |   moveFromInstr (node, LABEL { ... }, tabla) = Splaymap.insert (tabla,node,false)
  
       
      
      
      (* fun foldinstr x = let val t = tabNueva ()
	                    in ListPair.foldr useFromInstr t x 
	                    end *)
	  
	  val tabVacia : (tigertemp.temp list) table = newTable()
	  val tabVaciab : bool table = newTable()
	  
	  val use (* : (tigertemp.temp list) table *) = 
	      ListPair.foldr useFromInstr tabVacia (node_list, instr_list)
	  
	  val def = ListPair.foldr defFromInstr tabVacia (node_list, instr_list)
	  
	  val isMove = ListPair.foldr moveFromInstr tabVaciab (node_list, instr_list)
	  
	  (*fun findlabel node_list lab = *)  
	  
	  (* val use = foldinstr (instr_nodes, instr_list) *)
      (* val use = ListPair.foldr useFromInstr (tabNueva()) (instr_nodes, instr_list) *)
  
  
    (*  fun getdef OPER {_ , dst, _ , _}  = dst
      |   getdef MOVE {_  ,dst, _ } = dst
      |   getdef LABEL {_, _} = []
      
      fun getuse OPER {_ , _, src , _}  = src
      |   getuse MOVE {_  ,_, src } = src
      |   getuse LABEL {_, _} = []
        
      fun getmove OPER {_ , _, src , _}  = false
      |   getmove MOVE {_  ,_, src } = true
      |   getmove LABEL {_, _} = false
     
     
      
     *)
     
     (*  fun instrToNode (instr, tab) = tabRinsert ( (newNode(control), instr) , tab) 
      
      fun makeNodes instrlist = foldl instrToNode ttab instrlist
      
      fun findLabel lab table = 
      
      fun getdef OPER {_ , dst, _ , _}  = dst
      |   getdef MOVE {_  ,dst, _ } = dst
      |   getdef LABEL {_, _} = []
      
      fun getuse OPER {_ , _, src , _}  = src
      |   getuse MOVE {_  ,_, src } = src
      |   getuse LABEL {_, _} = []
        
      fun getmove OPER {_ , _, src , _}  = false
      |   getmove MOVE {_  ,_, src } = true
      |   getmove LABEL {_, _} = false
      
      fun getjump OPER {_ , _, _ , SOME labs}  = labs
      |   getjump OPER {_ , _, _ , NONE}  = []
      |   getjump MOVE {_  ,_, _ } = []
      |   getjump LABEL {_, _ } = []
      
      fun connect node labs = 
      
      val def = tabAplica getdef control
      val use = tabAplica getuse control
      val ismove = tabAplica getmove control


      fun findLabel lab *)
    in 
       {}
       
    end      



end
