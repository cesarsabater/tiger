structure tigermakegraph :> tigermakegraph = struct

    open tigerassem
    open tigergraph
    open tigertab
    
    
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
  
      val instr_nodes = map nodeFromInstr instr_list
  
      fun useFromInstr (node, OPER { src, ... }, tabla)  = tabInserta (node, src, tabla)
      |   useFromInstr (node, MOVE { src, ... }, tabla) = tabInserta (node, [src], tabla)
      |   useFromInstr (node, LABEL { ... }, tabla) = tabla
  
	  val foldinstr : (node list * instr list) -> instr table 
	  fun foldinstr = ListPair.foldr useFromInstr (tabNueva ()) 
	  val use = foldinstr (instr_nodes, instr_list)
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
