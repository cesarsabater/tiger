open tigerpila


type node = tigregraph.node
type move = (tigergraph.node * tigergraph.node)
type nodeSet = node Hashset.set
type moveSet = move Hashset.set

fun moveeq ((a,b),(c,d)) = (tigergraph.eq(a,c)) andalso (tigergraph.eq(b,d))

val emptySet : nodeSet = Hashset.empty(Hashset.hash,tigergraph.eq)

(*Conjuntos de nodos*)
val precolored : nodeset = Hashset.empty(Hashset.hash,tigergraph.eq) 
val initial : nodeSet = Hashset.empty(Hashset.hash,tigergraph.eq) 
val simplifyWorklist : nodeSet = Hashset.empty(Hashset.hash,tigergraph.eq) 
val freezeWorklist : nodeSet = Hashset.empty(Hashset.hash,tigergraph.eq) 
val spillWorklist : nodeSet = Hashset.empty(Hashset.hash,tigergraph.eq) 
val spilledNodes : nodeSet = Hashset.empty(Hashset.hash,tigergraph.eq) 
val coalescedNodes : nodeSet = Hashset.empty(Hashset.hash,tigergraph.eq) 
val coloredNodes : nodeSet = Hashset.empty(Hashset.hash,tigergraph.eq) 
val selectStack : tigergraph.node Pila = nuevaPila()

(*Conjuntos de moves*)
val worklistMoves : moveSet = Hashset.empty(Hashset.hash,moveeq)
val activeMoves : moveSet = Hashset.empty(Hashset.hash,moveeq)
val frozenMoves : moveSet = Hashset.empty(Hashset.hash,moveeq) 
val constrainedMoves : moveSet = Hashset.empty(Hashset.hash,moveeq)
val coalescedMoves : moveSet = Hashset.empty(Hashset.hash,moveeq)

val degree : (tigergraph.node,int) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,NotFound) 

(* Asegurarse que si (u,v) esta acá tmb está (v,u)*)
val adjSet : (node * node) Hashset.set = Hashset.empty(Hashset.hash,moveeq)

val adj_tbl : (tigergraph.node,nodeset) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,NotFound) 

val moveList : (tigergraph.node,move) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,NotFound)
val alias : (node,node) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,NotFound)
(* revisar tipo de color *)
val nodeColor : (node,tigertemp.temp) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,NotFound)

fun adjacent (v) = (Polyhash.find adj_tbl v)  


fun intersect a b = 
  let c = Hashset.empty(Hashset.hash,tigergraph.eq) in
     
     Hashset.app (fn x => if Hashset.member(b,x) then Hashset.add(c,x) else ()) a
  
  end
  

fun Simplify () = let val v = Hashset.unelem(simplifyWorklist) 
  in
     push selectStack v ;
     Hashset.delete simplifyWorklist v;    
     Hashset.app DecrementDegree adjacent(v)
  end  

fun NodeMoves(n) = 
  let c = Hashset.empty(Hashset.hash,tigergraph.eq) in
     Hashset.app (fun x => if (Hashset.member(activeMoves,x) orelse Hashset.member(worklistMoves,x)) 
                           then Hashset.add(c,x) else ()) (Polyhash.find moveList n) ;
     c 
  end                         

fun EnableMoves n =
   let fun body m = 
      if Hashset.member(activeMoves,m) then
         Hashset.delete(activeMoves,m) ;
         Hashset.add(worklistMoves,m)
      else   
         ()
   in           
     Hashset.app body NodeMoves(n)
   end

fun DecrementDegree m  = 
   let val d = (Polyhash.find degree m) in
     Polyhash.insert degree (m,d-1) ;
     if d = KCONST then (
       Hashset.app (fun x => if not(Hashset.member(selectStack,x) orelse Hashset.member(coalescedNodes,x)) then EnableMoves x else ())
                    adjacent(m) ; EnableMoves m ;
       Hashset.delete (spillWorklist,m) ;
       if Moverelated(m) then
          Hashset.add (freezeWorklist,m)  ;
       else
          Hashset.add (simplifyWorklist,m);   
     ) else ()
   
   
   end
 
fun Coalesce () = let val (x,y) = Hashset.unelem(worklistMoves) 
                      val (x',y') = (GetAlias(x),GetAlias(y))
                      val (u,v) = if Hashset.member(precolored,y') then (y',x') else (x',y') 
   in
      
      Hashset.delete(workListMoves,(x,y)) ;
      if tigergraph.eq(u,v) then ( (* u = v *)
         Hashset.add(coalescedMoves,(x,y)) ;
         AddWorkList(u)
      ) else if (Hashset.member(precolored,v) orelse Hashset.member(adjSet,(u,v))) then (
         Hashset.add(constrainedMoves,(x,y)) ;
         AddWorkList(u) ;
         AddWorkList(v) 
      ) else if (Hashset.member(precolored,u) andalso condition1 (u,v) ) orelse (not(Hashset.member(precolored,u)) andalso condition2(u,v) ) then (
         Hashset.add(coalescedMoves,(x,y)) ;
         Combine(u,v) ;
         AddWorkList(u);
      ) else  
         Hashset.add(activeMoves,(x,y)) 
 
   end       
               
 


fun condition1 (u,v) =
                        
    Hashset.all (fun x => if not(Hashset.member(selectStack,x) orelse Hashset.member(coalescedNodes,x)) then OKheur x v else true) 
    adjacent(v)

fun condition2 (u,v) =

   Conservative (adjacent (u), adjacent (v))       

fun AddWorkList(u) = 
   if not(Hashset.member(precolored,u)) andalso not(MoveRelated(u)) andalso (Polyhash.find(degree,u) < KCONST) then
      Hashset.delete(freezeWorklist,u)
      Hashset.add(simplifyWorklist,u) 
   else ()
 
fun OKheur t r =  (Polyhash.find(degree,t) < KCONST) orelse Hashset.member(precolored,t) orelse Hashset.member(adjSet,(t,r))    

fun Conservative (a,b) =
   let fun count (x,i)  = if (Polyhash.find degree x >= KCONST) (i+1) else i   
       val s1 = Hashset.fold (fun (x,i) => if not(Hashset.member(selectStack,x) orelse Hashset.member(coalescedNodes,x)) then count x i else i) 0 a
       val s2 = Hashset.fold (fun (x,i) => if not(Hashset.member(a,x) orelse Hashset.member(selectStack,x) orelse Hashset.member(coalescedNodes,x)) then count x i else i) s1 b
   in
     (s2 < KCONST)
   end

fun GetAlias (n) = if Hashset.member(coalescedNodes,n) then GetAlias(Polyhash.find(alias,n)) else n         

fun Combine(u,v) = let val movelistu = Polyhash.find moveList u 
                       val movelistv = Polyhash.find moveList v
                       val moveunion = Set.union(movelistu,movelistv) 
                       
                       fun body t = AddEdge(t,u) ;
                                    DecrementDegree(t)   
                       
 in
   (if Hashset.member(freezeWorklist,v) then
      Hashset.delete(freezeWorklist,v)
   else 
      Hashset.delete(spillWorklist,v)) ;
   Hashset.add(coalescedNodes,v);
   Polyhash.insert(alias,(v,u)) ;
   Polyhash.insert(movelist,(u,moveunion) ;
   enableMoves(v) ;
   (Hashset.app (fun t => if not(Hashset.member(selectStack,t) orelse Hashset.member(coalescedNodes,t)) then body t else ()) Adjacent(v)) ;
   if (Polyhash.find degree u >= KCONST) andalso (Hashset.member(freezeWorklist,u)) then
      Hashset.delete(freezeWorklist,u) ;
      Hashset.add(spillWorklist,u) 
   else ()
 end       

fun Freeze() = let val u = freezeheuristic() 
 in
    Hashset.delete(freezeWorklist,u) ;
    Hashset.add(simplifyWorklist,u) ;
    FreezeMoves(u)
 end
                                               (* GetAlias(y) = GetAlias(u)*)
fun FreezeMoves(u) = let fun body (x,y) = let (val v = if tigergraph.eq(GetAlias(y),GetAlias(u)) then GetAlias(x) else GetAlias(y))
                          in
                            Hashset.delete(activeMoves,(x,y)) ;
                            Hashset.add(frozenMoves(x,y)) ;
                            if (Hashset.isEmpty(NodeMoves(v))) andalso (Polyhash.find degree v < KCONST) then
                               Hashset.delete(freezeWorklist,v) ;
                               Hashset.add(simplifyWorklist,v)
                            else 
                               ()
                          end     
  in
     Hashset.app body NodeMoves(u)
  end           
 
 
(* Spill worklist*) 
fun spillheuristic() = 

fun SelectSpill() = let val m = spillheuristic() 
  
  in
     Hashset.delete(spillWorklist,m) ;
     Hashset.add(simplifyWorklist,m) ;
     FreezeMoves(m)
  end                                      

fun AssignColors() = let fun body() = let val n = topPila(SelectStack)
                                          val okColors
                                          fun rmvColors w = if (Hashset.member(coloredNodes,GetAlias(w)) orelse
                                                                Hashset.member(precolored,GetAlias(w))) then
                                                                Hashset.remove(okColors,(Polyhash.find color GetAlias(w))) 
                                                             else () 
                          in
                            popPila(SelectStack) ;
                            Hashset.app rmvColors (Polyhash.find(adjList,n)) ;
                            if Hashset.isEmpty(okColors) then
                              Hashset.add(spilledNodes,n) 
                            else (
                              Hashset.add(coloredNodes,n) ;
                              Polyhash.insert color (n,Hashset.unelem(okColors)) 
                            ) ;
                          end  
                         fun coalescedcolor n = Polyhash.insert color (n,(Polyhash.find color GetAlias(n))) 
                            
 in                            
   (while not(estaVacia(SelectStack)) do body()) ;
   Hashset.app coalescedcolor coalescedNodes
 end
    
  
  
 
