open tigerpila
open tigerliveness

type node = tigregraph.node
type move = (tigergraph.node * tigergraph.node)
type nodeSet = node Hashset.set
type moveSet = move Hashset.set

fun moveeq ((a,b),(c,d)) = (tigergraph.eq(a,c)) andalso (tigergraph.eq(b,d))

val emptySet : nodeSet = Hashset.empty(Hashset.hash,tigergraph.eq)
val emptySplaySet = Splayset.empty tigergraph.cmp

(*Conjuntos de nodos*)
val precolored
val initial 
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
  

fun Simplify () =
  
  let fun body v = push selectStack v ;
                   Hashset.delete simplifyWorklist v;    
                   Hashset.app DecrementDegree adjacent(v)
  in
     Hashset.app body simplifyWorklist                
  
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
 
fun Coalesce () =
   let fun body (x,y) = 
     
     let fun process (u,v) =
       
       if (u = v) then (
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
          Hashset.add(activeMoves,(x,y)) ;
     in
       Hashset.delete(workListMoves,(x,y)) ;
       if Hashset.member(precolored,y) then process(y,x) else process (x,y) 
               
     end  
   in
     Hashset.app body worklistMoves
   end 


fun condition1 (u,v) =
                        
    Hashset.all (fun x => if not(Hashset.member(selectStack,x) orelse Hashset.member(coalescedNodes,x)) then OKheur x v else true) 
    adjacent(v)

fun condition2 (u,v) =

   Conservative (adjacent (u)) andalso Conservative (adjacent (v))       

fun AddWorkList(u) = 
   if not(Hashset.member(precolored,u)) andalso not(MoveRelated(u)) andalso (Polyhash.find(degree,u) < KCONST) then
      Hashset.delete(freezeWorklist,u)
      Hashset.add(simplifyWorklist,u) 
   else ()
 
fun OKheur t r =  (Polyhash.find(degree,t) < KCONST) orelse Hashset.member(precolored,t) orelse Hashset.member(adjSet,(t,r))    

fun bigDegree n = length (adj n) > 10

fun nodelist2set l = Hashset.addList(Hashset.empty(Hashset.hash,tigergraph.eq), l)

(* agregamos nodos a las listas, ojo, como agregamos precolored ?? *)
fun initWorkLists (IGRAPH{graph, tnode, gtemp, moves})  = 
	
wlet
in
	List.app selectwl (nodes igraph)  
end
	
	
