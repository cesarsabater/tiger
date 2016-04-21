structure tigercolor :> tigercolor = 
struct

open tigerpila
open tigerliveness
open tigerset

type node = tigergraph.node
type move = (tigergraph.node * tigergraph.node)
type nodeSet = node set
type moveSet = move set

fun moveeq  ((a,b),(c,d)) = (tigergraph.eq(a,c)) andalso (tigergraph.eq(b,d))
fun movecmp ((a,b),(c,d)) = case tigergraph.cmp(a,c) of EQUAL => tigergraph.cmp(b,d)
                                                      |   x   => x


val emptySet : nodeSet = tigerset.newEmpty tigergraph.cmp


(*Conjuntos de nodos*)
val precolored : nodeSet = tigerset.newEmpty tigergraph.cmp
val initial : nodeSet = tigerset.newEmpty tigergraph.cmp


type allocation = (tigertemp.temp, tigerframe.register) Polyhash.hash_table



(*
val simplifyWorklist : nodeSet = tigerset.empty(tigerset.hash,tigergraph.eq) 
val freezeWorklist : nodeSet = tigerset.empty(tigerset.hash,tigergraph.eq) 
val spillWorklist : nodeSet = tigerset.empty(tigerset.hash,tigergraph.eq) 
val spilledNodes : nodeSet = tigerset.empty(tigerset.hash,tigergraph.eq) 
val coalescedNodes : nodeSet = tigerset.empty(tigerset.hash,tigergraph.eq) 
val coloredNodes : nodeSet = tigerset.empty(tigerset.hash,tigergraph.eq) 
val selectStack : tigergraph.node Pila = nuevaPila()

(*Conjuntos de moves*)
val worklistMoves : moveSet = tigerset.empty(tigerset.hash,moveeq)
val activeMoves : moveSet = tigerset.empty(tigerset.hash,moveeq)
val frozenMoves : moveSet = tigerset.empty(tigerset.hash,moveeq) 
val constrainedMoves : moveSet = tigerset.empty(tigerset.hash,moveeq)
val coalescedMoves : moveSet = tigerset.empty(tigerset.hash,moveeq)

val degree : (tigergraph.node,int) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,NotFound) 

(* Asegurarse que si (u,v) esta acá tmb está (v,u)*)
val adjSet : (node * node) tigerset.set = tigerset.empty(tigerset.hash,moveeq)

val adj_tbl : (tigergraph.node,nodeset) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,NotFound) 

val moveList : (tigergraph.node,moveSet) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,NotFound)
val alias : (node,node) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,NotFound)
(* revisar tipo de color *)
val nodeColor : (node,tigertemp.temp) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,NotFound)

fun adjacent (v) = (Polyhash.find adj_tbl v)  

fun intersect a b = 
  let c = tigerset.empty(tigerset.hash,tigergraph.eq) in
     
     tigerset.app (fn x => if tigerset.member(b,x) then tigerset.add(c,x) else ()) a
  
  end
  

fun Simplify () =
  
  let fun body v = push selectStack v ;
                   tigerset.delete simplifyWorklist v;    
                   tigerset.app DecrementDegree adjacent(v)
  in
     tigerset.app body simplifyWorklist                
    
  end  

fun NodeMoves(n) = 
  let c = tigerset.empty(tigerset.hash,tigergraph.eq) in
     tigerset.app (fun x => if (tigerset.member(activeMoves,x) orelse tigerset.member(worklistMoves,x)) 
                           then tigerset.add(c,x) else ()) (Polyhash.find moveList n) ;
     c
  end                         

fun EnableMoves n =
   let fun body m = 
      if tigerset.member(activeMoves,m) then
         tigerset.delete(activeMoves,m) ;
         tigerset.add(worklistMoves,m)
      else   
         ()
   in           
     tigerset.app body NodeMoves(n)
   end

fun DecrementDegree m  = 
   let val d = (Polyhash.find degree m) in
     Polyhash.insert degree (m,d-1) ;
     if d = KCONST then (
       tigerset.app (fun x => if not(tigerset.member(selectStack,x) orelse tigerset.member(coalescedNodes,x)) then EnableMoves x else ())
                    adjacent(m) ; EnableMoves m ;
       tigerset.delete (spillWorklist,m) ;
       if Moverelated(m) then
          tigerset.add (freezeWorklist,m)  ;
       else
          tigerset.add (simplifyWorklist,m);   
     ) else ()
   
   
   end
 
fun Coalesce () =
   let fun body (x,y) = 
     
     let fun process (u,v) =
       
       if (u = v) then (
          tigerset.add(coalescedMoves,(x,y)) ;
          AddWorkList(u)
       ) else if (tigerset.member(precolored,v) orelse tigerset.member(adjSet,(u,v))) then (
          tigerset.add(constrainedMoves,(x,y)) ;
          AddWorkList(u) ;
          AddWorkList(v) 
       ) else if (tigerset.member(precolored,u) andalso condition1 (u,v) ) orelse (not(tigerset.member(precolored,u)) andalso condition2(u,v) ) then (
          tigerset.add(coalescedMoves,(x,y)) ;
          Combine(u,v) ;
          AddWorkList(u);
       ) else  
          tigerset.add(activeMoves,(x,y)) ;
     in
       tigerset.delete(workListMoves,(x,y)) ;
       if tigerset.member(precolored,y) then process(y,x) else process (x,y) 
               
     end  
   in
     tigerset.app body worklistMoves
   end 


fun condition1 (u,v) =
                        
    tigerset.all (fun x => if not(tigerset.member(selectStack,x) orelse tigerset.member(coalescedNodes,x)) then OKheur x v else true) 
    adjacent(v)

fun condition2 (u,v) =

   Conservative (adjacent (u)) andalso Conservative (adjacent (v))       

fun AddWorkList(u) = 
   if not(tigerset.member(precolored,u)) andalso not(MoveRelated(u)) andalso (Polyhash.find(degree,u) < KCONST) then
      tigerset.delete(freezeWorklist,u)
      tigerset.add(simplifyWorklist,u) 
   else ()
 
fun OKheur t r =  (Polyhash.find(degree,t) < KCONST) orelse tigerset.member(precolored,t) orelse tigerset.member(adjSet,(t,r))    
*)

fun nodelist2set l = tigerset.addList((tigerset.newEmpty tigergraph.cmp), l)

fun initialize (IGRAPH{graph, tnode, gtemp, moves})  = 
let
(*
    fun addEdges node = 
    let
        val scc = succ node
        val edges = filter (fn x => not (member (precolored, x))) (adj node)
    in
        Polyhash.insert(adj_tbl, nodelist2set edges);
        Polyhash.insert(degree, length edges);
        List.app (fn x => tigerset.addList(adjSet, [(x, node),(node,x)])) scc
    end
    fun peekorempty t n = case tigerset.peek(t,n) of
                            SOME s => s
                            | NONE => tigerset.empty(tigerset.hash, moveeq) 
    fun addMoves (d,s) = 
    let
        val movelistd = (peekorempty moveList d) 
        val movelists = (peekorempty moveList s)
    in 
        tigerset.add(movelistd, (d,s));
        tigerset.add(movelists, (d,s));
        tigerset.add(wokrlistMoves, (d,s);
        Polyhash.insert(d, movelistd);
        Polyhash.insert(s, movelists);
    end
*)
    
    val precoloredList = Splayset.listItems(tigerliveness.getPrecoloredNodes())
in
    (* precolored *)
	tigerset.addList(precolored, precoloredList);
    (* initial *)
    tigerset.addList(initial, tigergraph.nodes graph);
    List.app (fn x => tigerset.delete(initial, x)) precoloredList
    (* add_tbl (adjList), adjSet y degree *)
(*
    List.app addEdges (nodes graph)
*)
    (* moveList y workListmoves *) 
(*
    List.app addMoves moves
*)
end


(* mandamos cada nodo a su worklist *) 
(*
fun selectWL node = 
let
    val deg = Polyhash.find(degree, node)
in
    if deg >= tigerset.numItems(precolored)
    then tigerset.add(spillWorklist, node)
    else if MoveRelated(node) 
        then tigerset.add(freezeWorkList, node)
        else tigerset.add(simplifyWorkList, node)   
end

fun makeWorklist () = tigerset.app selectWL initial
*)

fun main igraph = initialize igraph 

end

