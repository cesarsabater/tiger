structure tigercolor :> tigercolor = 
struct

open tigerpila
open tigerliveness
open tigerset

val KCONST = List.length(tigerframe.usable)

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


(*Conjuntos de nodos*)
val precolored       : nodeSet = tigerset.newEmpty tigergraph.cmp
val initial          : nodeSet = tigerset.newEmpty tigergraph.cmp
val simplifyWorklist : nodeSet = tigerset.newEmpty(tigergraph.cmp) 
val freezeWorklist   : nodeSet = tigerset.newEmpty(tigergraph.cmp)
val spillWorklist    : nodeSet = tigerset.newEmpty(tigergraph.cmp)
val spilledNodes     : nodeSet = tigerset.newEmpty(tigergraph.cmp)
val coalescedNodes   : nodeSet = tigerset.newEmpty(tigergraph.cmp)
val coloredNodes     : nodeSet = tigerset.newEmpty(tigergraph.cmp) 
(*Pila y conjunto*)
val selectStack      : nodeSet = tigerset.newEmpty(tigergraph.cmp)
val selectPila       : tigergraph.node Pila = nuevaPila()

(*Conjuntos de moves*)
val worklistMoves    : moveSet = tigerset.newEmpty(movecmp)
val activeMoves      : moveSet = tigerset.newEmpty(movecmp)
val frozenMoves      : moveSet = tigerset.newEmpty(movecmp)
val constrainedMoves : moveSet = tigerset.newEmpty(movecmp)
val coalescedMoves   : moveSet = tigerset.newEmpty(movecmp)


val degree : (tigergraph.node,int) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,tigerset.NotFound) 

(* Asegurarse que si (u,v) esta acá tmb está (v,u)*)
val adjSet : (node * node) tigerset.set = tigerset.newEmpty movecmp

val adj_tbl : (tigergraph.node,nodeSet) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,tigerset.NotFound) 



val moveList : (tigergraph.node,moveSet) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,NotFound)
(*
val alias : (node,node) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,NotFound)
(* revisar tipo de color *)
val nodeColor : (node,tigertemp.temp) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,tigergraph.eq) (1000,NotFound)
*)

fun adjacent (v) = difference(Polyhash.find adj_tbl v,union(selectStack,coalescedNodes)) 

fun adj_app v p = tigerset.app (fn x => if not(tigerset.member(selectStack,x) orelse tigerset.member(coalescedNodes,x)) then p x else ()) (Polyhash.find adj_tbl v)  

fun push v = (pushPila selectPila v ; tigerset.add (selectStack,v))
             
fun pop() = let val n = topPila(selectPila)
 in
   popPila(selectPila) ;
   delete(selectStack,n) ;
   n             
end
(*
fun intersect a b = 
  let c = tigerset.empty(tigerset.hash,tigergraph.eq) in
     
     tigerset.app (fn x => if tigerset.member(b,x) then tigerset.add(c,x) else ()) a
  
  end
  
*)


	

fun NodeMoves(n) = 
     intersection((Polyhash.find moveList n),union(activeMoves,worklistMoves)) 

fun MoveRelated(n) = not(isEmpty(NodeMoves(n)))

fun EnableMoves n =
   let fun body m = 
      if tigerset.member(activeMoves,m) then
         (tigerset.delete(activeMoves,m) ;
         tigerset.add(worklistMoves,m)   )
      else   
         ()
   in           
     tigerset.app body (NodeMoves(n))
   end


fun DecrementDegree m  = 
   let val d = (Polyhash.find degree m) in
     Polyhash.insert degree (m,d-1) ;
     if d = KCONST then (
       adj_app m EnableMoves ; 
       EnableMoves m ;
       tigerset.delete (spillWorklist,m) ;
       if MoveRelated(m) then
          tigerset.add (freezeWorklist,m)
       else
          tigerset.add (simplifyWorklist,m)   
     ) else ()
   
   
   end
     


   
fun Simplify () = let val v = tigerset.unElem(simplifyWorklist) 
  in
     push v ;
     tigerset.delete (simplifyWorklist,v);    
     adj_app v DecrementDegree
  end      
 (*
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

    fun addEdges node = 
    let
        val scc = tigergraph.succ node
        val edges = List.filter (fn x => not (member (precolored, x))) (tigergraph.adj node)
    in
        Polyhash.insert(adj_tbl, (node,nodelist2set edges));
        Polyhash.insert(degree, (node,length edges));
        List.app (fn x => tigerset.addList(adjSet, [(x, node),(node,x)])) scc
    end
    fun peekorempty t n = case tigerset.peek(t,n) of
                            SOME s => s
                            | NONE => tigerset.empty(tigerset.hash, moveeq) 
    
(*    
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
    List.app addEdges (nodes graph)
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

