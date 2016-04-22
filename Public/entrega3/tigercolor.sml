structure tigercolor :> tigercolor = 
struct

open tigerpila
open tigerliveness
open tigerset

val KCONST = List.length(tigerframe.usable)

type node = tigertemp.temp
type move = (node * node)
type nodeSet = node set
type moveSet = move set

fun nodeeq (a,b) = (String.compare(a,b) = EQUAL)
fun nodecmp (a,b) = String.compare(a,b)

fun moveeq  ((a,b),(c,d)) = nodeeq(a,c) andalso nodeeq(b,d)
fun movecmp ((a,b),(c,d)) = case nodecmp(a,c) of EQUAL => nodecmp(b,d)
                                                       |   x   => x
fun nodelist2set l = 
let 
	val newset = tigerset.newEmpty nodecmp 
in 
	tigerset.addList(newset,l) ; 
	newset 
end

val emptySet : nodeSet = tigerset.newEmpty nodecmp

(*Conjuntos de nodos*)
val precolored : nodeSet = tigerset.newEmpty nodecmp
val initial : nodeSet = tigerset.newEmpty nodecmp

type allocation = (tigertemp.temp, tigerframe.register) Polyhash.hash_table


(*Conjuntos de nodos*)
val precolored  : nodeSet = nodelist2set tigerframe.usable
val initial          : nodeSet = tigerset.newEmpty nodecmp
val simplifyWorklist : nodeSet = tigerset.newEmpty(nodecmp) 
val freezeWorklist   : nodeSet = tigerset.newEmpty(nodecmp)
val spillWorklist    : nodeSet = tigerset.newEmpty(nodecmp)
val spilledNodes     : nodeSet = tigerset.newEmpty(nodecmp)
val coalescedNodes   : nodeSet = tigerset.newEmpty(nodecmp)
val coloredNodes     : nodeSet = tigerset.newEmpty(nodecmp) 
(*Pila y conjunto*)
val selectStack      : nodeSet = tigerset.newEmpty(nodecmp)
val selectPila       : node Pila = nuevaPila()

(*Conjuntos de moves*)
val worklistMoves    : moveSet = tigerset.newEmpty(movecmp)
val activeMoves      : moveSet = tigerset.newEmpty(movecmp)
val frozenMoves      : moveSet = tigerset.newEmpty(movecmp)
val constrainedMoves : moveSet = tigerset.newEmpty(movecmp)
val coalescedMoves   : moveSet = tigerset.newEmpty(movecmp)


val degree : (node,int) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,tigerset.NotFound) 

(* Asegurarse que si (u,v) esta acá tmb está (v,u)*)
val adjSet : (node * node) tigerset.set = tigerset.newEmpty movecmp

val adjList : (node,nodeSet) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,tigerset.NotFound) 



val moveList : (node,moveSet) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,NotFound)

val alias : (node,node) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,NotFound)
(*

(* revisar tipo de color *)
val nodeColor : (node,tigertemp.temp) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,NotFound)
*)

fun adjacent (v) = difference(Polyhash.find adjList v,union(selectStack,coalescedNodes)) 

fun adj_app v p = tigerset.app (fn x => if not(tigerset.member(selectStack,x) orelse tigerset.member(coalescedNodes,x)) then p x else ()) (Polyhash.find adjList v)  

fun adj_fold v f e = tigerset.fold (fn (x,b) => if not(tigerset.member(selectStack,x) orelse tigerset.member(coalescedNodes,x)) then f(x,b) else b) e (Polyhash.find adjList v)


fun push v = (pushPila selectPila v ; tigerset.add (selectStack,v))
             
fun pop() = let val n = topPila(selectPila)
 in
   popPila(selectPila) ;
   delete(selectStack,n) ;
   n             
end
(*
fun intersect a b = 
  let c = tigerset.empty(tigerset.hash,nodeeq) in
     
     tigerset.app (fn x => if tigerset.member(b,x) then tigerset.add(c,x) else ()) a
  
  end
  
*)

fun AddEdge(u,v) =
   if not(member(adjSet,(u,v))) andalso not(nodeeq(u,v)) then (
      addList(adjSet,[(u,v),(v,u)]) ; (
      if not(member(precolored,u)) then (
         add(Polyhash.find adjList u,v) ;
         Polyhash.insert degree (u,((Polyhash.find degree u)+ 1))) else () ;
      if not(member(precolored,v)) then (
         add(Polyhash.find adjList v,u) ;
         Polyhash.insert degree (v,(Polyhash.find degree v)+1)) else ())
   ) else ()        
      
	

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
 
       
fun GetAlias (n) = if member(coalescedNodes,n) then GetAlias(Polyhash.find alias n) else n  

fun AddWorkList(u) = 
   if not(member(precolored,u)) andalso not(MoveRelated(u)) andalso (Polyhash.find degree u < KCONST) then
      (delete(freezeWorklist,u) ;
      add(simplifyWorklist,u)) 
   else ()

fun OKheur (t,r) = (Polyhash.find degree t) < KCONST orelse member(precolored,t) orelse member(adjSet,(t,r)) 

fun Conservative (nodes) = let fun count (x,i)  = if (Polyhash.find degree x >= KCONST) then (i+1) else i
 in
   (tigerset.fold count 0 nodes) < KCONST
 end 

fun condition1 (u,v) = adj_fold v (fn (t,b) =>  b andalso OKheur(t,u)) true 

fun condition2 (u,v) =
    Conservative(union(adjacent(u),adjacent(v)))

   

  
 
fun Combine(u,v) = let val movelistu = Polyhash.find moveList u 
                       val movelistv = Polyhash.find moveList v
                       val moveunion = union(movelistu,movelistv) 
                                              
 in
   (if member(freezeWorklist,v) then
       delete(freezeWorklist,v)
   else 
      delete(spillWorklist,v)
   ) ;
   add(coalescedNodes,v);
   Polyhash.insert alias (v,u) ;
   Polyhash.insert moveList (u,moveunion) ;
   EnableMoves(v) ;
   adj_app v (fn t => (AddEdge(t,u);DecrementDegree(t)) )  ;
   if (Polyhash.find degree u >= KCONST) andalso (member(freezeWorklist,u)) then( 
      delete(freezeWorklist,u) ;
      add(spillWorklist,u) ) 
   else ()
 end       

fun Coalesce () = let val (x,y) = unElem(worklistMoves) 
                      val (x',y') = (GetAlias(x),GetAlias(y))
                      val (u,v) = if member(precolored,y') then (y',x') else (x',y') 
 in
      
    delete(worklistMoves,(x,y)) ;
    if nodeeq(u,v) then ( (* u = v *)
       add(coalescedMoves,(x,y)) ;
       AddWorkList(u)
    ) else if (member(precolored,v) orelse member(adjSet,(u,v))) then (
       add(constrainedMoves,(x,y)) ;
       AddWorkList(u) ;
       AddWorkList(v) 
    ) else if (member(precolored,u) andalso condition1 (u,v) ) orelse (not(member(precolored,u)) andalso condition2(u,v)) then (
         add(coalescedMoves,(x,y)) ;
         Combine(u,v) ;
         AddWorkList(u)
      ) else  
         add(activeMoves,(x,y)) 
 end       


(*
fun Freeze() = let val u = freezeheuristic() 
 in
    Hashset.delete(freezeWorklist,u) ;
    Hashset.add(simplifyWorklist,u) ;
    FreezeMoves(u)
 end
                                               (* GetAlias(y) = GetAlias(u)*)
fun FreezeMoves(u) = let fun body (x,y) = let (val v = if nodeeq(GetAlias(y),GetAlias(u)) then GetAlias(x) else GetAlias(y))
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
    
*)

(*
fun initialize (IGRAPH{graph, tnode, gtemp, moves})  = 
let

    fun addEdges node = 
    let
        val scc = tigergraph.succ node
        val edges = List.filter (fn x => not (member (precolored, x))) (tigergraph.adj node)
    in
        Polyhash.insert adjList (node,nodelist2set edges) ;
        Polyhash.insert degree (node,length edges) ;
        List.app (fn x => tigerset.addList(adjSet, [(x, node),(node,x)])) scc
    end
   
*)
    
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
    
(*
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
    List.app addMoves moves
end
*)

								
												
fun peekorempty table element comparacion = case Polyhash.peek table element  of
					 SOME s => s
					| NONE => tigerset.newEmpty comparacion


fun MakeWorklist () = 
let
	fun selectWL tmp = 
	let
		val deg = Polyhash.find degree tmp
	in
		if deg >= KCONST
		then tigerset.add(spillWorklist, tmp)
		else if MoveRelated(tmp) 
			then tigerset.add(freezeWorklist, tmp)
			else tigerset.add(simplifyWorklist, tmp)   
	end
in
	tigerset.app selectWL initial
end

fun main liveout (tigerflow.FGRAPH{control, def, use, ismove}, ilist) = 
let
	fun Build () = 
	let
		(* valores vivos en cada instruccion, empezando por la ultima *)
		fun procInstr instr = 
		let
			val live = nodelist2set (liveout instr)
			val ismove' = Splaymap.find(ismove, instr)
			val use' = nodelist2set(Splaymap.find(use, instr))
			val def' = nodelist2set(Splaymap.find(def, instr))
			fun addToMoveList n mv = 
			let  
				val moveList_n = peekorempty moveList n movecmp
			in 
				tigerset.add(moveList_n, mv);
				Polyhash.insert moveList (n, moveList_n)
			end
		in 	
			(*agregamos nodos a initial *)
			initial := !(tigerset.union(initial, union(use',def')));
			(*hacemos el resto del build*)
			if ismove' then 
				let
					val src = unElem use'
					val dst = unElem def'
				in 
					(tigerset.delete(live, src);
					addToMoveList src (dst, src); 
					addToMoveList dst (dst, src);
					add (worklistMoves, (dst, src)))
				end
			else () ;
			app (fn d => app (fn l => AddEdge(l,d)) live) def'
		end	
	in  
		List.app procInstr (rev ilist); (*app aplica de izquierda a derecha entonces funca*)
		initial := !(difference(initial, precolored))
	end
in
	Build () ; 
	MakeWorklist ()
end
	

end

