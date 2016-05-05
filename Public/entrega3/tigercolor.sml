structure tigercolor :> tigercolor = 
struct

open tigerpila
open tigerliveness
open tigerset

(*Definiciones de nodos y moves*)
type node = tigertemp.temp
type move = (node * node)
type nodeSet = node set
type moveSet = move set


type allocation = (tigertemp.temp, tigerframe.register) Polyhash.hash_table


fun getSpilled (instrlist, frame) = 
let

(*KCONST colores*)
val KCONST = List.length(tigerframe.usable)

fun printWL wl = 
  List.app (fn tmp => print (tmp^"\n")) (listItems wl)



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

(**)

val emptySet : nodeSet = tigerset.newEmpty nodecmp

(*Conjuntos de nodos*)



(*Conjuntos de nodos*)
val precolored  : nodeSet = nodelist2set tigerframe.usable
val specialreg : nodeSet = nodelist2set tigerframe.specialregs
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

exception ErrorDegree
exception ErrorAdjList
exception ErrorMoveList
exception ErrorAlias
exception ErrorColor
exception ErrorCount

val degree : (node,int) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,ErrorDegree) 

(* Asegurarse que si (u,v) esta acá tmb está (v,u)*)
val adjSet : (node * node) tigerset.set = tigerset.newEmpty movecmp

val adjList : (node,nodeSet) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,ErrorAdjList) 

val moveList : (node,moveSet) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,ErrorMoveList)

val alias : (node,node) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,ErrorAlias)
(*

(* revisar tipo de color *)
val nodeColor : (node,tigertemp.temp) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,NotFound)
*)

val color : (node,tigertemp.temp) Polyhash.hash_table = Polyhash.mkTable(Polyhash.hash,nodeeq) (1000,ErrorColor)

val usedefcount : (node,int) Polyhash.hash_table = Polyhash.mkTable(Polyhash.hash,nodeeq) (1000,ErrorCount)

fun debugfind ht i = case Polyhash.peek ht i of SOME d => d
                                             |  NONE => (print i ; Polyhash.find ht i) 

fun findinitNS ht i = let val ne : nodeSet = newEmpty(nodecmp) in 
     case Polyhash.peekInsert ht (i,ne) of SOME s => s
                                         | NONE => ne  
end

fun findinitI ht i = case Polyhash.peekInsert ht (i,0) of SOME n => n
                                                         |  NONE => 0
                                                         
fun findinitMS ht i = let val ne : moveSet = newEmpty(movecmp) in 
      case Polyhash.peekInsert ht (i,ne) of SOME s => s
                                           | NONE => ne  
end    



fun adjacent (v) = difference(findinitNS adjList v,union(selectStack,coalescedNodes)) 

fun adj_app v p = tigerset.app (fn x => if not(tigerset.member(selectStack,x) orelse tigerset.member(coalescedNodes,x)) then p x else ()) (findinitNS adjList v)  

fun adj_fold v f e = tigerset.fold (fn (x,b) => if not(tigerset.member(selectStack,x) orelse tigerset.member(coalescedNodes,x)) then f(x,b) else b) e (findinitNS adjList v)

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

fun peekOrEmpty table element comparacion = case Polyhash.peek table element  of
			 SOME s => s
			| NONE => tigerset.newEmpty comparacion


fun AddEdge(u,v) =
   if not(member(adjSet,(u,v))) andalso not(nodeeq(u,v)) then (
      addList(adjSet,[(u,v),(v,u)]) ; (
      if not(member(precolored,u)) then (
         add(findinitNS adjList u,v) ;
         Polyhash.insert degree (u,((findinitI degree u)+ 1))) else () ;
      if not(member(precolored,v)) then (
         add(findinitNS adjList v,u) ;
         Polyhash.insert degree (v,(findinitI degree v)+1)) else ())
   ) else ()        
      

fun NodeMoves(n) = 
     intersection((findinitMS moveList n),union(activeMoves,worklistMoves)) 

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
   let val d = (findinitI degree m) in
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
   if not(member(precolored,u)) andalso not(MoveRelated(u)) andalso (findinitI degree u < KCONST) then
      (delete(freezeWorklist,u) ;
      add(simplifyWorklist,u)) 
   else ()

fun OKheur (t,r) = (findinitI degree t) < KCONST orelse member(precolored,t) orelse member(adjSet,(t,r)) 

fun Conservative (nodes) = let fun count (x,i)  = if (findinitI degree x >= KCONST) then (i+1) else i
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
   if (findinitI degree u >= KCONST) andalso (member(freezeWorklist,u)) then( 
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
 
                                                          (* GetAlias(y) = GetAlias(u)*)
fun FreezeMoves(u) = let fun body (x,y) = let val v = if nodeeq(GetAlias(y),GetAlias(u)) then GetAlias(x) else GetAlias(y)
                          in
                            delete(activeMoves,(x,y)) ;
                            add(frozenMoves,(x,y)) ;
                            if (isEmpty(NodeMoves(v))) andalso (findinitI degree v < KCONST) then
                               (delete(freezeWorklist,v) ;
                               add(simplifyWorklist,v))
                            else 
                               ()
                          end     
  in
     tigerset.app body (NodeMoves(u))
  end           


fun Freeze() = let val u = unElem freezeWorklist 
 in
    delete(freezeWorklist,u) ;
    add(simplifyWorklist,u) ;
    FreezeMoves(u)
 end
 
 
(* Spill worklist*) 
fun spillcost n = Real.fromInt(Polyhash.find usedefcount n) / Real.fromInt(findinitI degree n) 

fun spillheuristic() = let val x = unElem spillWorklist 
                           val (spillnode, _ ) = fold (fn (n,(min,mincost)) => if (spillcost n < mincost) then (n,spillcost n) else (min,mincost)) (x,spillcost x) spillWorklist
 in
    spillnode
 end
fun SelectSpill() = let val m = spillheuristic() 
  
  in
     delete(spillWorklist,m) ;
     add(simplifyWorklist,m) ;
     FreezeMoves(m)
  end                                      

exception Special

fun AssignColors() = let fun body() = let val n = pop()
                                          val okColors = nodelist2set tigerframe.usable
                                          fun rmvColors w = (
                                                             if (member(coloredNodes,GetAlias(w)) orelse
                                                                member(precolored,GetAlias(w))) then (
                                                                
                                                                let val wcolor = Polyhash.find color (GetAlias(w)) in
                                                                ( 
                                                                 (if not(member(specialreg,wcolor)) then delete(okColors,wcolor) else ()) 
                                                                
                                                                )
                                                                end
                                                                )
                                                             else () )
                          in
                            
                            if not(member(specialreg,n)) then (
                              tigerset.app rmvColors (Polyhash.find adjList n) ;
                              
                              if isEmpty(okColors) then
                                add(spilledNodes,n) 
                              else ( 
                                add(coloredNodes,n) ;
                                Polyhash.insert color (n, unElem(okColors)) 
                              ) )
                            else ()   
                          end  
                          fun coalescedcolor n =  ( (* print "coalesced" ;
                                                  print n ; print "\n" ; *)
                                                  Polyhash.insert color (n,(Polyhash.find color (GetAlias(n)))) 
                            )
 in                     
(*
   print "SelectStack : \n" ;       
   printWL selectStack ;
   print "\n\n" ;
*)
   (while not(isEmpty(selectStack)) do body()) ;
   tigerset.app coalescedcolor coalescedNodes
 end
    

fun printTable ht = Polyhash.apply (fn (x,d) => (print x ; print "->" ; print d ; print "\n" )) ht

type temp = tigertemp.temp
fun newTempSet () = tigerset.newEmpty String.compare
										 

fun MakeWorklist () = 
let
	fun selectWL tmp = 
	let
		val deg = findinitI degree tmp
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


	(* flowgraph *)
	val (flowgraph, ilist) = tigerflow.instrs2graph instrlist
	val (tigerflow.FGRAPH{control, def, use, ismove}) = flowgraph
	(* liveness analysis*)
	val (_,liveout) = tigerliveness.interferenceGraph flowgraph
	
	fun Build () = 
	let
		(* valores vivos en cada instruccion, empezando por la ultima *)
		fun procInstr instr = 
		let
			val live =  tigerset.difference ( 
					   nodelist2set (liveout instr)
			           ,specialreg) 
			val ismove' = Splaymap.find(ismove, instr)  
			val use' = tigerset.difference ( 
			           nodelist2set(Splaymap.find(use, instr))
			           ,specialreg) 
			val def' =  tigerset.difference ( 
			            nodelist2set(Splaymap.find(def, instr)) 
			            ,specialreg)
			fun addToMoveList n mv = 
			let  
				val moveList_n = peekOrEmpty moveList n movecmp
			in 
				tigerset.add(moveList_n, mv);
				Polyhash.insert moveList (n, moveList_n)
			end
			
			fun count t = 
				case (Polyhash.peekInsert  usedefcount (t,1)) of 
						SOME d => Polyhash.insert usedefcount (t,d+1)
			          | NONE   => ()
			   
		in 	
		    (**)
		    app count (union(use',def'));
			(*agregamos nodos a initial *)
			initial := !(tigerset.union(initial, union(use',def')));
			(*hacemos el resto del build*)
			if (ismove' andalso notEmpty(use') andalso notEmpty(def')) then 
				let
					val src = unElem use'
					val dst = unElem def'
				in 
					(if member(live,src) then tigerset.delete(live, src) else ();
					addToMoveList src (dst, src); 
					addToMoveList dst (dst, src);
					add (worklistMoves, (dst, src)))
				end
			else ();
			app (fn d => app (fn l => AddEdge(l,d)) live) def'
		end	
	in  
		List.app procInstr (rev (tigergraph.nodes control)); (*app aplica de izquierda a derecha entonces funca*)
		initial := !(difference(initial, precolored))
	end
	
	fun Loop() =   
	           (if notEmpty(simplifyWorklist) then (Simplify() ; Loop())
             else if notEmpty(worklistMoves) then (Coalesce() ; Loop())
             else if notEmpty(freezeWorklist) then (Freeze() ; Loop())
             else if notEmpty(spillWorklist) then (SelectSpill(); Loop())
             else ())	

	fun printWL wl = 
		List.app (fn tmp => print (tmp^"\n")) (listItems wl)
	
    

	fun Init () = (
(*
                    cleanTables (); 
*)
                   app (fn x=> Polyhash.insert color (x,x)) precolored ;
	               app (fn x=> Polyhash.insert color (x,x)) specialreg )
	
in
    Init () ;
	Build () ; 
(*
	print "\n\ninitials:\n" ;
	printWL initial;
*)
(*	
	print "\n\nprecolored:\n" ;
	printWL precolored;
*) 
	MakeWorklist ()  ;
(*
	print "\n\nsimplify:\n" ;
	printWL simplifyWorklist;
	print "\n\nfreeze:\n" ;
	printWL freezeWorklist;
	print "\n\nspill:\n" ;
	printWL spillWorklist;
*)
	Loop()  ; 
	AssignColors() ;

(*	printTable color ; *)
  (*  print "spilled:\n" ;
    printWL spilledNodes;  *)
    (color, spilledNodes)
end


fun main (instrlist, frame) = 
let
    val (color, spilledNodes) = getSpilled (instrlist, frame)
in
    (if notEmpty(spilledNodes) 
        then
        let
            val (newinstrlist, _ ) = tigerregalloc.spill (spilledNodes,frame, instrlist)
        in
            main (newinstrlist, frame)
        end
        else (instrlist, color)) 
end    
    
end



(*
    
    fun cleanTables () =
    let
        fun vaciarTabla tbl = Polyhash.filter (fn _ => false) tbl
        
    in
        precolored       := nodelist2set tigerframe.usable ;
        specialreg       := nodelist2set tigerframe.specialregs ;
        initial          := tigerset.newEmpty nodecmp ;
        simplifyWorklist := tigerset.newEmpty(nodecmp)  ;
        reezeWorklist    := tigerset.newEmpty(nodecmp)  ;
        spillWorklist    := tigerset.newEmpty(nodecmp)  ;
        spilledNodes     := tigerset.newEmpty(nodecmp)  ;
        coalescedNodes   := tigerset.newEmpty(nodecmp)  ;
        coloredNodes     := tigerset.newEmpty(nodecmp)  ;
        (*Pila y conjunto*)
        selectStack      := tigerset.newEmpty(nodecmp)  ;
        selectPila       := nuevaPila() ;
        (*Conjuntos de moves*)
        worklistMoves    := tigerset.newEmpty(movecmp) ;
        activeMoves      := tigerset.newEmpty(movecmp) ;
        frozenMoves      := tigerset.newEmpty(movecmp) ;
        constrainedMoves := tigerset.newEmpty(movecmp) ;
        coalescedMoves   := tigerset.newEmpty(movecmp) 
        
        degree : (node,int) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,ErrorDegree) 

        adjSet         := tigerset.newEmpty movecmp

    

val adjList : (node,nodeSet) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,ErrorAdjList) 

val moveList : (node,moveSet) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,ErrorMoveList)

val alias : (node,node) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,ErrorAlias)
(*

(* revisar tipo de color *)
val nodeColor : (node,tigertemp.temp) Polyhash.hash_table = Polyhash.mkTable (Polyhash.hash,nodeeq) (1000,NotFound)
*)

val color : (node,tigertemp.temp) Polyhash.hash_table = Polyhash.mkTable(Polyhash.hash,nodeeq) (1000,ErrorColor)

val usedefcount : (node,int) Polyhash.hash_table = Polyhash.mkTable(Polyhash.hash,nodeeq) (1000,ErrorCount)

*)

