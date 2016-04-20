structure tigerliveness :> tigerliveness = 
struct
open tigertemp
open tigerflow
open tigergraph
open Splaymap
open Splayset

datatype igraph = 
		IGRAPH of {graph: tigergraph.graph,
					tnode: tigertemp.temp -> tigergraph.node, 
					gtemp: tigergraph.node -> tigertemp.temp, 
					moves: (tigergraph.node * tigergraph.node) list
				}
				
type liveSet = temp Splayset.set
type liveMap = liveSet tigergraph.table

val lIn = ref (newTable () : liveMap)
val lOut = ref (newTable () : liveMap)

fun emptyLiveSet () = Splayset.empty String.compare
fun list2set lst = Splayset.addList (emptyLiveSet(), lst)

fun curry f = fn x => fn y => f (x,y)

fun listeq ([],[]) _ = true
|   listeq (x::lx, y::ly) cmp = cmp(x,y) andalso (listeq (lx,ly) cmp)
|   listeq _ _ = false

fun lsetcmp ((_, s1: liveSet),(_, s2: liveSet)) = Splayset.equal(s1, s2)

fun lmapeq (m1 : liveMap, m2 : liveMap) = listeq (Splaymap.listItems m1, Splaymap.listItems m2) lsetcmp

fun peekorempty dict el =  case (Splaymap.peek (dict,el)) of 
                                SOME s => s
                                | NONE => emptyLiveSet()


fun calcLiveness (FGRAPH {control, def, use, ismove}) = 
let
	fun intGraph (livein : liveMap, liveout : liveMap) = 
	let 
		fun intNode (node, (lin, lout)) = 
		let
			
			val inset = peekorempty lin node
			val outset = peekorempty lout node
			val defset = list2set (Splaymap.find (def,node))
			val useset = list2set (Splaymap.find (use,node))
			val inset' = union (useset, (difference (outset, defset)))
			val listofosets' = List.map (peekorempty lin) (succ node)
			val outset' = List.foldr union (emptyLiveSet ()) listofosets'
        in 
			( Splaymap.insert(lin,node,inset') , Splaymap.insert(lout,node,outset') )
		end
		val _ = print "liveiter!\n" 
		val (livein', liveout') = List.foldr intNode (livein, liveout) (nodes control)
	in
		if lmapeq(livein',livein) andalso lmapeq(liveout',liveout) 
		then (livein', liveout') 
		else  intGraph (livein', liveout') 
	end
	
    val (livein, liveout) = intGraph (tigergraph.newTable(), tigergraph.newTable())
in 
    (lIn := livein ; lOut := liveout)
end

fun liveout node =  Splayset.listItems (Splaymap.find (!lOut, node))

fun printLiveOut fg =
let
    val (FGRAPH {control, def, use, ismove}) = fg
    fun printNode n = 
     (  print (nodename n^"-> ") ; 
        List.map (fn t => print (t^",")) (liveout n); 
        print "\n"
     ) 
in 
    List.app printNode (nodes control)
end

(**--------------------------------------*)
(*El grafo de interferencia. Aditivo*)
val igraph = newGraph()

(*Las Tablas : nodo a temp y de temp a nodo. Aditivas *)
val nodeTab : (tigergraph.node,tigertemp.temp)  Polyhash.hash_table = Polyhash.mkTable(Polyhash.hash,tigergraph.eq) (100,NotFound)  
val tempTab : (tigertemp.temp, tigergraph.node) Polyhash.hash_table = Polyhash.mkTable(Polyhash.hash, (fn (a,b) => (String.compare(a,b) = EQUAL))) (100,NotFound)  

(*tnode y gtemp *)
fun tnode temp = case (Polyhash.peek tempTab temp) 
                 of SOME node => node
                  | NONE => (let val n = newNode(igraph) in Polyhash.insert tempTab (temp,n) ;
                                                            Polyhash.insert nodeTab (n,temp) ;
                                                            n 
                             end)

(* No deberia pasar que no la encuentre *)
fun gtemp node = Polyhash.find nodeTab node

(* agrega aristas dirigidas de un nodo a una lista de nodos *)
fun mk_edges x blist = 
	List.app (fn y => mk_edge {from= tnode(x), to= tnode(y)}) blist

(* agrega las aristas *)
fun mk_iedges (alist,blist) = List.app (fn x => mk_edges x blist) alist

val movesref = ref ([] : ((tigergraph.node * tigergraph.node) List.list))

fun interferenceGraph (FGRAPH{control = fgraph, def, use, ismove}) =
let
   (* procesa un nodo del flowgraph*)
      fun instr_interf flownode = let
			 val ismove' = Splaymap.find (ismove,flownode)
			 val def'    = Splaymap.find (def,flownode)
			 val use'    = Splaymap.find (use,flownode)
	  in
		if ismove' then (
		   mk_iedges(def', List.filter (fn x => x <> List.hd use') (liveout flownode) ) ; 
		   movesref := ((tnode(List.hd def'),tnode(List.hd use')) :: !movesref )   )  (*OJO ACA CON EL ORDEN def use*)
		else
		   mk_iedges(def', liveout flownode)
	  end
	 
	 val precolored = (tigerframe.specialregs
						@tigerframe.argregs
						@tigerframe.callersaves
						@tigerframe.calleesaves)
	 (* agrega nodos precoloreados *) 
	 fun addPrecolored ls [] = ()
	   | addPrecolored rs (t::ts) = 
			let 
				val newreg = tnode t 
				fun make_edges n ns = List.app (fn y => mk_edge {from=n, to=y}) ns
			in 
				make_edges newreg rs; 
				addPrecolored (newreg::rs) ts 
			end
in                     
    List.app instr_interf (nodes fgraph) ;
    addPrecolored [] precolored ; 
    (IGRAPH {graph = igraph,
		    tnode  = tnode, 
			gtemp  = gtemp, 
			moves  = !movesref
		   }, liveout)
end     
(**-------------------------------*)

end
