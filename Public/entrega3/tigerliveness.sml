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


fun emptyLiveSet () = Splayset.empty String.compare
fun list2set lst = Splayset.addList (emptyLiveSet(), lst)

fun curry f = fn x => fn y => f (x,y)

fun listeq ([],[]) _ = true
|   listeq (x::lx, y::ly) cmp = cmp(x,y) andalso listeq (lx,ly) cmp
|   listeq _ _ = false

fun lsetcmp ((_, s1: liveSet),(_, s2: liveSet)) = Splayset.equal(s1, s2)

fun lmapeq (m1 : liveMap, m2 : liveMap) = listeq (Splaymap.listItems m1, Splaymap.listItems m2) lsetcmp

fun livenessFunc (FGRAPH {control, def, use, ismove}) flownode = 
let
	fun intGraph (livein : liveMap, liveout : liveMap) = 
	let 
		fun intNode (node, (lin, lout)) = 
		let
			val inset = Splaymap.find (lin,node)
			val outset = Splaymap.find (lout,node)
			val defset = list2set (Splaymap.find (def,node))
			val useset = list2set (Splaymap.find (use,node))
			val inset' = union (useset, (difference (outset, defset)))
			val listofosets' = List.map ((curry Splaymap.find) livein) (succ node)
			val outset' = List.foldr union (emptyLiveSet ()) listofosets' 
        in 
			( Splaymap.insert(lin,node,inset') , Splaymap.insert(lout,node,outset') )
		end
		val (livein', liveout') = List.foldr intNode (livein, liveout) (nodes control)   
	in
		if lmapeq(livein',livein) andalso lmapeq(liveout',liveout) 
		then (livein', liveout') 
		else  intGraph (livein', liveout') 
	end
	
	val (livein, liveout) = intGraph (tigergraph.newTable(), tigergraph.newTable())
in 
	Splayset.listItems (Splaymap.find(liveout,flownode))
end

end
