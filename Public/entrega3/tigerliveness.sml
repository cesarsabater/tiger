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

fun list2set lst = addList ((Splayset.empty String.compare), lst)

fun livenessFunc (FGRAPH {control, def, use, ismove}) flownode = 
let
	fun intGraph (livein,liveout) = 
	let 
		fun intNode node (lin, lout) = 
		let
			val inset = find lin node
			val outset = find lout node
			val defset = list2set (find (def,node))
			val useset = list2set (find (use,node))
			val inset' = union useset (difference outset defset)
			val outlist' = concat (List.map (Splaymap.find livein) (succ node))
			val outset' = list2set outlist'
		in 
			(insert lin inset', insert lout outset')
		end
		val (livein', liveout') = List.foldr intNode (livein, liveout) (nodes control) 
	in
		if (livein' = livein) andalso (liveout' = liveout) 
		then (livein', liveout') 
		else  intGraph (livein', liveout') 
	end
	
	val (livein, liveout) = intGraph (tigergraph.newTable(), tigergraph.newTable())

in 
	listItems (find liveout flownode)
end

end
