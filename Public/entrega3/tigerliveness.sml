structure tigerliveness :> tigerliveness = 
struct
open tigertemp
open tigerflow
open tigergraph

datatype igraph = 
		IGRAPH of {graph: tigergraph.graph,
					tnode: tigertemp.temp -> tigergraph.node, 
					gtemp: tigergraph.node -> tigertemp.temp, 
					moves: (tigergraph.node * tigergraph.node) list
				}
				
type liveSet = temp Splayset.set
type liveMap = liveSet tigergraph.table

fun interferenceGraph (FGRAPH {control, def, use, ismove}) = 
let

	fun list2set lst : temp list -> liveSet = addList (Splayset.empty (<)) lst
	
	fun intNode (inset, outset) node = 
	let
		inset' = list2set 
	
	fun intGraph (livein,liveout) = 
	let 
		livein' = 
		
in 
	
end
