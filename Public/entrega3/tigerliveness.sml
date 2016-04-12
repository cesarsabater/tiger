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

fun list2set lst : temp list -> liveSet = addList (Splayset.empty (<)) lst

fun interferenceGraph (FGRAPH {control, def, use, ismove}) = 
let
	fun intGraph (livein,liveout) = 
	let 
		fun intNode (inset, outset) node = 
		let
			val defset = (list2set (Splaymap.find def node))
			val useset = (list2set (Splaymap.find use node)) 
			val inset' = union useset (difference outset defset)
			val outlist' = concat (List.map Splaymap.find (succ node))
			val outset' = list2set outlist'
		in 
			(inset', outset')
		end
		
		
		
in 
	
end
