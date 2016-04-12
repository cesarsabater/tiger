structure tigerliveness :> tigerliveness = 
struct
open tigertemp

datatype igraph = 
		IGRAPH of {graph: tigergraph.graph,
					tnode: tigertemp.temp -> tigergraph.node, 
					gtemp: tigergraph.node -> tigertemp.temp, 
					moves: (tigergraph.node * tigergraph.node) list
				}
				
type 'a table = (temp, 'a) Splaymap.dict
type liveSet = unit table * temp list
type liveMap = liveSet tigergraph.table

end
