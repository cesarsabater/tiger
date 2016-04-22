signature tigercolor = 
sig

    type allocation = (tigertemp.temp, tigerframe.register) Polyhash.hash_table


(*
	val color : { interference : tigerliveness.igraph,
				  initial: allocation, 
				  spillCost: tigergraph.node -> int, 
				  registers: tigerframe.register list} 
					-> allocation * tigertemp.temp list
*)              
    val main : tigerliveness.liveness * (tigerflow.flowgraph * tigergraph.node list) -> unit 
end
