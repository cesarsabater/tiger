signature tigercolor = 
sig

    type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict


	val color : { interference : tigerliveness.igraph,
				  initial: allocation, 
				  spillCost: tigergraph.node -> int, 
				  registers: tigerframe.register list} 
					-> allocation * tigertemp.temp list
                    

    val main : tigerliveness.igraph -> unit 
end
