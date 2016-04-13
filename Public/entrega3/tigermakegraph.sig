signature tigermakegraph =
sig
  
    val instrs2graph : tigerassem.instr list ->
                          tigerflow.flowgraph * tigergraph.node list

end
