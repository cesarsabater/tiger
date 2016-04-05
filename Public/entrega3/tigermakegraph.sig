structure tigermakegraph:

sig
  
    val instrs2graph : tigerassem.instr list ->
                          tigerflow.flowgraph * tigerflow.Graph.node list

end
