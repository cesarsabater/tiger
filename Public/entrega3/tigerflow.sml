structure tigerflow = struct

   structure Graph
   datatype flowgraph =
   
      FGRAPH of {control : Graph.graph, 
                 def : tigertemp.temp list Graph.Table.table,
                 use : tigertemp.temp list Graph.Table.table,
                 ismove : bool Graph.Table.table }
end

