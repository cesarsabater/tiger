structure tigerflow :> tigerflow =
struct
  
     open tigertab    

     datatype flowgraph =
   
    FGRAPH of {control : tigergraph.graph, 
                 def :  (tigertemp.temp list) tigergraph.table,
                 use :  (tigertemp.temp list) tigergraph.table,
                 ismove : bool tigergraph.table }

end

