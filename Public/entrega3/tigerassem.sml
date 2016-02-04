structure tigerassem :> tigerassem = 
struct
    type reg = string
	type temp = tigertemp.temp
	type label = tigertemp.label

    datatype instr = OPER of {  assem: string,
								dst: temp list,
								src: temp list,
								jump: label list option }
					| LABEL of { assem: string,
								 lab: label  }
					| MOVE of {  assem: string, 
								 dst: temp, 
								 src: temp } 
    
    
    
    fun format1 _ [] _ = ""
      | format1 n (#"\'"::(c::(i::st))) (dst,src,jump) = 
        if ((Char.isDigit i) and ((c = #"s") or (c = #"d"))) then 
            let val list = case c of 
                     #"s" => src
                    | #"d" => dst
                    | _ => raise Fail ("Error -- no deberia pasar (format1)\n")
                val index = (ord i) - (ord 0)
            in 
                (n (nth (list, index)))^(format1 n st (dst, src, jump))
            end
        else 
            "\'"^(format1 n (c::(i::st)) (dst, src, jump))
            
     | format1 n (c::st) (d,s,j) = 
            (Char.toString c)^(format1 n st (d,s,j))

    fun format n (OPER {assem, dst, src, jump} ) =
            String.implode(format1 n (String.explode assem) (dst, src, jump))
      | format n (MOVE {assem, dst, src} = 
            String.implode(format1 n (String.explode assem) (dst, src, NONE))
      | format n (LABEL {assem, lab} = (n lab)^":\n" 
    
     (*  
    |   format _ (OPER {assem=assem, ...}) = assem
    |   format _ (LABEL {assem=assem, ...}) = assem
    |   format _ (MOVE {assem=assem, ...}) = assem *)
end
