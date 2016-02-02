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

    fun format _ (OPER {assem=assem, ...}) = assem
    |   format _ (LABEL {assem=assem, ...}) = assem
    |   format _ (MOVE {assem=assem, ...}) = assem
end
