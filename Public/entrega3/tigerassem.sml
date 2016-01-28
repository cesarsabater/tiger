structure tigerassem :> tigerassem = 
struct
    fun format _ (OPER {assem, ...}) = assem
    |   format _ (LABEL {assem, ...}) = assem
    |   fomrat _ (MOVE {assem, ...}) = assem
end
