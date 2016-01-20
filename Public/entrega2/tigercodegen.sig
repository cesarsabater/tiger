signature tigercodegen =
sig
	(* esto para que sirve?? structure frame : tigerframe *)
	val codegen : tigerframe.frame -> tigertree.stm -> tigerassem.instr list
end
