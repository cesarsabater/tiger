signature tigerutils = 
sig

val canonize: tigertree.stm -> tigertree.stm list

val genCanonFmts: tigerframe.frag list -> tigerframe.canonfrag list

val geninstr: tigerframe.canonfrag list ->  (tigerassem.instr list * tigerframe.frame) list

val printFragments: tigerframe.frag list -> unit

val printCanonFmts: tigerframe.canonfrag list -> unit

(*
val printFragmentCode: tigerassem.instr list -> unit
*)

val printCode: (tigerassem.instr list * tigerframe.frame) list -> unit

val printFinal : tigercolor.allocation -> tigerassem.instr list -> unit


end

