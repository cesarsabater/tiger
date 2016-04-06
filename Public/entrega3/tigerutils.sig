signature tigerutils = 
sig

val canonize: tigertree.stm -> tigertree.stm list

val genCanonFmts: tigerframe.frag list -> tigerframe.canonfrag list

val geninstr: tigerframe.canonfrag list ->  tigerassem.instr list

val printFragments: tigerframe.frag list -> unit

val printCanonFmts: tigerframe.canonfrag list -> unit

val printCode: tigerassem.instr list -> unit

end

