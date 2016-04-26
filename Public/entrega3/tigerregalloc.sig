signature tigerregalloc = 
sig
	type allocation = (tigertemp.temp, tigerframe.register) Polyhash.hashtable
	(*val alloc : tigerassem.instr list * tigerframe.frame -> tigerassem.instr list * allocation *)
	
	val spill : ((tigertemp.temp) tigerset.set) * tigerframe.frame * tigerassem.instr list -> (tigerassem.instr list * (tigertemp.temp) tigerset.set) 
end
