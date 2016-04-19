signature tigerregalloc = 
sig
	type allocation = (tigertemp.temp, tigerframe.register) Polyhash.hashtable
	val alloc : tigerassem.instr list * tigerframe.frame -> tigerassem.instr list * allocation
end
