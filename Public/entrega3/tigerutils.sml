structure tigerutils :> tigerutils = struct

open tigertrans
open tigercanon
open tigercodegen
open tigerassem

		fun canonize x = traceSchedule (basicBlocks (linearize x)) 

		(* manipulacion de fragmentos *)
	(*	fun getStrings [] = []
			| getStrings ((tigerframe.STRING(l,s))::fmts) = (l,s)::(getStrings fmts)
			| getStrings (_::fmts) = getStrings fmts *)

		fun getCanonFmts [] = []
			| getCanonFmts ((tigerframe.PROC {body, frame})::fmts) = 
						tigerframe.CANONPROC { body = canonize body, frame=frame}::(getCanonFmts fmts)
			| getCanonFmts (tigerframe.STRING (l, s)::fmts) = 
						tigerframe.CANONSTRING (l, s)::getCanonFmts fmts 

		fun printIR [] = () 
			   | printIR ((tigerframe.CANONPROC {body, frame=f})::fmts) =
					let
						val _ = print ("\nFragment \""^(tigerframe.name f)^"\":\n")
						val _ = map (fn st => print (tigerit.tree st)) (body)
					in	
						printIR(fmts) 
					end
			   | printIR (tigerframe.CANONSTRING s::fmts) = 
					let 
						val _ = print("\nString Fragment:\n")
						val _ = print(Ir([tigerframe.STRING s]))
					in 
							printIR(fmts)
					end

		(* generacion de instrucciones *)
		fun geninstr1 _ [] = []
		|   geninstr1 frame (st::stl) = (codegen frame st)@(geninstr1 frame stl) 
		
		fun geninstr [] = []
		|   geninstr (tigerframe.CANONPROC {body, frame}::l) = (geninstr1 frame body)@(geninstr l)
		|   geninstr (tigerframe.CANONSTRING (l,s)::cfl) = geninstr cfl
		
		val instr2string = format (fn t => t)
		
		fun code2string [] = ""
		|   code2string (instr::l) = (instr2string instr)^(code2string l)
		
		fun printCode fmts =
			( print "\nCodigo:\n"; 
			  print (code2string (geninstr fmts));
			  print "\n"
			)


end
