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

		fun genCanonFmts [] = []
			| genCanonFmts ((tigerframe.PROC {body, frame})::fmts) = 
						tigerframe.CANONPROC { body = canonize body, frame=frame}::(genCanonFmts fmts)
			| genCanonFmts (tigerframe.STRING (l, s)::fmts) = 
						tigerframe.CANONSTRING (l, s)::genCanonFmts fmts 

		fun printFragments [] = () 
			   | printFragments ((tigerframe.PROC {body, frame=f})::fmts) =
					let
						val _ = print ("\nFragment \""^(tigerframe.name f)^"\":\n")
						val _ = print (tigerit.tree body)
					in	
						printFragments(fmts) 
					end
			   | printFragments (s::fmts) = 
					let 
						val _ = print("\nString Fragment:\n")
						val _ = print(Ir([s]))
					in 
							printFragments(fmts)
					end


		fun printCanonFmts [] = () 
			   | printCanonFmts ((tigerframe.CANONPROC {body, frame=f})::fmts) =
					let
						val _ = print ("\nFragment \""^(tigerframe.name f)^"\":\n")
						val _ = map (fn st => print (tigerit.tree st)) (body)
					in	
						printCanonFmts(fmts) 
					end
			   | printCanonFmts (tigerframe.CANONSTRING s::fmts) = 
					let 
						val _ = print("\nString Fragment:\n")
						val _ = print(Ir([tigerframe.STRING s]))
					in 
							printCanonFmts(fmts)
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
		
		fun printCode instrlist =
			( print "\nCodigo:\n"; 
			  print (code2string instrlist);
			  print "\n"
			)

end
