structure tigerutils :> tigerutils = struct

open tigertrans
open tigercanon
open tigercodegen
open tigerassem
open tigerframe

		fun canonize x = traceSchedule (basicBlocks (linearize x)) 

		(* manipulacion de fragmentos *)
	(*	fun getStrings [] = []
			| getStrings ((tigerframe.STRING(l,s))::fmts) = (l,s)::(getStrings fmts)
			| getStrings (_::fmts) = getStrings fmts *)

		fun genCanonFmts [] = []
			| genCanonFmts ((tigerframe.PROC {body, frame})::fmts) = 
						(CPROC { body = canonize body, frame=frame})::(genCanonFmts fmts)
			| genCanonFmts (tigerframe.STRING (l, s)::fmts) = 
						(CSTR (l, s))::genCanonFmts fmts 

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
			   | printCanonFmts ((CPROC {body, frame=f})::fmts) =
					let
						val _ = print ("\nFragment \""^(tigerframe.name f)^"\":\n")
						val _ = map (fn st => print (tigerit.tree st)) (body)
					in	
						printCanonFmts(fmts) 
					end
			   | printCanonFmts ((CSTR s)::fmts) = 
					let 
						val _ = print("\nString Fragment:\n")
						val _ = print(Ir([tigerframe.STRING s]))
					in 
							printCanonFmts(fmts)
					end
   
        (*aux*)
        fun genLabel name = LABEL { assem= name ^ ":\n",
								    lab= name  }

		(* generacion de instrucciones *)
		fun geninstr1 _ [] = []
		|   geninstr1 frame (st::stl) = (codegen frame st)@(geninstr1 frame stl) 
		
		fun geninstr [] = []
		|   geninstr ((CPROC {body, frame})::l) = 
         (IPROC (*(genLabel(tigerframe.name frame))::*)((geninstr1 frame body), frame) )::(geninstr l)
		|   geninstr ((CSTR (l,s))::cfl) = (ISTR (l,s))::geninstr cfl
		
		val instr2string = format (fn t => t)
		
		fun allocinstr alloc = format (fn t => (case (Polyhash.peek alloc t) of SOME d => d | NONE => t ))
		
		fun code2string [] = ""
		|   code2string (instr::l) = (instr2string instr)^(code2string l)
		
		fun printCode instrfrags =
        let 
            fun prIproc (IPROC (instrlist, frm)) = 
                (print ((tigerframe.name frm)^":\n");print (code2string instrlist); print "\n")
            |   prIproc (ISTR _ ) = () 
        in    
			 print "\nCodigo sin colorear:\n";
			 List.app prIproc instrfrags 
        end
			
		fun printFinal alloc instrlist =  (
(*
                print "\nCodigo:\n"; 
*)
			  print (List.foldr (fn (inst,str) => (allocinstr alloc inst)^str) "\n" instrlist )	)

       fun sameMove alloc (tigerassem.MOVE {assem = assem,src = src, dst = dst}) = if (String.compare(Polyhash.find alloc src,Polyhash.find alloc dst) = EQUAL) then false else true
        |  sameMove  _           _  = true
         
end
