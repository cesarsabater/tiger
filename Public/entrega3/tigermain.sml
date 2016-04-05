open tigerlex
open tigergrm
open tigerescap
open tigerseman
open tigertrans
open tigercanon
open tigercodegen
open tigerassem
open BasicIO Nonstdio

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")
fun main(args) =
	let	fun arg(l, s) =
			(List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes") 
		val (ir, l3)		= arg(l2, "-ir") 
		val (canon, l4)		= arg(l3, "-canon") 
		val (code, l5)		= arg(l4, "-code") 
		val (flow, l6)		= arg(l5, "-flow") 
		val (inter, l7)		= arg(l6, "-inter") 
		val entrada =
			case l7 of
			[n] => ((open_in n)
					handle _ => raise Fail (n^" no existe!"))
			| [] => std_in
			| _ => raise Fail "opcio'n dsconocida!"
		val lexbuf = lexstream entrada
		val expr = prog Tok lexbuf handle _ => errParsing lexbuf
		val _ = findEscape(expr)
		val _ = if arbol then tigerpp.exprAst expr else ()
		
		
		
		(* utileria *)
		fun canonize x = traceSchedule (basicBlocks (linearize x)) 

		(* manipulacion de fragmentos *)
		fun getStrings [] = []
			| getStrings ((tigerframe.STRING(l,s))::fmts) = (l,s)::(getStrings fmts)
			| getStrings (_::fmts) = getStrings fmts

		fun getCanonFmts [] = []
			| getCanonFmts ((tigerframe.PROC {body, frame})::fmts) = 
						tigerframe.CANONPROC { body = canonize body, frame=frame}::(getCanonFmts fmts)
			| getCanonFmts (tigerframe.STRING (l, s)::fmts) = 
						tigerframe.CANONSTRING (l, s)::getCanonFmts fmts 

		fun printIR [] = [] 
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
			
			
		(* aca empieza la magia *)
		val _ = transProg(expr)
		(* obtenemos e imprimimos resultados *)
		val res = getResult()
		val canonfmts = getCanonFmts res 
		(* val _ = print(Ir(res)) *)
		val _ = printIR(canonfmts)		
		(* val _ = tigerinterp.inter false canonfmts (getStrings res) *)
		val _ = printCode canonfmts
		
	in
		
		print "yes!!\n"
		
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
