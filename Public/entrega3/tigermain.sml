open tigerlex
open tigergrm
open tigerescap
open tigerseman
open tigerutils
open tigertrans
open tigerliveness
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
		
		(* aca empieza la magia *)
		val _ = transProg(expr)
		
		(* obtenemos el codigo intermedio *)
		val fragments = getResult()
		val _ = print "CODIGO INTERMEDIO: \n\n" 
		val _ = printFragments(fragments)
		val _ = print "\n\n\n"
		
		(* caninizamos el codigo intermedio *)
		val canonfmts = genCanonFmts fragments
		val _ = print "CODIGO INTERMEDIO CANONIZADO: \n\n" 
		val _ = printCanonFmts(canonfmts)	
		val _ = print "\n\n\n"	
		
		(* val _ = tigerinterp.inter false canonfmts (getStrings res) *)
		val instructions = geninstr canonfmts
		val _ = printCode instructions
		
        val (flowgraph, ilist) = tigerflow.instrs2graph instructions
		val tigerflow.FGRAPH{control = cgraph, ...} = flowgraph 
		val _ = tigergraph.printGraph  cgraph
		
          
        val (igraph,liveout) = interferenceGraph flowgraph 
        val _ = print "Liveout Temps:\n\n\n"
        val _ = printLiveOut flowgraph
		val _ = print "Grafo de Interferencia\n\n\n\n"
        val _ = tigerliveness.show igraph
        
        val _ = tigercolor.main liveout (flowgraph,ilist)
        
	in
		
		print "yes!!\n"
		
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
