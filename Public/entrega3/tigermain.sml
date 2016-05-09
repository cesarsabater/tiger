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
		val (strings, procfmts) = divideFrags ([],[]) fragments
		val canonfmts = genCanonFmts procfmts
		val _ = print "CODIGO INTERMEDIO CANONIZADO: \n\n" 
		val _ = printCanonFmts (strings, canonfmts)	
		val _ = print "\n\n\n"
		
		(* val _ = tigerinterp.inter false canonfmts (getStrings res) *)
		val uncolored_frags = geninstr canonfmts
		val _ = printCode uncolored_frags
		
        
        fun bigcolor (instrlist, frame) =
		let 
			val il'' = tigerframe.procEntryExit2(frame, instrlist)
			val (il', alloc) = tigercolor.main (il'', frame)
			val il = List.filter (sameMove alloc) il'
			val {prolog, epilog, ...} = tigerframe.procEntryExit3 (frame, il)
		in 
				prolog^(genFinal alloc il)^epilog 
		end
     
        val strings_final = concat (List.map tigerframe.genstring strings)
        val functions_final = concat (List.map bigcolor uncolored_frags)
      
		
		val progname = "program.s" 
		val compiler = "arm-linux-gnueabi-gcc -march=armv7-a" 
		val TheCode =   "\t.syntax unified\n"^
                        "\t.arch armv7-a\n"^
                        "\t.thumb\n"^
                        "        .file     \""^progname^"\"\n"^
						"        .section  .rodata\n"^
						strings_final^
						"        .text\n"^
						functions_final
		
		val _ = (print "\nCODIGO FINAL:\n\n"; print TheCode)
		
		
		val outFile = (TextIO.openOut (progname))
                              handle _ => raise Fail "Falló al abrir el archivo de salida"
        val _ =  TextIO.output (outFile, TheCode)
        val _ = TextIO.closeOut outFile
        val _ = Process.system(compiler^" runtime.c "^progname^" -o a.out")
	in
		
		print "yes!!\n"
	end	
		handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
