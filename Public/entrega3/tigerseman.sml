structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigertrans
open tigercanon
open topsort
open tigercodegen
open tigerassem

(* sacar esto *)

type expty = {exp: unit, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val tab_tipos : (string, Tipo) Tabla = tabInserList(
	tabNueva(),
	[("int", TInt RW), ("string", TString)])

val levelPila: tigertrans.level tigerpila.Pila = tigerpila.nuevaPila1(tigertrans.outermost) 
fun pushLevel l = tigerpila.pushPila levelPila l
fun popLevel() = tigerpila.popPila levelPila 
fun topLevel() = tigerpila.topPila levelPila

val tab_vars : (string, EnvEntry) Tabla = tabInserList(
	tabNueva(),
	[("print", Func{level=topLevel(), label="print",
		formals=[TString], result=TUnit, extern=true}),
	("flush", Func{level=topLevel(), label="flush",
		formals=[], result=TUnit, extern=true}),
	("getchar", Func{level=topLevel(), label="getstr",
		formals=[], result=TString, extern=true}),
	("ord", Func{level=topLevel(), label="ord",
		formals=[TString], result=TInt RW, extern=true}),
	("chr", Func{level=topLevel(), label="chr",
		formals=[TInt RW], result=TString, extern=true}),
	("size", Func{level=topLevel(), label="size",
		formals=[TString], result=TInt RW, extern=true}),
	("substring", Func{level=topLevel(), label="substring",
		formals=[TString, TInt RW, TInt RW], result=TString, extern=true}),
	("concat", Func{level=topLevel(), label="concat",
		formals=[TString, TString], result=TString, extern=true}),
	("not", Func{level=topLevel(), label="not",
		formals=[TInt RW], result=TInt RW, extern=true}),
	("exit", Func{level=topLevel(), label="exit",
		formals=[TInt RW], result=TUnit, extern=true})
	])

fun tipoReal (TTipo (s, ref (SOME (t)))) = tipoReal t
  | tipoReal t = t

fun tiposIguales (TRecord _) TNil = true
  | tiposIguales TNil (TRecord _) = true
  | tiposIguales (TInt _) (TInt _) = true 
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2 )) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | tiposIguales (TTipo (_, r)) b =
		let
			val a = case !r of
				SOME t => t
				| NONE => raise Fail "No debería pasar! (1)"
		in
			tiposIguales a b
		end
  | tiposIguales a (TTipo (_, r)) =
		let
			val b = case !r of
				SOME t => t
				| NONE => raise Fail "No debería pasar! (2)"
		in
			tiposIguales a b
		end
  | tiposIguales a b = (a=b)

fun isTInt t = 
	case (tipoReal t) of 
		TInt _ => true
		| _ => false

(* utileria *) 
fun isIn x = 
		List.foldl (fn (y, b) => b orelse (x = y)) false 
		
fun isRepList [] = false
	| isRepList (x::xs) = if (isIn x xs) then true
						else isRepList xs

fun transExp(venv, tenv) =
	let fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")
		fun trexp(VarExp v) = trvar(v)
		| trexp(UnitExp _) = {exp=unitExp(), ty=TUnit}
		| trexp(NilExp _)= {exp=nilExp(), ty=TNil}
		| trexp(IntExp(i, _)) = {exp=intExp i, ty=TInt RW}
		| trexp(StringExp(s, _)) = {exp=stringExp(s), ty=TString}
		| trexp(CallExp({func, args}, nl)) =
			let 
				val (targs, ext, tret, lab, lev) = 
					case tabBusca(func, venv) of
					SOME (Func {level, label, formals, result, extern}) 
							=> (formals, extern, result, label, level)
					| SOME _ => error (func^": no es funcion", nl )
					| NONE => error (func^": no existe", nl)
				
				fun aux [] [] r = r
				| aux [] _ _ = error(func^": Muchos argumentos", nl) 
				| aux _ [] _ = error(func^": Pocos argumentos", nl)
				| aux (t::tt) (a::aa) r = 
						let 
							val {exp=ac', ty=at'} = trexp a (* tipo de la expresion *) 
							val _ = if tiposIguales t at' then "ok!" 
									else error ("Error de tipos en la funcion "^func, nl) (* tiene que conincidir con el tipo declarado *)
						in 
							aux tt aa r@[{exp=ac', ty=at'}] 
						end
				
				val leargs = aux targs args [] 
				val leargs' = map (fn {exp, ty} => exp) leargs 
				val isproc = if (tiposIguales tret TUnit) then true else false 
			in 
				{exp=callExp(lab, ext, isproc, lev, leargs'), ty=tret} 
			end (* {exp=(), ty=TUnit} *) (*COMPLETAR*)
		| trexp(OpExp({left, oper=EqOp, right}, nl)) =
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then 
					{exp=if tiposIguales tyl TString then binOpStrExp {left=expl,oper=EqOp,right=expr} else binOpIntRelExp {left=expl,oper=EqOp,right=expr}, ty=TInt RW}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then 
					{exp=if tiposIguales tyl TString then binOpStrExp {left=expl,oper=NeqOp,right=expr} else binOpIntRelExp {left=expl,oper=NeqOp,right=expr}, ty=TInt RW}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper, right}, nl)) = 
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr then
					case oper of
						PlusOp => if (isTInt tyl) then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt RW} else error("Error de tipos", nl)
						| MinusOp => if (isTInt tyl) then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt RW} else error("Error de tipos", nl)
						| TimesOp => if (isTInt tyl) then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt RW} else error("Error de tipos", nl)
						| DivideOp => if (isTInt tyl) then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt RW} else error("Error de tipos", nl)
						| LtOp => if (isTInt tyl) orelse tipoReal tyl=TString then
							{exp=if (isTInt tyl) then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt RW} 
							else error("Error de tipos", nl)
						| LeOp => if (isTInt tyl) orelse tipoReal tyl=TString then 
							{exp=if (isTInt tyl) then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt RW} 
							else error("Error de tipos", nl)
						| GtOp => if (isTInt tyl) orelse tipoReal tyl=TString then
							{exp=if (isTInt tyl) then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt RW} 
							else error("Error de tipos", nl)
						| GeOp => if (isTInt tyl) orelse tipoReal tyl=TString then
							{exp=if (isTInt tyl) then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt RW} 
							else error("Error de tipos", nl)
						| _ => raise Fail "No debería pasar! (3)"
				else error("Error de tipos", nl)
			end
		| trexp(RecordExp({fields, typ}, nl)) =
			let
				(* Traducir cada expresión de fields *)
				(* val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields *)

				(* Buscar el tipo *)
				val (tyr, cs) = case tabBusca(typ, tenv) of
					SOME t => (case tipoReal t of
						TRecord (cs, u) => (TRecord (cs, u), cs)
						| _ => error(typ^" no es de tipo record", nl))
					| NONE => error("Tipo inexistente ("^typ^")", nl)
				
				val _ = if (length cs) = (length fields) then "ok!"
						else error("La cantidad de campos es incorrecta", nl)
						
				fun aux [] r = r
				  | aux ((s,e)::(flds)) r = 
						let 
							val (t',i') = 
								case (List.find (fn x => (#1 x) = s) cs) of
									SOME s => (#2 s, #3 s)
									| NONE => error("Campo Inexistente: "^s, nl)
							val {exp=e', ty=te'} = trexp e
							val _ = if (tiposIguales te' t') then "ok!" 
									else error("El campo "^s^" del record es de tipo incorrecto", nl)
						in 
							aux flds ((e',i')::r)
						end
				val r = aux fields []
			in 
				if (not (isRepList (map #2 r))) then {exp=recordExp r, ty=tyr}
				else error("Hay campos repetidos", nl)
			end
			
		| trexp(SeqExp(s, nl)) =
			let	val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti
				val {exp, ty=tipo} = hd(rev lexti)
			in	{ exp=seqExp (exprs), ty=tipo } end
		| trexp(AssignExp({var=SimpleVar s, exp}, nl)) =
			let
				val { exp=ve, ty=tv'} = trvar (SimpleVar s, nl)
				val { exp=ee, ty=tx'} = trexp exp 
			in 
				if  (tipoReal tv') <> TInt RO andalso (tiposIguales tv' tx') then { exp=assignExp{var=ve, exp=ee}, ty=TUnit}
				else if (tipoReal tv' = TInt RO) then error (s^"No se le puede asignar un valor a una variable de solo lectura", nl)
					else error (s^": El valor asignado no es del tipo asignado", nl)
			end (* {exp=(), ty=TUnit}  *) (* COMPLETAR *)
		| trexp(AssignExp({var, exp}, nl)) =
			let 
				val { exp=ve, ty=tv'} = trvar (var, nl)
				val { exp=ee, ty=tx'} = trexp exp
			in 
				if tiposIguales tv' tx' then { exp=assignExp{var=ve, exp=ee}, ty=TUnit }
				else error ("El valor asignado no es del tipo asignado", nl)
			end  (* {exp=(), ty=TUnit} *) (*COMPLETAR*)
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=testexp, ty=tytest} = trexp test
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				if (isTInt tytest) andalso tiposIguales tythen tyelse then
				{exp=if tipoReal tythen=TUnit then ifThenElseExpUnit {test=testexp,then'=thenexp,else'=elseexp} else ifThenElseExp {test=testexp,then'=thenexp,else'=elseexp}, ty=tythen}
				else error("Error de tipos en if" ,nl)
			end
		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=exptest,ty=tytest} = trexp test
			    val {exp=expthen,ty=tythen} = trexp then'
			in
				if (isTInt tytest) andalso tythen=TUnit then
				{exp=ifThenExp{test=exptest, then'=expthen}, ty=TUnit}
				else error("Error de tipos en if", nl)
			end
		| trexp(WhileExp({test, body}, nl)) =
			let
				val ttest = trexp test
				val _ = preWhileForExp()
				val tbody = trexp body
				val exp = whileExp {test=(#exp ttest), body=(#exp tbody), lev=topLevel()}
				val _ = postWhileForExp()
			in
				if isTInt (#ty ttest) andalso #ty tbody = TUnit then {exp=exp, ty=TUnit}
				else if not (isTInt (#ty ttest)) then error("Error de tipo en la condición", nl)
				else error("El cuerpo de un while no puede devolver un valor", nl)
			end
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) =
			let
				val { ty=tlo, exp=elo } = trexp lo
				val { ty=thi, exp=ehi } = trexp hi
				
				val acc = allocLocal (topLevel()) (!escape)
				val nv = Var {ty=TInt RO, level=getActualLev(), access=acc}
				
				val venv' = tabRInserta(var, nv, venv)
				val vexp = simpleVar(acc, getActualLev())
				
				val _ = preWhileForExp()
				val { ty=tbody, exp=bexp } = transExp (venv', tenv) body
				val fexp = forExp {lo=elo, hi=ehi, var=vexp, body=bexp}
				val _ = postWhileForExp()
			in 
				if isTInt thi andalso isTInt tlo andalso tbody = TUnit then { exp=fexp, ty=TUnit } 
				else if not (isTInt tlo) then error("Error de tipo en la cota inferior del bucle", nl) 
				else if not (isTInt thi) then error("Error de tipo en la cota superior del bucle", nl)
				else error("El cuerpo de un for no puede devolver un valor", nl)
			end   (*  {exp=(), ty=TUnit} *) (*COMPLETAR*)
		| trexp(LetExp({decs, body}, _)) =
			let
				fun aux (d, (v, t, exps1)) =
				let
					val (v', t', exps2) = trdec (v, t) d
				in
					(v', t', exps1@exps2)
				end
				val (venv', tenv', expdecs) = List.foldl aux (venv, tenv, []) decs
				val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
			in 
				{exp=seqExp(expdecs@[expbody]), ty=tybody}
			end
		| trexp(BreakExp nl) =
			let
				val exp = breakExp() 
							handle _ => error("break incorrecto", nl)
			in
				{exp=exp, ty=TUnit} (*COMPLETAR*)
			end
		| trexp(ArrayExp({typ, size, init}, nl)) =
			let
				val aty = case tabBusca(typ, tenv) of 
					SOME t => tipoReal t
					| NONE => error("Tipo inexistente: "^typ, nl)
				
				val ety = case aty of 
							TArray (t,_) => t
							| _ => error(typ^" no es de tipo array", nl)
							
				val { ty=tsize, exp=esize } = trexp size
				val { ty=tini, exp=eini } = trexp init
			in
				if ((tiposIguales ety tini) andalso (isTInt tsize)) then { exp=arrayExp{size=esize, init=eini},ty=aty }
				else if (not (tiposIguales ety tini)) then error("El tipo de inicializacion del array no es correcto", nl)
				else error("El tamaño de inicializacion debe ser de tipo entero", nl)
			end
		and trvar(SimpleVar s, nl) =
			let 
				val {ty=tvar, access=acc, level=l} = 
					case tabBusca(s, venv) of
						SOME (Var  v ) => v
					  | SOME _ => error (s^": no es variable", nl)
					  | NONE => error (s^": no existe", nl) 
			in
				{ exp=simpleVar(acc, l), ty=tvar }
			end	
		| trvar(FieldVar(v, s), nl) =
			let
				val {ty=tyv', exp=rexp} = trvar(v, nl)
				val tyfl' = case (tipoReal tyv') of 
					TRecord (fl, _) => fl
					| _  => error("La variable que accede a "^s^" no es un record", nl)
				fun findField s [] = error ("El campo "^s^" no existe en el record", nl)
					| findField s ((n, t, i)::fl) =
						if (s = n) then (t,i) else findField s fl
				val field = (findField s tyfl')
				val typ' = #1 field
				val ind = #2 field
			in
				{exp=fieldVar(rexp, ind), ty=typ'} (* {exp=(), ty=TUnit} *) (*COMPLETAR*)
			end
		| trvar(SubscriptVar(v, e), nl) =
			let
				val {ty=aty, exp=vexp } = trvar(v, nl)
				val ety = case (tipoReal aty) of 
								TArray (et, _) => et
								| _ => error("Se intenta acceder a una variable como si fuera un array, pero no lo es", nl)
				val {ty=ity, exp=iexp} = trexp e
			in	
				if (isTInt ity) then {exp=subscriptVar(vexp, iexp), ty=ety} 
				else error ("El indice del array debe ser un entero", nl)
			end
		and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) = 
			let 
				val {exp = ie', ty=it'} = transExp (venv, tenv) init (* checkeamos el tipo del valor inicial *)
				
				val _ = if (it' = TNil) orelse (it' = TUnit)
						 then error("asignacion de nil o unit sin poder inferir tipo real", pos)
						 else ()
				
				val ty' = if (isTInt it') then TInt RW else it' 
				
				val lev = topLevel()
				val acc = allocLocal lev (!escape)	
				val nv = Var {ty=ty', level=getActualLev(), access=acc}
				val exp'=assignExp{var=varDec acc, exp=ie'}
			in
				(tabRInserta(name, nv, venv), tenv, [exp'])
			end (*COMPLETAR*)
		| trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) =
			let
				val {exp=ie', ty=it'} = transExp (venv, tenv) init (* checkeamos el tipo del valor inicial *)
				val tyv' = 
					case tabBusca(s, tenv) of
						SOME t => t
						| NONE => error ("Tipo inexistente: "^s, pos)
				val lev = topLevel()
				val acc = allocLocal lev (!escape)
				val nv = Var {ty=tyv', level=getActualLev(), access=acc}
				val exp'=assignExp{var=varDec acc, exp=ie'}
			in
				if tiposIguales tyv' it' then (tabRInserta(name, nv, venv), tenv, [exp'])
				else error(name^": El valor de inicializacion no es de tipo "^s, pos) 
			end (*COMPLETAR*)
		| trdec (venv,tenv) (FunctionDec fs) =
			let 
				val fsyms = map (fn ({name, ...},_) => name) fs
				val _ = if not (isRepList fsyms) then ()
							else raise Fail ("Nombres de funciones repetidos en conjunto de declaraciones de funciones")
			
				fun res2tip tenv (SOME s, nl) = ( case tabBusca(s, tenv) of 
								SOME t => t 
								| NONE => error("Tipo de retorno inexistente: "^s, nl) )  
					| res2tip tenv (NONE, _) = TUnit	
					
                fun procParam ({name, typ=(NameTy s), escape},nl) = 
						        (   case tabBusca(s,tenv) of 
							        SOME t => (t, (!escape) ) 
							        | NONE =>  error ("Tipo inexistente: "^s, nl) )
					        | procParam _ = raise Fail "Esto no deberia pasar! (4)"    
                    	
				fun procFHeader ({name, params, result, body}, nl) = 
					let
						val psyms = map (fn {name, ...} => name ) params
						val _ = if not (isRepList psyms) then ()
									else error ("Nombres de parametros repetidos en la declaracion de "^name, nl)
						val pms = map ( fn p => procParam (p,nl) ) params
						val fms = map (fn p => (#1 p)) pms
						val escapes = map (fn p => (#2 p)) pms
						
						val flab = if name="_tigermain" then "main" 
							else tigertemp.newlabel()
						val lev = newLevel{parent=topLevel(), formals=escapes, name=flab}
						
						val res = res2tip tenv (result, nl)
					in 
						Func {level=lev, label=flab, formals=fms, result=res, extern=false}
					end
				
				fun fname ({name,...},_) = name				
				fun splitf f g t = ( f t, g t )
				val splitfunc = splitf fname procFHeader

				val venv' = tabInserList (venv, map splitfunc fs)
				
                fun procBody venv ({name, params, result, body}, nl) =
					let 
						fun pname ({name, ... }, _) = name
						
						val lev = (case tabBusca(name, venv) of 
									  SOME (Func {level=l, ...}) => l
									| _ => raise Fail "Esto no deberia pasar! (5)" )
						
						(* pusheamos la funcion en la pila *)
						val _ = pushLevel(lev) 
						val _ = preFunctionDec()
						
						(* IMPORTANTE: hay que hacer el preFunDec para que Actuallev funcione *)
						val fms = formals lev
						fun procVar (v, a) =
								Var {ty= #1 (procParam (v,nl)), access=a, level=getActualLev() } 
														
						val splitparam = splitf pname procVar
						val varacc = ListPair.zip (params,fms)
						val venv'' = tabInserList (venv, map splitparam varacc)
						val tdec = res2tip tenv (result,nl)
						val isproc = tiposIguales tdec TUnit
						
						val {exp=bexp, ty=tbody} = transExp(venv'', tenv) body
						val _ = functionDec(bexp, lev, isproc)                             
                        
                        val _ = postFunctionDec()
						(* sacamos el objeto de la pila *)
						val _ = popLevel()
                        
					in  (* IMPORTANTE: checkear el tipado de los procesos*)
						if isproc orelse (tiposIguales tbody tdec)  then ()
						else error ("Error en la declaracion de "^name^": El tipo explicitado en la declaracon no es igual al reotrnado en su cuerpo", nl)
					end
                val _ = map (procBody venv') fs
			in 
				(venv', tenv, []) 
			end		
			 
			 (* (venv, tenv, []) *)  (*COMPLETAR*)
		| trdec (venv,tenv) (TypeDec ts) =
			let 
				(* transTy : "Tabla" -> ty -> Tipo *)
				fun transTy tenv (NameTy s, nl) = ( case tabBusca(s, tenv) of
								SOME t' => t'
							| _ => error ("Tipo inexistente: "^s, nl) )
					| transTy tenv (ArrayTy s, nl) =
						let 
							val t = case tabBusca(s, tenv) of
								SOME t => t
								| _ =>  error ("Tipo inexistente: "^s, nl)
						in 
							TArray (t, ref ())
						end
					| transTy tenv (RecordTy flist, nl) = (* TRecord ([], ref() ) *)
						let
							val fnames = map (fn {name, ...} => name) flist
							val _ = if (isRepList fnames) then error("Campos repetidos en record", nl)
										else ()
							fun transFields [] _ = []
							  |	transFields ({name, typ, ...}::fs) n = (name, transTy tenv (typ,nl), n)::(transFields fs (n+1))
						in 
							TRecord (transFields flist 0, ref ())
						end
						
				(* lista de simbolos *)
				val syms = map (fn ({name, ... }, _ ) => name ) ts
			
				(* checkeamos declaraciones repetidas *)
				val _ = if not (isRepList syms) then ()
						else raise Fail "Tipos repetidos en declaracion"
							
				(* generamos grafo de dependencias *)
				fun ty2Edge ({name, ty=(NameTy s)},_) = (name,s)
					| ty2Edge ({name, ty=(ArrayTy s)},_) = (name,s)
					| ty2Edge _ = raise Fail ("Esto no deberia pasar! (5)") 
				fun filDepTy ({ty=(NameTy s), ...},_) = true
					| filDepTy ({ty=(ArrayTy s), ...}, _) = true
					| filDepTy _ = false
				fun genGraph ts' = map ty2Edge (List.filter filDepTy ts')
				
				(* ordenamos segun dependencias y detectamos ciclos *)
				val tlist = topsort (genGraph ts) 
                                handle _ => raise Fail "Hay un ciclo en la declaracion de tipos"
				
				(* insertamos los tipos, pero sin referencias *) 
				fun formatTy ({name, ... },_) = (name, TTipo (name, ref NONE))
				val tenv' = tabInserList (tenv, (map formatTy ts))
				
				(* funcionjes de procesamiento de tipos *)
				fun procTy tab ({name, ty}, pos) =
					let 
						val t = transTy tab (ty,pos)
					in
						( case tabBusca(name,tab) of
							 SOME (TTipo (_, r)) => (r := SOME t)
							|NONE => error("Tipo inexistente ("^name^")", pos)
							| _   => error("Esto no deberia pasar (3)", pos) )
					end
				(* asociamos los punteros de tipos correctamente *)
				val _ = map (procTy tenv') ts
			in 
				(venv, tenv', [])
			end  (* (venv, tenv, []) *) (*COMPLETAR*)
        
        (* DEBUG *)
        and	printList [] = () 
            | printList (x::xs) = 
                let 
                    val _ = print (x^"\n")
                in
                    printList xs
                end
	in trexp end
    
fun transProg ex =
let
    (* canonizacion *)
    fun canonize x = traceSchedule (basicBlocks (linearize x)) 
    (* manipulacion de fragmentos *)
    fun getStrings [] = []
        | getStrings ((tigerframe.STRING(l,s))::fmts) = (l,s)::(getStrings fmts)
        | getStrings (_::fmts) = getStrings fmts

    fun getCanonFmts [] = []
        | getCanonFmts ((tigerframe.PROC {body, frame})::fmts) = (canonize body, frame)::(getCanonFmts fmts)
        | getCanonFmts (_::fmts) = getCanonFmts fmts 

    fun printIR [] = [] 
           | printIR ((tigerframe.PROC {body=b, frame=f})::fmts) =
                let
                    val _ = print ("\nFragment \""^(tigerframe.name f)^"\":\n")
                    val _ = map (fn st => print (tigerit.tree st)) (canonize b)
                in	
                    printIR(fmts) 
                end
           | printIR (s::fmts) = 
                let 
                    val _ = print("\nString Fragment:\n")
                    val _ = print(Ir([s]))
                in 
                        printIR(fmts)
                end

    (* generacion de instrucciones *)
    fun geninstr1 _ [] = []
    |   geninstr1 frame (st::stl) = (codegen frame st)@(geninstr1 frame stl) 
    
    fun geninstr [] = []
    |   geninstr ((stl,frame)::l) = (geninstr1 frame stl)@(geninstr l)
    
    val instr2string = format (fn t => t)
    
    fun code2string [] = ""
    |   code2string (instr::l) = (instr2string instr)^(code2string l)
    fun printCode fmts =
        ( print "\nCodigo:\n"; 
          print (code2string (geninstr fmts));
          print "\n"
        )
        
    (* main *)
    val main =
        LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
                        result=NONE, body=ex}, 0)]],
                body=UnitExp 0}, 0)
    (* generamos codigo intermedio *)
    val _ = transExp(tab_vars, tab_tipos) main
    
    (* obtenemos e imprimimos resultados *)
    val res = getResult()
    val canonfmts = getCanonFmts res
    (* val _ = print(Ir(res)) *)
    val _ = printIR(getResult())		
    (* val _ = tigerinterp.inter false canonfmts (getStrings res) *)
    val _ = printCode canonfmts

in	
        ( print "bien!\n" ) 
end
    
end
