structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open topsort

type expty = {exp: unit, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val tab_tipos : (string, Tipo) Tabla = tabInserList(
	tabNueva(),
	[("int", TInt RW), ("string", TString)])

val tab_vars : (string, EnvEntry) Tabla = tabInserList(
	tabNueva(),
	[("print", Func{level=mainLevel, label="print",
		formals=[TString], result=TUnit, extern=true}),
	("flush", Func{level=mainLevel, label="flush",
		formals=[], result=TUnit, extern=true}),
	("getchar", Func{level=mainLevel, label="getstr",
		formals=[], result=TString, extern=true}),
	("ord", Func{level=mainLevel, label="ord",
		formals=[TString], result=TInt RW, extern=true}),
	("chr", Func{level=mainLevel, label="chr",
		formals=[TInt RW], result=TString, extern=true}),
	("size", Func{level=mainLevel, label="size",
		formals=[TString], result=TInt RW, extern=true}),
	("substring", Func{level=mainLevel, label="substring",
		formals=[TString, TInt RW, TInt RW], result=TString, extern=true}),
	("concat", Func{level=mainLevel, label="concat",
		formals=[TString, TString], result=TString, extern=true}),
	("not", Func{level=mainLevel, label="not",
		formals=[TInt RW], result=TInt RW, extern=true}),
	("exit", Func{level=mainLevel, label="exit",
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
		| trexp(UnitExp _) = {exp=(), ty=TUnit}
		| trexp(NilExp _)= {exp=(), ty=TNil}
		| trexp(IntExp(i, _)) = {exp=(), ty=TInt RO}
		| trexp(StringExp(s, _)) = {exp=(), ty=TString}
		| trexp(CallExp({func, args}, nl)) = 
			let 
				val (targs, ext, tret, lab, lev) = 
					case tabBusca(func, venv) of
					SOME (Func {level, label, formals, result, extern}) 
							=> (formals, extern, result, label, level)
					| SOME _ => error (func^": no es funcion", nl )
					| NONE => error (func^": no existe", nl)
				fun checkTipo(t1, t2, tenv, nl) = 
					if tiposIguales t1 t2 then "ok!" 
						else error ("Error de tipos en la funcion "^func, nl)
				fun aux [] [] r = r
				| aux [] _ _ = error(func^": Muchos argumentos", nl) 
				| aux _ [] _ = error(func^": Pocos argumentos", nl)
				| aux (t::tt) (a::aa) r = 
						let 
							val {exp=ac', ty=at'} = trexp a (* tipo de la expresion *) 
							val _ = checkTipo(t, at', tenv, nl) (* tiene que conincidir con el tipo declarado *)
						in 
							aux tt aa r@[{exp=ac', ty=at'}] 
						end
				val leargs = aux targs args [] 
				val leargs' = map (fn {exp, ty} => exp) leargs 
			in 
				{exp=(), ty=tret} 
			end (* {exp=(), ty=TUnit} *) (*COMPLETAR*)
		| trexp(OpExp({left, oper=EqOp, right}, nl)) = 
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt RW}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt RW}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper, right}, nl)) = 
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr then
					case oper of
						PlusOp => if (isTInt tyl) then {exp=(),ty=TInt RO} else error("Error de tipos", nl)
						| MinusOp => if (isTInt tyl) then {exp=(),ty=TInt RO} else error("Error de tipos", nl)
						| TimesOp => if (isTInt tyl) then {exp=(),ty=TInt RO} else error("Error de tipos", nl)
						| DivideOp => if (isTInt tyl) then {exp=(),ty=TInt RO} else error("Error de tipos", nl)
						| LtOp => if (isTInt tyl)  orelse tipoReal tyl=TString then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| LeOp => if (isTInt tyl) orelse tipoReal tyl=TString then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| GtOp => if (isTInt tyl)  orelse tipoReal tyl=TString then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| GeOp => if (isTInt tyl)  orelse tipoReal tyl=TString then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| _ => raise Fail "No debería pasar! (3)"
				else error("Error de tipos", nl)
			end
		| trexp(RecordExp({fields, typ}, nl)) =
			let
				(* Traducir cada expresión de fields *)
				val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

				(* Buscar el tipo *)
				val (tyr, cs) = case tabBusca(typ, tenv) of
					SOME t => (case tipoReal t of
						TRecord (cs, u) => (TRecord (cs, u), cs)
						| _ => error(typ^" no es de tipo record", nl))
					| NONE => error("Tipo inexistente ("^typ^")", nl)
				
				(* Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde *)
				fun verificar [] [] = ()
				  | verificar (c::cs) [] = error("Faltan campos", nl)
				  | verificar [] (c::cs) = error("Sobran campos", nl)
				  | verificar ((s,t,_)::cs) ((sy,{exp,ty})::ds) =
						if s<>sy then error("Error de campo", nl)
						else if tiposIguales ty t then verificar cs ds
							 else error("Error de tipo del campo "^s, nl)
				val _ = verificar cs tfields
			in
				{ exp=(), ty=tyr }
			end
		| trexp(SeqExp(s, nl)) =
			let	val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti
				val {exp, ty=tipo} = hd(rev lexti)
			in	{ exp=(), ty=tipo } end
		| trexp(AssignExp({var=SimpleVar s, exp}, nl)) =
			let
				val { ty=tv', ... } = trvar (SimpleVar s, nl)
				val { ty=tx', ... } = trexp exp 
			in 
				if  (tipoReal tv') <> TInt RO andalso (tiposIguales tv' tx') then { exp=(), ty=TUnit}
				else if (tipoReal tv' = TInt RO) then error (s^"No se le puede asignar un valor a una variable de solo lectura", nl)
					else error (s^": El valor asignado no es del tipo asignado", nl)
			end (* {exp=(), ty=TUnit}  *) (* COMPLETAR *)
		| trexp(AssignExp({var, exp}, nl)) =
			let 
				val { ty=tv' , ... } = trvar (var, nl)
				val { ty=tx' , ... } = trexp exp
			in 
				if tiposIguales tv' tx' then { exp=(), ty=TUnit }
				else error ("El valor asignado no es del tipo asignado", nl)
			end  (* {exp=(), ty=TUnit} *) (*COMPLETAR*)
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=testexp, ty=tytest} = trexp test
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				if (isTInt tytest) andalso tiposIguales tythen tyelse then {exp=(), ty=tythen}
				else error("Error de tipos en if" ,nl)
			end
		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=exptest,ty=tytest} = trexp test
			    val {exp=expthen,ty=tythen} = trexp then'
			in
				if (isTInt tytest) andalso tythen=TUnit then {exp=(), ty=TUnit}
				else error("Error de tipos en if", nl)
			end
		| trexp(WhileExp({test, body}, nl)) =
			let
				val ttest = trexp test
				val tbody = trexp body
			in
				if isTInt (#ty ttest) andalso #ty tbody = TUnit then {exp=(), ty=TUnit}
				else if not (isTInt (#ty ttest))  then error("Error de tipo en la condición", nl)
				else error("El cuerpo de un while no puede devolver un valor", nl)
			end
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) =
			let
				val { ty=tlo, ... } = trexp lo
				val { ty=thi, ... } = trexp hi
				val venv' = tabRInserta(var, Var { ty = TInt RO }, venv)
				val { ty=tbody, ... } = transExp (venv', tenv) body
			in 
				if isTInt thi andalso isTInt tlo andalso tbody = TUnit then { exp=(), ty=TUnit } 
				else if not (isTInt tlo) then error("Error de tipo en la cota inferior del bucle", nl) 
				else if not (isTInt thi) then error("Error de tipo en la cota superior del bucle", nl)
				else error("El cuerpo de un for no puede devolver un valor", nl)
			end   (*  {exp=(), ty=TUnit} *) (*COMPLETAR*)
		| trexp(LetExp({decs, body}, _)) =
			let
				val (venv', tenv', _) = List.foldl (fn (d, (v, t, _)) => trdec(v, t) d) (venv, tenv, []) decs
				val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
			in 
				{ exp=(), ty=tybody }
			end
		| trexp(BreakExp nl) =
			{exp=(), ty=TUnit} (*COMPLETAR*)
		| trexp(ArrayExp({typ, size, init}, nl)) =
			let
				val aty = case tabBusca(typ, tenv) of 
					SOME t => tipoReal t
					| NONE => error("Tipo inexistente: "^typ, nl)
				
				val ety = case aty of 
							TArray (t,_) => t
							| _ => error(typ^" no es de tipo array", nl)
							
				val { ty=tsize, ...} = trexp size
				val { ty=tini, ...} = trexp init
			in
				if ((tiposIguales ety tini) andalso (isTInt tsize)) then { exp=(),ty=aty }
				else if (not (tiposIguales ety tini)) then error("El tipo de inicializacion del array no es correcto", nl)
				else error("El tamaño de inicializacion debe ser de tipo entero", nl)
			end
				
			(* COMPLETAR *)
		and trvar(SimpleVar s, nl) = 
			let 
				val tvar = 
					case tabBusca(s, venv) of
					SOME (Var {ty}) => ty
					| SOME _ => error (s^": no es variable", nl)
					| NONE => error (s^": no existe", nl) 
			in
				{ exp=(), ty=tvar }
			end		(*	{exp=(), ty=TUnit} 	*) 
		| trvar(FieldVar(v, s), nl) =
			let
				val {ty=tyv', ...} = trvar(v, nl)
				val tyfl' = case (tipoReal tyv') of 
					TRecord (fl, _) => fl
					| _  => error("La variable que accede a "^s^" no es un record", nl)
				fun findField s [] = error ("El campo "^s^" no existe en el record", nl)
					| findField s ((n, t, _)::fl) =
						if (s = n) then t else findField s fl
				val typ' = findField s tyfl' 
			in
				{exp=(), ty=typ'} (* {exp=(), ty=TUnit} *) (*COMPLETAR*)
			end
		| trvar(SubscriptVar(v, e), nl) =
			let
				val {ty=aty, ...} = trvar(v, nl)
				val ety = case (tipoReal aty) of 
								TArray (et, _) => et
								| _ => error("Se intenta acceder a una variable como si fuera un array, pero no lo es", nl)
				val {ty=ity, ...} = trexp e
			in	
				if (isTInt ity) then {exp=(), ty=ety} 
				else error ("El indice del array debe ser un entero", nl)
			end
			(*COMPLETAR*)
		and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) = 
			let 
				val {ty=it', ... } = transExp (venv, tenv) init (* checkeamos el tipo del valor inicial *)
				val ty' = if (isTInt it') then TInt RW else it' 
				val venv' = tabRInserta(name, Var {ty=ty'}, venv)
			in
				if not (it' = TNil) then (venv', tenv, [])
				else error("Declaracion con nil de valor inicial pero sin especificar el tipo", pos)
			end (*COMPLETAR*)
		| trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) =
			let
				val {exp=ie', ty=it'} = transExp (venv, tenv) init (* checkeamos el tipo del valor inicial *) 
				val tyv' = 
					case tabBusca(s, tenv) of
						SOME t => t
						| NONE => error ("Tipo inexistente: "^s, pos)
				val venv' = tabRInserta(name, Var {ty=it'}, venv)
				
			in
				if tiposIguales tyv' it' then (venv', tenv, [])
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
					
                fun procParam ({name, typ=(NameTy s), ...},nl) = 
						        (   case tabBusca(s,tenv) of 
							        SOME t => t
							        | NONE =>  error ("Tipo inexistente: "^s, nl) )
					        | procParam _ = raise Fail "Esto no deberia pasar! (4)"    
                    	
				fun procFHeader ({name, params, result, body}, nl) = 
					let
						val psyms = map (fn {name, ...} => name ) params
						val _ = if not (isRepList psyms) then ()
									else error ("Nombres de parametros repetidos en la declaracion de "^name, nl)
						val fms = map ( fn p => procParam (p,nl) ) params
						val res = res2tip tenv (result, nl)
					in 
						Func {level=(), label=name, formals=fms, result=res, extern=false}
					end
				
				fun fname ({name,...},_) = name				
				fun splitf f g t = ( f t, g t )
				val splitfunc = splitf fname procFHeader

				val venv' = tabInserList (venv, map splitfunc fs)
				
                fun procBody venv ({name, params, result, body}, nl) =
					let 
						fun pname {name, ... } = name
                        val procVar = (fn v => Var {ty=(procParam (v,nl))})
						val splitparam = splitf pname procVar 
						val venv'' = tabInserList (venv, map splitparam params)
						val {ty=tbody,...} = transExp(venv'', tenv) body
                        val tdec = res2tip tenv (result,nl)
					in 
						if (tiposIguales tdec TUnit) orelse (tiposIguales tbody tdec)  then ()
						else error ("Error en la declaracion de "^name^": El tipo explicitado en la declaracon no es igual al reotrnado en su cuerpo", nl)
					end
                val _ = map (procBody venv') fs
			in 
				(venv', tenv, [])
			end 
					
			 (* (venv, tenv, []) *)  (*COMPLETAR*)
		| trdec (venv,tenv) (TypeDec ts) =
			let 
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
        and transTy tenv (NameTy s, nl) = ( case tabBusca(s, tenv) of
								SOME t' => t'
							| _ => error ("Tipo inexistente: "^s, nl) )
               (* en la tabla quedaria  asi: name -> TTipo s sref, o asi: name TTipo name sref  ??*)
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
                    fun transField {name, typ, ...} = (name, transTy tenv (typ,nl), 0) 
                in 
                    TRecord ((map transField flist), ref ())
                end
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
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=NONE, body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val _ = transExp(tab_vars, tab_tipos) main
		(* val _ = transExp(tab_vars, tab_tipos) ex *) 
	in	print "bien!\n" end
end
