module Builtins

open Utils
open Types
open Impls

#nowarn "0025"

let bad_args name args = failwith $"bad args %A{args} for builtin {name}"

type private MethodEntry = (string * (string * string * Type) list * Type option * NativeMethod)

module Object =
    let setClassVar (cls: Object) name value =
        match Dict.tryGet cls.CVars name with
        | None -> failwith $"class \"{cls.Name}\" does not have field \"{name}\""
        | Some (t, cell) ->
            let vt = Expr.getType value
            if vt.MatchesType t
            then cell := value
            else failwith $"expected type `{t.Repr()}` but got `{vt.Repr()}` for class field \"{name}\""

    let setInstVar (cls: Object) (inst: ObjectInst) name value =
        match Dict.tryGet cls.IVars name with
        | None -> failwith $"instance of class \"{cls.Name}\" does not have field \"{name}\""
        | Some t ->
            let vt = Expr.getType value
            if vt.MatchesType t
            then Env.set name value inst
            else failwith $"expected type `{t.Repr()}` but got `{vt.Repr()}` for instance field \"{name}\""

let buildBuiltins () =
    for name, value in [|
        "global", ENativeMacro <| function
            | [EName(name, _); value] ->
                let value' = Eval.eval value
                Env.addGlobal name value'
                value'
            | a -> bad_args "global" a

        "macro", ENativeMacro <| function
            | EName(name, _) :: EList args :: body ->
                let mutable isVa = false
                let argsN = args.Length
                let args' = args |> List.mapi (fun i -> function
                    | EName(n, _) ->
                        if i = argsN-1 && n[0] = '&' then
                            isVa <- true
                            n[1..]
                        else n
                    | _ -> bad_args "macro" args)
                let macro = EMacro {
                    Args = args'
                    IsVariadic = isVa
                    Body = body
                }
                Env.addGlobal name macro
                macro
            | a -> bad_args "macro" a

        "progn", ENativeMacro <| fun exprs -> Eval.evalAll exprs

        "do", ENativeMacro <| function
            | EList args :: body ->
                let mutable isVa = false
                let rec loop args' = function
                    | [] -> List.rev args'
                    | EName(name, _) :: EList anno :: rest ->
                        let t = Eval.evalType anno
                        let name' = if rest = [] && name[0] = '&' then
                                        isVa <- true
                                        name[1..]
                                    else name
                        loop ((name', t) :: args') rest
                    | EName(name, _) :: rest ->
                        let name' = if rest = [] && name[0] = '&' then
                                        isVa <- true
                                        name[1..]
                                    else name
                        loop ((name', TyAny) :: args') rest
                    | _ -> bad_args "do" args
                
                let targs = loop [] args
                let ret, body' = match body with
                                 | EName("->", _) :: ret :: body' -> Some (Eval.evalTypeAnno ret), body'
                                 | _ -> None, body
                EFunc {
                    Args = targs
                    Ret = ret
                    IsVariadic = isVa
                    Body = body'
                }
            | a -> bad_args "do" a

        "eval", ENativeFunc <| function
            | [EString s] ->
                s
                |> Parser.parse
                |> List.fold (fun _ -> Eval.eval) ENil
            | [EList l] ->
                l
                |> List.fold (fun _ -> Eval.eval) ENil
            | [EName(name, env)] ->
                Env.get name env
            | a -> bad_args "eval" a

        "apply", ENativeFunc <| function
            | [func; EList args] ->
                Eval.callAnyValue func args
            | a -> bad_args "apply" a

        let buildEnv vars body =
            let env = Env.empty()
            vars |> List.iter (function
                | EList [EName(name, _); value] -> Env.add name (Eval.eval value) env
                | EName(name, _) -> Env.add name ENil env
                | _ -> bad_args "let" vars)

            body
            |> Expr.bindAll env
            |> Eval.evalAll
        "let", ENativeMacro <| function
            | EList vars :: body -> buildEnv vars body
            | EArray vars :: body -> buildEnv (List.ofSeq vars) body
            | a -> bad_args "let" a

        "set", ENativeMacro <| function
            | EName(name, env) :: value :: [] ->
                let value = Eval.eval value
                if name[0] = '$'
                then Env.setGlobal name[1..] value
                else Env.set name value env
                value
            | EClassField(name, cls) :: value :: [] ->
                let value = Eval.eval value
                Object.setClassVar cls name value
                value
            | EInstField(name, cls, inst) :: value :: [] ->
                let value = Eval.eval value
                Object.setInstVar cls inst name value
                value
            | a -> bad_args "set" a

        "class", ENativeMacro <| function
            | EName(className, _) :: rest ->
                let parent, body =
                    match rest with
                    | EName("is", _) :: value :: rest' ->
                        match Eval.eval value with
                        | EType (TyObject p) -> Some p, rest'
                        | EType t -> failwith $"expected parent to be object type but got `{t.Repr()}` instead"
                        | a -> bad_args "class" a
                    | _ -> None, rest

                let cls = Object.New(className, parent)
                // TODO: add extensions
                Env.addGlobal className (EType <| TyObject cls)

                // TODO: instancetype/this
                let parseMethod exprs =
                    let ret, rest =
                        match exprs with
                        | (EList _ as t) :: rest -> Some (Eval.evalTypeAnno t), rest
                        | _ -> None, exprs

                    let rec loop name args = function
                        | EName(n, _) :: EName("is", _) :: body when args <> [] -> failwith "bad"
                        | EName(n, _) :: EName("is", _) :: body -> n, [], body
                        | EName("is", _) :: body when name = "" -> failwith "bad"
                        | EName("is", _) :: body -> name, List.rev args, body
                        | EKeyword kw :: EName(n, _) :: (EList _ as t) :: rest ->
                            loop (name + kw + ":") ((kw, n, Eval.evalTypeAnno t) :: args) rest
                        | EKeyword kw :: (EList _ as t) :: rest ->
                            loop (name + kw + ":") ((kw, kw, Eval.evalTypeAnno t) :: args) rest
                        | _ -> failwith "bad"
                    
                    let name, args, body = loop "" [] rest
                    ret, name, args, body

                // this is lazy but eh
                for EList exprs in body do
                    match exprs with
                    | EName("cvar", _) :: cvars ->
                        let rec loop = function
                            | [] -> ()
                            | EList (EName(name, _) :: t :: attrs) :: rest ->
                                let t' = Eval.evalTypeAnno t
                                cls.CVars[name] <- (t', ref ENil)
                                let rec loop' get set = function
                                    | [] -> get, set
                                    | EAtom "get" :: attrs' -> loop' true set attrs'
                                    | EAtom "set" :: attrs' -> loop' get true attrs'
                                    | EAtom "access" :: attrs' -> loop' true true attrs'
                                    | _ -> failwith "bad"
                                let get, set = loop' false false attrs
                                if get then
                                    cls.CMethods[name] <- {
                                        Sender = cls
                                        IsStatic = true
                                        Args = []
                                        Ret = Some t'
                                        Body = [EClassField(name, cls)]
                                    }, Some (fun (cls: Object) _ [] -> !(snd cls.CVars[name]))
                                if set then
                                    cls.CMethods[name+":"] <- {
                                        Sender = cls
                                        IsStatic = true
                                        Args = [name, "newValue", t']
                                        Ret = Some t'
                                        Body = [EList [EName("set", Env.Global); EClassField(name, cls); EName("newValue", Env.Global)]]
                                    }, Some (fun (cls: Object) _ [newValue] ->
                                        snd cls.CVars[name] := newValue
                                        newValue)
                                loop rest
                            | _ -> failwith "bad"
                        loop cvars

                    | EName("ivar", _) :: cvars ->
                        let rec loop = function
                            | [] -> ()
                            | EList (EName(name, _) :: t :: attrs) :: rest ->
                                let t' = Eval.evalTypeAnno t
                                cls.IVars[name] <- t'
                                let rec loop' get set = function
                                    | [] -> get, set
                                    | EAtom "get" :: attrs' -> loop' true set attrs'
                                    | EAtom "set" :: attrs' -> loop' get true attrs'
                                    | EAtom "access" :: attrs' -> loop' true true attrs'
                                    | _ -> failwith "bad"
                                let get, set = loop' false false attrs
                                if get then
                                    cls.IMethods[name] <- {
                                        Sender = cls
                                        IsStatic = false
                                        Args = []
                                        Ret = Some t'
                                        Body = [EName("@"+name, Env.Global)]
                                    }, Some (fun _ (Some (EObject(_, inst))) [] -> Env.get name inst)
                                if set then
                                    cls.IMethods[name+":"] <- {
                                        Sender = cls
                                        IsStatic = false
                                        Args = [name, "newValue", t']
                                        Ret = Some t'
                                        Body = [EList [EName("set", Env.Global); EName("@"+name, Env.Global); EName("newValue", Env.Global)]]
                                    }, Some (fun _ (Some (EObject(_, inst))) [newValue] ->
                                        Env.set name newValue inst
                                        newValue)
                                loop rest
                            | rest' -> failwith $"bad {rest'}"
                        loop cvars

                    | EName(("+" | "cmethod"), _) :: rest ->
                        let ret, name, args, body = parseMethod rest
                        cls.CMethods[name] <- {
                            Sender = cls
                            IsStatic = true
                            Args = args
                            Ret = ret
                            Body = body |> List.map (Expr.bindToObject cls)
                        }, None
                    
                    | EName(("-" | "imethod"), _) :: rest ->
                        let ret, name, args, body = parseMethod rest
                        cls.IMethods[name] <- {
                            Sender = cls
                            IsStatic = false
                            Args = args
                            Ret = ret
                            Body = body |> List.map (Expr.bindToObject cls)
                        }, None
                    
                    | rest ->
                        failwith $"invalid class member `{rest}`"

                ENil
            | a -> bad_args "class" a

        "list", ENativeFunc <| fun values ->
            EList values

        "array", ENativeFunc <| fun values ->
            EArray (ResizeArray.ofList values)

        "dict", ENativeFunc <| fun values ->
            EDict (Dict (dict [| for [v1; v2] in List.chunkBySize 2 values do v1, v2 |]))

        "print", ENativeFunc <| function
            | [EString s] -> printfn "%s" s; ENil
            | [value] -> printfn "%A" value; ENil
            | a -> bad_args "print" a

        // TODO: make these variadic
        "+", ENativeFunc <| (
            let rec f = function
                    | a :: b :: rest -> f ((Eval.callMethod a [EKeyword "+"; b]) :: rest)
                    | a :: [] -> a
                    | [] -> ENil
            f)
        "-", ENativeFunc <| (
            let rec f = function
                    | a :: b :: rest -> f ((Eval.callMethod a [EKeyword "-"; b]) :: rest)
                    | a :: [] -> a
                    | [] -> ENil
            f)

        "*", ENativeFunc <| (
            let rec f = function
                    | a :: b :: rest -> f ((Eval.callMethod a [EKeyword "*"; b]) :: rest)
                    | a :: [] -> a
                    | [] -> ENil
            f)

        "/", ENativeFunc <| (
            let rec f = function
                    | a :: b :: rest -> f ((Eval.callMethod a [EKeyword "/"; b]) :: rest)
                    | a :: [] -> a
                    | [] -> ENil
            f)

        "eq", ENativeFunc <| function
            | (EObject _ as a) :: b :: [] -> Eval.callMethod a [EKeyword "equals"; EQuote b]
            | EList a :: EList b :: [] -> EBool (a = b)
            | a :: b :: [] -> EBool (a = b)
            | a -> bad_args "eq" a

        "ne", ENativeFunc <| function
            | (EObject _ as a) :: b :: [] -> match Eval.callMethod a [EKeyword "equals"; EQuote b] with
                                             | EBool b -> EBool (not b)
                                             | e -> Eval.callMethod e [EName("not", Env.Global)]
            | a :: b :: [] -> EBool (a <> b)
            | a -> bad_args "ne" a

        "not", ENativeFunc <| function
            | [EBool b] -> EBool (not b)
            | [e] -> Eval.callMethod e [EName("not", Env.Global)]
            | a -> bad_args "not" a

        "cond", ENativeMacro <| fun args ->
            let rec loop = function
                | EList (cond :: values) :: rest ->
                    match Eval.eval cond with
                    | ENil | EBool false -> loop rest
                    | _ -> Eval.evalAll values
                | [] -> ENil
                | _ -> failwith "bad"
            loop args

        "case", ENativeMacro <| function
            | value :: body ->
                let value' = Eval.eval value
                let rec loop = function
                    | EList (cond :: values) :: rest ->
                        if value' = Eval.eval cond
                        then Eval.evalAll values
                        else loop rest
                    | [] -> ENil
                    | _ -> failwith "bad"
                loop body
            | a -> bad_args "case" a

        "while", ENativeMacro <| function
            | cond :: body ->
                while Expr.isTruthy (Eval.eval cond) do
                    ignore <| Eval.evalAll body
                ENil
            | a -> bad_args "while" a

        "concat", ENativeFunc <| fun values ->
            values
            |> List.collect (function
                             | EList values' -> values'
                             | _ -> failwith "bad")
            |> EList

        "cons", ENativeFunc <| function
            | value :: EList values :: [] -> EList (value :: values)
            | a -> bad_args "cons" a

        "car", ENativeFunc <| function
            | EList [] :: [] -> ENil // is this right? I'm not sure
            | EList (value :: _) :: [] -> value
            | a -> bad_args "car" a

        "cdr", ENativeFunc <| function
            | EList [] :: [] -> ENil // is this right? I'm not sure
            | EList (_ :: values) :: [] -> EList values
            | a -> bad_args "cdr" a
    |] do
        Env.addGlobal name value
        //printfn $"%A{Env.Global.Entries.Keys}"

    Env.addGlobal "nil" ENil
    Env.addGlobal "true" (EBool true)
    Env.addGlobal "false" (EBool false)
    
    let addNativeMethods (cls: Object) (cmethods: MethodEntry seq) (imethods: MethodEntry seq) =
        for name, args, ret, mth in cmethods do
            cls.AddCMethod name { Args = args; Ret = ret; Body = [] } (Some mth)
        for name, args, ret, mth in imethods do
            cls.AddIMethod name { Args = args; Ret = ret; Body = [] } (Some mth)

    Object.Builtins <- Map [
        let tAny = Object.New(name = "Any", parent = None, native = TyAny)
        let tNil = Object.New(name = "Nil", parent = Some tAny, native = TyNil)
        let tBool = Object.New(name = "Bool", parent = Some tAny, native = TyBool)
        let tInt = Object.New(name = "Int", parent = Some tAny, native = TyInt)
        let tFloat = Object.New(name = "Float", parent = Some tAny, native = TyFloat)
        let tString = Object.New(name = "String", parent = Some tAny, native = TyString)
        let tList = Object.New(name = "List", parent = Some tAny, native = TyList)
        let tArray = Object.New(name = "Array", parent = Some tAny, native = TyArray)
        let tDict = Object.New(name = "Dict", parent = Some tAny, native = TyDict)
        let tRef = Object.New(name = "Ref", parent = Some tAny, native = TyRef)
        let tName = Object.New(name = "Name", parent = Some tAny, native = TyName)
        let tAtom = Object.New(name = "Atom", parent = Some tAny, native = TyAtom)
        let tKeyword = Object.New(name = "Keyword", parent = Some tAny, native = TyKeyword)
        let tType = Object.New(name = "Type", parent = Some tAny, native = TyType)

        addNativeMethods tAny [
            "class-name", [], Some TyString,
                fun cls _ [] -> EString cls.Name

            "new", [], Some TyAny,
                fun cls _ [] ->
                    let fields = dict [| for KeyValue(name, _) in cls.IVars do name, ENil |] // I don't like this but whatever
                    EObject(cls, Env(Dict fields))
        ] [
            "class", [], Some TyType,
                fun cls _ [] -> EType (TyObject cls)

            "class-name", [], Some TyString,
                fun cls _ [] -> EString cls.Name

            "isa:", ["isa", "type", TyType], Some TyBool,
                fun cls _ [EType t] -> EBool <| (TyObject cls).MatchesType t

            "equals:", ["equals", "other", TyAny], Some TyBool,
                fun _ (Some self) [other] -> EBool (self = other)
            
            "exact-equals:", ["exact-equals", "other", TyAny], Some TyBool,
                fun _ (Some self) [other] -> EBool (self = other)

            "hash", [], Some TyInt,
                fun _ (Some self) [] -> match self with
                                        | EObject(_, inst) -> EInt (hash inst)
                                        | _ -> failwith "this value cannot be hashed!"

            "true:", ["true", "then", TyAny], Some TyAny,
                fun _ _ [val1] ->
                    val1

            "false:", ["false", "else", TyAny], Some TyAny,
                fun _ _ [val2] ->
                    ENil

            "true:false:", ["true", "then", TyAny; "false", "else", TyAny], Some TyAny,
                fun _ _ [val1; val2] ->
                    val1

            "string", [], Some TyString,
                fun cls (Some self) [] -> EString ($"<{cls.Name}#{hash self}>")
        ]

        addNativeMethods tNil [
            "new", [], Some TyString,
                fun cls _ [] -> ENil
        ] [
            "true:", ["true", "then", TyAny], Some TyAny,
                fun _ _ [val1] ->
                    ENil

            "false:", ["false", "else", TyAny], Some TyAny,
                fun _ _ [val2] ->
                    val2

            "true:false:", ["true", "then", TyAny; "false", "else", TyAny], Some TyAny,
                fun _ _ [val1; val2] ->
                    val2

            "hash", [], Some TyInt,
                fun _ _ [] -> EInt 0

            "string", [], Some TyString,
                fun _ _ [] -> EString "nil"
        ]

        addNativeMethods tBool [
            "true", [], Some TyBool,
                fun _ _ [] -> EBool true
            
            "false", [], Some TyBool,
                fun _ _ [] -> EBool false
        ] [
            "true:", ["true", "then", TyAny], Some TyAny,
                fun _ (Some (EBool self)) [val1] ->
                    if self then val1 else ENil

            "false:", ["false", "else", TyAny], Some TyAny,
                fun _ (Some (EBool self)) [val2] ->
                    if self then ENil else val2

            "true:false:", ["true", "then", TyAny; "false", "else", TyAny], Some TyAny,
                fun _ (Some (EBool self)) [val1; val2] ->
                    if self then val1 else val2

            "and:", ["and", "other", TyBool], Some TyBool,
                fun _ (Some (EBool self)) [EBool other] ->
                    EBool (self && other)

            "or:", ["or", "other", TyBool], Some TyBool,
                fun _ (Some (EBool self)) [EBool other] ->
                    EBool (self || other)

            "xor:", ["xor", "other", TyBool], Some TyBool,
                fun _ (Some (EBool self)) [EBool other] ->
                    EBool (self <> other)

            "nor:", ["nor", "other", TyBool], Some TyBool,
                fun _ (Some (EBool self)) [EBool other] ->
                    EBool <| not (self || other)

            "nand:", ["nand", "other", TyBool], Some TyBool,
                fun _ (Some (EBool self)) [EBool other] ->
                    EBool <| not (self && other)

            "hash", [], Some TyInt,
                fun _ (Some (EBool self)) [] ->
                    EInt (hash self)

            "string", [], Some TyString,
                fun _ (Some (EBool self)) [] -> EString (if self then "true" else "false")
        ]

        addNativeMethods tInt [
            "zero", [], Some TyInt,
                fun _ _ [] -> EInt 0

            "one", [], Some TyInt,
                fun _ _ [] -> EInt 1

            "from-string:", ["from-string", "str", TyString], Some TyInt,
                fun _ _ [EString s] ->
                    EInt (int s)

            "from-float:", ["from-float", "f", TyFloat], Some TyInt,
                fun _ _ [EFloat f] ->
                    EInt (int f)

            "from-int:", ["from-int", "i", TyInt], Some TyInt,
                fun _ _ [EInt i] ->
                    EInt i
        ] [
            "-", [], Some TyInt,
                fun _ (Some (EInt self)) [] ->
                    EInt (- self)

            "+:", ["+", "other", TyInt], Some TyInt,
                fun _ (Some (EInt self)) [EInt other] ->
                    EInt (self + other)

            "-:", ["-", "other", TyInt], Some TyInt,
                fun _ (Some (EInt self)) [EInt other] ->
                    EInt (self - other)

            "*:", ["*", "other", TyInt], Some TyInt,
                fun _ (Some (EInt self)) [EInt other] ->
                    EInt (self * other)

            "/:", ["/", "other", TyInt], Some TyInt,
                fun _ (Some (EInt self)) [EInt other] ->
                    EInt (self / other)

            "%:", ["%", "other", TyInt], Some TyInt,
                fun _ (Some (EInt self)) [EInt other] ->
                    EInt (self % other)

            "pow:", ["pow", "exponent", TyInt], Some TyFloat,
                fun _ (Some (EInt self)) [EInt other] ->
                    EFloat (self ** other)

            "~and:", ["~and", "other", TyInt], Some TyInt,
                fun _ (Some (EInt self)) [EInt other] ->
                    EInt (self &&& other)

            "~or:", ["~or", "other", TyInt], Some TyInt,
                fun _ (Some (EInt self)) [EInt other] ->
                    EInt (self ||| other)

            "~xor:", ["~xor", "other", TyInt], Some TyInt,
                fun _ (Some (EInt self)) [EInt other] ->
                    EInt (self ^^^ other)

            "~shl:", ["~shl", "other", TyInt], Some TyInt,
                fun _ (Some (EInt self)) [EInt other] ->
                    EInt (self <<< other)

            "~shr:", ["~shr", "other", TyInt], Some TyInt,
                fun _ (Some (EInt self)) [EInt other] ->
                    EInt (self >>> other)

            "~not", [], Some TyInt,
                fun _ (Some (EInt self)) [] ->
                    EInt (~~~ self)

            "abs", [], Some TyInt,
                fun _ (Some (EInt self)) [] ->
                    EInt (abs self)

            "negate", [], Some TyInt,
                fun _ (Some (EInt self)) [] ->
                    EInt (-self)

            "sign", [], Some TyInt,
                fun _ (Some (EInt self)) [] ->
                    EInt (sign self)

            "equals:", ["equals", "other", TyAny], Some TyBool,
                fun _ (Some (EInt self)) [other] -> EBool <| match other with
                                                             | EInt i -> self = i
                                                             | EFloat f -> float self = f
                                                             | _ -> false

            "compare-to:", ["compare-to", "to", TyAny], Some TyInt,
                fun _ (Some (EInt self)) [other] -> EInt <| match other with
                                                            | EInt i -> self - i
                                                            | EFloat f -> int (float self - f)
                                                            | _ -> -2

            "hash", [], Some TyInt,
                fun _ (Some (EInt self)) [] ->
                    EInt (hash self)
            // ...

            "string", [], Some TyString,
                fun _ (Some (EInt self)) [] ->
                    EString (string self)

            "float", [], Some TyFloat,
                fun _ (Some (EInt self)) [] ->
                    EFloat (float self)

            "int", [], Some TyInt,
                fun _ (Some self) [] -> self

            "bool", [], Some TyBool,
                fun _ (Some (EInt self)) [] ->
                    EBool (self <> 0)
        ]
        
        addNativeMethods tFloat [
            "zero", [], Some TyFloat,
                fun _ _ [] -> EFloat 0.0

            "one", [], Some TyFloat,
                fun _ _ [] -> EFloat 1.0

            "nan", [], Some TyFloat, fun _ _ [] -> EFloat nan
            "e", [], Some TyFloat, fun _ _ [] -> EFloat System.Math.E
            "pi", [], Some TyFloat, fun _ _ [] -> EFloat System.Math.PI
            "tau", [], Some TyFloat, fun _ _ [] -> EFloat System.Math.Tau

            "from-string:", ["from-string", "str", TyString], Some TyFloat,
                fun _ _ [EString s] ->
                    EFloat (float s)

            "from-float:", ["from-float", "f", TyFloat], Some TyFloat,
                fun _ _ [EFloat f] ->
                    EFloat f

            "from-int:", ["from-int", "i", TyInt], Some TyFloat,
                fun _ _ [EInt i] ->
                    EFloat (float i)
        ] [
            "-", [], Some TyFloat,
                fun _ (Some (EFloat self)) [] ->
                    EFloat (- self)

            "+:", ["+", "other", TyFloat], Some TyFloat,
                fun _ (Some (EFloat self)) [EFloat other] ->
                    EFloat (self + other)

            "-:", ["-", "other", TyFloat], Some TyFloat,
                fun _ (Some (EFloat self)) [EFloat other] ->
                    EFloat (self - other)

            "*:", ["*", "other", TyFloat], Some TyFloat,
                fun _ (Some (EFloat self)) [EFloat other] ->
                    EFloat (self * other)

            "/:", ["/", "other", TyFloat], Some TyFloat,
                fun _ (Some (EFloat self)) [EFloat other] ->
                    EFloat (self / other)

            "%:", ["%", "other", TyFloat], Some TyFloat,
                fun _ (Some (EFloat self)) [EFloat other] ->
                    EFloat (self % other)

            "pow:", ["pow", "exponent", TyUnion [TyInt; TyFloat]], Some TyFloat,
                fun _ (Some (EFloat self)) [other] ->
                    match other with
                    | EInt i -> EFloat (self ** i)
                    | EFloat f -> EFloat (self ** f)
                    | _ -> bad_args "Float#pow:" [other]

            "abs", [], Some TyFloat,
                fun _ (Some (EFloat self)) [] ->
                    EFloat (abs self)

            "negate", [], Some TyFloat,
                fun _ (Some (EFloat self)) [] ->
                    EFloat (-self)

            "sign", [], Some TyFloat,
                fun _ (Some (EFloat self)) [] ->
                    EFloat (sign self)

            "copy-sign", ["copy-sign", "other", TyFloat], Some TyFloat,
                fun _ (Some (EFloat self)) [EFloat other] ->
                    EFloat <| System.Math.CopySign(self, other)

            "sqrt", [], Some TyFloat,
                fun _ (Some (EFloat self)) [] ->
                    EFloat (sqrt self)

            "sin", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (sin self)
            "cos", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (cos self)
            "tan", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (tan self)
            "sinh", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (sinh self)
            "cosh", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (cosh self)
            "tanh", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (tanh self)
            "asin", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (asin self)
            "acos", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (acos self)
            "atan", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (atan self)
            "atan-2:", ["atan-2", "x", TyFloat], Some TyFloat,
                fun _ (Some (EFloat self)) [EFloat other] ->
                    EFloat (atan2 self other)
            "asinh", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (System.Math.Asinh self)
            "acosh", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (System.Math.Acosh self)
            "atanh", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (System.Math.Atanh self)
            "exp", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (exp self)
            "log:", ["log", "base", TyFloat], Some TyFloat,
                fun _ (Some (EFloat self)) [EFloat base'] ->
                    EFloat <| System.Math.Log (self, base')
            "log-e", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (log self)
            "log-2", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (System.Math.Log2 self)
            "log-10", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (log10 self)
            "floor", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (floor self)
            "ceiling", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (ceil self)
            "truncate", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (truncate self)
            "round", [], Some TyFloat, fun _ (Some (EFloat self)) [] -> EFloat (round self)
            (** Rounding Modes:
             * #even
             * #away
             * #zero
             * #inf-
             * #inf+
             **)
            let getRoundingMode = function
                                  | "even" -> System.MidpointRounding.ToEven
                                  | "away" -> System.MidpointRounding.AwayFromZero
                                  | "zero" -> System.MidpointRounding.ToZero
                                  | "inf-" -> System.MidpointRounding.ToNegativeInfinity
                                  | "inf+" -> System.MidpointRounding.ToPositiveInfinity
                                  | mode -> failwith $"invalid rounding mode #{mode}"

            "round-to:", ["round-to", "digits", TyInt], Some TyFloat,
                fun _ (Some (EFloat self)) [EInt digits] ->
                    EFloat <| System.Math.Round(self, digits)

            "round-as:", ["round", "mode", TyAtom], Some TyFloat,
                fun _ (Some (EFloat self)) [EAtom mode] ->
                    EFloat <| System.Math.Round(self, getRoundingMode mode)
            
            "round-to:as:", ["round-to", "digits", TyInt; "as", "mode", TyAtom], Some TyFloat,
                fun _ (Some (EFloat self)) [EInt digits; EAtom mode] ->
                    EFloat <| System.Math.Round(self, digits, getRoundingMode mode)

            "equals:", ["equals", "other", TyAny], Some TyBool,
                fun _ (Some (EFloat self)) [other] -> EBool <| match other with
                                                               | EInt i -> self = float i
                                                               | EFloat f -> self = f
                                                               | _ -> false

            "compare-to:", ["compare-to", "to", TyAny], Some TyInt,
                fun _ (Some (EFloat self)) [other] -> EInt <| match other with
                                                              | EInt i -> int (self - float i)
                                                              | EFloat f -> int (self - f)
                                                              | _ -> -2

            "hash", [], Some TyInt,
                fun _ (Some (EFloat self)) [] ->
                    EInt (hash self)
            // ...

            "string", [], Some TyString,
                fun _ (Some (EFloat self)) [] ->
                    EString (string self)

            "float", [], Some TyFloat,
                fun _ (Some self) [] -> self

            "int", [], Some TyInt,
                fun _ (Some (EFloat self)) [] ->
                    EInt (int self)

            "bool", [], Some TyBool,
                fun _ (Some (EFloat self)) [] ->
                    EBool (self <> 0.0)
        ]

        addNativeMethods tString [
            "from-bytes:", ["from-bytes", "bytes", TyList], Some TyString,
                fun _ _ ([EArray bytes] as args) ->
                    let getByte i = match bytes[i] with
                                    | EInt i' -> string (char i')
                                    | _ -> bad_args "String@from-bytes:" args
                    EString (String.init bytes.Count getByte)

            "fill:with:", ["fill", "length", TyInt; "with", "char", TyString], Some TyString,
                fun _ _ ([EInt len; EString chr] as args) ->
                    EString (String.replicate len chr)
        ] [
            "+:", ["+", "other", TyAny], Some TyString,
                fun _ (Some (EString self)) [other] ->
                    match other with
                    | EString str -> EString (self + str)
                    | _ -> match Eval.callMethod other [EName("string", Env.Global)] with
                           | EString str -> EString (self + str)
                           | _ -> bad_args "String#+:" [other]

            "*:", ["*", "times", TyInt], Some TyString,
                fun _ (Some (EString self)) [EInt times] ->
                    EString (String.replicate times self)

            "split-on:", ["split-on", "sep", TyString], Some TyList,
                fun _ (Some (EString self)) [EString sep] ->
                    self.Split(sep)
                    |> List.ofArray
                    |> List.map EString
                    |> EList

            "split-on:count:", ["split-on", "sep", TyString; "count", "count", TyInt], Some TyList,
                fun _ (Some (EString self)) [EString sep; EInt count] ->
                    self.Split(sep, count)
                    |> List.ofArray
                    |> List.map EString
                    |> EList

            "split-on-any:count:", ["split-on-any", "seps", TyUnion [TyList; TyArray]; "count", "count", TyInt], Some TyList,
                fun _ (Some (EString self)) [seps; EInt count] ->
                    let seps' = match seps with
                                | EList s -> Array.ofList s
                                | EArray s -> s.ToArray()
                                | _ -> bad_args "String#split-on-any:" [seps]
                    let seps'' = [| for sep in seps' do
                                    match sep with
                                    | EString s -> s
                                    | _ -> bad_args "String#split-on-any:" [seps] |]
                    self.Split(seps'', count, System.StringSplitOptions.None)
                    |> List.ofArray
                    |> List.map EString
                    |> EList
        ]


        // ...

        addNativeMethods tType [
            "any", [], Some TyType, fun _ _ [] -> EType TyAny
            "nil", [], Some TyType, fun _ _ [] -> EType TyNil
            "bool", [], Some TyType, fun _ _ [] -> EType TyBool
            "int", [], Some TyType, fun _ _ [] -> EType TyInt
            "float", [], Some TyType, fun _ _ [] -> EType TyFloat
            "string", [], Some TyType, fun _ _ [] -> EType TyString
            "list", [], Some TyType, fun _ _ [] -> EType TyList
            "array", [], Some TyType, fun _ _ [] -> EType TyArray
            "dict", [], Some TyType, fun _ _ [] -> EType TyDict
            "ref", [], Some TyType, fun _ _ [] -> EType TyRef
            "name", [], Some TyType, fun _ _ [] -> EType TyName
            "atom", [], Some TyType, fun _ _ [] -> EType TyAtom
            "keyword", [], Some TyType, fun _ _ [] -> EType TyKeyword
            "type", [], Some TyType, fun _ _ [] -> EType TyType
        ] [
            "isa:", ["isa", "type", TyType], Some TyBool,
                fun _ (Some (EType self)) [EType t] -> EBool <| self.MatchesType t
            
            "string", [], Some TyString,
                fun _ (Some (EType self)) [] -> EString <| self.Repr()
        ]

        TyAny, tAny
        TyNil, tNil
        TyBool, tBool
        TyInt, tInt
        TyFloat, tFloat
        TyString, tString
        TyList, tList
        TyArray, tArray
        TyDict, tDict
        TyRef, tRef
        TyName, tName
        TyAtom, tAtom
        TyKeyword, tKeyword
        TyType, tType
    ]

    Object.Builtins |> Map.iter (fun t cls ->
        Env.addGlobal (match t with TyAny -> "Object" | TyType -> "Class" | _ -> cls.Name) (EType (TyObject cls))
        // TODO: list/dict
        match t with
        | TyNil | TyList | TyArray | TyDict -> ()
        | _ -> Env.addGlobal (cls.Name.ToLower()) (EType t))

