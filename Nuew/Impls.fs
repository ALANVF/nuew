module Impls

open Utils
open Types

module Env =
    let has name (env: Env) =
        env.Entries.ContainsKey name

    let rec get name (env: Env) =
        let mutable value = ENil
        if env.Entries.TryGetValue(name, &value)
        then value
        else failwith $"var \"{name}\" doesn't exist!"

    let rec tryGet name (env: Env) =
        if has name env
        then Some env.Entries[name]
        else None

    let rec set name value (env: Env) =
        if has name env
        then env.Entries[name] <- value
        else failwith $"var \"{name}\" doesn't exist!"

    let add name value (env: Env) =
        if has name env
        then env.Entries[name] <- value
        else env.Entries.Add(name, value)

    let hasGlobal name = has name Env.Global

    let getGlobal name = get name Env.Global

    let tryGetGlobal name = tryGet name Env.Global

    let setGlobal name value = set name value Env.Global

    let addGlobal name value = add name value Env.Global

    let empty () = Env(Dict())

module Expr =
    let rec bind (env: Env) = function
        | EList l -> EList (List.map (bind env) l)
        | EArray a -> EArray (ResizeArray.map (bind env) a)
        | EDict d -> EDict (Dict.map2 (fun k v -> bind env k, bind env v) d)
        | EQuote e -> EQuote (bind env e)
        | EUnquote e -> EUnquote (bind env e)
        | EQuasiquote e -> EQuasiquote (bind env e)
        | ESpliceUnquote e -> ESpliceUnquote (bind env e)
        | EName(name, _) when Env.has name env -> EName(name, env)
        | expr -> expr

    let rec bindToObject (cls: Object) = function
        | EList l -> EList (List.map (bindToObject cls) l)
        | EArray a -> EArray (ResizeArray.map (bindToObject cls) a)
        | EDict d -> EDict (Dict.map2 (fun k v -> bindToObject cls k, bindToObject cls v) d)
        | EQuote e -> EQuote (bindToObject cls e)
        | EUnquote e -> EUnquote (bindToObject cls e)
        | EQuasiquote e -> EQuasiquote (bindToObject cls e)
        | ESpliceUnquote e -> ESpliceUnquote (bindToObject cls e)
        | EName(name, _) when Dict.containsKey cls.CVars name -> EClassField(name, cls)
        | expr -> expr

    let rec bindToObjectInst (cls: Object) (inst: ObjectInst) = function
        | EList l -> EList (List.map (bindToObjectInst cls inst) l)
        | EArray a -> EArray (ResizeArray.map (bindToObjectInst cls inst) a)
        | EDict d -> EDict (Dict.map2 (fun k v -> bindToObjectInst cls inst k, bindToObjectInst cls inst v) d)
        | EQuote e -> EQuote (bindToObjectInst cls inst e)
        | EQuasiquote e -> EQuasiquote (bindToObjectInst cls inst e)
        | EUnquote e -> EUnquote (bindToObjectInst cls inst e)
        | ESpliceUnquote e -> ESpliceUnquote (bindToObjectInst cls inst e)
        | EName(name, _) as n when name[0] = '@' ->
            if name[1] = '@' then
                if Dict.containsKey cls.CVars name
                then EClassField(name, cls)
                else n
            elif Dict.containsKey cls.IVars name
                then EInstField(name, cls, inst)
            else n
        | expr -> expr

    let bindAll env exprs = List.map (bind env) exprs
    
    let isTruthy = function
        | ENil | EBool false -> false
        // TODO: case for Object overload
        | _ -> true

    let getType = function
        | ENil -> TyNil
        | EBool _ -> TyBool
        | EInt _ -> TyInt
        | EFloat _ -> TyFloat
        | EString _ -> TyString
        | EList _ -> TyList
        | EArray _ -> TyArray
        | EDict _ -> TyDict
        | ERef _ -> TyRef
        | EName _ -> TyName
        | EAtom _ -> TyAtom
        | EKeyword _ -> TyKeyword
        | EObject(obj, _) -> TyObject obj
        | EClassField(_, _)
        | EInstField(_, _, _) -> TyName
        | EMacro m -> TyMacro(m.Args.Length, m.IsVariadic)
        | ENativeMacro _ -> TyMacro(0, true) // TODO: more info
        | EFunc f -> TyFunc(f.Args |> List.map snd, f.Ret, f.IsVariadic)
        | ENativeFunc _ -> TyFunc([], None, true) // TODO: more info
        | EMethod m -> TyMethod(Some m.Sender, m.IsStatic, m.Args |> List.map (fun (l, _, t) -> l, t), m.Ret)
        | ENativeMethod(_, isStatic) -> TyMethod(None, isStatic, [], None) // TODO: more info
        | EQuote _ -> TyAny
        | EQuasiquote _ -> TyAny
        | EUnquote _ -> TyAny
        | ESpliceUnquote _ -> TyAny
        | EType _ -> TyType

    let getObjectType = getType >> function
        | TyNil | TyBool | TyInt | TyFloat | TyString
        | TyList | TyArray | TyDict | TyRef | TyName | TyAtom | TyKeyword
        | TyType as t -> Object.Builtins[t]
        | TyObject obj -> obj
        | _ -> failwith "bad"