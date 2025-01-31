module Eval

open Utils
open Types
open Impls

let rec eval = function
    | EList exprs -> evalCall exprs
    | EName(name, _) when name[0] = '$' -> Env.getGlobal name[1..]
    | EName(name, env) -> Env.get name env
    | EQuote expr -> expr
    // ...
    | e -> e

and evalCall = function
    | [] -> ENil
    | [expr] -> callAnyValue (eval expr) []
    | caller :: args -> callAnyValue (eval caller) args

and callAnyValue expr args =
    match expr with
    | EFunc _ | ENativeFunc _
    | EMacro _ | ENativeMacro _
    | ENativeMethod _ -> callAnyFunc expr args
    | ENil | EBool _ | EInt _ | EFloat _ | EString _
    | EList _ | EArray _ | EDict _ | ERef _ | EName _ | EAtom _ | EKeyword _
    | EObject _ | EType _ ->
        if args.IsEmpty
        then failwith $"attempt to call value `{expr}` by itself"
        else callMethod expr args
    | _ -> failwith $"bad {expr}"

and callAnyFunc expr args =
    //printfn $">> %A{expr} <| %A{args}"
    match expr with
    | EFunc fn -> callFunc fn <| List.map eval args
    | ENativeFunc fn -> fn <| List.map eval args
    | EMacro mcr -> callMacro mcr args (*|> (fun e -> printfn $"%A{e}";e)*) |> eval
    | ENativeMacro mcr -> mcr args //|> eval
    | ENativeMethod(mth, false) -> mth (Expr.getObjectType expr) None <| List.map eval args
    | ENativeMethod(mth, true) ->
        let sender :: args' = List.map eval args
        mth (Expr.getObjectType expr) (Some sender) args'
    | _ -> failwith "bad"

and callFunc func args =
    let env = Env.empty()
    //printfn $"{args}"
    let rec loop args values =
        match args, values with
        | [(name, t)], values when func.IsVariadic ->
            if t <> TyAny then
                for value in values do
                    let vt = Expr.getType value
                    if not <| vt.MatchesType t then
                        failwith $"failed type check: got `{vt.Repr()}` but expected `{t.Repr()}`"
            Env.add name (EList values) env
        | (name, t) :: args', value :: values' ->
            if t <> TyAny then
                let vt = Expr.getType value
                if not <| vt.MatchesType t then
                    failwith $"failed type check: got `{vt.Repr()}` but expected `{t.Repr()}`"
            Env. add name value env

            loop args' values'
        | [], [] -> ()
        | _, [] -> failwith $"not enough args %A{func}"
        | [], _ -> failwith "too many args"
    
    loop func.Args args
    
    func.Body
    |> List.map (Expr.bind env)
    |> List.fold (fun _ -> eval) ENil

and callMethod expr args =
    let name, args' = parseMethodArgs args
    let isStatic, cls =
        match expr with
        | EType (TyObject obj) -> true, obj
        | EType _ -> true, Expr.getObjectType expr
        | EObject(obj, _) -> false, obj
        | _ -> false, Expr.getObjectType expr

    match (if isStatic then cls.GetCMethod else cls.GetIMethod) name with
    | _, Some nat ->
        nat cls (if isStatic then None else Some expr) args'
    | mth, None ->
        let env = Env(Dict (dict [
            "self", expr
            if cls.Parent.IsSome then
                "super", EType <| TyObject (match expr with
                                            | EObject(_, _) | EType(TyObject _) -> cls.Parent.Value
                                            | _ ->  Object.Builtins[TyAny])
        ]))

        ENil

and parseMethodArgs = function
    | [EName(name, _)] -> name, []
    | args ->
        let rec loop labels args = function
            | EKeyword kw :: arg :: rest ->
                loop (labels + kw + ":") (eval arg :: args) rest
            | e :: _ -> failwith $"unexpected `{e}` in method call"
            | [] -> labels, List.rev args
        loop "" [] args

and callMacro macro args =
    let env = Env.empty()
    
    let rec loop args values =
        match args, values with
        | [name], values when macro.IsVariadic ->
            Env.add name (EList values) env
        | name :: args', value :: values' ->
            Env.add name value env

            loop args' values'
        | [], [] -> ()
        | _, [] -> failwith "not enough args"
        | [], _ -> failwith "too many args"
    
    let uid =  System.DateTime.Now.Millisecond |> int |> string // ehhhh kinda hacky buit it should work
    loop macro.Args args
    //printfn $"%A{env.Entries}"
    macro.Body
    |> Expr.bindAll env
    |> List.fold (fun _ -> evalMacro uid) ENil
    |> (function
        | EList [e] -> e
        | e -> EList [EName("progn", Env.Global); e])

and evalMacro uid = function
    | EList exprs ->
        EList (exprs |> List.map (evalMacro uid))
    | EArray values ->
        EArray (values |> ResizeArray.map (evalMacro uid))
    | EDict dict ->
        EDict (dict |> Dict.map2 (fun k v -> evalMacro uid k, evalMacro uid v))
    | EName(name, env) when name.StartsWith("__") ->
        EName(name[2..] + uid, env)
    | EQuote e -> EQuote (evalMacro uid e) // ?
    | EQuasiquote e -> EList (expandQuasi uid e)
    | ESpliceUnquote _ -> failwith "unexpected splice-unquote outside quasiquote"
    | e -> e

and expandQuasi uid = function
    | EList exprs ->
        [ EList (exprs |> List.collect (expandQuasi uid)) ]
    | EArray values ->
        [ EArray (values |> ResizeArray.collect (seq << expandQuasi uid)) ]
    | EDict dict ->
        // TODO
        [ EDict (dict |> Dict.map2 (fun k v -> evalMacro uid k, evalMacro uid v)) ]
    | EName(name, env) when name.StartsWith("__") ->
        [EName(name[2..] + uid, env)]
    | EQuote e -> [EQuote (evalMacro uid e)] // ?
    | EUnquote e -> [eval e]
    | ESpliceUnquote e ->
        match eval e with
        | EList exprs -> exprs
        | e' -> [e']
    | EQuasiquote _ -> failwith "unexpected quasiquote in quasiquote"
    | e -> [e]
// ...

and evalTypeAnno = function EList l -> evalType l | _ -> failwith "bad type anno"

and evalType = function
    | [] -> failwith "bad type anno"
    | EName("union", _) :: ((_ :: _) as types) ->
        types
        |> List.map (function
                     | EName _ as n -> evalType [n]
                     | EList l -> evalType l
                     | _ -> failwith "bad")
        |> TyUnion
    // ...
    | [EName(name, env)] ->
        match Env.get name env with
        | EType TyType when name = "nil" && env = Env.Global -> TyNil
        | EType t -> t
        | _ -> failwith "bad type anno"
    | _ -> failwith "bad type anno"




// ...

let evalAll = List.fold (fun _ -> eval) ENil