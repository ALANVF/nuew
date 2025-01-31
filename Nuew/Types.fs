module Types

open Utils

type Env(entries: Dict<string, Expr>) =
    static let env = Env(Dict())
    static member Global = env
    
    member _.Entries = entries


and NativeMethod = (Object -> Expr option -> Expr list -> Expr)

and [<CustomEquality>] [<CustomComparison>] Expr =
| ENil
| EBool of bool
| EInt of int
| EFloat of float
| EString of string
| EList of Expr list
| EArray of Expr ResizeArray
| EDict of Dict<Expr, Expr>
| ERef of Expr ref

| EName of string * bound: Env
| EAtom of string
| EKeyword of string

| EObject of Object * inst: ObjectInst
| EClassField of string * cls: Object
| EInstField of string * cls: Object * inst: ObjectInst

// TODO: globals

| EMacro of MacroExpr
| ENativeMacro of (Expr list -> Expr)
| EFunc of FuncExpr
| ENativeFunc of (Expr list -> Expr)
| EMethod of MethodExpr
| ENativeMethod of NativeMethod * isStatic: bool

| EQuote of Expr
| EQuasiquote of Expr
| EUnquote of Expr
| ESpliceUnquote of Expr
| EType of Type
with
    // generated via chatgpt (with some edits), there's no way in hell I was gonna type this all out lol
    static let rec _equals this other =
        match this, other with
        | ENil, ENil -> true
        | EBool b1, EBool b2 -> b1 = b2
        | EInt i1, EInt i2 -> i1 = i2
        | EFloat f1, EFloat f2 -> f1 = f2
        | EString s1, EString s2 -> s1 = s2
        | EList l1, EList l2 -> l1 = l2
        | EArray a1, EArray a2 -> a1 = a2
        | EDict d1, EDict d2 -> d1 = d2
        | ERef r1, ERef r2 -> !r1 = !r2
        | EName (n1, b1), EName (n2, b2) -> n1 = n2 && b1 = b2
        | EAtom a1, EAtom a2 -> a1 = a2
        | EKeyword k1, EKeyword k2 -> k1 = k2
        | EObject(_, o1), EObject(_, o2) -> obj.ReferenceEquals(o1, o2)
        | EMacro m1, EMacro m2 -> m1 = m2
        | ENativeMacro f1, ENativeMacro f2 -> obj.ReferenceEquals(f1, f2)
        | EFunc f1, EFunc f2 -> f1 = f2
        | ENativeFunc f1, ENativeFunc f2 -> obj.ReferenceEquals(f1, f2)
        | EMethod m1, EMethod m2 -> m1 = m2
        | ENativeMethod (f1, s1), ENativeMethod (f2, s2) -> obj.ReferenceEquals(f1, f2) && s1 = s2
        | EQuote q1, EQuote q2 -> _equals q1 q2
        | EQuasiquote q1, EQuasiquote q2 -> _equals q1 q2
        | EUnquote u1, EUnquote u2 -> _equals u1 u2
        | ESpliceUnquote s1, ESpliceUnquote s2 -> _equals s1 s2
        | EType t1, EType t2 -> t1 = t2
        | _ -> false

    interface System.IEquatable<Expr> with
        member this.Equals(other: Expr) = _equals this other

    static let rec _compareTo this other =
        if match this, other with
           | ENil, ENil -> true
           | EBool b1, EBool b2 -> b1 = b2
           | EInt i1, EInt i2 -> i1 = i2
           | EFloat f1, EFloat f2 -> f1 = f2
           | EString s1, EString s2 -> s1 = s2
           | EList l1, EList l2 -> l1 = l2
           | EArray a1, EArray a2 -> a1 = a2
           | EDict d1, EDict d2 -> d1 = d2
           | ERef r1, ERef r2 -> !r1 = !r2
           | EName (n1, b1), EName (n2, b2) -> n1 = n2 && b1 = b2
           | EAtom a1, EAtom a2 -> a1 = a2
           | EKeyword k1, EKeyword k2 -> k1 = k2
           | EObject(_, o1), EObject(_, o2) -> obj.ReferenceEquals(o1, o2)
           | EMacro m1, EMacro m2 -> m1 = m2
           | ENativeMacro f1, ENativeMacro f2 -> obj.ReferenceEquals(f1, f2)
           | EFunc f1, EFunc f2 -> f1 = f2
           | ENativeFunc f1, ENativeFunc f2 -> obj.ReferenceEquals(f1, f2)
           | EMethod m1, EMethod m2 -> m1 = m2
           | ENativeMethod (f1, s1), ENativeMethod (f2, s2) -> obj.ReferenceEquals(f1, f2) && s1 = s2
           | EQuote q1, EQuote q2 -> _equals q1 q2
           | EQuasiquote q1, EQuasiquote q2 -> _equals q1 q2
           | EUnquote u1, EUnquote u2 -> _equals u1 u2
           | ESpliceUnquote s1, ESpliceUnquote s2 -> _equals s1 s2
           | EType t1, EType t2 -> t1 = t2
           | _ -> false
        then 0 else -1

    interface System.IComparable with
        member this.CompareTo (other: obj) = 
            if obj.ReferenceEquals(this, other)
            then 0
            else match other with :? Expr as o -> _compareTo this o | _ -> -1

    override this.GetHashCode() =
        match this with
        | ENil -> 0
        | EBool b -> hash b
        | EInt i -> hash i
        | EFloat f -> hash f
        | EString s -> hash s
        | EList l -> hash l
        | EArray a -> hash a
        | EDict d -> hash d
        | ERef r -> hash r
        | EName (n, b) -> hash (n, b)
        | EAtom a -> hash a
        | EKeyword k -> hash k
        | EObject(o, f) -> hash o + hash f
        | EClassField(n, o) -> hash n + hash o
        | EInstField(n, o, i) -> hash n + hash o + hash i
        | EMacro m -> hash m
        | ENativeMacro f -> hash (f :> obj)
        | EFunc f -> hash f
        | ENativeFunc f -> hash (f :> obj)
        | EMethod m -> hash m
        | ENativeMethod (f, s) -> hash (hash (f :> obj), s)
        | EQuote q -> 1 + hash q
        | EQuasiquote q -> 2 + hash q
        | EUnquote u -> 3 + hash u
        | ESpliceUnquote s -> 4 + hash s
        | EType t -> hash t

and MacroExpr = { //Env: Env
                  Args: string list
                  IsVariadic: bool
                  Body: Expr list }

and FuncExpr = { //Env: Env
                 Args: (string * Type) list
                 Ret: Type option
                 IsVariadic: bool
                 Body: Expr list }

and MethodExpr = { //Env: Env
                   Sender: Object
                   IsStatic: bool
                   Args: (string * string * Type) list
                   Ret: Type option
                   //IsVariadic: bool
                   Body: Expr list }

and BasicMethodExpr = { Args: (string * string * Type) list
                        Ret: Type option
                        //IsVariadic: bool
                        Body: Expr list }

and Object(
    name: string,
    parent: Object option,
    native: Type option,
    cvars: Dict<string, Type * Expr ref>,
    ivars: Dict<string, Type>,
    cmethods: Dict<string, MethodExpr * NativeMethod option>,
    imethods: Dict<string, MethodExpr * NativeMethod option>
) =
    inherit System.Object() // necessary for hashcode override?

    static member val Builtins = Map<Type, Object> [] with get, set

    static member New (name, parent, ?native) = Object(name, parent, native, Dict(), Dict(), Dict(), Dict())
    
    member _.Name = name
    member _.Parent = parent
    member _.Native = native

    member val CVars = cvars with get, set
    member val IVars =  ivars with get, set

    member val CMethods = cmethods with get, set
    member val IMethods = imethods with get, set

    member _.HasParent obj =
        match parent with
        | None -> false
        | Some p -> p = obj || p.HasParent obj

    member this.AddCMethod name (method: BasicMethodExpr) (native: NativeMethod option) =
        cmethods.Add(name, ({ Sender = this; IsStatic = true; Args = method.Args; Ret = method.Ret; Body = method.Body }, native))

    member this.AddIMethod name (method: BasicMethodExpr) (native: NativeMethod option) =
        imethods.Add(name, ({ Sender = this; IsStatic = false; Args = method.Args; Ret = method.Ret; Body = method.Body }, native))
    
    member _.GetCMethod msg =
        if Dict.containsKey cmethods msg
        then cmethods[msg]
        else match parent with
             | Some p -> p.GetCMethod msg
             | None -> failwith $"class \"{name}\" does not respond to method `{msg}`"

    member _.GetIMethod msg =
        if Dict.containsKey imethods msg
        then imethods[msg]
        else match parent with
             | Some p -> p.GetIMethod msg
             | None -> failwith $"instance of class \"{name}\" does not respond to method `{msg}`"
    

    interface System.IComparable with
        member this.CompareTo (other: obj) = 
            if System.Object.ReferenceEquals(this, other) then 0 else -1

    override this.Equals (other: obj) = 
        System.Object.ReferenceEquals(this, other)

    override this.GetHashCode () = 
        base.GetHashCode()
 
 and ObjectInst = Env //Dict<string, Expr>

and Type =
| TyAny
| TyNil
| TyBool
| TyInt
| TyFloat
| TyString
| TyList
| TyArray
| TyDict
| TyRef
| TyName
| TyAtom
| TyKeyword
| TyObject of Object
| TyMacro of argsN: int * isVariadic: bool
| TyFunc of args: Type list * ret: Type option * isVariadic: bool
| TyMethod of sender: Object option * isStatic: bool * args: (string * Type) list * ret: Type option
| TyUnion of Type list
| TyType
with
    member this.MatchesType t =
        this = t ||
        match t with
        | TyAny -> true
        | TyObject parent ->
            match this with
            | TyObject child -> child = parent || child.HasParent parent
            | _ -> parent.Native |> Option.exists this.MatchesType
        | TyUnion ts -> ts |> List.forall this.MatchesType
        | _ ->
            match this with
            | TyAny -> false
            | TyUnion ts -> ts |> List.exists _.MatchesType(t)
            | TyObject child -> child.Native |> Option.exists _.MatchesType(t)
            | _ -> false
    
    member this.Repr() =
        let try_va = function true -> "..." | false -> ""
        let try_ret = function None -> "" | Some (ret: Type) -> $" ({ret.Repr()})"
        match this with
        | TyAny -> "any"
        | TyNil -> "nil"
        | TyBool -> "bool"
        | TyInt -> "int"
        | TyFloat -> "float"
        | TyString -> "string"
        | TyList -> "list"
        | TyArray -> "array"
        | TyDict -> "dict"
        | TyRef -> "ref"
        | TyName -> "name"
        | TyAtom -> "atom"
        | TyKeyword -> "keyword"
        | TyMacro(n, va) ->
            let args' =
                { 0 .. n }
                |> Seq.map (fun i -> $"_{i}")
                |> String.concat " "
            $"macro ({args'}{try_va va})"
        | TyFunc(args, ret, va) ->
            let args' =
                args
                |> List.map (fun t -> $"({t.Repr()})")
                |> String.concat " "
            $"func ({args'}{try_va va}){try_ret ret}"
        | TyMethod(sender, isStatic, args, ret) ->
            let isStatic' = if isStatic then '+' else '-'
            let sender' = match sender with None -> "(?)" | Some obj -> $"({obj.Name})"
            let args' =
                args
                |> List.map (fun (l, t) -> $"{l}: ({t.Repr()})")
                |> String.concat " "
            $"method{isStatic'}{try_ret ret} {sender'} ({args'})"
        | TyObject obj -> obj.Name
        | TyUnion types ->
            let types' =
                types
                |> List.map _.Repr()
                |> String.concat " "
            $"union {types'}"
        | TyType -> "type"