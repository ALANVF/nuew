module Utils

[<return: Struct>]
let inline (|Range|_|) (low, high) n =
    if low <= n && n <= high
    then ValueSome n
    else ValueNone

module ResizeArray =
    let ofList (l: 't list) =
        ResizeArray l

    let contains (arr: 't ResizeArray) value =
        arr.Contains value

    let exists (cond: 't -> bool) (arr: 't ResizeArray) =
        arr.Exists cond

    let filter (cond: 't -> bool) (arr: 't ResizeArray) =
        ResizeArray [ for value in arr do
                        if cond value then yield value ]

    let iter (func: 't -> unit) (arr: 't ResizeArray) =
        arr.ForEach func

    let map (func: 't -> 'u) (arr: 't ResizeArray) : 'u ResizeArray =
        arr.ConvertAll func

    let collect (func: 't -> 'u seq) (arr: 't ResizeArray) : 'u ResizeArray =
        let arr' = ResizeArray<'u>()
        for value in arr do
            arr.AddRange (func value)
        arr'

    let reverse (arr: 't ResizeArray) =
        arr.Reverse()
        arr

    // ... etc

type Dict<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

module Dict =
    let containsKey (dict: Dict<'k, 'v>) key = dict.ContainsKey key

    let containsValue (dict: Dict<'k, 'v>) value = dict.ContainsKey value

    let get (dict: Dict<'k, 'v>) key = dict[key]

    let tryGet (dict: Dict<'k, 'v>) key = if containsKey dict key then Some dict[key] else None

    let set (dict: Dict<'k, 'v>) key value = dict[key] <- value

    let iter2 (func: 'k -> 'v -> unit) (dict: Dict<'k, 'v>) =
        for KeyValue(k, v) in dict do
            func k v

    let map2 (func: 'k -> 'v -> 'k * 'v) (d: Dict<'k, 'v>) =
        Dict (dict [for KeyValue(k, v) in d do func k v])

    // ... etc

[<AutoOpen>]
module String =
    let escape =
        String.collect <| function
            | '\\' -> "\\\\"
            | '"' -> "\\\""
            | '\t' -> "\\t"
            | '\n' -> "\\n"
            | '\r' -> "\\r"
            | '\f' -> "\\f"
            | c when c < ' ' -> sprintf "\\x%02x" (int c)
            | c -> string c