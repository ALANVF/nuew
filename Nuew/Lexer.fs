module Lexer

open System.Text

open Utils
open Tokens

let rec lex (code: string) =
    let rec loop tokens pos =
        if eof code pos then tokens
        else
            let token, pos'' = lexToken code pos
            loop (token :: tokens) pos''
    let TEnd :: tokens | tokens = loop [] (skipWs code 0)
    List.rev tokens


and eof code pos = pos >= code.Length

and notEof code pos = not (eof code pos)

and skipWs code pos =
    if eof code pos then pos else
    match code[pos] with
    | '\t' | '\n' | '\f' | ' ' -> skipWs code (pos + 1)
    | ';' ->
        let mutable i = pos
        while notEof code pos && code[i] <> '\n' && code[i] <> '\r' do
            i <- i + 1
        i
    | _ -> pos

and lexToken code pos =
    // hacky null for eof is yucky but I'm simply too lazy
    if eof code pos then
        match code[pos-1] with
        | '\t' | '\n' | '\r' | '\f' | ' ' -> TEnd, pos
        | _ -> failwith $"eof near \"{code[max 0 (pos-5) .. pos-1]}\""
    else // why do you yell at me without newline this fuck you
    match code[pos] with
    | '\t' | '\n' | '\r' | '\f' | ' ' -> lexToken code (pos + 1)
    | ';' ->
        let mutable i = pos
        while notEof code pos && code[i] <> '\n' && code[i] <> '\r' do
            i <- i + 1
        if eof code i then TEnd, i else lexToken code i
    | '\'' -> TQuote, pos+1
    | '`' -> TQuasiquote, pos+1
    | ',' ->
        if notEof code (pos+1) && code[pos+1] = '@'
        then TSpliceUnquote, pos+2
        else TUnquote, pos+1
    | '(' -> TLParen, pos+1
    | ')' -> TRParen, pos+1
    | '[' -> TLBrack, pos+1
    | ']' -> TRBrack, pos+1
    | '{' -> TLBrace, pos+1
    | '}' -> TRBrace, pos+1
    | '#' when code[pos+1] = '"' ->
        let str, pos'' = lexString code (pos + 2)
        TAtom str, pos''
    | '#' ->
        let ident, pos' = lexIdent code (pos + 1)
        TAtom ident, pos'
    | Range('0', '9') _ -> lexNum code pos
    | '"' ->
        let str, pos' = lexString code (pos + 1)
        TString str, pos'
    | c ->
        let lexName () =
            let ident, pos' = lexIdent code pos
            if pos = pos' then failwithf "Unknown syntax \"%c\"" c
            else
                if notEof code pos' && code[pos'] = ':'
                then TKeyword ident, pos'+1
                else TName ident, pos'
        if c = '-' && notEof code (pos+1) then
            match code[pos+1] with
            | Range('0', '9') _ -> lexNum code pos
            | _ -> lexName ()
        else lexName ()

and lexIdent code pos =
    let ident = StringBuilder(15) // should be fine for most things
    let mutable i = pos

    while notEof code i && (
    match code[i] with
    | Range('A', 'Z') _ | Range('a', 'z') _ | '_'
    | '+' | '-' | '*' | '/' | '<' | '>' | '.' | '?' | '!'
    | '|' | '\\' | '=' | '&' | '$' | '%' | '^' | '@' -> true
    | '\'' | Range('0', '9') _ when pos <> i -> true
    | _ -> false
    ) do
        ignore <| ident.Append code[i]
        i <- i + 1

    string ident, i

and lexNum code pos =
    let num = StringBuilder(5) // should be fine for most things
    let mutable i = pos

    if code[i] = '-' then
        ignore <| num.Append '-'
        i <- i + 1

    while notEof code i && '0' <= code[i] && code[i] <= '9' do
        ignore <| num.Append code[i]
        i <- i + 1

    if notEof code i && code[i] = '.' then
        ignore <| num.Append '.'
        i <- i + 1
        while notEof code i && '0' <= code[i] && code[i] <= '9' do
            ignore <| num.Append code[i]
            i <- i + 1
        TFloat (float <| string num), i
    else
        TInt (int <| string num), i

and lexString code pos =
    let str = StringBuilder()
    let mutable i = pos

    while notEof code i && code[i] <> '"' do
        match code[i] with
        | '\\' when eof code (i+1) -> failwith "eof after string escape"
        | '\\' ->
            let added, by =
                match code[i+1] with
                | '\\' | '\"' as c -> c, 1
                | 't' -> '\t', 1
                | 'n' -> '\n', 1
                | 'r' -> '\r', 1
                | 'f' -> '\f', 1
                | 'x' when eof code (i+1+1+2) -> failwith "eof during string escape"
                | 'x' -> char <| int $"0x{code[i+2]}{code[i+3]}", 4
                | esc -> failwith $"invalid escape sequence \"\\{esc}\""
            
            ignore <| str.Append added
            i <- i + 1+by
        | c ->
            ignore <| str.Append c
            i <- i + 1
    
    string str, i + 1