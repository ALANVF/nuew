module Parser

open Utils
open Tokens
open Types

let rec parse code =
    code
    |> Lexer.lex
    |> parseExprs

and parseExprs = function
    | [] -> []
    | TLParen :: tokens ->
        let call, tokens' = parseParen tokens
        call :: parseExprs tokens'
    | token :: _ -> failwith $"invalid syntax `{token.Repr()}`"

and parseExpr = function
    | [] -> failwith "eof"
    | TInt i :: rest -> EInt i, rest
    | TFloat f :: rest -> EFloat f, rest
    | TString s :: rest -> EString s, rest
    | TName n :: rest -> EName(n, Env.Global), rest
    | TAtom a :: rest -> EAtom a, rest
    | TKeyword k :: rest -> EKeyword k, rest
    | TLParen :: rest -> parseParen rest
    | TLBrack :: rest -> parseArray rest
    | TLBrace :: rest -> parseDict rest
    | TQuote :: rest ->
        let expr, rest' = parseExpr rest
        EQuote expr, rest'
    | TQuasiquote :: rest ->
        let expr, rest' = parseExpr rest
        EQuasiquote expr, rest'
    | TUnquote :: rest ->
        let expr, rest' = parseExpr rest
        EUnquote expr, rest'
    | TSpliceUnquote :: rest ->
        let expr, rest' = parseExpr rest
        ESpliceUnquote expr, rest'
    | token :: _ -> failwith $"invalid syntax `{token.Repr()}`"

and parseParen tokens =
    let rec loop exprs tokens =
        match tokens with
        | [] -> failwith "eof"
        | TRParen :: rest -> List.rev exprs, rest
        | _ ->
            let expr, rest = parseExpr tokens
            loop (expr :: exprs) rest
    
    let exprs, rest = loop [] tokens
    EList exprs, rest

and parseArray tokens =
    let arr = ResizeArray()
    
    let rec loop tokens =
        match tokens with
        | [] -> failwith "eof"
        | TRBrack :: rest -> rest
        | _ ->
            let expr, rest = parseExpr tokens
            arr.Add expr
            loop rest
    
    let rest = loop tokens
    EArray arr, rest

and parseDict tokens =
    let dict = Dict()
    
    let rec loop tokens =
        match tokens with
        | [] -> failwith "eof"
        | TRBrace :: rest -> rest
        | _ ->
            let key, rest = parseExpr tokens
            match rest with
            | [] -> failwith "eof"
            | TRBrace :: _ -> failwith "uneven number of pairs in dict"
            | _ ->
                let value, rest' = parseExpr rest
                dict[key] <- value
                loop rest'
    
    let rest = loop tokens
    EDict dict, rest