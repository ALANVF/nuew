module Tokens

open Utils

type Token =
| TEnd

| TQuote
| TQuasiquote
| TUnquote
| TSpliceUnquote

| TLParen | TRParen
| TLBrack | TRBrack
| TLBrace | TRBrace

| TName of string
| TAtom of string
| TKeyword of string

| TInt of int
| TFloat of float
| TString of string
with
    member this.Repr() =
        match this with
        | TEnd -> "<end>"
        | TQuote -> "'"
        | TQuasiquote -> "`"
        | TUnquote -> ","
        | TSpliceUnquote -> ",@"
        | TLParen -> "("
        | TRParen -> ")"
        | TLBrack -> "["
        | TRBrack -> "]"
        | TLBrace -> "{"
        | TRBrace -> "}"
        | TName name -> name
        | TAtom atom -> "#" + atom
        | TKeyword keyword -> keyword + ":"
        | TInt i -> string i
        | TFloat f -> string f
        | TString s -> String.escape s