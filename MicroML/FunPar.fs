// Implementation file for parser generated by fsyacc
module FunPar
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 1 "./FunPar.fsy"

 (* File Fun/FunPar.fsy 
    Parser for micro-ML, a small functional language; one-argument functions.
    sestoft@itu.dk * 2009-10-19
  *)

 open Absyn;

# 15 ".\FunPar.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | NTH
  | COMMA
  | FUN
  | LAMBDA
  | EOF
  | LPAR
  | RPAR
  | EQ
  | NE
  | GT
  | LT
  | GE
  | LE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | ELSE
  | END
  | FALSE
  | IF
  | IN
  | LET
  | NOT
  | THEN
  | TRUE
  | CSTBOOL of (bool)
  | NAME of (string)
  | CSTINT of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_NTH
    | TOKEN_COMMA
    | TOKEN_FUN
    | TOKEN_LAMBDA
    | TOKEN_EOF
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_EQ
    | TOKEN_NE
    | TOKEN_GT
    | TOKEN_LT
    | TOKEN_GE
    | TOKEN_LE
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_MOD
    | TOKEN_ELSE
    | TOKEN_END
    | TOKEN_FALSE
    | TOKEN_IF
    | TOKEN_IN
    | TOKEN_LET
    | TOKEN_NOT
    | TOKEN_THEN
    | TOKEN_TRUE
    | TOKEN_CSTBOOL
    | TOKEN_NAME
    | TOKEN_CSTINT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_Main
    | NONTERM_Expr
    | NONTERM_AtExpr
    | NONTERM_AppExpr
    | NONTERM_Exprs
    | NONTERM_Exprs1
    | NONTERM_Const

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | NTH  -> 0 
  | COMMA  -> 1 
  | FUN  -> 2 
  | LAMBDA  -> 3 
  | EOF  -> 4 
  | LPAR  -> 5 
  | RPAR  -> 6 
  | EQ  -> 7 
  | NE  -> 8 
  | GT  -> 9 
  | LT  -> 10 
  | GE  -> 11 
  | LE  -> 12 
  | PLUS  -> 13 
  | MINUS  -> 14 
  | TIMES  -> 15 
  | DIV  -> 16 
  | MOD  -> 17 
  | ELSE  -> 18 
  | END  -> 19 
  | FALSE  -> 20 
  | IF  -> 21 
  | IN  -> 22 
  | LET  -> 23 
  | NOT  -> 24 
  | THEN  -> 25 
  | TRUE  -> 26 
  | CSTBOOL _ -> 27 
  | NAME _ -> 28 
  | CSTINT _ -> 29 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_NTH 
  | 1 -> TOKEN_COMMA 
  | 2 -> TOKEN_FUN 
  | 3 -> TOKEN_LAMBDA 
  | 4 -> TOKEN_EOF 
  | 5 -> TOKEN_LPAR 
  | 6 -> TOKEN_RPAR 
  | 7 -> TOKEN_EQ 
  | 8 -> TOKEN_NE 
  | 9 -> TOKEN_GT 
  | 10 -> TOKEN_LT 
  | 11 -> TOKEN_GE 
  | 12 -> TOKEN_LE 
  | 13 -> TOKEN_PLUS 
  | 14 -> TOKEN_MINUS 
  | 15 -> TOKEN_TIMES 
  | 16 -> TOKEN_DIV 
  | 17 -> TOKEN_MOD 
  | 18 -> TOKEN_ELSE 
  | 19 -> TOKEN_END 
  | 20 -> TOKEN_FALSE 
  | 21 -> TOKEN_IF 
  | 22 -> TOKEN_IN 
  | 23 -> TOKEN_LET 
  | 24 -> TOKEN_NOT 
  | 25 -> TOKEN_THEN 
  | 26 -> TOKEN_TRUE 
  | 27 -> TOKEN_CSTBOOL 
  | 28 -> TOKEN_NAME 
  | 29 -> TOKEN_CSTINT 
  | 32 -> TOKEN_end_of_input
  | 30 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startMain 
    | 1 -> NONTERM_Main 
    | 2 -> NONTERM_Expr 
    | 3 -> NONTERM_Expr 
    | 4 -> NONTERM_Expr 
    | 5 -> NONTERM_Expr 
    | 6 -> NONTERM_Expr 
    | 7 -> NONTERM_Expr 
    | 8 -> NONTERM_Expr 
    | 9 -> NONTERM_Expr 
    | 10 -> NONTERM_Expr 
    | 11 -> NONTERM_Expr 
    | 12 -> NONTERM_Expr 
    | 13 -> NONTERM_Expr 
    | 14 -> NONTERM_Expr 
    | 15 -> NONTERM_Expr 
    | 16 -> NONTERM_Expr 
    | 17 -> NONTERM_Expr 
    | 18 -> NONTERM_Expr 
    | 19 -> NONTERM_Expr 
    | 20 -> NONTERM_AtExpr 
    | 21 -> NONTERM_AtExpr 
    | 22 -> NONTERM_AtExpr 
    | 23 -> NONTERM_AtExpr 
    | 24 -> NONTERM_AtExpr 
    | 25 -> NONTERM_AppExpr 
    | 26 -> NONTERM_AppExpr 
    | 27 -> NONTERM_Exprs 
    | 28 -> NONTERM_Exprs 
    | 29 -> NONTERM_Exprs1 
    | 30 -> NONTERM_Exprs1 
    | 31 -> NONTERM_Const 
    | 32 -> NONTERM_Const 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 32 
let _fsyacc_tagOfErrorTerminal = 30

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | NTH  -> "NTH" 
  | COMMA  -> "COMMA" 
  | FUN  -> "FUN" 
  | LAMBDA  -> "LAMBDA" 
  | EOF  -> "EOF" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | EQ  -> "EQ" 
  | NE  -> "NE" 
  | GT  -> "GT" 
  | LT  -> "LT" 
  | GE  -> "GE" 
  | LE  -> "LE" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | TIMES  -> "TIMES" 
  | DIV  -> "DIV" 
  | MOD  -> "MOD" 
  | ELSE  -> "ELSE" 
  | END  -> "END" 
  | FALSE  -> "FALSE" 
  | IF  -> "IF" 
  | IN  -> "IN" 
  | LET  -> "LET" 
  | NOT  -> "NOT" 
  | THEN  -> "THEN" 
  | TRUE  -> "TRUE" 
  | CSTBOOL _ -> "CSTBOOL" 
  | NAME _ -> "NAME" 
  | CSTINT _ -> "CSTINT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | NTH  -> (null : System.Object) 
  | COMMA  -> (null : System.Object) 
  | FUN  -> (null : System.Object) 
  | LAMBDA  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | NE  -> (null : System.Object) 
  | GT  -> (null : System.Object) 
  | LT  -> (null : System.Object) 
  | GE  -> (null : System.Object) 
  | LE  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | TIMES  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | MOD  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | END  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | IN  -> (null : System.Object) 
  | LET  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | THEN  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | CSTBOOL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | CSTINT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us;65535us;1us;65535us;0us;1us;27us;65535us;0us;2us;6us;7us;8us;9us;10us;11us;12us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;18us;39us;19us;40us;20us;41us;21us;42us;22us;43us;23us;44us;24us;46us;25us;47us;26us;51us;28us;52us;33us;57us;27us;62us;28us;63us;29us;66us;30us;67us;31us;69us;32us;74us;33us;29us;65535us;0us;4us;4us;71us;5us;72us;6us;4us;8us;4us;10us;4us;12us;4us;34us;4us;35us;4us;36us;4us;37us;4us;38us;4us;39us;4us;40us;4us;41us;4us;42us;4us;43us;4us;44us;4us;46us;4us;47us;4us;51us;4us;52us;4us;57us;4us;62us;4us;63us;4us;66us;4us;67us;4us;69us;4us;74us;4us;27us;65535us;0us;5us;6us;5us;8us;5us;10us;5us;12us;5us;34us;5us;35us;5us;36us;5us;37us;5us;38us;5us;39us;5us;40us;5us;41us;5us;42us;5us;43us;5us;44us;5us;46us;5us;47us;5us;51us;5us;52us;5us;57us;5us;62us;5us;63us;5us;66us;5us;67us;5us;69us;5us;74us;5us;1us;65535us;52us;53us;2us;65535us;52us;73us;74us;75us;29us;65535us;0us;58us;4us;58us;5us;58us;6us;58us;8us;58us;10us;58us;12us;58us;34us;58us;35us;58us;36us;58us;37us;58us;38us;58us;39us;58us;40us;58us;41us;58us;42us;58us;43us;58us;44us;58us;46us;58us;47us;58us;51us;58us;52us;58us;57us;58us;62us;58us;63us;58us;66us;58us;67us;58us;69us;58us;74us;58us;|]
let _fsyacc_sparseGotoTableRowOffsets = [|0us;1us;3us;31us;61us;89us;91us;94us;|]
let _fsyacc_stateToProdIdxsTableElements = [| 1us;0us;1us;0us;12us;1us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;1us;1us;2us;2us;25us;2us;3us;26us;1us;4us;12us;4us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;1us;4us;12us;4us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;1us;4us;12us;4us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;1us;5us;12us;5us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;12us;6us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;12us;6us;7us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;12us;6us;7us;8us;8us;9us;10us;11us;12us;13us;14us;15us;16us;12us;6us;7us;8us;9us;9us;10us;11us;12us;13us;14us;15us;16us;12us;6us;7us;8us;9us;10us;10us;11us;12us;13us;14us;15us;16us;12us;6us;7us;8us;9us;10us;11us;11us;12us;13us;14us;15us;16us;12us;6us;7us;8us;9us;10us;11us;12us;12us;13us;14us;15us;16us;12us;6us;7us;8us;9us;10us;11us;12us;13us;13us;14us;15us;16us;12us;6us;7us;8us;9us;10us;11us;12us;13us;14us;14us;15us;16us;12us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;15us;16us;12us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;16us;12us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;17us;12us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;17us;12us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;19us;12us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;22us;12us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;22us;12us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;23us;12us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;23us;12us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;24us;13us;6us;7us;8us;9us;10us;11us;12us;13us;14us;15us;16us;29us;30us;1us;6us;1us;7us;1us;8us;1us;9us;1us;10us;1us;11us;1us;12us;1us;13us;1us;14us;1us;15us;1us;16us;1us;17us;1us;17us;1us;17us;1us;17us;3us;18us;22us;23us;3us;18us;22us;23us;2us;18us;22us;1us;18us;1us;18us;1us;18us;1us;19us;1us;19us;1us;19us;1us;20us;1us;21us;2us;22us;23us;2us;22us;23us;1us;22us;1us;22us;1us;22us;1us;23us;1us;23us;1us;23us;1us;23us;1us;24us;1us;24us;1us;25us;1us;26us;1us;28us;1us;30us;1us;30us;1us;31us;1us;32us;|]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us;2us;4us;17us;19us;22us;25us;27us;40us;42us;55us;57us;70us;72us;85us;98us;111us;124us;137us;150us;163us;176us;189us;202us;215us;228us;241us;254us;267us;280us;293us;306us;319us;332us;346us;348us;350us;352us;354us;356us;358us;360us;362us;364us;366us;368us;370us;372us;374us;376us;380us;384us;387us;389us;391us;393us;395us;397us;399us;401us;403us;406us;409us;411us;413us;415us;417us;419us;421us;423us;425us;427us;429us;431us;433us;435us;437us;439us;|]
let _fsyacc_action_rows = 78
let _fsyacc_actionTableElements = [|9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;0us;49152us;12us;32768us;4us;3us;7us;39us;8us;40us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;0us;16385us;5us;16386us;5us;69us;23us;60us;27us;77us;28us;59us;29us;76us;5us;16387us;5us;69us;23us;60us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;12us;32768us;7us;39us;8us;40us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;25us;8us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;12us;32768us;7us;39us;8us;40us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;18us;10us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;11us;16388us;7us;39us;8us;40us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;3us;16389us;15us;36us;16us;37us;17us;38us;3us;16390us;15us;36us;16us;37us;17us;38us;3us;16391us;15us;36us;16us;37us;17us;38us;0us;16392us;0us;16393us;0us;16394us;9us;16395us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;9us;16396us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;5us;16397us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;5us;16398us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;5us;16399us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;5us;16400us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;12us;32768us;1us;47us;7us;39us;8us;40us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;12us;32768us;6us;48us;7us;39us;8us;40us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;9us;16403us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;12us;32768us;7us;39us;8us;40us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;22us;63us;12us;32768us;7us;39us;8us;40us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;19us;64us;12us;32768us;7us;39us;8us;40us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;22us;67us;12us;32768us;7us;39us;8us;40us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;19us;68us;12us;32768us;6us;70us;7us;39us;8us;40us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;12us;16413us;1us;74us;7us;39us;8us;40us;9us;41us;10us;42us;11us;43us;12us;44us;13us;34us;14us;35us;15us;36us;16us;37us;17us;38us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;1us;32768us;5us;46us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;0us;16401us;1us;32768us;28us;50us;2us;32768us;7us;51us;28us;65us;10us;32768us;0us;45us;2us;55us;5us;69us;6us;52us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;16411us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;1us;32768us;5us;54us;0us;16402us;1us;32768us;28us;56us;1us;32768us;3us;57us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;0us;16404us;0us;16405us;1us;32768us;28us;61us;2us;32768us;7us;62us;28us;65us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;0us;16406us;1us;32768us;7us;66us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;0us;16407us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;0us;16408us;0us;16409us;0us;16410us;0us;16412us;9us;32768us;0us;45us;2us;55us;5us;69us;14us;12us;21us;6us;23us;49us;27us;77us;28us;59us;29us;76us;0us;16414us;0us;16415us;0us;16416us;|]
let _fsyacc_actionTableRowOffsets = [|0us;10us;11us;24us;25us;31us;37us;47us;60us;70us;83us;93us;105us;115us;119us;123us;127us;128us;129us;130us;140us;150us;156us;162us;168us;174us;187us;200us;210us;223us;236us;249us;262us;275us;288us;298us;308us;318us;328us;338us;348us;358us;368us;378us;388us;398us;400us;410us;420us;421us;423us;426us;437us;447us;449us;450us;452us;454us;464us;465us;466us;468us;471us;481us;491us;492us;494us;504us;514us;515us;525us;526us;527us;528us;529us;539us;540us;541us;|]
let _fsyacc_reductionSymbolCounts = [|1us;2us;1us;1us;6us;2us;3us;3us;3us;3us;3us;3us;3us;3us;3us;3us;3us;6us;6us;4us;1us;1us;7us;8us;3us;2us;2us;0us;1us;1us;3us;1us;1us;|]
let _fsyacc_productionToNonTerminalTable = [|0us;1us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;3us;3us;3us;3us;3us;4us;4us;5us;5us;6us;6us;7us;7us;|]
let _fsyacc_immediateActions = [|65535us;49152us;65535us;16385us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;16401us;65535us;65535us;65535us;65535us;65535us;16402us;65535us;65535us;65535us;16404us;16405us;65535us;65535us;65535us;65535us;16406us;65535us;65535us;65535us;16407us;65535us;16408us;16409us;16410us;16412us;65535us;16414us;16415us;16416us;|]
let _fsyacc_reductions = lazy [|
# 283 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : 'gentype__startMain));
# 292 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "./FunPar.fsy"
                                                               _1 
                   )
# 38 "./FunPar.fsy"
                 : Absyn.expr));
# 303 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "./FunPar.fsy"
                                                               _1                     
                   )
# 42 "./FunPar.fsy"
                 : Absyn.expr));
# 314 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "./FunPar.fsy"
                                                               _1                     
                   )
# 43 "./FunPar.fsy"
                 : Absyn.expr));
# 325 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> Absyn.expr in
            let _4 = parseState.GetInput(4) :?> Absyn.expr in
            let _6 = parseState.GetInput(6) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "./FunPar.fsy"
                                                               If(_2, _4, _6)         
                   )
# 44 "./FunPar.fsy"
                 : Absyn.expr));
# 338 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "./FunPar.fsy"
                                                               Prim("-", CstI 0, _2)  
                   )
# 45 "./FunPar.fsy"
                 : Absyn.expr));
# 349 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _3 = parseState.GetInput(3) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "./FunPar.fsy"
                                                               Prim("+",  _1, _3)     
                   )
# 46 "./FunPar.fsy"
                 : Absyn.expr));
# 361 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _3 = parseState.GetInput(3) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "./FunPar.fsy"
                                                               Prim("-",  _1, _3)     
                   )
# 47 "./FunPar.fsy"
                 : Absyn.expr));
# 373 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _3 = parseState.GetInput(3) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "./FunPar.fsy"
                                                               Prim("*",  _1, _3)     
                   )
# 48 "./FunPar.fsy"
                 : Absyn.expr));
# 385 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _3 = parseState.GetInput(3) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "./FunPar.fsy"
                                                               Prim("/",  _1, _3)     
                   )
# 49 "./FunPar.fsy"
                 : Absyn.expr));
# 397 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _3 = parseState.GetInput(3) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "./FunPar.fsy"
                                                               Prim("%",  _1, _3)     
                   )
# 50 "./FunPar.fsy"
                 : Absyn.expr));
# 409 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _3 = parseState.GetInput(3) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "./FunPar.fsy"
                                                               Prim("=",  _1, _3)     
                   )
# 51 "./FunPar.fsy"
                 : Absyn.expr));
# 421 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _3 = parseState.GetInput(3) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "./FunPar.fsy"
                                                               Prim("<>", _1, _3)     
                   )
# 52 "./FunPar.fsy"
                 : Absyn.expr));
# 433 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _3 = parseState.GetInput(3) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "./FunPar.fsy"
                                                               Prim(">",  _1, _3)     
                   )
# 53 "./FunPar.fsy"
                 : Absyn.expr));
# 445 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _3 = parseState.GetInput(3) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "./FunPar.fsy"
                                                               Prim("<",  _1, _3)     
                   )
# 54 "./FunPar.fsy"
                 : Absyn.expr));
# 457 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _3 = parseState.GetInput(3) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "./FunPar.fsy"
                                                               Prim(">=", _1, _3)     
                   )
# 55 "./FunPar.fsy"
                 : Absyn.expr));
# 469 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _3 = parseState.GetInput(3) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "./FunPar.fsy"
                                                               Prim("<=", _1, _3)     
                   )
# 56 "./FunPar.fsy"
                 : Absyn.expr));
# 481 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _3 = parseState.GetInput(3) :?> Absyn.expr in
            let _5 = parseState.GetInput(5) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "./FunPar.fsy"
                                                               Prim("nth", _3, _5)    
                   )
# 57 "./FunPar.fsy"
                 : Absyn.expr));
# 493 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> string in
            let _5 = parseState.GetInput(5) :?> 'gentype_Exprs in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "./FunPar.fsy"
                                                                           Tup( _5 )              
                   )
# 58 "./FunPar.fsy"
                 : Absyn.expr));
# 505 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> string in
            let _4 = parseState.GetInput(4) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "./FunPar.fsy"
                                                               Fun(_2, _4)            
                   )
# 59 "./FunPar.fsy"
                 : Absyn.expr));
# 517 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "./FunPar.fsy"
                                                               _1                     
                   )
# 63 "./FunPar.fsy"
                 : Absyn.expr));
# 528 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "./FunPar.fsy"
                                                               Var _1                 
                   )
# 64 "./FunPar.fsy"
                 : Absyn.expr));
# 539 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> string in
            let _4 = parseState.GetInput(4) :?> Absyn.expr in
            let _6 = parseState.GetInput(6) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "./FunPar.fsy"
                                                               Let(_2, _4, _6)        
                   )
# 65 "./FunPar.fsy"
                 : Absyn.expr));
# 552 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> string in
            let _3 = parseState.GetInput(3) :?> string in
            let _5 = parseState.GetInput(5) :?> Absyn.expr in
            let _7 = parseState.GetInput(7) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "./FunPar.fsy"
                                                               Letfun(_2, _3, _5, _7) 
                   )
# 66 "./FunPar.fsy"
                 : Absyn.expr));
# 566 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "./FunPar.fsy"
                                                               _2                     
                   )
# 67 "./FunPar.fsy"
                 : Absyn.expr));
# 577 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _2 = parseState.GetInput(2) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "./FunPar.fsy"
                                                               Call(_1, _2)           
                   )
# 71 "./FunPar.fsy"
                 : Absyn.expr));
# 589 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _2 = parseState.GetInput(2) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 72 "./FunPar.fsy"
                                                               Call(_1, _2)           
                   )
# 72 "./FunPar.fsy"
                 : Absyn.expr));
# 601 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "./FunPar.fsy"
                                                               []       
                   )
# 76 "./FunPar.fsy"
                 : 'gentype_Exprs));
# 611 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_Exprs1 in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 77 "./FunPar.fsy"
                                                               _1       
                   )
# 77 "./FunPar.fsy"
                 : 'gentype_Exprs));
# 622 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 81 "./FunPar.fsy"
                                                               [_1]     
                   )
# 81 "./FunPar.fsy"
                 : 'gentype_Exprs1));
# 633 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Absyn.expr in
            let _3 = parseState.GetInput(3) :?> 'gentype_Exprs1 in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 82 "./FunPar.fsy"
                                                               _1 :: _3 
                   )
# 82 "./FunPar.fsy"
                 : 'gentype_Exprs1));
# 645 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 87 "./FunPar.fsy"
                                                               CstI(_1)               
                   )
# 87 "./FunPar.fsy"
                 : Absyn.expr));
# 656 ".\FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> bool in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 88 "./FunPar.fsy"
                                                               CstB(_1)               
                   )
# 88 "./FunPar.fsy"
                 : Absyn.expr));
|]
# 668 ".\FunPar.fs"
let tables : FSharp.Text.Parsing.Tables<_> = 
  { reductions = _fsyacc_reductions.Value;
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 33;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = tables.Interpret(lexer, lexbuf, startState)
let Main lexer lexbuf : Absyn.expr =
    engine lexer lexbuf 0 :?> _
