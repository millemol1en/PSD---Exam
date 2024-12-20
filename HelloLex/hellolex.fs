
# 1 "./hellolex.fsl"
  // starting

module Hello_fslex
open FSharp.Text.Lexing
open System

# 9 "./hellolex.fs"
let trans : uint16[] array = 
    [| 
    (* State 0 *)
     [| 2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;1us;1us;1us;1us;1us;1us;1us;1us;1us;1us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;2us;65535us;|];
    (* State 1 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 2 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    |] 
let actions : uint16[] = [|65535us;0us;1us;|]
let _fslex_tables = FSharp.Text.Lexing.UnicodeTables.Create(trans,actions)
let rec _fslex_dummy () = _fslex_dummy() 
// Rule Tokenize
and Tokenize  lexbuf =
  match _fslex_tables.Interpret(0,lexbuf) with
  | 0 -> ( 
# 9 "./hellolex.fsl"
                                     LexBuffer<char>.LexemeString lexbuf 
# 28 "./hellolex.fs"
          )
  | 1 -> ( 
# 10 "./hellolex.fsl"
                                     failwith "Lexer error: illegal symbol" 
# 33 "./hellolex.fs"
          )
  | _ -> failwith "Tokenize"

# 13 "./hellolex.fsl"
  // ending

[<EntryPoint>]
let main argv =
      printfn "Hello World from FsLex!\n\nPlease pass a digit:"
      let input = Console.ReadLine()
      let res=Tokenize (LexBuffer<char>.FromString input)
      printfn "The lexer recognizes %s" res
      0


# 50 "./hellolex.fs"
# 3000000 "./hellolex.fs"
