// Fil med udpluk fra eksamenssæt.
// Det er en mulighed at omdøbe og bygge aflevering på denne fil.


// ****Opgave 1

(*
Folder med filer i repositorie: Lectures/Lec11/Cont/
For at køre Icon:
  dotnet fsi Icon.fs
  open Icon;;
*)

let examEx1 = Write(FromTo(1,10))

// run examEx1;;

// run (Every(Write(FromList([1..5]))));;

// run (Every(Write(FromMergeList([1..5],[6..12]))));;


// ****Opgave 2
(*
Folder med filer i repositorie: Lectures/Lec05/Fun/
For at køre Micro-ML i Interactive

  fsyacc --module FunPar FunPar.fsy
  fslex --unicode FunLex.fsl   
  dotnet fsi -r <path to FsLexYacc.Runtime.dll> Util.fs Absyn.fs FunPar.fs FunLex.fs Parse.fs HigherFun.fs ParseAndRunHigher.fs

   open ParseAndRunHigher;;
   run (fromString @"let twice f = let g x = f(f(x)) in g end 
                     in let mul3 z = z*3 in twice mul3 2 end end");;
*)



(*
let tup = (4,5,6) in
  nth(0,tup) + nth(1,tup) + nth(2,tup)
end
*)

(*
type expr = 
  | CstI of int
  ...
  | Tup of expr list (* Exam *)
  ...
*)

(*
> fromString "let tup = (4,5,6) in nth(0,tup) + nth(1,tup) + nth(2,tup) end";;
val it: Absyn.expr =
  Let ("tup", Tup [CstI 4; CstI 5; CstI 6],
        Prim ("+", Prim ("+", Prim ("nth", CstI 0, Var "tup"),
           Prim ("nth", CstI 1, Var "tup")), Prim ("nth", CstI 2, Var "tup")))
*)

type value = 
  | Int of int
  | Closure of string * string * expr * value env (* (f, x, fBody, fDeclEnv) *)
  | TupVal of value list (* Exam *)

(*
> open ParseAndRunHigher;;
> run(fromString "let tup = (4,5,6) in nth(0,tup) + nth(1,tup) + nth(2,tup) end");;
*)      

// ****Opgave 3

// Fil printstat.c findes til download sammen med eksamenssæt.

(*
Folder med filer i repositorie: Lectures/Lec06/MicroC/
For at anvende Micro-C i denne opgave

  fslex --unicode CLex.fsl
  fsyacc --module CPar CPar.fsy
  dotnet fsi -r <path to FsLexYacc.Runtime.dll> Util.fs Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Contcomp.fs ParseAndContcomp.fs   

   open ParseAndContcomp;;
   contCompileToFile (fromFile "ex11.c") "ex11.out";;
   compile "ex11";;
   #q;;

   javac Machine.java
   java Machine ex11.out 8      
*)


(*
% java Machine printstat.out
479001600 479001600 15 12 
*)

// ****Opgave 4

(*
  LDARGS;                                   // Load parameter fra kommandolinie
                                            // (der er ikke nogen).
  CALL (0, "L1");                           // Kald main.
  STOP;                                     // Stop ved retur fra main.
Label "L1";                                 //
  CSTI 12; CALL (1, "L2"); PRINTI;          //
  INCSP -1;                                 //
  CSTI 12; CSTI 1; CALL (2, "L3"); PRINTI;  //
  INCSP -1;                                 //
  NUMCALLS; PRINTI;                         // Print antal CALLs.
  INCSP -1;                                 //
  NUMTCALLS; PRINTI;                        // Print antal TCALLs.
  RET 0;                                    //
Label "L2";                                 //
    GETBP; LDI; IFNZRO "L4";                //
    CSTI 1; RET 1;                          //
  Label "L4";                               //
    GETBP; LDI;                             //
    GETBP; LDI; CSTI 1; SUB;                //
    CALL (1, "L2");                         //
    MUL;                                    //
    RET 1;                                  //
Label "L3";                                 //
    GETBP; LDI; IFNZRO "L5";                //
    GETBP; CSTI 1; ADD; LDI; RET 2;         //
  Label "L5";                               //
    GETBP; LDI; CSTI 1; SUB;                //
    GETBP; LDI;                             //
    GETBP; CSTI 1; ADD; LDI;                //
    MUL;                                    //
    TCALL (2, 2, "L3")]                     //

*)

(*
24         // LDARGS
19 0 5     // CALL(0,5)
25         // STOP
0 12       //
19 1 31    //
22         //
15 -1      //
*)

