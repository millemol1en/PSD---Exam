# BSWU BPRD 2022 Jan
## Opgave 1 (20%): Micro–ML: Sets
### Opgave Beskrivelse:
"_Opgaven er at udvide funktionssproget med mængder (eng. sets), således at vi kan evaluere udtryk der manipulerer med mængder, se eksempel `ex01` nedenfor:_"

```fsharp
let s1 = {2, 3} in
    let s2 = {1, 4} in
        s1 ++ s2 = {2,4,3,1}
    end
end
```

"_Vi har tilføjet to syntaktiske konstruktioner:_"
 - En ikke tom mængde: `{e1, . . . , en}, n ≥ 1`.
 - En operator `e1 ++ e2` som laver union på to mænger repræsenteret ved udtrykkene `e1` og `e2`. Operator `++` har samme præcedens og associativitet som operatoren `+`.



### Question 1:
#### Spørgsmål:
"_Du skal udvide lexer `FunLex.fsl` og parser `FunPar.fsy` med support for mængder og `++` operatoren defineret ovenfor. Den abstrakte syntaks i `Absyn.fs` er udvidet med Set der repræsenterer et mængdeudtryk._"

#### Svar:
Changes in `Absyn.fs`:
```fsharp
type expr =
    ...
    | Set of expr list (* Exam *)
    ...
```

Changes in `FunLex.fsl`:
```text
rule Token = parse
  ...
  | '{'             { LCPAR }
  | '}'             { RCPAR }
  | ','             { COMMA }
  | "++"            { PLUSPLUS }
```

Changes in `FunPar.fsy`:
```text
Expr:
    AtExpr                              { $1                     }
  ...
  | Expr PLUSPLUS Expr                  { Prim("++", $1, $3)     }
  | LCPAR SetElem RCPAR                 { Set($2)                }
;

SetElem:
  | Expr                                { [$1]                   }
  | Expr COMMA SetElem                  { $1 :: $3               }
;
```

### Question 2:
#### Spørgsmål:
...

#### Svar:
...

### Question 3:
#### Spørgsmål:
"_Udvid typen value og funktionen eval i `HigherFun.fs`, således at udtryk med mængder kan evalueres som defineret af reglerne ovenfor. Vi repræsenterer mængder med den indbyggede Set–type i F#, som både understøtter foreningsmængde, `Set.union`, og lighed `=`._"

#### Svar:
```fsharp
let rec eval (e : expr) (env : value env) : value =
    match e with
    ...
    | Prim(ope, e1, e2) -> 
      let v1 = eval e1 env
      let v2 = eval e2 env
      match (ope, v1, v2) with
      ...
      | ("++", SetV v1, SetV v2) -> SetV (Set.union v1 v2)     
      | ("=", SetV v1, SetV v2)  -> Int(if (Set.isSubset v1 v2 && Set.isSubset v2 v1) then 1 else 0) 
    ...
    | Set (eLst) ->
      let rec aux acc lst =
        match lst with
        | []    -> acc
        | x::xs -> aux (Set.add (eval x env) acc) xs
      
      SetV(aux Set.empty eLst)
```

Running the provided example `ex01`:
```fsharp
> fromString @"let s1 = {2, 3} in let s2 = {1, 4} in s1 ++ s2 = {2,4,3,1} end end";;
val it: expr =
  Let
    ("s1", Set [CstI 2; CstI 3],
     Let
       ("s2", Set [CstI 1; CstI 4],
        Prim
          ("=", Prim ("++", Var "s1", Var "s2"),
           Set [CstI 2; CstI 4; CstI 3; CstI 1])))

> run it;;
val it: HigherFun.value = Int 1
```

## Opgave 2 (30%) Micro–C: Print Stack
### Opgave Beskrivelse:
"_I denne opgave tilføjer vi et nyt statement `printStack e`, som udskriver stakken på skærmen på det givne sted efter nedenstående skabelon:_"

```text
-Print Stack <e>----------------
Stack Frame
Lokale variable og Temporære værdier
Base peger
Retur adresse
Stack Frame
...
Global
Globale variable
```

"_Målet med opgaven er at få uddata svarende til nedenstående, når programmet `fac.c` afvikles med Java bytekode maskinen._"

```text
MicroC % java Machine fac.out 1
-Print Stack 1----------------
Stack Frame
  s[9]: Local/Temp = 0
  s[8]: bp = 4
  s[7]: ret = 39
Stack Frame
  s[6]: Local/Temp = 1
  s[5]: Local/Temp = 0
  s[4]: Local/Temp = 1
  s[3]: bp = -999
  s[2]: ret = 8
Global
  s[1]: 0
  s[0]: 1
-Print Stack 42----------------
Stack Frame
  s[5]: Local/Temp = 1
  s[4]: Local/Temp = 1
  s[3]: bp = -999
  s[2]: ret = 8
Global
  s[1]: 1
  s[0]: 1
```

"_Du skal udvide lexer `CLex.fsl`, parser `CPar.fsy` og `Absyn.fs` med support for `printStack e;`. Du ser den abstrakte syntaks for ovenstående eksempel `fac.c` nedenfor:_"

### Question 1:
```text

```

### Question 2:


### Question 3:

### Question 4:


### Question 5:
"_Bytekoden, som du får i ovenstående opgave, vil ligne nedenstående. Din opgave er, udfor hver linie, `//`, at beskrive hvilke fragmenter af `fac.c` de vedrører. De første 8 linier er udfyldt som eksempel._"
```fsharp
INCSP 1;                                                // nFac som global variabel
INCSP 1;                                                // resFac som global variabel
LDARGS;                                                 // Loade parameter n fra kommandolinie
CALL (1,"L1");                                          // Kalde main med n som argument.
STOP;                                                   // Stop ved retur fra main.

Label "L1";                                             // Main
    INCSP 1;                                            // i som lokal variabel
    GETBP; CSTI 1; ADD; CSTI 0; STI; INCSP -1;          // i = 0
    CSTI 0; CSTI 0; STI; INCSP -1;                      // nFac = 0
    GOTO "L4";                                          // Kickstart the while loop [L4 = WHILE]

Label "L3";                                             // Represents our WHILE LOOP BLOCK
    CSTI 1;                                             // Place a constant of 1 on the stack
    GETBP; CSTI 1; ADD; LDI;                            // Load the value at BP + 1 which `i`
    CALL (1,"L2"); STI; INCSP -1;                       // Call the "fac()" functions at L2
    GETBP; CSTI 1; ADD;                                 // Get `i` (located at BP + 1)
    GETBP; CSTI 1; ADD; LDI;                            // Get the value of `i` with the LDI instructions
    CSTI 1; ADD; STI; INCSP -1;                         // i = i + 1 [INCSP -1 decrements the stack pointer and removes the CSTI 1 we just placed]
    INCSP 0;                                            // Return nothing

Label "L4";                                             // Represents our WHILE LOOP SHELL [SHELL = the conditional statement].
    GETBP; CSTI 1; ADD; LDI;                            // Load in the variable at BP + 1 which is `i`.
    GETBP; CSTI 0; ADD; LDI;                            // Load in the variable at BP which is `n` [AS IT IS OUR ARGUMENT].
    LT; IFNZRO "L3";                                    // Jump to the inner while loop block if the condition is NOT zero.
    CSTI 42; PRINTSTACK;                                // Call PrintStack with a constant value of 42.
    INCSP -1;                                           // Decrement stackpointer - clearing the constant value of 42.
    RET 0;                                              // Return from the stack frame - 0 indicating we have no variables to return.

Label "L2";                                             // Represents our FAC() FUNCTION and the IF porition of our IF-STATEMENT
    CSTI 0; CSTI 0; LDI; CSTI 1; ADD; STI; INCSP -1;    // nFac = nFac + 1
    CSTI 0; LDI; PRINTSTACK;                            // Print nFac
    GETBP; CSTI 0; ADD; LDI;                            // Get the value at BP + 0 - it is 'n'
    CSTI 0; EQ; IFZERO "L5";                            // Check to see if n == 0
    CSTI 1; RET 1; GOTO "L6";                           // Return from the stack frame - 1 indicating we have a variable to return. In this case it will be CSTI 1.

Label "L5";                                             // Represents the ELSE condition of our IF-STATEMENT.
    GETBP; CSTI 0; ADD; LDI;                            // Load the value of "n"
    GETBP; CSTI 0; ADD; LDI; CSTI 1; SUB;               // Load the value of "n" and subtract 1. This will be passed to the function "fac()".
    CALL (1,"L2");                                      // Call the function "fac()"
    MUL; RET 1;                                         // Multiply the values at the top of the stack, this being the return value from calling "fac()" and "n". Return from the stack frame - 1 indicating we have a variable to return. It will be the value at the top of our stack.

Label "L6";                                             // 
    INCSP 0; RET 0                                      // Go back to callee
```

Question 6:

```fsharp
MicroC % java Machine fac.out 1
-Print Stack 1----------------
Stack Frame                     // Funktion fac
 s[9]: Local/Temp = 0           // Stakplads til lokal variabel n
 s[8]: bp = 4
 s[7]: ret = 39
Stack Frame                     // 
 s[6]: Local/Temp = 1           // "i"
 s[5]: Local/Temp = 0           // "n"
 s[4]: Local/Temp = 1           // temporary value of 1 for "i < n"
 s[3]: bp = -999
 s[2]: ret = 8
Global
 s[1]: 0                        // "nFac"
 s[0]: 1                        // "resFac"
-Print Stack 42----------------
Stack Frame                     // Main
 s[5]: Local/Temp = 1           // "n"
 s[4]: Local/Temp = 1           // "i"
 s[3]: bp = -999
 s[2]: ret = 8
Global
 s[1]: 1                        // "nFac"
 s[0]: 1                        // "resFac"
```

## Opgave 3 (25%) Micro–C: Intervalcheck
### Opgave Beskrivelse:
"_Opgaven er at udvide micro–C med intervalcheck, som er udtryk af formen `e` within `[e1,e2]` der returnerer
sand (1) eller falsk (0) afhængig af om `v1 ≤ v ≤ v2` er opfyldt, hvor `v`, `v1` og `v2` er resultaterne af at evaluere `e`, `e1`
og `e2`. Der er følgende semantiske krav til implementationen:_"
 - Udtrykkene `e`, `e1` og `e2` skal altid evalueres præcis en gang. 
 - Nøgleordet within har samme præcedens som andre sammenligningsoperatorer, f.eks. `<` og `>`.

```c++
void main() {
    print (0 within [print 1,print 2]);             // Expected output: 1 2 0
    print (3 within [print 1,print 2]);             // Expected output: 1 2 0
    print (print 42 within [print 40,print 44]);    // Expected output: 40 44 1 1
    print ((print 42) within [print 40,print 44]);  // Expected output: 42 40 44 1
}
```

"_Målet med opgaven er at få uddata svarende til nedenstående, når programmet within.c afvikles med Java
bytekode maskinen_"

```fsharp
MicroC % java Machine within.out
1 2 0 1 2 0 40 44 1 1 42 40 44 1
```

### Question 1:
"Du skal udvide lexer `CLex.fsl`, parser `CPar.fsy` og `Absyn.fs` med support for udtrykket `e` within `[e1,e2]`."


#### Svar:
Changes in file `CLex.fsl`:
```fsharp
let keyword s =
    match s with
    ...
    | "within"  -> WITHIN
```


Changes in file `CPar.fsy`:
```fsharp
%token PRINTSTACK WITHIN

...

%left GT LT GE LE WITHIN

...

ExprNotAccess:
    AtExprNotAccess                     { $1                  }
  ...
  | Expr WITHIN LBRACK Expr COMMA Expr RBRACK { WithIn($1, $4, $6) }
;
```

### Question 2:
...


### Question 3:
#### Spørgsmål:
"_Du kan nu anvende dit oversætterskema ovenfor til at implementere oversættelsen af within–udtrykket i `Comp.fs`:_"

#### Svar:
___
**Take note!** This question does not require us to make changes in either `Machine.java` or `machine.c`. As such, the answer must be constructed from the combination of instructions - just like with `While`, `If`, `Orelse`.

Crucially, the answer below takes into the consideration one of the requirements from the `Opgave Beskrivelse` section, specifically: 
"_Udtrykkene `e`, `e1` og `e2` skal altid evalueres præcis en gang_". As such, we need to use instructions like `DUP` and `SWAP` to effectively manipulate the stack. 

**SUGGESTION** I strongly suggest literally drawing out a stack and using the books `Before & After` for each instruction, to determine which instruction to use and how it manipulates the stack.
___
Changes in `Comp.fs`:
```fsharp
| WithIn (e1, e2, e3) ->
    let labtrueEnd = newLabel()             // If the statement is completely true 
    let labfalse1  = newLabel()             // If the first statement was false
    let labfalse2  = newLabel()             // If the second statement was false
    
    let tar = cExpr e1 varEnv funEnv        // Evaluating e1
    let min = cExpr e2 varEnv funEnv        // Evaluating e2
    let max = cExpr e3 varEnv funEnv        // Evaluating e3
    
    tar @ [DUP] @ min                       // Evaluate `tar` (e1) and duplicate it. We need 1 copy for `min` (e2) and one for max (e3)
    @ [LT; NOT; IFZERO labfalse1]           // Perform the ">=" for `tar` (e1) and `min` (e2). If it is 0, we jump to our label "labfalse1".
    
    @ max                                   // We evaluate `max` (e3).
    @ [SWAP; LT; NOT; IFZERO labfalse2]     // We perform "<=" (as indicated by [SWAP; LT; NOT]) on our 2nd copy of `tar` (e1) and `max` (e3)
    @ [CSTI 1; GOTO labtrueEnd]             // If it is true, we don't jump to "labfalse2" and will instead put 1 on the stack and go to "labtrueEnd".
    
    @ [Label labfalse1] @ max @ [CSTI 0]    // We must evaluate `max` (e3) 
    @ [Label labfalse2] @ [CSTI 0]          // We've already evaluated `max` (e3) so just place 0 on the stack.
    @ [Label labtrueEnd]                    // Label for if both conditions were true
```

Changes to file 
```fsharp
static int execcode(int[] p, int[] s, int[] iargs, boolean trace) {
    int bp = -999;	// Base pointer, for local variable access 
    int sp = -1;	// Stack top pointer
    int pc = 0;		// Program counter: next instruction
    for (;;) {
      if (trace) 
        printsppc(s, bp, sp, p, pc);
      switch (p[pc++]) {
      ...
      case WITHIN: {
        int v1 = s[sp];                 // 1st value on stack
        int v2 = s[sp--];               // 2nd value on stack
        int v3 = s[sp--];               // 3rd value on stack
      
        if ((v1 > v2) && (v1 < v3))     // Make sure v1 is more than v2 but less than v3
        {
            s[sp] = 1;                  // If so - return "true"
        }
        else
        {
            s[sp] = 0;                  // If so - return "false"
        }
      } break;
      ...
```

Final print:
```text
java Machine within.out
1 2 0 1 2 0 40 44 0 0 42 40 44 0 
```

## Opgave 4 (20%) Icon
### Question 1:
"Skriv et Icon udtryk, som udskriver værdierne `1 2 3 4 5 6 7 8 9 10` på skærmen:"

#### Svar:
```fsharp
Every(Write(FromTo(1,10)))
```

### Question 2:
"_Skriv et Icon udtryk, som udskriver 10–tals tabellen, som en lang linie af tal._"

```fsharp
1 2 3 4 5 6 7 8 9 10 2 4 6 8 10 12 14 16 18 20 3 6 9 12 15 18 21 24 27
30 4 8 12 16 20 24 28 32 36 40 5 10 15 20 25 30 35 40 45 50 6 12 18 24
30 36 42 48 54 60 7 14 21 28 35 42 49 56 63 70 8 16 24 32 40 48 56 64
72 80 9 18 27 36 45 54 63 72 81 90 10 20 30 40 50 60 70 80 90 100
val it : value = Int 0
```

#### Svar:
```fsharp
let ttt = Every(Write(Prim("*", FromTo(1,10), FromTo(1,10))))
```

### Question 3:
""


### Question 4:
"_Udvid implementationen af `Icon` med en ny generator `Random(min,max,num)`, som genererer num
tilfældige værdier i intervallet `min` og `max`, begge inklusive. Det antages, at `min ≤ max og num ≥ 0`.
Generatoren `Random` fejler med det samme, hvis `min` `>` `max` eller `num` `≤` `0`._"

```fsharp
type expr = 
  ...
  | Random of int * int * int
  ...

| Random (min, max, num) ->        
    let rec loop tN =
      let random = System.Random()
      let randomNext = random.Next(min,max+1)
      
      if tN > 0 then 
          cont (Int randomNext) (fun () -> loop (tN - 1))
      else 
          econt ()
    loop num
```