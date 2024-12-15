# BSWU BPRD 2018 Jan
## Opgave 1 (25 %): Icon
### Question 1:
#### Spørgsmål:
"_Omskriv eksemplet `iconEx1`, så værdierne `8 9 10` udskrives på skærmen_"

#### Svar:
```fsharp
let iconEx1_a = Every(Write(Prim("<",CstI 7,FromTo(1,10))))
let iconEx1_b = Every(Write(FromTo(8,10)))
```


### Question 2:
#### Spørgsmål:
"_Betragt nedenstående `Icon` udtryk_"

```fsharp
// Kode:
let iconEx2 = Every(Write(And(FromTo(1,4), And(Write (CstS "\n"),FromTo(1,4)))))

// Udskrivning:
> run iconEx2;;
1 2 3 4
1 2 3 4
1 2 3 4
1 2 3 4 val it : value = Int 0
```

"_Forklar hvorfor udtrykket giver ovenstående resultat. Derefter, skal du omskriv eksemplet `iconEx2`, så nedenstående udskrives på skærmen:_"

```fsharp
> run ...
1 2 3 4
2 4 6 8
3 6 9 12
4 8 12 16 val it : value = Int 0
```
#### Svar:
Explanation:
```text
Every(                 |  Will write for each situation 
Write(                 |        
And(                   |                
FromTo(1,4),           |        
And(                   |
Write                  |
(CstS "\n"),           |
FromTo(1,4)))))        |
```

Re-writing `iconEx2`:

```fsharp
let iconEx2 = Every(Write(Prim("*", FromTo(1,4), And(Write (CstS "\n"), FromTo(1, 4)))))
```

### Question 3:
#### Spørgsmål:
"_Udvid implementationen af `Icon` med en ny generator `Find(pat,str)`, som genererer en sekvens af
indekser i strengen `str`, hvori mønsteret `pat` findes som en delstreng. `Find` starter med at søge efter `pat` fra
starten af `str` og returnerer indekset for det første match den finder. Derefter starter den efter foregående
match og returnerer indekset på næste match indtil den fejler, når der ikke længere findes et match. Find
fejler, hvis pat eller str er de tomme strenge eller pat ikke findes som en delstreng i `str`. Første indeks i
strengen `str` er `0` (som i F#). Nedenstående illustrerer brugen af `Find`_."

**Hint**: "_.NET metoden `String.IndexOf` kan med fordel benyttes._"

```fsharp
let str = "Hi there - if there are anyone"
> val str : string = "Hi there - if there are anyone"
run (Every(Write(Find("there",str))))
> 3 14 val it : value = Int 0
```

#### Svar:
```fsharp
| Find (pat : string, str : string) ->        
    let rec aux (accIdx : int) =
        let idx = str.IndexOf(pat, accIdx)
                    
        if (idx > 0) then
            cont (Int idx) (fun () -> aux (idx + 1))
        else
            econt ()
            
    aux 0
```

### Question 4:
#### Spørgsmål:
"_Lav 3 relevante testeksempler af `Find` ovenfor, og vis resultatet af at evaluere dem._"

#### Svar:


### Question 5:
#### Spørgsmål:
"_Skriv, og evaluer, et `Icon` udtryk, som udskriver indekser på alle tegn "`e`", der findes i strengen `"Hi there - if there are anyone"`_"

```fsharp
> run ...
5 7 16 18 22 29 val it : value = Int 0
```

"_Derefter, omskriv dit svar til ovenstående, således at det kun er alle indekser på "`e`", som er større end `10`, der udskrives_"

```fsharp
> run ...
16 18 22 29 val it : value = Int 0
```

#### Svar:
```fsharp
// Mikkel solution :)
> run (Every(Write(Find("e",str))));;    
5 7 16 18 22 29 val it: value = Int 0


// Mikkel solution :)
> run Every(Write(Prim("<",CstI 10,Find("e",str))));;
16 18 22 29 val it: value = Int 0
```



## Opgave 2 (15 %): Parsing Records i micro-ML
### Opgave Beskrivelse:
"_Nedenfor ses 5 eksempler, som viser hvorledes records skabes og hvorledes felter i records tilgås._"

```fsharp
let x = { } in x end                                        (* ex1 *)
let x = {field1 = 32} in x.field1 end                       (* ex2 *)
let x = {field1 = 32; field2 = 33} in x end                 (* ex3 *)
let x = {field1 = 32; field2 = 33} in x.field1 end          (* ex4 *)
let x = {field1 = 32; field2 = 33} in x.field1+x.field2 end (* ex5 *)
```

"_Opgaven er at udvide lexer (`FunLex.fsl`) og parser (`FunPar.fsy`) således at records svarende til eksemplerne ovenfor er understøttet i micro–ML. Grammatikken for records er vist nedenfor._"

### Question 1:
#### Spørgsmål:
"_Filen `Absyn.fs` indeholder den abstrakte syntaks for micro–ML. Du skal udvide typen expr, med følgende to konstruktioner:_"

```fsharp
type expr =
    ...
    | Field of expr * string
    | Record of (string * expr) list
    ...
```

"_Adgang til felter udtrykkes med `Field(e,s)`, hvor e repræsenterer udtrykket for en record og s navnet på
et felt i denne record. En record udtrykkes med `Record([(s1, e1);. . .;(sn, en)])`, hvor si er feltnavne
og ei er udtryk, `1 ≤ i ≤ n`. Eksempelvis parses eksempel ex2 ovenfor til den abstrakte syntaks nedenfor:_"

```fsharp
Let ("x",Record [("field1", CstI 32)],Field (Var "x","field1"))
```

"_Angiv den abstrakte syntaks, som værdier af typen expr, for de resterende eksempler, `ex1`, `ex3`, `ex4` og `ex5`, ovenfor._"

#### Svar:
This is provided in question 2 as it was NOT done the intended way - by hand. Fuck that. 


### Question 2:
#### Spørgsmål:
"_Udvid lexer (FunLex.fsl) og parser (FunPar.fsy), således at records er understøttet med grammatikken vist ovenfor:_"

**Hint**: "_Du har brug for nogle nye tokens, fx. `.` (punktum)._"

**Hint**: "_Du har brug for at parse `0`, `1` eller flere felter og udtryk, `s = e`, adskilt af semikolon. Parseren for
micro–C har eksempler på at parse andre typer af sekvenser._"

"_Vis resultatet af at parse de 5 eksempler ovenfor og dokumenter, at din parser giver de forventede abstrakte
syntakstræer._"

#### Svar:
Changes made to file `FunLex.fsl`
```fsharp
rule Token = parse
  ...
  | '{'             { LCPAR }
  | '}'             { RCPAR }
  | '.'             { DOT }
  | ';'             { SEMI }
  ...
```


Changes made to file `FunPar.fsy`
```fsharp
%token DOT LCPAR RCPAR SEMI 
...
%left PLUS MINUS DOT
...
Expr:
    AtExpr                              { $1                     }
  ...
  | Expr DOT NAME                       { Field($1, $3)          }   // #1
  | LCPAR Records RCPAR                 { Record($2)             }
;

Records:                                                            // #2
    /* empty */                          { []                    }
   | Fields                              { [$1]                  }
   | Fields SEMI Records                 { $1 :: $3              }
;

Fields:
   | NAME EQ Expr                       { ($1, $3)               } 
;
```

**Note #1** I initially thought it had to be `NAME DOT NAME` but after compilation failed, it turned out it had to be `Field of expr * string`. This is what was provided. Always, always, always use the basics, the provided hints and keep the abstract syntax provided close.

**Note #2** This implementation is, as suggested, inspired from the example of `Paramdecs` from the microC `CPar.fsy`

Output of the 5 examples:
```fsharp
// ex1
> fromString @"let x = { } in x end ";;
val it: expr = Let ("x", Record [], Var "x")

// ex2
> fromString @"let x = {field1 = 32} in x.field1 end";;
val it: expr = Let ("x", Record [("field1", CstI 32)], Field (Var "x", "field1"))

// ex3
> fromString @"let x = {field1 = 32; field2 = 33} in x end";;
val it: expr = Let ("x", Record [("field1", CstI 32); ("field2", CstI 33)], Var "x")


// ex4
> fromString @"let x = {field1 = 32; field2 = 33} in x.field1 end";;
val it: expr = Let ("x", Record [("field1", CstI 32); ("field2", CstI 33)], Field (Var "x", "field1"))


// ex5
> fromString @"let x = {field1 = 32; field2 = 33} in x.field1+x.field2 end";;
val it: expr = Let ("x", Record [("field1", CstI 32); ("field2", CstI 33)], Prim ("+", Field (Var "x", "field1"), Field (Var "x", "field2")))
```

## Opgave 3 (25 %): Evaluering af Records i micro–ML:
### Question 1:
#### Spørgsmål:
"_Tegn et evaluerings træ, med reglerne i figur 4.3 på side 65 i PLC, samt `e10` og `e11` nedenfor, for udtrykket
nedenfor (ex2 fra opgave 2)._"

![alt text](./Pictures/Tree%20Rules%201.png)

![alt text](./Pictures/Tree%20Rules%202.png)

"_`ex2` fra opgave 2 ses sådan her ud_"

![alt text](./Pictures/Tree%20Start.png)

#### Svar:
... ikke gjort ... 

### Question 2:
#### Spørgsmål:
"_For at kunne repræsentere records, som værdier, udvider vi typen value i filen `HigherFun.fs` med en
ny konstruktion `RecordV`:_"

```fsharp
type value =
    | Int of int
    | RecordV of (string * value) list
    | Closure of string * string * expr * value env
```

"_Konstruktionen `RecordV[(s1, v1);. . .;(sn,vn)]` repræsenterer en record som værdi. Hvert felt si er parret med dets værdi vi. Eksempelvis giver resultatet af at evaluere udtrykket `Record[("field1",CstI 32);("field2",CstI 33)]` værdien `RecordV[("field1",Int 32);("field2",Int 33)]`. Udvid funktionen eval i `HigherFun.fs`, med evaluering af records svarende til `e10` og `e11` ovenfor. Reglerne beskriver ikke hvad der skal ske, hvis der er sammenfald i feltnavne. Du skal beslutte og dokumentere, hvad du vil gøre i dette tilfælde. Eksempelvis vil evaluering af eksempel ex2 fra opgave 3 give følgende:_"

#### Svar:
Changes to `HigherFun.fs`:
```fsharp
let rec eval (e : expr) (env : value env) : value =
    match e with
    ...
    | Field (varName, varValue) ->
      match (eval varName env) with
      | RecordV record ->
        match (List.tryFind (fun (name, _) -> name = varValue ) record) with
        | Some target ->
          (snd target)
        | None -> failwith "Error! Could not find the associated value for the field"
      | _ -> failwith "Error! A record was NOT found"

    | Record records ->
      let rec aux (rLst : (string * expr) list) (acc : (string * value) list) =
        match rLst with
        | []    -> RecordV (acc)
        | x::xs ->
          let v = (eval (snd x) env)
          aux xs (((fst x), v) :: acc)
      aux records []
```

Running the provided examples:
```fsharp
> open Absyn;;
> ParseAndRunHigher.run (Let ("x",Record [("field1", CstI 32)],
Field (Var "x","field1")));;
val it : HigherFun.value = Int 32

> fromString @"let x = {field1 = 32; field2 = 33} in x.field1+x.field2 end";;
val it: Absyn.expr = Let ("x", Record [("field1", CstI 32); ("field2", CstI 33)], Prim ("+", Field (Var "x", "field1"), Field (Var "x", "field2")))
> run it;;
val it: HigherFun.value = Int 65
```

### Question 3:
#### Spørgsmål:
"_Lav mindst 5 eksempler med records, hvor du bl.a. tester din beslutning om sammenfaldne feltnavne. Vis
resultatet af at køre eksemplerne og forklar hvorvidt resultatet er som forventet. Du vælger selv om du vil
lave eksemplerne i konkret syntaks og anvende parseren i opgave 2 eller om du laver eksemplerne i abstrakt
syntaks._"

#### Svar:
```fsharp
let exam1 = @"let x = {foo = 1; baz = 2; bar = 3} in x.foo * x.baz * x.bar end";;
let exam2 = @"let x = {field1 = 22} in let y = {field1 = 10} in x.field1 + y.field1 end end";;
... just imagine 3 more examples ...
```


## Opgave 4 (20 %): Breakpoints i micro–C
### Opgave Beskrivelse:
"_I denne opgave tilføjer vi et nyt statement `break e`, hvis formål er, at man kan indsætte breakpoints i koden
og dermed kontrollere, hvornår man vil have vist stakken. For hvert breakpoint angiver man et udtryk `e`, som skal
evaluere til enten sand eller falsk. Hvis sand skal bytekode maskinen vente på, at brugeren trykker på `ENTER`
tasten inden programafvikling fortsætter. For at simplificere opgaven, udskriver vi stakken, som den ser ud efter
evaluering af `e`, i samme format, som ved trace._"

"_Nedenfor ses to eksempler på anvendelsen af break for eksempel `ex1.c`. Til venstre ventes ikke på at bruger trykker på `ENTER` tasten efter hvert breakpoint._"

```c++
void main(int n) {
    while (n > 0)
    {
        print n;
        break true;
        n = n -1;
    }
    println;
}
```

### Question 1:
#### Spørgsmål:
"Du skal først udvide lexer og parser med support for `break e`, hvor `e` kan være et vilkårligt udtryk understøttet
af `micro–C`. Du kan antage, at `e` evaluerer til `0` (falsk) eller `1` (sand)."

#### Svar:
Changes made to file `CPar.fsy`
 - The `break` is added to statement as the provided example doesnt' place it within an expression but instead isolates it to its own line. 
```text
StmtM:  /* No unbalanced if-else */
  ...
  | BREAK Expr SEMI                     { Break($2)            }
;
```

Changes made to file `CLex.fsl`
```fsharp
let keyword s =
    match s with
    ...
    | "break"   -> BREAK
```


### Question 2:
#### Spørgsmål:
"_Du skal udvide bytekode maskinen, `Machine.java` eller `machine.c` efter eget valg, med to nye bytekode instruktioner `BREAK` og `WAITKEYPRESS`:_"

**Hint**: "_Du får brug for kode der kan afbryde programafvikling og afvente ENTER. For C kan du forsøge
med system("read"); for Mac og Linux og med system("pause"); på Windows. For Java kan
du forsøge med try {System.in.read();} catch (Exception e) {...}._"

#### Svar:
Changes made to file `Machine.java`
```fsharp

```

### Question 3:
#### Spørgsmål:
"_Nu skal du lave et oversætterskema for oversættelse af `break e` til bytekode. Du kan se eksempler på oversætter skemaer i PLC figur 8.5 på side 151, eller slide 25 fra lektion 7 den 11. oktober._"


#### Svar:


### Question 4:
#### Spørgsmål:
"Du skal udvide oversætteren således at der genereres kode for `break e`. Du skal bl.a. udvide den abstrakte
syntaks (`Absyn.fs`), tilføje bytekodeinstruktioner (`Machine.fs`), og generere kode. Det forventes at du
benytter `Contcomp.fs` _**og ikke**_ `Comp.fs`."

#### Svar:
Changes to file `Contcomp.fs`:
```fsharp
| Break (e) ->
    let (labNoKeyPress, C1) = addLabel C
    cExpr e varEnv funEnv (BREAK :: IFZERO labNoKeyPress :: WAITKEYPRESS :: C1) 
```

### Question 5:
#### Spørgsmål:
"_Du laver mindst et eksempel med `break e`, hvor e ikke er en simpel konstant, `true` eller `false`.
Eksempelvis kan man med et logisk udtryk såsom `(n%2 == 0)` styre at bruger kun skal trykke `ENTER`
når `n` er lige. Variablen `n` kunne fx. være en tæller i en løkke._"

#### Svar:
```fsharp
void main(int n) {
    while (n > 0) {
        print n;
        break (n%2 == 0);
        n = n - 1;
    }
    println;
}
```

## Opgave 5 (15 %): Arrays i micro–C
### Question 1:
#### Spørgsmål:
"_Tegn og beskriv indholdet af stakken når programafviklingen når til det sted, hvor kommentaren er indsat
i funktionen `printArray`. Du skal opdele stakken i aktiveringsposter (eng. stack frames) og angive
basepeger (eng. base pointer) og stakpeger (eng. stack pointer)._"

**Hint**: "_Du kan anvende den abstrakte maskines mulighed for at udskrive stakken under afvikling af programmet, f.eks. `java Machinetrace exam01.out` eller `./machine -trace exam01.out`. Du lavede noget lignende i opgave 8.1 i PLC._"

#### Svar:

| Addr | Value | Comment                   |
|------|-------|---------------------------|
| 0    | 4     | Return address            |
| 1    | -999  | old bp                    |
| 2    | 42    | a[0]                      |
| 3    | 42    |                           |
| 4    | 42    |                           |
| 5    | 42    |                           |
| 6    | 42    | a[4]                      |
| 7    | 2     | a, pointer to &a[0]       |
| 8    | 5     | i                         |
| 9    | 131   | return addr to printArray |
| 10   | 2     | old bp for main           |
| 11   | 2     | argument a[]              |
| 12   | 0     | i                         |


### Question 2:
#### Spørgsmål:
"_Nedenfor ses uddata fra at afvikle programmet exam01.c med den sidste statement i main (print 43;)
fjernet. Det antages at programmet er oversat med Contcomp.fs (og ikke Comp.fs)._"


#### Svar:
"_Årsagen til det fejlagtige resultat er at når `print 43;` er fjernet laves kaldet til printArray som et halekald og tabellen a fjernes fra stakken fordi aktiveringsposten for `main` fjernes. `Comp.fs` implementerer ikke halekald, hvorfor problemet ikke opstår der._"