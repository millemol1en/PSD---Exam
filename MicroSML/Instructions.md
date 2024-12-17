### Instructions:
1. **Generating Lexer & Parser:**
   - First generate the lexer `FunLex.fsl` and parser `FunPar.fsy` with the provided commands.

2. **Generating an `.out` file**
   - To compile a program run: `dotnet run test01.sml`
   - This will give you `test01.out` which you run using the commands in for the byte code machine.

3. **Flags when generating an `.out` file:**
   - The compiler takes 4 optional arguments:
     1. `-opt`
        - Will enable simple optimizations similar to micro-C.
        - `dotnet run -opt test01.sml`
     2. `-verbose`
        - Will output intermediate AST with type information and byte code instructions.
     3. `-eval`:
        - Will execute the program with the interpreter as part of this compilation phase.
        - `dotnet run -eval test01.sml`
     4. `-debug`:
        - Will print various debug information.

4. **Using the Byte Code Machine:**
   - Building and running the machine requires the following command:
     - Building: `gcc -Wall msmlmachine.c -o msmlmachine`
     - Running: `./MsmlVM/src/msmlmachine test.out`
   - Additionally, the byte code machine `msmlmachine` takes 2 optional arguments:
     1. `-trace`
        - Will output the stack content at each instruction execution.
     2. `-silent`
        - Will suppress output from the garbage collector.