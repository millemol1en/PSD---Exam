### Instructions:

1. **Compiling a file:**
   -  `dotnet run ./Examples/ex02.sml`
   - Should result in the message:

    "Compiled to file `./Examples/ex02.out`"

2. **Executing the compiled file**
   - `./MsmlVM/src/msmlmachine ./Examples/ex02.out`

3. **Using the MicroSML Compiler:**
   - The compiler takes 4 optional arguments:
     1. `-opt`
        - Will enable simple optimizations similar to micro-C.
     2. `-verbose`
        - Will output intermediate AST with type information and byte code instructions.
     3. `eval`:
        - Will execute the program with the interpreter as part of this compilation phase.
     4. `-debug`:
        - Will print various debug information.

4. **Using the Byte Code Machine:**
   - The byte code machine `msmlmachine` takes 2 optional arguments:
     1. `-trace`
        - Will output the stack content at each instruction execution.
     2. `-silent`
        - Will suppress output from the garbage collector.