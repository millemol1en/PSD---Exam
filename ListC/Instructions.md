1. Commands in ListC:
    - Take note that all the commands for ListC are contained inside a file in the ListVM folders called `exe_script.sh`.
    - This shell file contains commands to compile and run the exam. BE AWARE! You MUST have the exam file in the ListC directory and not in any other sub-directories AND it must be called `exam.lc`.
    - Inside the folder ./ListVM/ListVM write `source exe_script.sh`.
2. Parsing the file:
   - To parse the file you have to first build the `listc.fsproj` file by writing the command: `dotnet build listc.fsproj`
   - Thereafter, we can parse and compile a file to byte code using the command: `dotnet run exam.lc` where `exam.lc` is the file which you wish to parse and compile. 