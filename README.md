## Instant Compiler
Ins to jvm/llvm compiler. 
Ins (Instant) is a simple language consisted of expressions and assignments.

### Usage:
```
make                            # build
./insc_jvm program.ins          # Creates program.j and program.class files
./insc_llvm program.ins         # Creates program.ll and program.bc files
```

### Project structure:
* **lib**
* **src**
    * **InstantParser** - Parser module, generated with BNFC
    * **JVMCompiler.hs** - JVM Compiler module
    * **LLVMCompiler.hs** - LLVM Compiler module
    * **jvm.hs** - JVM Compiler main file
    * **llvm.hs** - LLVM Compiler main file
    * **Makefile**
* **Makefile**

## Dependencies
Compiler uses **BNFC** to generate Instant parser and **Jasmin** for converting **assembler-like java class** to **binary Java class files**
