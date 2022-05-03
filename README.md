# Compiler Design Project
### How to Compile the Source Code
- Run the Makefile in the root directory and `./compiler` executable will be built.

### How to Compile the Sample Program
- Run `run.sh` or `./compiler < input.txt > out.c 2> err.log`. This will generate a C program named `out.c` and show any compilation errors in `err.log`.
- Run `gcc -o program out.c` to compiler the generated program.
- Test the generated program by running `./program`.