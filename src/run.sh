# flex scan.l
# # gcc -o lexical_analysis lex.yy.c -lfl -L"/opt/homebrew/opt/flex/lib"
# gcc -o lexical_analysis lex.yy.c -lfl -L"/usr/lib64"
# cat sample_input.txt | ./lexical_analysis·
# rm lex.yy.c

./compiler < input.txt > out.c 2> err.log