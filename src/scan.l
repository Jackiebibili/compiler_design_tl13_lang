%option noyywrap
%option nounput
%option noinput

%{
#include <stdlib.h>
#include "parse.aux.h"
#include "y.tab.h"

int line_number = 1;
%}

%%
[\n]              { line_number++; }
[ \t\r]+          { }
%.*               { /* in-line comment */ }
([1-9][0-9]*)|0   {
                     yylval.sval = malloc(strlen(yytext));
                     strncpy(yylval.sval, yytext, strlen(yytext));
                     return(NUM); 
                  }
"false"|"true"    {
                     yylval.sval = malloc(strlen(yytext));
                     strncpy(yylval.sval, yytext, strlen(yytext));
                     return(BOOLLIT); 
                  }
[A-Z][A-Z0-9]*    { 
                     yylval.sval = malloc(strlen(yytext));
                     strncpy(yylval.sval, yytext, strlen(yytext));
                     return(IDENT); 
                  }
"("               { return(LP); }
")"               { return(RP); }
":="              { return(ASGN); }
";"               { return(SC); }
"*"|"div"|"mod"   { 
                     yylval.sval = malloc(strlen(yytext));
                     strncpy(yylval.sval, yytext, strlen(yytext));
                     return(OP2); 
                  }
"+"|"-"           { 
                     yylval.sval = malloc(strlen(yytext));
                     strncpy(yylval.sval, yytext, strlen(yytext));
                     return(OP3); 
                  }
"="|"!="|"<"|">"|"<="|">=" { 
                     yylval.sval = malloc(strlen(yytext));
                     strncpy(yylval.sval, yytext, strlen(yytext));
                     return(OP4); 
                  }
"if"              { return(IF); }
"then"            { return(THEN); }
"else"            { return(ELSE); }
"begin"           { return(BEGON); }
"end"             { return(END); }
"while"           { return(WHILE); }
"do"              { return(DO); }
"program"         { return(PROGRAM); }
"var"             { return(VAR); }
"as"              { return(AS); }
"int"             { return(INT); }
"bool"            { return(BOOL); }
"writeInt"        { return(WRITEINT); }
"readInt"         { return(READINT); }
%%
