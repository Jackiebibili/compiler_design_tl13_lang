%{
#include <stdio.h>
#include <stdlib.h>
#include "parse.aux.h"
int yylex(void);
int yyerror(char *);

extern int line_number;
void generate_code(Program *program);
%}

%union
{
   char *sval;
   Declarations *decls;
   int type;
   StatementSequence *stmtSeq;
   Statement *stmt;
   Assignment *assign;
   IfStatement *ifStmt;
   ElseClause *elseClause;
   WhileStatement *whileStmt;
   WriteInt *writeInt;
   Expression *expr;
   SimpleExpression *simpExpr;
   Term *term;
   Factor *factor;
};
%token PROGRAM
%token BEGON
%token END
%token VAR
%token AS
%token SC
%token INT
%token BOOL
%token ASGN
%token READINT
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token WRITEINT
%token <sval> OP4
%token <sval> OP3
%token <sval> OP2
%token <sval> NUM
%token <sval> BOOLLIT
%token <sval> IDENT
%token LP
%token RP

%type <decls> Declarations
%type <type> Type
%type <stmtSeq> StatementSequence
%type <stmt> Statement
%type <assign> Assignment
%type <ifStmt> IfStatement
%type <elseClause> ElseClause
%type <whileStmt> WhileStatement
%type <writeInt> WriteInt
%type <expr> Expression
%type <simpExpr> SimpleExpression
%type <term> Term
%type <factor> Factor

%start Program

%%
Program:
      PROGRAM Declarations BEGON StatementSequence END { 
            Program* prog = (Program*) malloc(sizeof(Program));
            prog->decl = $2;
            prog->stmtSeq = $4;
            prog->t = PROG;
            prog->line = line_number;
            printf("// Finish constructing the parse tree.\n");
            printf("// Start generating the code.\n");
            generate_code(prog);
      }
      ;
   
Declarations:
      /* empty */ { $$ = (Declarations*) NULL; }
      | VAR IDENT AS Type SC Declarations { Declarations* declPtr = (Declarations*) malloc(sizeof(Declarations)); 
                                            declPtr->ident = $2;
                                            declPtr->type = $4;
                                            declPtr->decl = (struct Declarations_s*)$6;
                                            declPtr->t = DECL;
                                            declPtr->line = line_number;
                                            $$ = declPtr; 
                                            }
      ;
   
Type:
      INT { $$ = INT_TYPE; }
      | BOOL { $$ = BOOL_TYPE; }
      ;

StatementSequence:
      /* empty */ { $$ = (StatementSequence*) NULL; }
      | Statement SC StatementSequence { StatementSequence* stmtSeqPtr = (StatementSequence*) malloc(sizeof(StatementSequence)); 
                                         stmtSeqPtr->stmt = $1;
                                         stmtSeqPtr->stmtSeq = (struct StatementSequence_s*)$3;
                                         stmtSeqPtr->t = STMTSEQ;
                                         stmtSeqPtr->line = line_number;
                                         $$ = stmtSeqPtr; }
      ;

Statement:
      Assignment { Statement* stmtPtr = (Statement*) malloc(sizeof(Statement));
                   StatementU* stmtUPtr = (StatementU*) malloc(sizeof(StatementU));
                   stmtUPtr->assign = $1;
                   stmtPtr->stmt = stmtUPtr; 
                   stmtPtr->t = STMT;
                   stmtPtr->next = ASSIGN;
                   stmtPtr->line = line_number;
                   $$ = stmtPtr; }
      | IfStatement { Statement* stmtPtr = (Statement*) malloc(sizeof(Statement)); 
                   StatementU* stmtUPtr = (StatementU*) malloc(sizeof(StatementU));
                   stmtUPtr->ifStmt = $1;
                   stmtPtr->stmt = stmtUPtr;
                   stmtPtr->t = STMT;
                   stmtPtr->next = IFSTMT;
                   stmtPtr->line = line_number;
                   $$ = stmtPtr; }
      | WhileStatement { Statement* stmtPtr = (Statement*) malloc(sizeof(Statement)); 
                   StatementU* stmtUPtr = (StatementU*) malloc(sizeof(StatementU));
                   stmtUPtr->whileStmt = $1;
                   stmtPtr->stmt = stmtUPtr;
                   stmtPtr->t = STMT;
                   stmtPtr->next = WHILESTMT;
                   stmtPtr->line = line_number;
                   $$ = stmtPtr; }
      | WriteInt { Statement* stmtPtr = (Statement*) malloc(sizeof(Statement)); 
                   StatementU* stmtUPtr = (StatementU*) malloc(sizeof(StatementU));
                   stmtUPtr->writeInt = $1;
                   stmtPtr->stmt = stmtUPtr;
                   stmtPtr->t = STMT;
                   stmtPtr->next = WRITE_INT;
                   stmtPtr->line = line_number;
                   $$ = stmtPtr; }
      ;

Assignment:
      IDENT ASGN Expression /* assignment1 */ {
            Assignment* assignPtr = (Assignment*) malloc(sizeof(Assignment));
            AssignmentU* assignUPtr = (AssignmentU*) malloc(sizeof(AssignmentU));
            Assignment1* assignPtr1 = (Assignment1*) malloc(sizeof(Assignment1));
            assignPtr1->ident = $1;
            assignPtr1->expr = $3;
            assignPtr1->t = ASSIGN1;
            assignPtr1->line = line_number;
            assignUPtr->assign1 = assignPtr1;
            assignPtr->assign = assignUPtr;
            assignPtr->t = ASSIGN;
            assignPtr->next = ASSIGN1;
            assignPtr->line = line_number;
            $$ = assignPtr; }
      | IDENT ASGN READINT /* assignment2 */ {
            Assignment* assignPtr = (Assignment*) malloc(sizeof(Assignment));
            AssignmentU* assignUPtr = (AssignmentU*) malloc(sizeof(AssignmentU));
            Assignment2* assignPtr2 = (Assignment2*) malloc(sizeof(Assignment2));
            assignPtr2->ident = $1;
            assignPtr2->t = ASSIGN2;
            assignPtr2->line = line_number;
            assignUPtr->assign2 = assignPtr2;
            assignPtr->assign = assignUPtr;
            assignPtr->t = ASSIGN;
            assignPtr->next = ASSIGN2;
            assignPtr->line = line_number;
            $$ = assignPtr; }
      ;

IfStatement:
      IF Expression THEN StatementSequence ElseClause END {
            IfStatement* ifStmt = (IfStatement*) malloc(sizeof(IfStatement));
            ifStmt->expr = $2;
            ifStmt->stmtSeq = $4;
            ifStmt->elseClause = $5;
            ifStmt->t = IFSTMT;
            ifStmt->line = line_number;
            $$ = ifStmt; }
      ;

ElseClause:
      /* empty */ { $$ = (ElseClause*) NULL; }
      | ELSE StatementSequence {
            ElseClause* elseClause = (ElseClause*) malloc(sizeof(ElseClause));
            elseClause->stmtSeq = $2;
            elseClause->t = ELSECLAUSE;
            elseClause->line = line_number;
            $$ = elseClause; }
      ;

WhileStatement:
      WHILE Expression DO StatementSequence END {
            WhileStatement* whileStmt = (WhileStatement*) malloc(sizeof(WhileStatement));
            whileStmt->expr = $2;
            whileStmt->stmtSeq = $4;
            whileStmt->t = WHILESTMT;
            whileStmt->line = line_number;
            $$ = whileStmt;
      }
      ;

WriteInt:
      WRITEINT Expression {
            WriteInt* writeIntPtr = (WriteInt*) malloc(sizeof(WriteInt));
            writeIntPtr->expr = $2;
            writeIntPtr->t = WRITE_INT;
            writeIntPtr->line = line_number;
            $$ = writeIntPtr;
      }
      ;

Expression:
      SimpleExpression {
            Expression* exprPtr = (Expression*) malloc(sizeof(Expression));
            ExpressionU* exprUPtr = (ExpressionU*) malloc(sizeof(ExpressionU));
            Expression1* exprPtr1 = (Expression1*) malloc(sizeof(Expression1));
            exprPtr1->simpExpr = $1;
            exprPtr1->t = EXPR1;
            exprPtr1->line = line_number;
            exprUPtr->expr1 = exprPtr1;
            exprPtr->expr = exprUPtr;
            exprPtr->t = EXPR;
            exprPtr->next = EXPR1;
            exprPtr->id = ($1)->id;
            exprPtr->line = line_number;
            $$ = exprPtr;
      }
      | SimpleExpression OP4 SimpleExpression {
            Expression* exprPtr = (Expression*) malloc(sizeof(Expression));
            ExpressionU* exprUPtr = (ExpressionU*) malloc(sizeof(ExpressionU));
            Expression2* exprPtr2 = (Expression2*) malloc(sizeof(Expression2));
            exprPtr2->simpExpr1 = $1;
            exprPtr2->op4 = $2;
            exprPtr2->simpExpr2 = $3;
            exprPtr2->t = EXPR2;
            exprPtr2->line = line_number;
            exprUPtr->expr2 = exprPtr2;
            exprPtr->expr = exprUPtr;
            exprPtr->t = EXPR;
            exprPtr->next = EXPR2;
            exprPtr->id = BOOL_TYPE;
            exprPtr->line = line_number;
            $$ = exprPtr;
      }
      ;

SimpleExpression:
      Term OP3 Term {
            SimpleExpression* simpExprPtr = (SimpleExpression*) malloc(sizeof(SimpleExpression));
            SimpleExpressionU* simpExprUPtr = (SimpleExpressionU*) malloc(sizeof(SimpleExpressionU));
            SimpleExpression1* simpExprPtr1 = (SimpleExpression1*) malloc(sizeof(SimpleExpression1));
            simpExprPtr1->term1 = $1;
            simpExprPtr1->op3 = $2;
            simpExprPtr1->term2 = $3;
            simpExprPtr1->t = SIMPEXPR1;
            simpExprPtr1->line = line_number;
            simpExprUPtr->simpExpr1 = simpExprPtr1;
            simpExprPtr->simpExpr = simpExprUPtr;
            simpExprPtr->t = SIMPEXPR;
            simpExprPtr->next = SIMPEXPR1;
            simpExprPtr->id = INT_TYPE;
            simpExprPtr->line = line_number;
            $$ = simpExprPtr;
      }
      | Term {
            SimpleExpression* simpExprPtr = (SimpleExpression*) malloc(sizeof(SimpleExpression));
            SimpleExpressionU* simpExprUPtr = (SimpleExpressionU*) malloc(sizeof(SimpleExpressionU));
            SimpleExpression2* simpExprPtr2 = (SimpleExpression2*) malloc(sizeof(SimpleExpression2));
            simpExprPtr2->term = $1;
            simpExprPtr2->t = SIMPEXPR2;
            simpExprPtr2->line = line_number;
            simpExprUPtr->simpExpr2 = simpExprPtr2;
            simpExprPtr->simpExpr = simpExprUPtr;
            simpExprPtr->t = SIMPEXPR;
            simpExprPtr->next = SIMPEXPR2;
            simpExprPtr->id = ($1)->id;
            simpExprPtr->line = line_number;
            $$ = simpExprPtr;
      }
      ;

Term:
      Factor OP2 Factor {
            Term* termPtr = (Term*) malloc(sizeof(Term));
            TermU* termUPtr = (TermU*) malloc(sizeof(TermU));
            Term1* termPtr1 = (Term1*) malloc(sizeof(Term1));
            termPtr1->factor1 = $1;
            termPtr1->op2 = $2;
            termPtr1->factor2 = $3;
            termPtr1->t = TERM1;
            termPtr1->line = line_number;
            termUPtr->term1 = termPtr1;
            termPtr->term = termUPtr;
            termPtr->t = TERM;
            termPtr->next = TERM1;
            termPtr->id = INT_TYPE;
            termPtr->line = line_number;
            $$ = termPtr;
      }
      | Factor {
            Term* termPtr = (Term*) malloc(sizeof(Term));
            TermU* termUPtr = (TermU*) malloc(sizeof(TermU));
            Term2* termPtr2 = (Term2*) malloc(sizeof(Term2));
            termPtr2->factor = $1;
            termPtr2->t = TERM2;
            termPtr2->line = line_number;
            termUPtr->term2 = termPtr2;
            termPtr->term = termUPtr;
            termPtr->t = TERM;
            termPtr->next = TERM2;
            termPtr->id = ($1)->id;
            termPtr->line = line_number;
            $$ = termPtr;
      }
      ;

Factor:
      IDENT {
            Factor* factorPtr = (Factor*) malloc(sizeof(Factor));
            FactorU* factorUPtr = (FactorU*) malloc(sizeof(FactorU));
            factorUPtr->ident = $1;
            factorPtr->factor = factorUPtr;
            factorPtr->t = FACTOR;
            factorPtr->next = IDENT_TYPE;
            factorPtr->id = IDENT_TYPE;
            factorPtr->line = line_number;
            $$ = factorPtr;      }
      | NUM {
            Factor* factorPtr = (Factor*) malloc(sizeof(Factor));
            FactorU* factorUPtr = (FactorU*) malloc(sizeof(FactorU));
            factorUPtr->num = $1;
            factorPtr->factor = factorUPtr;
            factorPtr->t = FACTOR;
            factorPtr->next = INT_TYPE;
            factorPtr->id = INT_TYPE;
            factorPtr->line = line_number;
            $$ = factorPtr;      }
      | BOOLLIT {
            Factor* factorPtr = (Factor*) malloc(sizeof(Factor));
            FactorU* factorUPtr = (FactorU*) malloc(sizeof(FactorU));
            factorUPtr->boolean = $1;
            factorPtr->factor = factorUPtr;
            factorPtr->t = FACTOR;
            factorPtr->next = BOOL_TYPE;
            factorPtr->id = BOOL_TYPE;
            factorPtr->line = line_number;
            $$ = factorPtr;      }
      | LP Expression RP {
            Factor* factorPtr = (Factor*) malloc(sizeof(Factor));
            FactorU* factorUPtr = (FactorU*) malloc(sizeof(FactorU));
            factorUPtr->expr = $2;
            factorPtr->factor = factorUPtr;
            factorPtr->t = FACTOR;
            factorPtr->next = EXPR;
            factorPtr->id = ($2)->id;
            factorPtr->line = line_number;
            $$ = factorPtr;      }
      ;

%%

int yyerror(char* s) {
   printf("yyerror: %s\n", s);
   return 1;
}

int main(void) {
   yyparse();
}