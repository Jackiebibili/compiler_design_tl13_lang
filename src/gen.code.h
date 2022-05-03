#ifndef GEN_CODE_H
#define GEN_CODE_H

#include "parse.aux.h"
#include "uthash.h"

extern int line_number;

typedef union Node_union
{
   Program *prog;
   Declarations *decl;
   StatementSequence *stmtSeq;
   Statement *stmt;
   Assignment *assign;
   Assignment1 *assign1;
   Assignment2 *assign2;
   Expression *expr;
   Expression1 *expr1;
   Expression2 *expr2;
   SimpleExpression *simpExpr;
   SimpleExpression1 *simpExpr1;
   SimpleExpression2 *simpExpr2;
   Term *term;
   Term1 *term1;
   Term2 *term2;
   Factor *factor;
   IfStatement *ifStmt;
   ElseClause *elseClause;
   WhileStatement *whileStmt;
   WriteInt *writeInt;
} node_u;

typedef struct Node
{
   node_u n;
   int type;
} node;

struct table_value
{
   unsigned short isInitialized;
   unsigned int type;
};

struct entry
{
   const char *id;
   const struct table_value *value;
   UT_hash_handle hh;
};

void generate_code(Program *program);

void generate_code_helper(node n);

void free_table(struct entry *store);

struct entry *find_entry(struct entry *store, const char *key);

unsigned short add_entry(struct entry **store, const char *key, int var_type, unsigned short initialized);

unsigned short replace_entry(struct entry **store, const char *key, unsigned short initialized);

char *getDataTypeById(int id);

const char *PROGRAM_HEADER = "#include <stdlib.h>\n#include <stdio.h>\n#include <stdbool.h>\n\nint main() {\n";

const char *PROGRAM_FOOTER = "return 0;\n}\n";

char *getDeclaration(int type, char *ident);

char *getInput(char *ident);

const char *NEWLINE = "\n";
const char *SEMICOLON = ";";
const char *LEFTP = "(";
const char *RIGHTP = ")";
const char *PROGRAM_ASSIGNMENT = "=";
const char *PROGRAM_IF_PREFIX = "if(";
const char *PROGRAM_IF_MIDDLE = "){";
const char *PROGRAM_IF_SUFFIX = "}";
const char *PROGRAM_ELSE_PREFIX = "else {";
const char *PROGRAM_ELSE_SUFFIX = "}";
const char *PROGRAM_WHILE_PREFIX = "while(";
const char *PROGRAM_WHILE_MIDDLE = "){";
const char *PROGRAM_WHILE_SUFFIX = "}";
const char *PROGRAM_OUTPUT_PREFIX = "printf(\"%d\\n\", ";
const char *PROGRAM_OUTPUT_SUFFIX = ");";
const char *PROGRAM_MAX_INT = "2147483647";

int isIntOverflow(char *num);

char *getOP4(char *op4);

char *getOP3(char *op3);

char *getOP2(char *op2);

#endif
