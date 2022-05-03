#ifndef PARSE_AUX_H
#define PARSE_AUX_H

// debug enabled
#ifdef YYDEBUG
yydebug = 1;
#endif

enum
{
   INT_TYPE = 10,
   BOOL_TYPE,
   IDENT_TYPE,
   PROG,
   DECL,
   STMTSEQ,
   STMT,
   ASSIGN,
   ASSIGN1,
   ASSIGN2,
   EXPR,
   EXPR1,
   EXPR2,
   SIMPEXPR,
   SIMPEXPR1,
   SIMPEXPR2,
   TERM,
   TERM1,
   TERM2,
   FACTOR,
   IFSTMT,
   ELSECLAUSE,
   WHILESTMT,
   WRITE_INT,
};

//----------------------------------------------------------------
// struct declarations

struct Declarations_s
{
   char *ident;
   int type;
   struct Declarations_s *decl;
   int t;
   int line;
};
typedef struct Declarations_s Declarations;

struct Statement_s
{
   union StatementUnion *stmt;
   int t;
   int next;
   int line;
};
typedef struct Statement_s Statement;

struct StatementSequence_s
{
   Statement *stmt;
   struct StatementSequence_s *stmtSeq;
   int t;
   int line;
};
typedef struct StatementSequence_s StatementSequence;

struct Program_s
{
   Declarations *decl;
   StatementSequence *stmtSeq;
   int t;
   int line;
};
typedef struct Program_s Program;

struct Factor_s
{
   union FactorUnion *factor;
   int t;
   int next;
   unsigned int id;
   int line;
};
typedef struct Factor_s Factor;

struct Term1_s
{
   Factor *factor1;
   char *op2;
   Factor *factor2;
   int t;
   unsigned int id;
   int line;
};
typedef struct Term1_s Term1;

struct Term2_s
{
   Factor *factor;
   int t;
   unsigned int id;
   int line;
};
typedef struct Term2_s Term2;

struct Term_s
{
   union TermUnion *term;
   int t;
   int next;
   unsigned int id;
   int line;
};
typedef struct Term_s Term;

struct SimpleExpression1_s
{
   Term *term1;
   char *op3;
   Term *term2;
   int t;
   unsigned int id;
   int line;
};
typedef struct SimpleExpression1_s SimpleExpression1;

struct SimpleExpression2_s
{
   Term *term;
   int t;
   unsigned int id;
   int line;
};
typedef struct SimpleExpression2_s SimpleExpression2;

struct SimpleExpression_s
{
   union SimpleExpressionUnion *simpExpr;
   int t;
   int next;
   unsigned int id;
   int line;
};
typedef struct SimpleExpression_s SimpleExpression;

struct Expression1_s
{
   SimpleExpression *simpExpr;
   int t;
   unsigned int id;
   int line;
};
typedef struct Expression1_s Expression1;

struct Expression2_s
{
   SimpleExpression *simpExpr1;
   char *op4;
   SimpleExpression *simpExpr2;
   int t;
   unsigned int id;
   int line;
};
typedef struct Expression2_s Expression2;

struct Expression_s
{
   union ExpressionUnion *expr;
   int t;
   int next;
   unsigned int id;
   int line;
};
typedef struct Expression_s Expression;

struct Assignment1_s
{
   char *ident;
   Expression *expr;
   int t;
   int line;
};
typedef struct Assignment1_s Assignment1;

struct Assignment2_s
{
   char *ident;
   int t;
   int line;
};
typedef struct Assignment2_s Assignment2;

struct Assignment_s
{
   union AssignmentUnion *assign;
   int t;
   int next;
   int line;
};
typedef struct Assignment_s Assignment;

struct ElseClause_s
{
   StatementSequence *stmtSeq;
   int t;
   int line;
};
typedef struct ElseClause_s ElseClause;

struct IfStatement_s
{
   Expression *expr;
   StatementSequence *stmtSeq;
   ElseClause *elseClause;
   int t;
   int line;
};
typedef struct IfStatement_s IfStatement;

struct WhileStatement_s
{
   Expression *expr;
   StatementSequence *stmtSeq;
   int t;
   int line;
};
typedef struct WhileStatement_s WhileStatement;

struct WriteInt_s
{
   Expression *expr;
   int t;
   int line;
};
typedef struct WriteInt_s WriteInt;

//----------------------------------------------------------------
// union declarations

typedef union StatementUnion
{
   Assignment *assign;
   IfStatement *ifStmt;
   WhileStatement *whileStmt;
   WriteInt *writeInt;
} StatementU;
typedef union AssignmentUnion
{
   Assignment1 *assign1;
   Assignment2 *assign2;
} AssignmentU;
typedef union ExpressionUnion
{
   Expression1 *expr1;
   Expression2 *expr2;
} ExpressionU;
typedef union SimpleExpressionUnion
{
   SimpleExpression1 *simpExpr1;
   SimpleExpression2 *simpExpr2;
} SimpleExpressionU;
typedef union TermUnion
{
   Term1 *term1;
   Term2 *term2;
} TermU;
typedef union FactorUnion
{
   char *ident;
   char *num;
   char *boolean;
   Expression *expr;
} FactorU;

#endif