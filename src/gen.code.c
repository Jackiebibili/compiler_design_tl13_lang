#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "gen.code.h"
#include "parse.aux.h"
#include "uthash.h"

struct entry *table = NULL;

void generate_code(Program *program)
{
   node prog = {.n.prog = program, .type = program->t};
   generate_code_helper(prog);
   free_table(table);
}

void generate_code_helper(node n)
{
   node node1, node2, node3;
   struct entry *pair;
   char *err = malloc(150);
   char *l1 = NULL;
   switch (n.type)
   {
   case PROG:
      // generate program prefix
      write(STDOUT_FILENO, PROGRAM_HEADER, strlen(PROGRAM_HEADER));
      node1.n.decl = n.n.prog->decl;
      node1.type = n.n.prog->decl->t;
      generate_code_helper(node1);
      node2.n.stmtSeq = n.n.prog->stmtSeq;
      if (node2.n.stmtSeq)
      {
         node2.type = n.n.prog->stmtSeq->t;
      }
      generate_code_helper(node2);
      // generate program suffix
      write(STDOUT_FILENO, PROGRAM_FOOTER, strlen(PROGRAM_FOOTER));

      break;
   case DECL:
      if (!n.n.decl)
      {
         break;
      }
      // handle declaration of variables
      if (add_entry(&table, n.n.decl->ident, n.n.decl->type, 0))
      {
         // redeclaration error
         sprintf(err, "Line %d - error: `%s` is redeclared\n", n.n.decl->line, n.n.decl->ident);
         write(STDERR_FILENO, err, strlen(err));
      }
      // generate variable declaration code
      l1 = getDeclaration(n.n.decl->type, n.n.decl->ident);
      write(STDOUT_FILENO, l1, strlen(l1));
      write(STDOUT_FILENO, SEMICOLON, strlen(SEMICOLON));
      write(STDOUT_FILENO, NEWLINE, strlen(NEWLINE));

      node1.n.decl = n.n.decl->decl;
      if (node1.n.decl)
      {
         node1.type = n.n.decl->decl->t;
      }
      generate_code_helper(node1);
      break;
   case STMTSEQ:
      if (!node1.n.stmtSeq)
      {
         break;
      }
      node1.n.stmt = n.n.stmtSeq->stmt;
      node1.type = n.n.stmtSeq->stmt->t;
      generate_code_helper(node1);

      node2.n.stmtSeq = n.n.stmtSeq->stmtSeq;
      if (node2.n.stmtSeq)
      {
         node2.type = n.n.stmtSeq->stmtSeq->t;
      }
      generate_code_helper(node2);
      break;
   case STMT:
      switch (n.n.stmt->next)
      {
      case ASSIGN:
         node1.n.assign = n.n.stmt->stmt->assign;
         node1.type = n.n.stmt->stmt->assign->t;
         generate_code_helper(node1);
         break;
      case IFSTMT:
         node1.n.ifStmt = n.n.stmt->stmt->ifStmt;
         node1.type = n.n.stmt->stmt->ifStmt->t;
         generate_code_helper(node1);
         break;
      case WHILESTMT:
         node1.n.whileStmt = n.n.stmt->stmt->whileStmt;
         node1.type = n.n.stmt->stmt->whileStmt->t;
         generate_code_helper(node1);
         break;
      case WRITE_INT:
         node1.n.writeInt = n.n.stmt->stmt->writeInt;
         node1.type = n.n.stmt->stmt->writeInt->t;
         generate_code_helper(node1);
         break;
      }
      break;
   case ASSIGN:
      switch (n.n.assign->next)
      {
      case ASSIGN1:
         node1.n.assign1 = n.n.assign->assign->assign1;
         node1.type = n.n.assign->assign->assign1->t;
         generate_code_helper(node1);
         break;
      case ASSIGN2:
         node1.n.assign2 = n.n.assign->assign->assign2;
         node1.type = n.n.assign->assign->assign2->t;
         generate_code_helper(node1);
         break;
      }
      write(STDOUT_FILENO, SEMICOLON, strlen(SEMICOLON));
      write(STDOUT_FILENO, NEWLINE, strlen(NEWLINE));
      break;
   case ASSIGN1:
      pair = find_entry(table, n.n.assign1->ident);

      write(STDOUT_FILENO, n.n.assign1->ident, strlen(n.n.assign1->ident));
      write(STDOUT_FILENO, PROGRAM_ASSIGNMENT, strlen(PROGRAM_ASSIGNMENT));

      node1.n.expr = n.n.assign1->expr;
      node1.type = n.n.assign1->expr->t;
      generate_code_helper(node1);
      n.n.assign1->expr->id = node1.n.expr->id;
      // test undefined variable error
      if (!pair)
      {
         // undefined variable error
         sprintf(err, "Line %d - error: `%s` is undefined\n", n.n.assign1->line, n.n.assign1->ident);
         write(STDERR_FILENO, err, strlen(err));
         break;
      }

      // test type mismatch error
      if (pair->value->type != n.n.assign1->expr->id)
      {
         // type mismatch error
         sprintf(err, "Line %d - error: cannot assign type %s to type %s\n", n.n.assign1->line,
                 getDataTypeById(n.n.assign1->expr->id), getDataTypeById(pair->value->type));
         write(STDERR_FILENO, err, strlen(err));
         break;
      }

      // mark initialized
      if (!pair->value->isInitialized)
      {
         replace_entry(&table, pair->id, 1);
      }
      break;
   case ASSIGN2:
      pair = find_entry(table, n.n.assign2->ident);

      l1 = getInput(n.n.assign2->ident);
      write(STDOUT_FILENO, l1, strlen(l1));

      // test undefined variable error
      if (!pair)
      {
         // undefined variable error
         sprintf(err, "Line %d - error: `%s` is undefined\n", n.n.assign2->line, n.n.assign2->ident);
         write(STDERR_FILENO, err, strlen(err));
         break;
      }
      // test type mismatch error
      if (pair->value->type != INT_TYPE)
      {
         // type mismatch error
         sprintf(err, "Line %d - error: READINT is assigned to a non-int type\n", n.n.assign2->line);
         write(STDERR_FILENO, err, strlen(err));
         break;
      }

      // mark initialized
      if (!pair->value->isInitialized)
      {
         replace_entry(&table, pair->id, 1);
      }
      break;
   case EXPR:
      switch (n.n.expr->next)
      {
      case EXPR1:
         node1.n.expr1 = n.n.expr->expr->expr1;
         node1.type = n.n.expr->expr->expr1->t;
         generate_code_helper(node1);
         n.n.expr->id = node1.n.expr1->id;
         break;
      case EXPR2:
         node1.n.expr2 = n.n.expr->expr->expr2;
         node1.type = n.n.expr->expr->expr2->t;
         generate_code_helper(node1);
         n.n.expr->id = node1.n.expr2->id;
         break;
      }
      break;
   case EXPR1:
      node1.n.simpExpr = n.n.expr1->simpExpr;
      node1.type = n.n.expr1->simpExpr->t;
      generate_code_helper(node1);
      n.n.expr1->id = node1.n.simpExpr->id;
      break;
   case EXPR2:
      node1.n.simpExpr = n.n.expr2->simpExpr1;
      node1.type = n.n.expr2->simpExpr1->t;
      generate_code_helper(node1);
      n.n.expr2->id = node1.n.simpExpr->id;
      // test type mismatch
      if (n.n.expr2->simpExpr1->id != INT_TYPE)
      {
         // type mismatch
         sprintf(err, "Line %d - error: %s operators must be int\n", n.n.expr2->line, n.n.expr2->op4);
         write(STDERR_FILENO, err, strlen(err));
         break;
      }
      // generate code
      l1 = getOP4(n.n.expr2->op4);
      write(STDOUT_FILENO, l1, strlen(l1));

      node2.n.simpExpr = n.n.expr2->simpExpr2;
      node2.type = n.n.expr2->simpExpr2->t;
      generate_code_helper(node2);
      n.n.expr2->simpExpr2->id = node2.n.simpExpr->id;
      // test type mismatch
      if (n.n.expr2->simpExpr2->id != INT_TYPE)
      {
         // type mismatch error
         sprintf(err, "Line %d - error: %s operators must be int\n", n.n.expr2->line, n.n.expr2->op4);
         write(STDERR_FILENO, err, strlen(err));
         break;
      }
      // recognize the type
      n.n.expr2->id = BOOL_TYPE;
      break;
   case SIMPEXPR:
      switch (n.n.simpExpr->next)
      {
      case SIMPEXPR1:
         node1.n.simpExpr1 = n.n.simpExpr->simpExpr->simpExpr1;
         node1.type = n.n.simpExpr->simpExpr->simpExpr1->t;
         generate_code_helper(node1);
         n.n.simpExpr->id = node1.n.simpExpr1->id;
         break;
      case SIMPEXPR2:
         node1.n.simpExpr2 = n.n.simpExpr->simpExpr->simpExpr2;
         node1.type = n.n.simpExpr->simpExpr->simpExpr2->t;
         generate_code_helper(node1);
         n.n.simpExpr->id = node1.n.simpExpr2->id;
         break;
      }
      break;
   case SIMPEXPR1:
      node1.n.term = n.n.simpExpr1->term1;
      node1.type = n.n.simpExpr1->term1->t;
      generate_code_helper(node1);
      n.n.simpExpr1->term1->id = node1.n.term->id;
      // test type mismatch
      if (n.n.simpExpr1->term1->id != INT_TYPE)
      {
         // type mismatch error
         sprintf(err, "Line %d - error: %s operators must be int\n", n.n.simpExpr1->line, n.n.simpExpr1->op3);
         write(STDERR_FILENO, err, strlen(err));
         break;
      }
      // generate code
      l1 = getOP3(n.n.simpExpr1->op3);
      write(STDOUT_FILENO, l1, strlen(l1));

      node2.n.term = n.n.simpExpr1->term2;
      node2.type = n.n.simpExpr1->term2->t;
      generate_code_helper(node2);
      n.n.simpExpr1->term2->id = node2.n.term->id;
      // test type mismatch
      if (n.n.simpExpr1->term2->id != INT_TYPE)
      {
         // type mismatch error
         sprintf(err, "Line %d - error: %s operators must be int\n", n.n.simpExpr1->line, n.n.simpExpr1->op3);
         write(STDERR_FILENO, err, strlen(err));
         break;
      }
      // recognize the type
      n.n.simpExpr1->id = INT_TYPE;
      break;
   case SIMPEXPR2:
      node1.n.term = n.n.simpExpr2->term;
      node1.type = n.n.simpExpr2->term->t;
      generate_code_helper(node1);
      n.n.simpExpr2->id = node1.n.term->id;
      break;
   case TERM:
      switch (n.n.term->next)
      {
      case TERM1:
         node1.n.term1 = n.n.term->term->term1;
         node1.type = n.n.term->term->term1->t;
         generate_code_helper(node1);
         n.n.term->id = node1.n.term1->id;
         break;
      case TERM2:
         node1.n.term2 = n.n.term->term->term2;
         node1.type = n.n.term->term->term2->t;
         generate_code_helper(node1);
         n.n.term->id = node1.n.term2->id;
         break;
      }
      break;
   case TERM1:
      node1.n.factor = n.n.term1->factor1;
      node1.type = n.n.term1->factor1->t;
      generate_code_helper(node1);
      n.n.term1->factor1->id = node1.n.factor->id;
      // generate code
      l1 = getOP2(n.n.term1->op2);
      write(STDOUT_FILENO, l1, strlen(l1));

      node2.n.factor = n.n.term1->factor2;
      node2.type = n.n.term1->factor2->t;
      generate_code_helper(node2);
      n.n.term1->factor2->id = node2.n.factor->id;
      // test type mismatch
      if (n.n.term1->factor1->id != INT_TYPE)
      {
         // type mismatch error
         sprintf(err, "Line %d - error: %s operators must be int\n", n.n.term1->line, n.n.term1->op2);
         write(STDERR_FILENO, err, strlen(err));
         break;
      }
      if (n.n.term1->factor2->id != INT_TYPE)
      {
         // type mismatch error
         sprintf(err, "Line %d - error: %s operators must be int\n", n.n.term1->line, n.n.term1->op2);
         write(STDERR_FILENO, err, strlen(err));
         break;
      }
      // recognize the type
      n.n.term1->id = INT_TYPE;
      break;
   case TERM2:
      node1.n.factor = n.n.term2->factor;
      node1.type = n.n.term2->factor->t;
      generate_code_helper(node1);
      n.n.term2->id = node1.n.factor->id;
      break;
   case FACTOR:
      switch (n.n.factor->next)
      {
      case IDENT_TYPE:
         pair = find_entry(table, n.n.factor->factor->ident);
         // test undefined variable error
         if (!pair)
         {
            // undefined variable error
            sprintf(err, "Line %d - error: `%s` is undefined\n", n.n.factor->line,
                    n.n.factor->factor->ident);
            write(STDERR_FILENO, err, strlen(err));
            break;
         }
         // test uninitialized error
         if (!pair->value->isInitialized)
         {
            // uninitialized error
            sprintf(err, "Line %d - error: `%s` is uninitialized\n", n.n.factor->line,
                    n.n.factor->factor->ident);
            write(STDERR_FILENO, err, strlen(err));
            // allow carrying on error detection
         }
         // recognize the type
         n.n.factor->id = pair->value->type;

         // generate code
         write(STDOUT_FILENO, n.n.factor->factor->ident, strlen(n.n.factor->factor->ident));

         break;
      case INT_TYPE:
         // test int overflow error
         if (isIntOverflow(n.n.factor->factor->num))
         {
            sprintf(err, "Line %d - error: `%s` is an illegal int\n", n.n.factor->line,
                    n.n.factor->factor->num);
            write(STDERR_FILENO, err, strlen(err));
         }
         // generate code
         write(STDOUT_FILENO, n.n.factor->factor->num, strlen(n.n.factor->factor->num));

         break;
      case BOOL_TYPE:

         // generate code
         write(STDOUT_FILENO, n.n.factor->factor->boolean, strlen(n.n.factor->factor->boolean));

         break;
      case EXPR:
         write(STDOUT_FILENO, LEFTP, strlen(LEFTP));

         node1.n.expr = n.n.factor->factor->expr;
         node1.type = n.n.factor->factor->expr->t;
         generate_code_helper(node1);
         n.n.factor->id = node1.n.expr->id;

         write(STDOUT_FILENO, RIGHTP, strlen(RIGHTP));
         break;
      }
      break;
   case IFSTMT:
      // generate code
      write(STDOUT_FILENO, PROGRAM_IF_PREFIX, strlen(PROGRAM_IF_PREFIX));

      node1.n.expr = n.n.ifStmt->expr;
      node1.type = n.n.ifStmt->expr->t;
      generate_code_helper(node1);
      n.n.ifStmt->expr->id = node1.n.expr->id;
      // test type mismatch
      if (n.n.ifStmt->expr->id != BOOL_TYPE)
      {
         // type mismatch error
         sprintf(err,
                 "Line %d - error: guarding expression must be evaluated to boolean but it was evaluated to %s\n",
                 n.n.ifStmt->line,
                 getDataTypeById(n.n.ifStmt->expr->id));
         write(STDERR_FILENO, err, strlen(err));
         break;
      }
      // generate code
      write(STDOUT_FILENO, PROGRAM_IF_MIDDLE, strlen(PROGRAM_IF_MIDDLE));
      write(STDOUT_FILENO, NEWLINE, strlen(NEWLINE));

      node2.n.stmtSeq = n.n.ifStmt->stmtSeq;
      if (node2.n.stmtSeq)
      {
         node2.type = n.n.ifStmt->stmtSeq->t;
      }
      generate_code_helper(node2);

      // generate code
      write(STDOUT_FILENO, PROGRAM_IF_SUFFIX, strlen(PROGRAM_IF_SUFFIX));
      write(STDOUT_FILENO, NEWLINE, strlen(NEWLINE));

      node3.n.elseClause = n.n.ifStmt->elseClause;
      if (node3.n.elseClause)
      {
         node3.type = n.n.ifStmt->elseClause->t;
      }
      generate_code_helper(node3);
      break;
   case ELSECLAUSE:
      if (!n.n.elseClause)
      {
         break;
      }
      write(STDOUT_FILENO, PROGRAM_ELSE_PREFIX, strlen(PROGRAM_ELSE_PREFIX));
      write(STDOUT_FILENO, NEWLINE, strlen(NEWLINE));

      node1.n.stmtSeq = n.n.elseClause->stmtSeq;
      if (node1.n.stmtSeq)
      {
         node1.type = n.n.elseClause->stmtSeq->t;
      }
      generate_code_helper(node1);

      write(STDOUT_FILENO, PROGRAM_ELSE_SUFFIX, strlen(PROGRAM_ELSE_SUFFIX));
      write(STDOUT_FILENO, NEWLINE, strlen(NEWLINE));
      break;
   case WHILESTMT:
      // generate code
      write(STDOUT_FILENO, PROGRAM_WHILE_PREFIX, strlen(PROGRAM_WHILE_PREFIX));

      node1.n.expr = n.n.whileStmt->expr;
      node1.type = n.n.whileStmt->expr->t;
      generate_code_helper(node1);
      n.n.whileStmt->expr->id = node1.n.expr->id;
      // test type mismatch
      if (n.n.whileStmt->expr->id != BOOL_TYPE)
      {
         // type mismatch error
         sprintf(err,
                 "Line %d - error: guarding expression must be evaluated to boolean but it was evaluated to %s\n",
                 n.n.whileStmt->line,
                 getDataTypeById(n.n.whileStmt->expr->id));
         write(STDERR_FILENO, err, strlen(err));
         break;
      }
      // generate code
      write(STDOUT_FILENO, PROGRAM_WHILE_MIDDLE, strlen(PROGRAM_WHILE_MIDDLE));
      write(STDOUT_FILENO, NEWLINE, strlen(NEWLINE));

      node2.n.stmtSeq = n.n.whileStmt->stmtSeq;
      if (node2.n.stmtSeq)
      {
         node2.type = n.n.whileStmt->stmtSeq->t;
      }
      generate_code_helper(node2);

      // generate code
      write(STDOUT_FILENO, PROGRAM_WHILE_SUFFIX, strlen(PROGRAM_WHILE_SUFFIX));
      write(STDOUT_FILENO, NEWLINE, strlen(NEWLINE));
      break;
   case WRITE_INT:
      // generate code
      write(STDOUT_FILENO, PROGRAM_OUTPUT_PREFIX, strlen(PROGRAM_OUTPUT_PREFIX));

      node1.n.expr = n.n.writeInt->expr;
      node1.type = n.n.writeInt->expr->t;
      generate_code_helper(node1);
      n.n.writeInt->expr->id = node1.n.expr->id;
      // test type mismatch
      if (n.n.writeInt->expr->id != INT_TYPE)
      {
         // type mismatch error
         sprintf(err,
                 "Line %d - error: argument to WRITEINT must be evaluated to int but it was evaluated to %s\n",
                 n.n.writeInt->line,
                 getDataTypeById(n.n.writeInt->expr->id));
         write(STDERR_FILENO, err, strlen(err));
         break;
      }
      // generate code
      write(STDOUT_FILENO, PROGRAM_OUTPUT_SUFFIX, strlen(PROGRAM_OUTPUT_SUFFIX));
      write(STDOUT_FILENO, NEWLINE, strlen(NEWLINE));
      break;
   }
}

void free_table(struct entry *store)
{
   // free the table
   struct entry *s, *tmp;
   HASH_ITER(hh, store, s, tmp)
   {
      HASH_DEL(store, s);
      free(s);
   }
}

struct entry *find_entry(struct entry *store, const char *key)
{
   // find an entry from the table
   struct entry *s;
   HASH_FIND_STR(store, key, s);
   if (s)
      return s;
   return NULL;
}

unsigned short add_entry(struct entry **store, const char *key, int var_type, unsigned short initialized)
{
   // add an entry to the table
   struct entry *old = find_entry(*store, key);
   if (old)
   {
      // redeclaration error
      return 1;
   }
   struct entry *s;
   s = (struct entry *)malloc(sizeof *s);
   s->id = key;
   struct table_value *val = NULL;
   val = (struct table_value *)malloc(sizeof *val);
   val->isInitialized = initialized;
   val->type = var_type;
   s->value = val;
   HASH_ADD_KEYPTR(hh, *store, s->id, strlen(s->id), s);

   return 0;
}

unsigned short replace_entry(struct entry **store, const char *key, unsigned short initialized)
{
   struct entry *old = find_entry(*store, key);
   if (!old)
   {
      // undefined variable error
      return 1;
   }
   struct entry *s;
   s = (struct entry *)malloc(sizeof *s);
   s->id = key;
   struct table_value *val = NULL;
   val = (struct table_value *)malloc(sizeof *val);
   val->isInitialized = initialized;
   val->type = old->value->type;
   s->value = val;
   HASH_REPLACE_STR(*store, id, s, old);
   return 0;
}

char *getDataTypeById(int id)
{
   switch (id)
   {
   case INT_TYPE:
      return "int";
   case BOOL_TYPE:
      return "bool";
   default:
      return "unknown";
   }
}

char *getDeclaration(int type, char *ident)
{
   char *t = getDataTypeById(type);
   char *res = malloc(strlen(t) + strlen(ident) + 1);
   strcpy(res, t);
   strcat(res, " ");
   strcat(res, ident);
   return res;
}

char *getInput(char *ident)
{
   char *res = malloc(14 + strlen(ident));
   sprintf(res, "scanf(\"%%d\", &%s)", ident);
   return res;
}

int isIntOverflow(char *num)
{
   if (strlen(num) > strlen(PROGRAM_MAX_INT))
   {
      return 1;
   }
   else if (strlen(num) < strlen(PROGRAM_MAX_INT))
   {
      return 0;
   }
   for (int i = 0; i < strlen(PROGRAM_MAX_INT); i++)
   {
      if (num[i] > PROGRAM_MAX_INT[i])
      {
         return 1;
      }
   }
   return 0;
}

char *getOP4(char *op4)
{
   if (strcmp(op4, "=") == 0)
   {
      return "==";
   }
   return op4;
}

char *getOP3(char *op3)
{
   return op3;
}

char *getOP2(char *op2)
{
   if (strcmp(op2, "*") == 0)
   {
      return "*";
   }
   else if (strcmp(op2, "div") == 0)
   {
      return "/";
   }
   else
   {
      return "%";
   }
}
