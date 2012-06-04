#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "ast.h"

AST ast (const char * text, ...)
{
    AST t, child;
    va_list ap;
    int i;

    if ((t = (AST) malloc(sizeof(struct ast_str))) == NULL)
        yyerror("out of memory");
    
    va_start(ap, text);
    t->text = text;
    
    for (i = 0; (child = va_arg(ap, AST)) != NULL; i++)
        if (i < AST_CHILDREN)
            t->child[i] = child;
        else
            yyerror("too many AST children");
    for (; i < AST_CHILDREN; i++)
        t->child[i] = NULL;
    va_end(ap);
    return t;
}

void ast_fix (AST t, const char * text)
{
    t->text = text;
}

static struct ast_str ast_nil_constant = {
    "$$LIST$$",
    { NULL, NULL, NULL, NULL, NULL }
};
const AST ast_nil = &ast_nil_constant;

AST ast_cons (AST head, AST tail)
{
    AST t;
    int i;
    
    if ((t = (AST) malloc(sizeof(struct ast_str))) == NULL)
        yyerror("out of memory");
    t->text = "$$LIST$$";
    t->child[0] = head;
    t->child[1] = tail;
    for (i = 2; i < AST_CHILDREN; i++)
        t->child[i] = NULL;
    return t;
}

AST ast_concat (AST first, AST second)
{
    AST t;
    
    if (first == ast_nil)
        return second;
    
    for (t = first; t->child[1] != ast_nil; t = t->child[1]);
    t->child[1] = second;
    return first;
}        

AST ast_id (const char * text)
{
    return ast("$$ID$$", ast(text, NULL), NULL);
}

AST ast_lit (const char * text)
{
    return ast(text, NULL);
}

static void printASTauxil (AST ast, bool paren)
{
    if (ast != NULL) {
        if (strcmp(ast->text, "$$LIST$$") == 0) {
            int needComma = 0;
            
            printf("[");
            while (ast->child[0] != NULL) {
                if (needComma)
                    printf(", ");
                printAST(ast->child[0]);
                needComma = 1;
                ast = ast->child[1];
            }            
            printf("]");
        }
        else if (strcmp(ast->text, "$$ID$$") == 0) {
            printf("\"");
            if (ast->child[0] != NULL)
                printAST(ast->child[0]);
            printf("\"");
        }
        else {
            int i;
	    bool hasChildren;
            
            for (i = 0, hasChildren = false; i < AST_CHILDREN; i++)
                if (ast->child[i] != NULL) {
                    hasChildren = true;
		    break;
		}
	    if (paren && hasChildren > 0)
                printf("(");
            printf("%s", ast->text);
	    for (i = 0; i < AST_CHILDREN; i++)
                if (ast->child[i] != NULL) {
                    printf(" ");
                    printASTauxil(ast->child[i], true);
		}
	    if (paren && hasChildren > 0)
                printf(")");
        }
    }
}

void printAST (AST ast)
{
    printASTauxil(ast, false);
}

void destroyAST (AST ast)
{
    if (ast != NULL && ast != ast_nil) {
        int i;

        for (i = 0; i < AST_CHILDREN; i++)
            destroyAST(ast->child[i]);
        free(ast);
    }
}
