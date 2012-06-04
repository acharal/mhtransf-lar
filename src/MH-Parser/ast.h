void yyerror (const char * msg);

void freeStringBuffer ();
extern int linecount;

#define AST_CHILDREN 5

typedef struct ast_str {
    const char * text;
    struct ast_str * child[AST_CHILDREN];
} * AST;

AST  ast        (const char * text, ...);
void ast_fix    (AST t, const char * text);
AST  ast_id     (const char * text);
AST  ast_lit    (const char * text);
AST  ast_cons   (AST t1, AST t2);
AST  ast_concat (AST t1, AST t2);

void printAST   (AST ast);
void destroyAST (AST ast);

extern const AST ast_nil;
