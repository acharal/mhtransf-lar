/*****************************************************************************1
 *  CVS version:
 *    $Id: mhparse.y,v 1.1 2004/05/17 09:28:23 nickie Exp $
 ******************************************************************************
 *  Haskell file : mhparse.y
 *  Project      : Intensional programming languages
 *  Version      : 1.0 alpha
 *  Written by   : Nikolaos S. Papaspyrou
 *  Date         : February 21, 2001
 *  Revised      : May 15, 2004
 *  Description  : Panos' JFP 2 transformation, MilliHaskell parser
 */


%{
#include <stdio.h>
#include <string.h>

#include "ast.h"

#define YYDEBUG 1
#define YYERROR_VERBOSE 1

int seterror = 0;
%}

%union {
    const char * text;
    AST          ast;
}
	
%token TK_if
%token TK_then
%token TK_else
%token TK_let
%token TK_in

%token<text> TK_varid
%token<text> TK_conid
%token<text> TK_atom
%token<text> TK_integer
%token<text> TK_double
%token<text> TK_varsym
%token<text> TK_consym

%token TK_dotdot
%token TK_dcolon
%token TK_equal
%token TK_lam
%token TK_vbar
%token TK_larrow
%token TK_rarrow
%token TK_at
%token TK_tilde
%token TK_darrow
%token TK_bang
%token TK_dot

%token TK_ocurly
%token TK_ccurly
%token TK_obrack
%token TK_cbrack
%token TK_oparen
%token TK_cparen
%token TK_semi
%token TK_comma
%token TK_backquote

%token TK_or
%token TK_and
%token TK_eq
%token TK_ne
%token TK_lt
%token TK_gt
%token TK_le
%token TK_ge
%token TK_colon
%token TK_plus
%token TK_minus
%token TK_times
%token TK_div

%left     TK_TRAILING
%left     TK_or
%left     TK_and
%nonassoc TK_eq TK_ne TK_lt TK_gt TK_le TK_ge
%right    TK_colon
%left     TK_plus TK_minus
%left     TK_times TK_div
%right    TK_UNARY

%type<ast> prog
%type<ast> def_l
%type<ast> def_n
%type<ast> def
%type<ast> var_l
%type<ast> var_n
%type<ast> sexpr
%type<ast> aexpr
%type<ast> expr
%type<ast> args
%type<ast> infix
%type<ast> expr_l
%type<ast> expr_n

%expect 28

%%

prog:   def_l
        { $$ = ast("ProgF", $1, NULL);
	  printAST($$);
	  printf("\n");
	  destroyAST($$);
        }
;

def_l:  /* empty */
        { $$ = ast_nil; }
|       def_n
        { $$ = $1; }
;

def_n:  def
        { $$ = ast_cons($1, ast_nil); }
|       def TK_semi def_n
        { $$ = ast_cons($1, $3); }
;

def:    TK_varid var_l TK_equal expr
        { $$ = ast("DefF", ast_id($1), $2, $4, NULL); }
;

var_l:  /* empty */
        { $$ = ast_nil; }
|       var_n
        { $$ = $1; }
;

var_n:  TK_varid 
        { $$ = ast_cons(ast_id($1), ast_nil); }
|       TK_varid var_n
        { $$ = ast_cons(ast_id($1), $2); }
;

sexpr:  TK_varid
        { $$ = ast("XF", ast_id($1), NULL); }
|       TK_integer
        { $$ = ast("ConF", ast_id($1), ast_nil, NULL); }
|       TK_double
        { $$ = ast("ConF", ast_id($1), ast_nil, NULL); }
|       TK_atom
        { $$ = ast("ConF", ast_id($1), ast_nil, NULL); }
|       TK_oparen infix TK_cparen
        { $$ = ast("ConF", $2, ast_nil, NULL); }
|       TK_oparen expr TK_cparen
        { $$ = $2; }
|       TK_lam TK_varid TK_rarrow expr                  %prec TK_TRAILING
        { yyerror("not in milli-Haskell: lambda"); }
|       TK_let TK_ocurly def_l TK_ccurly TK_in expr     %prec TK_TRAILING
        { yyerror("not in milli-Haskell: let"); }
|       TK_if expr TK_then expr TK_else expr            %prec TK_TRAILING
        { $$ = ast("ConF", ast_id("if"),
	           ast_cons($2, ast_cons($4, ast_cons($6, ast_nil))),
		   NULL); }
|       TK_obrack expr_l TK_cbrack
        { $$ = $2; }
;

aexpr:  sexpr
	{ $$ = $1; }
|	TK_varid args
        { $$ = ast("FF", ast_id($1), $2, NULL); }
|       TK_atom args
        { $$ = ast("ConF", ast_id($1), $2, NULL); }
;

args:   sexpr
        { $$ = ast_cons($1, ast_nil); }
|       sexpr args
        { $$ = ast_cons($1, $2); }
;

expr:   aexpr
        { $$ = $1; }
|       expr TK_plus expr
        { $$ = ast("ConF", ast_id("+"),
	           ast_cons($1, ast_cons($3, ast_nil)), NULL); }
|       expr TK_minus expr
        { $$ = ast("ConF", ast_id("-"),
	           ast_cons($1, ast_cons($3, ast_nil)), NULL); }
|       expr TK_times expr
        { $$ = ast("ConF", ast_id("*"),
	           ast_cons($1, ast_cons($3, ast_nil)), NULL); }
|       expr TK_div expr
        { $$ = ast("ConF", ast_id("/"),
	           ast_cons($1, ast_cons($3, ast_nil)), NULL); }
|       expr TK_eq expr
        { $$ = ast("ConF", ast_id("=="),
	           ast_cons($1, ast_cons($3, ast_nil)), NULL); }
|       expr TK_ne expr
        { $$ = ast("ConF", ast_id("/="),
	           ast_cons($1, ast_cons($3, ast_nil)), NULL); }
|       expr TK_gt expr
        { $$ = ast("ConF", ast_id(">"),
	           ast_cons($1, ast_cons($3, ast_nil)), NULL); }
|       expr TK_lt expr
        { $$ = ast("ConF", ast_id("<"),
	           ast_cons($1, ast_cons($3, ast_nil)), NULL); }
|       expr TK_ge expr
        { $$ = ast("ConF", ast_id(">="),
	           ast_cons($1, ast_cons($3, ast_nil)), NULL); }
|       expr TK_and expr
        { $$ = ast("ConF", ast_id("&&"),
	           ast_cons($1, ast_cons($3, ast_nil)), NULL); }
|       expr TK_or expr
        { $$ = ast("ConF", ast_id("||"),
	           ast_cons($1, ast_cons($3, ast_nil)), NULL); }
|       expr TK_le expr
        { $$ = ast("ConF", ast_id("<="),
	           ast_cons($1, ast_cons($3, ast_nil)), NULL); }
|       expr TK_colon expr
        { $$ = ast("ConF", ast_id(":"),
	           ast_cons($1, ast_cons($3, ast_nil)), NULL); }
|       TK_plus expr                                    %prec TK_UNARY
        { $$ = ast("ConF", ast_id("+u"),
	           ast_cons($2, ast_nil), NULL); }
|       TK_minus expr                                   %prec TK_UNARY
        { $$ = ast("ConF", ast_id("-u"),
	           ast_cons($2, ast_nil), NULL); }
|	sexpr TK_backquote TK_atom TK_backquote sexpr
        { $$ = ast("ConF", ast_id($3),
	           ast_cons($1, ast_cons($5, ast_nil)), NULL); }
|	sexpr TK_backquote TK_varid TK_backquote sexpr
        { $$ = ast("FF", ast_id($3),
	           ast_cons($1, ast_cons($5, ast_nil)), NULL); }
;

infix:  TK_varsym       { $$ = ast_id($1); }
|       TK_or           { $$ = ast_id("||"); }
|       TK_and          { $$ = ast_id("&&"); }
|       TK_eq           { $$ = ast_id("=="); }
|       TK_ne           { $$ = ast_id("/="); }
|       TK_lt           { $$ = ast_id("<"); }
|       TK_gt           { $$ = ast_id(">"); }
|       TK_le           { $$ = ast_id("<="); }
|       TK_ge           { $$ = ast_id(">="); }
|       TK_colon        { $$ = ast_id(":"); }
|       TK_plus         { $$ = ast_id("+"); }
|       TK_minus        { $$ = ast_id("-"); }
|       TK_times        { $$ = ast_id("*"); }
|       TK_div          { $$ = ast_id("/"); }
;

expr_l: /* empty */
        { $$ = ast("ConF", ast_id("[]"), ast_nil, NULL); }
|       expr_n
        { $$ = $1; }
;

expr_n: expr
        { $$ = ast("ConF", ast_id(":"),
                   ast_cons($1, ast_cons(ast("ConF", ast_id("[]"),
                                             ast_nil, NULL), ast_nil)),
                   NULL); }
|       expr TK_comma expr_n
        { $$ = ast("ConF", ast_id(":"),
                   ast_cons($1, ast_cons($3, ast_nil)), NULL); }
;

%%

void yyerror (const char * msg) {
   printf("Error %d: Line %d: %s\n", ++seterror, linecount, msg);
   fflush(stdout);
}               
        
int main (int argc, const char * argv[])
{
   int quiet = 0;
   int i, result;
        
   for (i=1; i<argc; i++)
      if (strcmp(argv[i], "-q") == 0)
         quiet = 1; 
      else if (strcmp(argv[i], "-d") == 0)
         yydebug = 1;
        
   if (!quiet)
      fprintf(stderr, "Parser for milli-Haskell, built on May 15, 2004\n"
                      "Nikolaos S. Papaspyrou (nickie@softlab.ntua.gr).\n"
                      "\n");
        
   result = yyparse();
   freeStringBuffer();
   return result;
}
