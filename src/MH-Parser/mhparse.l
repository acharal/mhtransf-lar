/******************************************************************************
 *  CVS version:
 *    $Id: mhparse.l,v 1.3 2004/09/20 15:26:36 nickie Exp $
 ******************************************************************************
 *  Haskell file : mhparse.l
 *  Project      : Intensional programming languages
 *  Version      : 1.0 alpha
 *  Written by   : Nikolaos S. Papaspyrou
 *  Date         : February 21, 2001
 *  Revised      : May 15, 2004
 *  Description  : Panos' JFP 2 transformation, MilliHaskell parser
 */


%{
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "mhparse.tab.h"

bool   isPredefined (const char * s);
char * duplicate (const char *);
void   freeStringBuffer ();

int linecount = 1;

typedef struct linked_list {
    struct linked_list * next;
    char text [0];
} * BufferType;

BufferType buffer = NULL;

%}

%x inSLComment
%x inMLComment

large           [A-Z]
small           [a-z]
digit           [0-9]
symbol          "!@#$%&*+./<=>?\\^|:-~"

%%

"--"                    BEGIN (inSLComment);
<inSLComment>[^*\n]+    /* comments ignored */ ;
<inSLComment>\n         { BEGIN (INITIAL); linecount++; }
<inSLComment>.          /* skip comments */ ;

"{-"                    BEGIN (inMLComment);
<inMLComment>[^-\n]+    /* comments ignored */ ;
<inMLComment>"-}"       BEGIN (INITIAL);
<inMLComment><<EOF>>    { yyerror("Unfinished comment"); }
<*>\n                   linecount++;
<inMLComment>.          /* skip comments */ ;


"if"    { return TK_if;   }
"then"  { return TK_then; }
"else"  { return TK_else; }
"let"   { return TK_let;  }
"in"    { return TK_in;   }

{small}({small}|{large}|{digit}|'|_)*   { yylval.text = duplicate(yytext);
                                          if (isPredefined(yytext))
                                             return TK_atom;
                                          else
                                             return TK_varid;
                                        }
{large}({small}|{large}|{digit}|'|_)*   { yylval.text = duplicate(yytext);
                                          if (isPredefined(yytext))
                                             return TK_atom;
                                          else
                                             return TK_conid;
                                        }

".."    { return TK_dotdot; }
"::"    { return TK_dcolon; }
"\\"    { return TK_lam; }
"|"     { return TK_vbar; }
"<-"    { return TK_larrow; }
"->"    { return TK_rarrow; }
"@"     { return TK_at; }
"~"     { return TK_tilde; }
"=>"    { return TK_darrow; }
"-"     { return TK_minus; }
"!"     { return TK_bang; }
"."     { return TK_dot; }

"{"     { return TK_ocurly; }
"}"     { return TK_ccurly; }
"["     { return TK_obrack; }
"]"     { return TK_cbrack; }
"("     { return TK_oparen; }
")"     { return TK_cparen; }
";"     { return TK_semi; }
","     { return TK_comma; }
"`"     { return TK_backquote; }

"||"    { return TK_or; }
"&&"    { return TK_and; }
"=="    { return TK_eq; }
"/="    { return TK_ne; }
"<"     { return TK_lt; }
">"     { return TK_gt; }
"<="    { return TK_le; }
">="    { return TK_ge; }
":"     { return TK_colon; }
"+"     { return TK_plus; }
"*"     { return TK_times; }
"/"     { return TK_div; }

"="     { return TK_equal; }


{symbol}({symbol}|:)*   { yylval.text = duplicate(yytext);
                          return TK_varsym; }
:({symbol}|:)*          { yylval.text = duplicate(yytext);
                          return TK_consym; }

{digit}+                                 { yylval.text = duplicate(yytext);
                                           return TK_integer; }

{digit}+"."{digit}+([Ee][+-]?{digit}+)?  { yylval.text = duplicate(yytext);
                                           return TK_double; }

[ \t\r]+ { /* nothing */ }
.        { fprintf(stderr, "Lexical error: line %d, unexpected \"%s\"\n",
           linecount, yytext); }

%%

bool isPredefined (const char * s)
{
    static const char * predefined [] = {
	     "True",
		  "False",
        "div",
        "mod",
        "null",
        "head",
        "tail",
        NULL
    };
    const char ** p;

    for (p = predefined; *p != NULL; p++)
        if (strcmp(*p, s) == 0)
            return true;
    return false;
}

char * duplicate (const char * src)
{
    BufferType n = (BufferType) malloc(sizeof(BufferType) + strlen(src) + 1);

    n->next = buffer;
    strcpy(n->text, src);
    buffer = n;
    return n->text;
}

void freeStringBuffer ()
{
    while (buffer != NULL) {
        BufferType t = buffer;

        buffer = buffer->next;
        free(t);
    }
}
