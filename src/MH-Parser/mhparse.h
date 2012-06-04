/******************************************************************************
 *  CVS version:
 *    $Id: mhparse.h,v 1.1 2004/05/17 09:28:23 nickie Exp $
 ******************************************************************************
 *  Haskell file : mhparse.h
 *  Project      : Intensional programming languages
 *  Version      : 1.0 alpha
 *  Written by   : Nikolaos S. Papaspyrou
 *  Date         : February 21, 2001
 *  Revised      : May 15, 2004
 *  Description  : Panos' JFP 2 transformation, MilliHaskell parser
 */


#ifndef __MHPARSE_H__
#define __MHPARSE_H__

#define MAX_CHARS 256

typedef enum {
    MH_VAR, MH_CONST, MH_APP, MH_DEF, MH_PROG
} NodeType;

typedef struct myStruct {
    NodeType code;
    char text [MAX_CHARS];
    struct myStruct * left;
    struct myStruct * right;
} * myYYSTYPE;

#define YYSTYPE myYYSTYPE
extern YYSTYPE yylval;
extern int linecount;

YYSTYPE mhGen  (NodeType, const char * s, YYSTYPE left, YYSTYPE right);
YYSTYPE mhGenQ (NodeType, const char * s, YYSTYPE left, YYSTYPE right);

typedef enum {true = 1, false = 0} bool;

void yyerror (const char * msg);

#endif
