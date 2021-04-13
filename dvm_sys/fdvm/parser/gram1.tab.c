/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     PERCENT = 1,
     AMPERSAND = 2,
     ASTER = 3,
     CLUSTER = 4,
     COLON = 5,
     COMMA = 6,
     DASTER = 7,
     DEFINED_OPERATOR = 8,
     DOT = 9,
     DQUOTE = 10,
     GLOBAL_A = 11,
     LEFTAB = 12,
     LEFTPAR = 13,
     MINUS = 14,
     PLUS = 15,
     POINT_TO = 16,
     QUOTE = 17,
     RIGHTAB = 18,
     RIGHTPAR = 19,
     AND = 20,
     DSLASH = 21,
     EQV = 22,
     EQ = 23,
     EQUAL = 24,
     FFALSE = 25,
     GE = 26,
     GT = 27,
     LE = 28,
     LT = 29,
     NE = 30,
     NEQV = 31,
     NOT = 32,
     OR = 33,
     TTRUE = 34,
     SLASH = 35,
     XOR = 36,
     REFERENCE = 37,
     AT = 38,
     ACROSS = 39,
     ALIGN_WITH = 40,
     ALIGN = 41,
     ALLOCATABLE = 42,
     ALLOCATE = 43,
     ARITHIF = 44,
     ASSIGNMENT = 45,
     ASSIGN = 46,
     ASSIGNGOTO = 47,
     ASYNCHRONOUS = 48,
     ASYNCID = 49,
     ASYNCWAIT = 50,
     BACKSPACE = 51,
     BAD_CCONST = 52,
     BAD_SYMBOL = 53,
     BARRIER = 54,
     BLOCKDATA = 55,
     BLOCK = 56,
     BOZ_CONSTANT = 57,
     BYTE = 58,
     CALL = 59,
     CASE = 60,
     CHARACTER = 61,
     CHAR_CONSTANT = 62,
     CHECK = 63,
     CLOSE = 64,
     COMMON = 65,
     COMPLEX = 66,
     COMPGOTO = 67,
     CONSISTENT_GROUP = 68,
     CONSISTENT_SPEC = 69,
     CONSISTENT_START = 70,
     CONSISTENT_WAIT = 71,
     CONSISTENT = 72,
     CONSTRUCT_ID = 73,
     CONTAINS = 74,
     CONTINUE = 75,
     CORNER = 76,
     CYCLE = 77,
     DATA = 78,
     DEALLOCATE = 79,
     HPF_TEMPLATE = 80,
     DEBUG = 81,
     DEFAULT_CASE = 82,
     DEFINE = 83,
     DERIVED = 84,
     DIMENSION = 85,
     DISTRIBUTE = 86,
     DOWHILE = 87,
     DOUBLEPRECISION = 88,
     DOUBLECOMPLEX = 89,
     DP_CONSTANT = 90,
     DVM_POINTER = 91,
     DYNAMIC = 92,
     ELEMENTAL = 93,
     ELSE = 94,
     ELSEIF = 95,
     ELSEWHERE = 96,
     ENDASYNCHRONOUS = 97,
     ENDDEBUG = 98,
     ENDINTERVAL = 99,
     ENDUNIT = 100,
     ENDDO = 101,
     ENDFILE = 102,
     ENDFORALL = 103,
     ENDIF = 104,
     ENDINTERFACE = 105,
     ENDMODULE = 106,
     ENDON = 107,
     ENDSELECT = 108,
     ENDTASK_REGION = 109,
     ENDTYPE = 110,
     ENDWHERE = 111,
     ENTRY = 112,
     EXIT = 113,
     EOLN = 114,
     EQUIVALENCE = 115,
     ERROR = 116,
     EXTERNAL = 117,
     F90 = 118,
     FIND = 119,
     FORALL = 120,
     FORMAT = 121,
     FUNCTION = 122,
     GATE = 123,
     GEN_BLOCK = 124,
     HEAP = 125,
     HIGH = 126,
     IDENTIFIER = 127,
     IMPLICIT = 128,
     IMPLICITNONE = 129,
     INCLUDE_TO = 130,
     INCLUDE = 131,
     INDEPENDENT = 132,
     INDIRECT_ACCESS = 133,
     INDIRECT_GROUP = 134,
     INDIRECT = 135,
     INHERIT = 136,
     INQUIRE = 137,
     INTERFACEASSIGNMENT = 138,
     INTERFACEOPERATOR = 139,
     INTERFACE = 140,
     INTRINSIC = 141,
     INTEGER = 142,
     INTENT = 143,
     INTERVAL = 144,
     INOUT = 145,
     IN = 146,
     INT_CONSTANT = 147,
     LABEL = 148,
     LABEL_DECLARE = 149,
     LET = 150,
     LOCALIZE = 151,
     LOGICAL = 152,
     LOGICALIF = 153,
     LOOP = 154,
     LOW = 155,
     MAXLOC = 156,
     MAX = 157,
     MAP = 158,
     MINLOC = 159,
     MIN = 160,
     MODULE_PROCEDURE = 161,
     MODULE = 162,
     MULT_BLOCK = 163,
     NAMEEQ = 164,
     NAMELIST = 165,
     NEW_VALUE = 166,
     NEW = 167,
     NULLIFY = 168,
     OCTAL_CONSTANT = 169,
     ONLY = 170,
     ON = 171,
     ON_DIR = 172,
     ONTO = 173,
     OPEN = 174,
     OPERATOR = 175,
     OPTIONAL = 176,
     OTHERWISE = 177,
     OUT = 178,
     OWN = 179,
     PARALLEL = 180,
     PARAMETER = 181,
     PAUSE = 182,
     PLAINDO = 183,
     PLAINGOTO = 184,
     POINTER = 185,
     POINTERLET = 186,
     PREFETCH = 187,
     PRINT = 188,
     PRIVATE = 189,
     PRODUCT = 190,
     PROGRAM = 191,
     PUBLIC = 192,
     PURE = 193,
     RANGE = 194,
     READ = 195,
     REALIGN_WITH = 196,
     REALIGN = 197,
     REAL = 198,
     REAL_CONSTANT = 199,
     RECURSIVE = 200,
     REDISTRIBUTE_NEW = 201,
     REDISTRIBUTE = 202,
     REDUCTION_GROUP = 203,
     REDUCTION_START = 204,
     REDUCTION_WAIT = 205,
     REDUCTION = 206,
     REMOTE_ACCESS_SPEC = 207,
     REMOTE_ACCESS = 208,
     REMOTE_GROUP = 209,
     RESET = 210,
     RESULT = 211,
     RETURN = 212,
     REWIND = 213,
     SAVE = 214,
     SECTION = 215,
     SELECT = 216,
     SEQUENCE = 217,
     SHADOW_ADD = 218,
     SHADOW_COMPUTE = 219,
     SHADOW_GROUP = 220,
     SHADOW_RENEW = 221,
     SHADOW_START_SPEC = 222,
     SHADOW_START = 223,
     SHADOW_WAIT_SPEC = 224,
     SHADOW_WAIT = 225,
     SHADOW = 226,
     STAGE = 227,
     STATIC = 228,
     STAT = 229,
     STOP = 230,
     SUBROUTINE = 231,
     SUM = 232,
     SYNC = 233,
     TARGET = 234,
     TASK = 235,
     TASK_REGION = 236,
     THEN = 237,
     TO = 238,
     TRACEON = 239,
     TRACEOFF = 240,
     TRUNC = 241,
     TYPE = 242,
     TYPE_DECL = 243,
     UNDER = 244,
     UNKNOWN = 245,
     USE = 246,
     VIRTUAL = 247,
     VARIABLE = 248,
     WAIT = 249,
     WHERE = 250,
     WHERE_ASSIGN = 251,
     WHILE = 252,
     WITH = 253,
     WRITE = 254,
     COMMENT = 255,
     WGT_BLOCK = 256,
     HPF_PROCESSORS = 257,
     IOSTAT = 258,
     ERR = 259,
     END = 260,
     OMPDVM_ATOMIC = 261,
     OMPDVM_BARRIER = 262,
     OMPDVM_COPYIN = 263,
     OMPDVM_COPYPRIVATE = 264,
     OMPDVM_CRITICAL = 265,
     OMPDVM_ONETHREAD = 266,
     OMPDVM_DO = 267,
     OMPDVM_DYNAMIC = 268,
     OMPDVM_ENDCRITICAL = 269,
     OMPDVM_ENDDO = 270,
     OMPDVM_ENDMASTER = 271,
     OMPDVM_ENDORDERED = 272,
     OMPDVM_ENDPARALLEL = 273,
     OMPDVM_ENDPARALLELDO = 274,
     OMPDVM_ENDPARALLELSECTIONS = 275,
     OMPDVM_ENDPARALLELWORKSHARE = 276,
     OMPDVM_ENDSECTIONS = 277,
     OMPDVM_ENDSINGLE = 278,
     OMPDVM_ENDWORKSHARE = 279,
     OMPDVM_FIRSTPRIVATE = 280,
     OMPDVM_FLUSH = 281,
     OMPDVM_GUIDED = 282,
     OMPDVM_LASTPRIVATE = 283,
     OMPDVM_MASTER = 284,
     OMPDVM_NOWAIT = 285,
     OMPDVM_NONE = 286,
     OMPDVM_NUM_THREADS = 287,
     OMPDVM_ORDERED = 288,
     OMPDVM_PARALLEL = 289,
     OMPDVM_PARALLELDO = 290,
     OMPDVM_PARALLELSECTIONS = 291,
     OMPDVM_PARALLELWORKSHARE = 292,
     OMPDVM_RUNTIME = 293,
     OMPDVM_SECTION = 294,
     OMPDVM_SECTIONS = 295,
     OMPDVM_SCHEDULE = 296,
     OMPDVM_SHARED = 297,
     OMPDVM_SINGLE = 298,
     OMPDVM_THREADPRIVATE = 299,
     OMPDVM_WORKSHARE = 300,
     OMPDVM_NODES = 301,
     OMPDVM_IF = 302,
     IAND = 303,
     IEOR = 304,
     IOR = 305,
     ACC_REGION = 306,
     ACC_END_REGION = 307,
     ACC_CHECKSECTION = 308,
     ACC_END_CHECKSECTION = 309,
     ACC_GET_ACTUAL = 310,
     ACC_ACTUAL = 311,
     ACC_TARGETS = 312,
     ACC_ASYNC = 313,
     ACC_HOST = 314,
     ACC_CUDA = 315,
     ACC_LOCAL = 316,
     ACC_INLOCAL = 317,
     ACC_CUDA_BLOCK = 318,
     BY = 319,
     IO_MODE = 320,
     CP_CREATE = 321,
     CP_LOAD = 322,
     CP_SAVE = 323,
     CP_WAIT = 324,
     FILES = 325,
     VARLIST = 326,
     STATUS = 327,
     EXITINTERVAL = 328,
     TEMPLATE_CREATE = 329,
     TEMPLATE_DELETE = 330,
     SPF_ANALYSIS = 331,
     SPF_PARALLEL = 332,
     SPF_TRANSFORM = 333,
     SPF_NOINLINE = 334,
     SPF_PARALLEL_REG = 335,
     SPF_END_PARALLEL_REG = 336,
     SPF_PRIVATES_EXPANSION = 337,
     SPF_FISSION = 338,
     SPF_SHRINK = 339,
     BINARY_OP = 342,
     UNARY_OP = 343
   };
#endif
/* Tokens.  */
#define PERCENT 1
#define AMPERSAND 2
#define ASTER 3
#define CLUSTER 4
#define COLON 5
#define COMMA 6
#define DASTER 7
#define DEFINED_OPERATOR 8
#define DOT 9
#define DQUOTE 10
#define GLOBAL_A 11
#define LEFTAB 12
#define LEFTPAR 13
#define MINUS 14
#define PLUS 15
#define POINT_TO 16
#define QUOTE 17
#define RIGHTAB 18
#define RIGHTPAR 19
#define AND 20
#define DSLASH 21
#define EQV 22
#define EQ 23
#define EQUAL 24
#define FFALSE 25
#define GE 26
#define GT 27
#define LE 28
#define LT 29
#define NE 30
#define NEQV 31
#define NOT 32
#define OR 33
#define TTRUE 34
#define SLASH 35
#define XOR 36
#define REFERENCE 37
#define AT 38
#define ACROSS 39
#define ALIGN_WITH 40
#define ALIGN 41
#define ALLOCATABLE 42
#define ALLOCATE 43
#define ARITHIF 44
#define ASSIGNMENT 45
#define ASSIGN 46
#define ASSIGNGOTO 47
#define ASYNCHRONOUS 48
#define ASYNCID 49
#define ASYNCWAIT 50
#define BACKSPACE 51
#define BAD_CCONST 52
#define BAD_SYMBOL 53
#define BARRIER 54
#define BLOCKDATA 55
#define BLOCK 56
#define BOZ_CONSTANT 57
#define BYTE 58
#define CALL 59
#define CASE 60
#define CHARACTER 61
#define CHAR_CONSTANT 62
#define CHECK 63
#define CLOSE 64
#define COMMON 65
#define COMPLEX 66
#define COMPGOTO 67
#define CONSISTENT_GROUP 68
#define CONSISTENT_SPEC 69
#define CONSISTENT_START 70
#define CONSISTENT_WAIT 71
#define CONSISTENT 72
#define CONSTRUCT_ID 73
#define CONTAINS 74
#define CONTINUE 75
#define CORNER 76
#define CYCLE 77
#define DATA 78
#define DEALLOCATE 79
#define HPF_TEMPLATE 80
#define DEBUG 81
#define DEFAULT_CASE 82
#define DEFINE 83
#define DERIVED 84
#define DIMENSION 85
#define DISTRIBUTE 86
#define DOWHILE 87
#define DOUBLEPRECISION 88
#define DOUBLECOMPLEX 89
#define DP_CONSTANT 90
#define DVM_POINTER 91
#define DYNAMIC 92
#define ELEMENTAL 93
#define ELSE 94
#define ELSEIF 95
#define ELSEWHERE 96
#define ENDASYNCHRONOUS 97
#define ENDDEBUG 98
#define ENDINTERVAL 99
#define ENDUNIT 100
#define ENDDO 101
#define ENDFILE 102
#define ENDFORALL 103
#define ENDIF 104
#define ENDINTERFACE 105
#define ENDMODULE 106
#define ENDON 107
#define ENDSELECT 108
#define ENDTASK_REGION 109
#define ENDTYPE 110
#define ENDWHERE 111
#define ENTRY 112
#define EXIT 113
#define EOLN 114
#define EQUIVALENCE 115
#define ERROR 116
#define EXTERNAL 117
#define F90 118
#define FIND 119
#define FORALL 120
#define FORMAT 121
#define FUNCTION 122
#define GATE 123
#define GEN_BLOCK 124
#define HEAP 125
#define HIGH 126
#define IDENTIFIER 127
#define IMPLICIT 128
#define IMPLICITNONE 129
#define INCLUDE_TO 130
#define INCLUDE 131
#define INDEPENDENT 132
#define INDIRECT_ACCESS 133
#define INDIRECT_GROUP 134
#define INDIRECT 135
#define INHERIT 136
#define INQUIRE 137
#define INTERFACEASSIGNMENT 138
#define INTERFACEOPERATOR 139
#define INTERFACE 140
#define INTRINSIC 141
#define INTEGER 142
#define INTENT 143
#define INTERVAL 144
#define INOUT 145
#define IN 146
#define INT_CONSTANT 147
#define LABEL 148
#define LABEL_DECLARE 149
#define LET 150
#define LOCALIZE 151
#define LOGICAL 152
#define LOGICALIF 153
#define LOOP 154
#define LOW 155
#define MAXLOC 156
#define MAX 157
#define MAP 158
#define MINLOC 159
#define MIN 160
#define MODULE_PROCEDURE 161
#define MODULE 162
#define MULT_BLOCK 163
#define NAMEEQ 164
#define NAMELIST 165
#define NEW_VALUE 166
#define NEW 167
#define NULLIFY 168
#define OCTAL_CONSTANT 169
#define ONLY 170
#define ON 171
#define ON_DIR 172
#define ONTO 173
#define OPEN 174
#define OPERATOR 175
#define OPTIONAL 176
#define OTHERWISE 177
#define OUT 178
#define OWN 179
#define PARALLEL 180
#define PARAMETER 181
#define PAUSE 182
#define PLAINDO 183
#define PLAINGOTO 184
#define POINTER 185
#define POINTERLET 186
#define PREFETCH 187
#define PRINT 188
#define PRIVATE 189
#define PRODUCT 190
#define PROGRAM 191
#define PUBLIC 192
#define PURE 193
#define RANGE 194
#define READ 195
#define REALIGN_WITH 196
#define REALIGN 197
#define REAL 198
#define REAL_CONSTANT 199
#define RECURSIVE 200
#define REDISTRIBUTE_NEW 201
#define REDISTRIBUTE 202
#define REDUCTION_GROUP 203
#define REDUCTION_START 204
#define REDUCTION_WAIT 205
#define REDUCTION 206
#define REMOTE_ACCESS_SPEC 207
#define REMOTE_ACCESS 208
#define REMOTE_GROUP 209
#define RESET 210
#define RESULT 211
#define RETURN 212
#define REWIND 213
#define SAVE 214
#define SECTION 215
#define SELECT 216
#define SEQUENCE 217
#define SHADOW_ADD 218
#define SHADOW_COMPUTE 219
#define SHADOW_GROUP 220
#define SHADOW_RENEW 221
#define SHADOW_START_SPEC 222
#define SHADOW_START 223
#define SHADOW_WAIT_SPEC 224
#define SHADOW_WAIT 225
#define SHADOW 226
#define STAGE 227
#define STATIC 228
#define STAT 229
#define STOP 230
#define SUBROUTINE 231
#define SUM 232
#define SYNC 233
#define TARGET 234
#define TASK 235
#define TASK_REGION 236
#define THEN 237
#define TO 238
#define TRACEON 239
#define TRACEOFF 240
#define TRUNC 241
#define TYPE 242
#define TYPE_DECL 243
#define UNDER 244
#define UNKNOWN 245
#define USE 246
#define VIRTUAL 247
#define VARIABLE 248
#define WAIT 249
#define WHERE 250
#define WHERE_ASSIGN 251
#define WHILE 252
#define WITH 253
#define WRITE 254
#define COMMENT 255
#define WGT_BLOCK 256
#define HPF_PROCESSORS 257
#define IOSTAT 258
#define ERR 259
#define END 260
#define OMPDVM_ATOMIC 261
#define OMPDVM_BARRIER 262
#define OMPDVM_COPYIN 263
#define OMPDVM_COPYPRIVATE 264
#define OMPDVM_CRITICAL 265
#define OMPDVM_ONETHREAD 266
#define OMPDVM_DO 267
#define OMPDVM_DYNAMIC 268
#define OMPDVM_ENDCRITICAL 269
#define OMPDVM_ENDDO 270
#define OMPDVM_ENDMASTER 271
#define OMPDVM_ENDORDERED 272
#define OMPDVM_ENDPARALLEL 273
#define OMPDVM_ENDPARALLELDO 274
#define OMPDVM_ENDPARALLELSECTIONS 275
#define OMPDVM_ENDPARALLELWORKSHARE 276
#define OMPDVM_ENDSECTIONS 277
#define OMPDVM_ENDSINGLE 278
#define OMPDVM_ENDWORKSHARE 279
#define OMPDVM_FIRSTPRIVATE 280
#define OMPDVM_FLUSH 281
#define OMPDVM_GUIDED 282
#define OMPDVM_LASTPRIVATE 283
#define OMPDVM_MASTER 284
#define OMPDVM_NOWAIT 285
#define OMPDVM_NONE 286
#define OMPDVM_NUM_THREADS 287
#define OMPDVM_ORDERED 288
#define OMPDVM_PARALLEL 289
#define OMPDVM_PARALLELDO 290
#define OMPDVM_PARALLELSECTIONS 291
#define OMPDVM_PARALLELWORKSHARE 292
#define OMPDVM_RUNTIME 293
#define OMPDVM_SECTION 294
#define OMPDVM_SECTIONS 295
#define OMPDVM_SCHEDULE 296
#define OMPDVM_SHARED 297
#define OMPDVM_SINGLE 298
#define OMPDVM_THREADPRIVATE 299
#define OMPDVM_WORKSHARE 300
#define OMPDVM_NODES 301
#define OMPDVM_IF 302
#define IAND 303
#define IEOR 304
#define IOR 305
#define ACC_REGION 306
#define ACC_END_REGION 307
#define ACC_CHECKSECTION 308
#define ACC_END_CHECKSECTION 309
#define ACC_GET_ACTUAL 310
#define ACC_ACTUAL 311
#define ACC_TARGETS 312
#define ACC_ASYNC 313
#define ACC_HOST 314
#define ACC_CUDA 315
#define ACC_LOCAL 316
#define ACC_INLOCAL 317
#define ACC_CUDA_BLOCK 318
#define BY 319
#define IO_MODE 320
#define CP_CREATE 321
#define CP_LOAD 322
#define CP_SAVE 323
#define CP_WAIT 324
#define FILES 325
#define VARLIST 326
#define STATUS 327
#define EXITINTERVAL 328
#define TEMPLATE_CREATE 329
#define TEMPLATE_DELETE 330
#define SPF_ANALYSIS 331
#define SPF_PARALLEL 332
#define SPF_TRANSFORM 333
#define SPF_NOINLINE 334
#define SPF_PARALLEL_REG 335
#define SPF_END_PARALLEL_REG 336
#define SPF_PRIVATES_EXPANSION 337
#define SPF_FISSION 338
#define SPF_SHRINK 339
#define BINARY_OP 342
#define UNARY_OP 343




/* Copy the first part of user declarations.  */
#line 341 "gram1.y"

#include <string.h>
#include "inc.h"
#include "extern.h"
#include "defines.h"
#include "fdvm.h"
#include "fm.h"

/* We may use builtin alloca */
#include "compatible.h"
#ifdef _NEEDALLOCAH_
#  include <alloca.h>
#endif

#define EXTEND_NODE 2  /* move the definition to h/ files. */

extern PTR_BFND global_bfnd, pred_bfnd;
extern PTR_SYMB star_symb;
extern PTR_SYMB global_list;
extern PTR_TYPE global_bool;
extern PTR_TYPE global_int;
extern PTR_TYPE global_float;
extern PTR_TYPE global_double;
extern PTR_TYPE global_char;
extern PTR_TYPE global_string;
extern PTR_TYPE global_string_2;
extern PTR_TYPE global_complex;
extern PTR_TYPE global_dcomplex;
extern PTR_TYPE global_gate;
extern PTR_TYPE global_event;
extern PTR_TYPE global_sequence;
extern PTR_TYPE global_default;
extern PTR_LABEL thislabel;
extern PTR_CMNT comments, cur_comment;
extern PTR_BFND last_bfnd;
extern PTR_TYPE impltype[];
extern int nioctl;
extern int maxdim;
extern long yystno;	/* statement label */
extern char stmtbuf[];	/* input buffer */
extern char *commentbuf;	/* comments buffer from scanner */
extern PTR_BLOB head_blob;
extern PTR_BLOB cur_blob;
extern PTR_TYPE vartype; /* variable type */
extern int end_group;
extern char saveall;
extern int privateall;
extern int needkwd;
extern int implkwd;
extern int opt_kwd_hedr;
/* added for FORTRAN 90 */
extern PTR_LLND first_unresolved_call;
extern PTR_LLND last_unresolved_call;
extern int data_stat;
extern char yyquote;

extern int warn_all;
extern int statement_kind; /* kind of statement: 1 - HPF-DVM-directive, 0 - Fortran statement*/ 
int extend_flag = 0;

static int do_name_err;
static int ndim;	/* number of dimension */
/*!!! hpf */
static int explicit_shape; /*  1 if shape specification is explicit */
/* static int varleng;*/	/* variable size */
static int lastwasbranch = NO;	/* set if last stmt was a branch stmt */
static int thiswasbranch = NO;	/* set if this stmt is a branch stmt */
static PTR_SYMB type_var = SMNULL;
static PTR_LLND stat_alloc = LLNULL; /* set if ALLOCATE/DEALLOCATE stmt has STAT-clause*/
/* static int subscripts_status = 0; */
static int type_options,type_opt;   /* The various options used to declare a name -
                                      RECURSIVE, POINTER, OPTIONAL etc.         */
static PTR_BFND module_scope;
static int position = IN_OUTSIDE;            
static int attr_ndim;           /* number of dimensions in DIMENSION (array_spec)
                                   attribute declaration */
static PTR_LLND attr_dims;     /* low level representation of array_spec in
                                   DIMENSION (array_spec) attribute declarartion. */
static int in_vec = NO;	      /* set if processing array constructor */


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 422 "gram1.y"
{
    int token;
    char charv;
    char *charp;
    PTR_BFND bf_node;
    PTR_LLND ll_node;
    PTR_SYMB symbol;
    PTR_TYPE data_type;
    PTR_HASH hash_entry;
    PTR_LABEL label;
}
/* Line 187 of yacc.c.  */
#line 875 "gram1.tab.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */
#line 626 "gram1.y"

void add_scope_level();
void delete_beyond_scope_level();
PTR_HASH look_up_sym();
PTR_HASH just_look_up_sym();
PTR_HASH just_look_up_sym_in_scope();
PTR_HASH look_up_op();
PTR_SYMB make_constant();
PTR_SYMB make_scalar();
PTR_SYMB make_array();
PTR_SYMB make_pointer();
PTR_SYMB make_function();
PTR_SYMB make_external();
PTR_SYMB make_intrinsic();
PTR_SYMB make_procedure();
PTR_SYMB make_process();
PTR_SYMB make_program();
PTR_SYMB make_module();
PTR_SYMB make_common();
PTR_SYMB make_parallel_region();
PTR_SYMB make_derived_type();
PTR_SYMB make_local_entity();
PTR_SYMB make_global_entity();
PTR_TYPE make_type_node();
PTR_TYPE lookup_type(), make_type();
void     process_type();
void     process_interface();
void     bind();
void     late_bind_if_needed();
PTR_SYMB component();
PTR_SYMB lookup_type_symbol();
PTR_SYMB resolve_overloading();
PTR_BFND cur_scope();
PTR_BFND subroutine_call();
PTR_BFND process_call();
PTR_LLND deal_with_options();
PTR_LLND intrinsic_op_node();
PTR_LLND defined_op_node();
int is_substring_ref();
int is_array_section_ref();
PTR_LLND dim_expr(); 
PTR_BFND exit_stat();
PTR_BFND make_do();
PTR_BFND make_pardo();
PTR_BFND make_enddoall();
PTR_TYPE install_array(); 
PTR_SYMB install_entry(); 
void install_param_list();
PTR_LLND construct_entry_list();
void copy_sym_data();
PTR_LLND check_and_install();
PTR_HASH look_up();
PTR_BFND get_bfnd(); 
PTR_BLOB make_blob();
PTR_LABEL make_label();
PTR_LABEL make_label_node();
int is_interface_stat();
PTR_LLND make_llnd (); 
PTR_LLND make_llnd_label (); 
PTR_TYPE make_sa_type(); 
PTR_SYMB procedure_call();
PTR_BFND proc_list();
PTR_SYMB set_id_list();
PTR_LLND set_ll_list();
PTR_LLND add_to_lowLevelList(), add_to_lowList();
PTR_BFND set_stat_list() ;
PTR_BLOB follow_blob();
PTR_SYMB proc_decl_init();
PTR_CMNT make_comment();
PTR_HASH correct_symtab();
char *copyn();
char *convic();
char *StringConcatenation();
int atoi();
PTR_BFND make_logif();
PTR_BFND make_if();
PTR_BFND make_forall();
void startproc();
void match_parameters();
void make_else();
void make_elseif();
void make_endif();
void make_elsewhere();
void make_elsewhere_mask();
void make_endwhere();
void make_endforall();
void make_endselect();
void make_extend();
void make_endextend();
void make_section();
void make_section_extend();
void doinclude();
void endproc();
void err();
void execerr();
void flline();
void warn();
void warn1();
void newprog();
void set_type();
void dclerr();
void enddcl();
void install_const();
void setimpl();
void copy_module_scope();
void delete_symbol();
void replace_symbol_in_expr();
long convci();
void set_expr_type();
void errstr();
void yyerror();
void set_blobs();
void make_loop();
void startioctl();
void endioctl();
void redefine_func_arg_type();
int isResultVar();

/* used by FORTRAN M */
PTR_BFND make_processdo();
PTR_BFND make_processes();
PTR_BFND make_endprocesses();

PTR_BFND make_endparallel();/*OMP*/
PTR_BFND make_parallel();/*OMP*/
PTR_BFND make_endsingle();/*OMP*/
PTR_BFND make_single();/*OMP*/
PTR_BFND make_endmaster();/*OMP*/
PTR_BFND make_master();/*OMP*/
PTR_BFND make_endordered();/*OMP*/
PTR_BFND make_ordered();/*OMP*/
PTR_BFND make_endcritical();/*OMP*/
PTR_BFND make_critical();/*OMP*/
PTR_BFND make_endsections();/*OMP*/
PTR_BFND make_sections();/*OMP*/
PTR_BFND make_ompsection();/*OMP*/
PTR_BFND make_endparallelsections();/*OMP*/
PTR_BFND make_parallelsections();/*OMP*/
PTR_BFND make_endworkshare();/*OMP*/
PTR_BFND make_workshare();/*OMP*/
PTR_BFND make_endparallelworkshare();/*OMP*/
PTR_BFND make_parallelworkshare();/*OMP*/



/* Line 216 of yacc.c.  */
#line 1032 "gram1.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   5535

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  344
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  521
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1258
/* YYNRULES -- Number of states.  */
#define YYNSTATES  2479

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   343

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint16 yytranslate[] =
{
       0,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,   150,   151,
     152,   153,   154,   155,   156,   157,   158,   159,   160,   161,
     162,   163,   164,   165,   166,   167,   168,   169,   170,   171,
     172,   173,   174,   175,   176,   177,   178,   179,   180,   181,
     182,   183,   184,   185,   186,   187,   188,   189,   190,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,   201,
     202,   203,   204,   205,   206,   207,   208,   209,   210,   211,
     212,   213,   214,   215,   216,   217,   218,   219,   220,   221,
     222,   223,   224,   225,   226,   227,   228,   229,   230,   231,
     232,   233,   234,   235,   236,   237,   238,   239,   240,   241,
     242,   243,   244,   245,   246,   247,   248,   249,   250,   251,
     252,   253,   254,   255,   256,   257,   258,   259,   260,   261,
     262,   263,   264,   265,   266,   267,   268,   269,   270,   271,
     272,   273,   274,   275,   276,   277,   278,   279,   280,   281,
     282,   283,   284,   285,   286,   287,   288,   289,   290,   291,
     292,   293,   294,   295,   296,   297,   298,   299,   300,   301,
     302,   303,   304,   305,   306,   307,   308,   309,   310,   311,
     312,   313,   314,   315,   316,   317,   318,   319,   320,   321,
     322,   323,   324,   325,   326,   327,   328,   329,   330,   331,
     332,   333,   334,   335,   336,   337,   338,   339,   340,   341,
       1,     2,   342,   343
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     8,    12,    16,    20,    24,    27,
      29,    31,    33,    37,    41,    46,    52,    58,    62,    67,
      71,    72,    75,    78,    81,    83,    85,    90,    96,   101,
     107,   110,   116,   118,   119,   121,   122,   124,   125,   128,
     132,   134,   138,   140,   142,   144,   145,   146,   147,   149,
     151,   153,   155,   157,   159,   161,   163,   165,   167,   169,
     171,   173,   176,   181,   184,   190,   192,   194,   196,   198,
     200,   202,   204,   206,   208,   210,   212,   214,   217,   221,
     227,   233,   236,   238,   240,   242,   244,   246,   248,   250,
     252,   254,   256,   258,   260,   262,   264,   266,   268,   270,
     272,   274,   276,   278,   283,   291,   294,   298,   306,   313,
     314,   317,   323,   325,   330,   332,   334,   336,   339,   341,
     346,   348,   350,   352,   354,   356,   358,   361,   364,   367,
     369,   371,   379,   383,   388,   392,   397,   401,   404,   410,
     411,   414,   417,   423,   424,   429,   435,   436,   439,   443,
     445,   447,   449,   451,   453,   455,   457,   459,   461,   463,
     464,   466,   472,   479,   486,   487,   489,   495,   505,   507,
     509,   512,   515,   516,   517,   520,   523,   529,   534,   539,
     543,   548,   552,   557,   561,   565,   570,   576,   580,   585,
     591,   595,   599,   601,   605,   608,   613,   617,   622,   626,
     630,   634,   638,   642,   646,   648,   653,   655,   657,   662,
     666,   667,   668,   673,   675,   679,   681,   685,   688,   692,
     696,   701,   704,   705,   707,   709,   713,   719,   721,   725,
     726,   728,   736,   738,   742,   745,   748,   752,   754,   756,
     761,   765,   768,   770,   772,   774,   776,   780,   782,   786,
     788,   790,   797,   799,   801,   804,   807,   809,   813,   815,
     818,   821,   823,   827,   829,   833,   839,   841,   843,   845,
     848,   851,   855,   859,   861,   865,   869,   871,   875,   877,
     879,   883,   885,   889,   891,   893,   897,   903,   904,   905,
     907,   912,   917,   919,   923,   927,   930,   932,   936,   940,
     947,   954,   962,   964,   966,   970,   972,   974,   976,   980,
     984,   985,   989,   990,   993,   997,   999,  1001,  1004,  1008,
    1010,  1012,  1014,  1018,  1020,  1024,  1026,  1028,  1032,  1037,
    1038,  1041,  1044,  1046,  1048,  1052,  1054,  1058,  1060,  1061,
    1062,  1063,  1066,  1067,  1069,  1071,  1073,  1076,  1079,  1084,
    1086,  1090,  1092,  1096,  1098,  1100,  1102,  1104,  1108,  1112,
    1116,  1120,  1124,  1127,  1130,  1133,  1137,  1141,  1145,  1149,
    1153,  1157,  1161,  1165,  1169,  1173,  1177,  1180,  1184,  1188,
    1190,  1192,  1194,  1196,  1198,  1200,  1206,  1213,  1218,  1224,
    1228,  1230,  1232,  1238,  1243,  1246,  1247,  1249,  1255,  1256,
    1258,  1260,  1264,  1266,  1270,  1273,  1275,  1277,  1279,  1281,
    1283,  1285,  1289,  1293,  1299,  1301,  1303,  1307,  1310,  1316,
    1321,  1326,  1330,  1333,  1335,  1336,  1337,  1344,  1346,  1348,
    1350,  1355,  1361,  1363,  1368,  1374,  1375,  1377,  1381,  1383,
    1385,  1387,  1390,  1394,  1398,  1401,  1403,  1406,  1409,  1412,
    1416,  1424,  1428,  1432,  1434,  1437,  1440,  1442,  1445,  1449,
    1451,  1453,  1455,  1461,  1469,  1470,  1477,  1482,  1494,  1508,
    1513,  1517,  1521,  1529,  1538,  1542,  1544,  1547,  1550,  1554,
    1556,  1560,  1561,  1563,  1564,  1566,  1568,  1571,  1577,  1584,
    1586,  1590,  1594,  1595,  1598,  1600,  1606,  1614,  1615,  1617,
    1621,  1625,  1632,  1638,  1645,  1650,  1656,  1662,  1665,  1667,
    1669,  1680,  1682,  1686,  1691,  1695,  1699,  1703,  1707,  1714,
    1721,  1727,  1736,  1739,  1743,  1747,  1755,  1763,  1764,  1766,
    1771,  1774,  1779,  1781,  1784,  1787,  1789,  1791,  1792,  1793,
    1794,  1797,  1800,  1803,  1806,  1809,  1811,  1814,  1817,  1821,
    1826,  1829,  1833,  1835,  1839,  1843,  1845,  1847,  1849,  1853,
    1855,  1857,  1862,  1868,  1870,  1872,  1876,  1880,  1882,  1887,
    1889,  1891,  1893,  1896,  1899,  1902,  1904,  1908,  1912,  1917,
    1922,  1924,  1928,  1930,  1936,  1938,  1940,  1942,  1946,  1950,
    1954,  1958,  1962,  1966,  1968,  1972,  1978,  1984,  1990,  1991,
    1992,  1994,  1998,  2000,  2002,  2006,  2010,  2014,  2018,  2021,
    2025,  2029,  2030,  2032,  2034,  2036,  2038,  2040,  2042,  2044,
    2046,  2048,  2050,  2052,  2054,  2056,  2058,  2060,  2062,  2064,
    2066,  2068,  2070,  2072,  2074,  2076,  2078,  2080,  2082,  2084,
    2086,  2088,  2090,  2092,  2094,  2096,  2098,  2100,  2102,  2104,
    2106,  2108,  2110,  2112,  2114,  2116,  2118,  2120,  2122,  2124,
    2126,  2128,  2130,  2132,  2134,  2136,  2138,  2140,  2142,  2144,
    2146,  2148,  2150,  2152,  2154,  2156,  2158,  2162,  2166,  2169,
    2173,  2175,  2179,  2181,  2185,  2187,  2191,  2193,  2198,  2202,
    2204,  2208,  2210,  2214,  2219,  2221,  2226,  2231,  2236,  2240,
    2244,  2246,  2250,  2254,  2256,  2260,  2264,  2266,  2270,  2274,
    2276,  2280,  2281,  2287,  2294,  2303,  2305,  2309,  2311,  2313,
    2315,  2320,  2322,  2323,  2326,  2330,  2333,  2338,  2339,  2341,
    2347,  2352,  2359,  2364,  2366,  2371,  2376,  2378,  2385,  2387,
    2391,  2393,  2397,  2399,  2404,  2406,  2408,  2412,  2414,  2416,
    2420,  2422,  2423,  2425,  2428,  2432,  2434,  2437,  2443,  2448,
    2453,  2460,  2462,  2466,  2468,  2470,  2477,  2482,  2484,  2488,
    2490,  2492,  2494,  2496,  2498,  2502,  2504,  2506,  2508,  2515,
    2520,  2522,  2527,  2529,  2531,  2533,  2535,  2540,  2543,  2551,
    2553,  2558,  2560,  2562,  2574,  2575,  2578,  2582,  2584,  2588,
    2590,  2594,  2596,  2600,  2602,  2606,  2608,  2612,  2614,  2618,
    2627,  2629,  2633,  2636,  2639,  2647,  2649,  2653,  2655,  2659,
    2661,  2666,  2668,  2672,  2674,  2676,  2677,  2679,  2681,  2684,
    2686,  2688,  2690,  2692,  2694,  2696,  2698,  2700,  2702,  2704,
    2713,  2720,  2729,  2736,  2738,  2745,  2752,  2759,  2766,  2768,
    2772,  2778,  2780,  2784,  2793,  2800,  2807,  2812,  2818,  2824,
    2827,  2830,  2831,  2833,  2837,  2839,  2844,  2852,  2854,  2858,
    2862,  2864,  2868,  2874,  2878,  2882,  2884,  2888,  2890,  2892,
    2896,  2900,  2904,  2908,  2919,  2928,  2939,  2940,  2941,  2943,
    2946,  2951,  2956,  2963,  2965,  2967,  2969,  2971,  2973,  2975,
    2977,  2979,  2981,  2983,  2985,  2992,  2997,  3002,  3006,  3016,
    3018,  3020,  3024,  3026,  3032,  3038,  3048,  3049,  3051,  3053,
    3057,  3061,  3065,  3069,  3073,  3080,  3084,  3088,  3092,  3096,
    3104,  3110,  3112,  3114,  3118,  3123,  3125,  3127,  3131,  3133,
    3135,  3139,  3143,  3146,  3150,  3155,  3160,  3166,  3172,  3174,
    3177,  3182,  3187,  3192,  3193,  3195,  3198,  3206,  3213,  3217,
    3221,  3229,  3235,  3237,  3241,  3243,  3248,  3251,  3255,  3259,
    3264,  3271,  3275,  3278,  3282,  3284,  3286,  3291,  3297,  3301,
    3308,  3311,  3316,  3319,  3321,  3325,  3329,  3330,  3332,  3336,
    3339,  3342,  3345,  3348,  3358,  3364,  3366,  3370,  3373,  3376,
    3379,  3389,  3394,  3396,  3400,  3402,  3404,  3407,  3408,  3416,
    3418,  3423,  3425,  3429,  3431,  3433,  3435,  3452,  3453,  3457,
    3461,  3465,  3469,  3476,  3486,  3492,  3494,  3498,  3504,  3506,
    3510,  3512,  3514,  3516,  3518,  3520,  3522,  3524,  3526,  3528,
    3530,  3532,  3534,  3536,  3538,  3540,  3542,  3544,  3546,  3548,
    3550,  3552,  3554,  3556,  3558,  3560,  3562,  3564,  3567,  3570,
    3575,  3579,  3584,  3590,  3592,  3594,  3596,  3598,  3600,  3602,
    3604,  3606,  3608,  3614,  3617,  3620,  3623,  3626,  3629,  3635,
    3637,  3639,  3641,  3646,  3651,  3656,  3661,  3663,  3665,  3667,
    3669,  3671,  3673,  3675,  3677,  3679,  3681,  3683,  3685,  3687,
    3689,  3691,  3696,  3700,  3705,  3711,  3713,  3715,  3717,  3719,
    3724,  3728,  3731,  3736,  3740,  3745,  3749,  3754,  3760,  3762,
    3764,  3766,  3768,  3770,  3772,  3774,  3782,  3788,  3790,  3792,
    3794,  3796,  3801,  3805,  3810,  3816,  3818,  3820,  3825,  3829,
    3834,  3840,  3842,  3844,  3847,  3849,  3852,  3857,  3861,  3866,
    3870,  3875,  3881,  3883,  3885,  3887,  3889,  3891,  3893,  3895,
    3897,  3899,  3901,  3903,  3906,  3911,  3915,  3918,  3923,  3927,
    3930,  3934,  3937,  3940,  3943,  3946,  3949,  3952,  3956,  3959,
    3965,  3968,  3974,  3977,  3983,  3985,  3987,  3991,  3995,  3996,
    3997,  3999,  4001,  4003,  4005,  4007,  4009,  4013,  4016,  4022,
    4027,  4030,  4036,  4041,  4044,  4047,  4049,  4051,  4055,  4058,
    4061,  4064,  4069,  4074,  4079,  4084,  4089,  4094,  4096,  4098,
    4100,  4104,  4107,  4110,  4112,  4114,  4116,  4118,  4120,  4122,
    4124,  4129,  4134,  4139,  4142,  4144,  4146,  4150,  4152,  4154,
    4160,  4166,  4168,  4172,  4174,  4176,  4178,  4184,  4190,  4196,
    4198,  4202,  4205,  4211,  4214,  4220,  4226,  4228,  4230
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     345,     0,    -1,    -1,   345,   346,   116,    -1,   347,   348,
     565,    -1,   347,   365,   565,    -1,   347,   510,   565,    -1,
     347,   133,   361,    -1,   347,   247,    -1,   257,    -1,     1,
      -1,   150,    -1,   193,   349,   356,    -1,    57,   349,   357,
      -1,   233,   349,   351,   358,    -1,   350,   233,   349,   351,
     358,    -1,   124,   349,   352,   358,   354,    -1,   353,   358,
     354,    -1,   114,   355,   358,   354,    -1,   164,   349,   355,
      -1,    -1,   202,   362,    -1,   195,   362,    -1,    95,   362,
      -1,   355,    -1,   355,    -1,   386,   124,   349,   355,    -1,
     386,   350,   124,   349,   355,    -1,   350,   124,   349,   355,
      -1,   350,   386,   124,   349,   355,    -1,   362,   363,    -1,
     362,   213,    15,   355,    21,    -1,   129,    -1,    -1,   355,
      -1,    -1,   355,    -1,    -1,    15,    21,    -1,    15,   359,
      21,    -1,   360,    -1,   359,     8,   360,    -1,   355,    -1,
       5,    -1,    64,    -1,    -1,    -1,    -1,   370,    -1,   371,
      -1,   372,    -1,   402,    -1,   398,    -1,   566,    -1,   407,
      -1,   408,    -1,   409,    -1,   467,    -1,   388,    -1,   403,
      -1,   413,    -1,   216,   477,    -1,   216,   477,   478,   444,
      -1,   123,   476,    -1,   183,   477,    15,   450,    21,    -1,
     378,    -1,   379,    -1,   384,    -1,   381,    -1,   383,    -1,
     399,    -1,   400,    -1,   401,    -1,   366,    -1,   454,    -1,
     452,    -1,   380,    -1,   142,   477,    -1,   142,   477,   355,
      -1,   141,   477,    15,   368,    21,    -1,   140,   477,    15,
      26,    21,    -1,   107,   519,    -1,    10,    -1,   367,    -1,
     369,    -1,    17,    -1,    16,    -1,     5,    -1,     9,    -1,
      37,    -1,    23,    -1,    22,    -1,    35,    -1,    38,    -1,
      34,    -1,    25,    -1,    32,    -1,    29,    -1,    28,    -1,
      31,    -1,    30,    -1,    33,    -1,    24,    -1,   245,   477,
     478,   355,    -1,   245,     8,   477,   362,   377,   478,   355,
      -1,   112,   477,    -1,   112,   477,   355,    -1,   386,   373,
     355,   477,   460,   392,   397,    -1,   372,     8,   355,   460,
     392,   397,    -1,    -1,     7,     7,    -1,     8,   362,   374,
       7,     7,    -1,   375,    -1,   374,     8,   362,   375,    -1,
     183,    -1,   377,    -1,    44,    -1,    87,   460,    -1,   119,
      -1,   145,    15,   376,    21,    -1,   143,    -1,   178,    -1,
     187,    -1,   216,    -1,   230,    -1,   236,    -1,   362,   148,
      -1,   362,   180,    -1,   362,   147,    -1,   194,    -1,   191,
      -1,   145,   477,    15,   376,    21,   478,   355,    -1,   378,
       8,   355,    -1,   178,   477,   478,   355,    -1,   379,     8,
     355,    -1,   230,   477,   478,   406,    -1,   380,     8,   406,
      -1,   191,   477,    -1,   191,   477,   478,   382,   446,    -1,
      -1,   219,   477,    -1,   194,   477,    -1,   194,   477,   478,
     385,   446,    -1,    -1,   390,   387,   394,   387,    -1,   244,
      15,   355,    21,   387,    -1,    -1,   389,   355,    -1,   388,
       8,   355,    -1,    13,    -1,     6,    -1,   391,    -1,   144,
      -1,   200,    -1,    68,    -1,    90,    -1,    91,    -1,   154,
      -1,    63,    -1,    -1,   393,    -1,     5,   540,   497,   541,
     387,    -1,     5,   540,    15,   541,     5,    21,    -1,     5,
     540,    15,   541,   483,    21,    -1,    -1,   393,    -1,    15,
     561,   395,   396,    21,    -1,    15,   561,   395,   396,     8,
     561,   395,   396,    21,    -1,   483,    -1,     5,    -1,   552,
     483,    -1,   552,     5,    -1,    -1,    -1,    26,   483,    -1,
      18,   483,    -1,    87,   478,   477,   355,   460,    -1,   398,
       8,   355,   460,    -1,    44,   477,   478,   406,    -1,   399,
       8,   406,    -1,   187,   477,   478,   406,    -1,   400,     8,
     406,    -1,   236,   477,   478,   406,    -1,   401,     8,   406,
      -1,    67,   477,   406,    -1,    67,   477,   405,   406,    -1,
     402,   534,   405,   534,   406,    -1,   402,     8,   406,    -1,
     167,   477,   404,   486,    -1,   403,   534,   404,   534,   486,
      -1,   403,     8,   486,    -1,    37,   355,    37,    -1,    23,
      -1,    37,   355,    37,    -1,   355,   460,    -1,   119,   477,
     478,   355,    -1,   407,     8,   355,    -1,   143,   477,   478,
     355,    -1,   408,     8,   355,    -1,   117,   477,   410,    -1,
     409,     8,   410,    -1,    15,   411,    21,    -1,   412,     8,
     412,    -1,   411,     8,   412,    -1,   355,    -1,   355,    15,
     482,    21,    -1,   491,    -1,   414,    -1,    80,   476,   415,
     417,    -1,   414,   534,   417,    -1,    -1,    -1,   418,    37,
     419,    37,    -1,   420,    -1,   418,     8,   420,    -1,   431,
      -1,   419,     8,   431,    -1,   421,   423,    -1,   421,   423,
     424,    -1,   421,   423,   425,    -1,   421,   423,   424,   425,
      -1,   421,   428,    -1,    -1,   355,    -1,   355,    -1,    15,
     426,    21,    -1,    15,   427,     7,   427,    21,    -1,   440,
      -1,   426,     8,   440,    -1,    -1,   440,    -1,    15,   429,
       8,   422,    26,   426,    21,    -1,   430,    -1,   429,     8,
     430,    -1,   423,   424,    -1,   423,   425,    -1,   423,   424,
     425,    -1,   428,    -1,   432,    -1,   421,   422,     5,   432,
      -1,   435,     5,   432,    -1,   421,   422,    -1,   434,    -1,
     436,    -1,   438,    -1,    36,    -1,    36,   246,   500,    -1,
      27,    -1,    27,   246,   500,    -1,    64,    -1,   433,    -1,
     421,   486,    15,   561,   479,    21,    -1,    59,    -1,   435,
      -1,    17,   435,    -1,    16,   435,    -1,   149,    -1,   149,
     246,   500,    -1,   437,    -1,    17,   437,    -1,    16,   437,
      -1,   201,    -1,   201,   246,   500,    -1,    92,    -1,    92,
     246,   500,    -1,    15,   439,     8,   439,    21,    -1,   436,
      -1,   434,    -1,   441,    -1,    17,   441,    -1,    16,   441,
      -1,   440,    17,   441,    -1,   440,    16,   441,    -1,   442,
      -1,   441,     5,   442,    -1,   441,    37,   442,    -1,   443,
      -1,   443,     9,   442,    -1,   149,    -1,   422,    -1,    15,
     440,    21,    -1,   445,    -1,   444,     8,   445,    -1,   406,
      -1,   405,    -1,   447,   449,   448,    -1,   446,     8,   447,
     449,   448,    -1,    -1,    -1,   355,    -1,   177,    15,   368,
      21,    -1,    47,    15,    26,    21,    -1,   451,    -1,   450,
       8,   451,    -1,   355,    26,   483,    -1,   163,   453,    -1,
     355,    -1,   453,     8,   355,    -1,   248,   477,   455,    -1,
     248,   477,   455,     8,   364,   458,    -1,   248,   477,   455,
       8,   364,   172,    -1,   248,   477,   455,     8,   364,   172,
     456,    -1,   355,    -1,   457,    -1,   456,     8,   457,    -1,
     459,    -1,   355,    -1,   459,    -1,   458,     8,   459,    -1,
     355,    18,   355,    -1,    -1,    15,   461,    21,    -1,    -1,
     462,   463,    -1,   461,     8,   463,    -1,   464,    -1,     7,
      -1,   483,     7,    -1,   483,     7,   464,    -1,     5,    -1,
     483,    -1,   466,    -1,   465,     8,   466,    -1,   149,    -1,
     130,   477,   468,    -1,   131,    -1,   469,    -1,   468,     8,
     469,    -1,   470,    15,   473,    21,    -1,    -1,   471,   472,
      -1,   231,   391,    -1,   386,    -1,   474,    -1,   473,     8,
     474,    -1,   475,    -1,   475,    16,   475,    -1,   129,    -1,
      -1,    -1,    -1,     7,     7,    -1,    -1,   481,    -1,   483,
      -1,   501,    -1,   552,   483,    -1,   561,   480,    -1,   481,
       8,   561,   480,    -1,   483,    -1,   482,     8,   483,    -1,
     484,    -1,    15,   483,    21,    -1,   499,    -1,   487,    -1,
     495,    -1,   502,    -1,   483,    17,   483,    -1,   483,    16,
     483,    -1,   483,     5,   483,    -1,   483,    37,   483,    -1,
     483,     9,   483,    -1,   367,   483,    -1,    17,   483,    -1,
      16,   483,    -1,   483,    25,   483,    -1,   483,    29,   483,
      -1,   483,    31,   483,    -1,   483,    28,   483,    -1,   483,
      30,   483,    -1,   483,    32,   483,    -1,   483,    24,   483,
      -1,   483,    33,   483,    -1,   483,    38,   483,    -1,   483,
      35,   483,    -1,   483,    22,   483,    -1,    34,   483,    -1,
     483,    23,   483,    -1,   483,   367,   483,    -1,    17,    -1,
      16,    -1,   355,    -1,   486,    -1,   489,    -1,   488,    -1,
     486,    15,   561,   479,    21,    -1,   486,    15,   561,   479,
      21,   493,    -1,   489,    15,   479,    21,    -1,   489,    15,
     479,    21,   493,    -1,   487,     3,   129,    -1,   486,    -1,
     489,    -1,   486,    15,   561,   479,    21,    -1,   489,    15,
     479,    21,    -1,   486,   493,    -1,    -1,   493,    -1,    15,
     494,     7,   494,    21,    -1,    -1,   483,    -1,   496,    -1,
     496,   246,   500,    -1,   497,    -1,   497,   246,   500,    -1,
     498,   492,    -1,    36,    -1,    27,    -1,   201,    -1,    92,
      -1,   149,    -1,    64,    -1,   486,   246,    64,    -1,   497,
     246,    64,    -1,    15,   484,     8,   484,    21,    -1,   486,
      -1,   497,    -1,   483,     7,   483,    -1,   483,     7,    -1,
     483,     7,   483,     7,   483,    -1,   483,     7,     7,   483,
      -1,     7,   483,     7,   483,    -1,     7,     7,   483,    -1,
       7,   483,    -1,     7,    -1,    -1,    -1,    14,   396,   503,
     558,   504,    20,    -1,   486,    -1,   489,    -1,   490,    -1,
     506,     8,   561,   490,    -1,   506,     8,   561,   552,   486,
      -1,   505,    -1,   507,     8,   561,   505,    -1,   507,     8,
     561,   552,   486,    -1,    -1,   486,    -1,   509,     8,   486,
      -1,   531,    -1,   530,    -1,   513,    -1,   521,   513,    -1,
     102,   539,   519,    -1,   103,   539,   518,    -1,   108,   519,
      -1,   511,    -1,   521,   511,    -1,   522,   531,    -1,   522,
     239,    -1,   521,   522,   239,    -1,    97,   539,    15,   483,
      21,   239,   518,    -1,    96,   539,   518,    -1,   106,   539,
     518,    -1,   514,    -1,    76,   539,    -1,   523,   531,    -1,
     523,    -1,   521,   523,    -1,   105,   539,   518,    -1,   567,
      -1,   830,    -1,   846,    -1,    89,   539,    15,   483,    21,
      -1,    89,   539,   540,   529,   541,   601,   512,    -1,    -1,
       8,   362,   254,    15,   483,    21,    -1,   254,    15,   483,
      21,    -1,   185,   539,   540,   529,   541,   534,   527,    26,
     483,     8,   483,    -1,   185,   539,   540,   529,   541,   534,
     527,    26,   483,     8,   483,     8,   483,    -1,    62,   539,
     515,   518,    -1,    84,   539,   518,    -1,   110,   539,   518,
      -1,   218,   539,   362,    62,    15,   483,    21,    -1,   521,
     218,   539,   362,    62,    15,   483,    21,    -1,    15,   517,
      21,    -1,   483,    -1,   483,     7,    -1,     7,   483,    -1,
     483,     7,   483,    -1,   516,    -1,   517,     8,   516,    -1,
      -1,   355,    -1,    -1,   355,    -1,    75,    -1,   520,     7,
      -1,   155,   539,    15,   483,    21,    -1,   122,   539,    15,
     524,   526,    21,    -1,   525,    -1,   524,     8,   525,    -1,
     527,    26,   501,    -1,    -1,     8,   483,    -1,   355,    -1,
     527,    26,   483,     8,   483,    -1,   527,    26,   483,     8,
     483,     8,   483,    -1,    -1,   149,    -1,   113,   539,   518,
      -1,    98,   539,   518,    -1,    98,   539,    15,   483,    21,
     518,    -1,   252,   539,    15,   483,    21,    -1,   521,   252,
     539,    15,   483,    21,    -1,   532,   483,    26,   483,    -1,
     188,   539,   487,    18,   483,    -1,    48,   539,   466,   240,
     355,    -1,    77,   539,    -1,   533,    -1,   542,    -1,    46,
     539,    15,   483,    21,   466,     8,   466,     8,   466,    -1,
     535,    -1,   535,    15,    21,    -1,   535,    15,   536,    21,
      -1,   214,   539,   494,    -1,   538,   539,   494,    -1,    79,
     539,   518,    -1,   115,   539,   518,    -1,    45,   539,    15,
     508,   506,    21,    -1,    81,   539,    15,   508,   507,    21,
      -1,   170,   539,    15,   509,    21,    -1,   253,   539,    15,
     483,    21,   487,    26,   483,    -1,   152,   416,    -1,   186,
     539,   466,    -1,    49,   539,   355,    -1,    49,   539,   355,
     534,    15,   465,    21,    -1,    69,   539,    15,   465,    21,
     534,   483,    -1,    -1,     8,    -1,    61,   539,   355,   561,
      -1,   561,   537,    -1,   536,     8,   561,   537,    -1,   483,
      -1,   552,   483,    -1,     5,   466,    -1,   184,    -1,   232,
      -1,    -1,    -1,    -1,   543,   549,    -1,   543,   564,    -1,
     543,     5,    -1,   543,     9,    -1,   545,   549,    -1,   547,
      -1,   553,   549,    -1,   553,   548,    -1,   553,   549,   556,
      -1,   553,   548,     8,   556,    -1,   554,   549,    -1,   554,
     549,   558,    -1,   555,    -1,   555,     8,   558,    -1,   544,
     539,   562,    -1,    53,    -1,   215,    -1,   104,    -1,   546,
     539,   562,    -1,   176,    -1,    66,    -1,   139,   539,   562,
     549,    -1,   139,   539,   562,   549,   558,    -1,   564,    -1,
       5,    -1,    15,   563,    21,    -1,    15,   550,    21,    -1,
     551,    -1,   550,     8,   561,   551,    -1,   563,    -1,     5,
      -1,     9,    -1,   552,   483,    -1,   552,     5,    -1,   552,
       9,    -1,   166,    -1,   197,   539,   562,    -1,   256,   539,
     562,    -1,   190,   539,   563,   562,    -1,   190,   539,     5,
     562,    -1,   557,    -1,   556,     8,   557,    -1,   487,    -1,
      15,   556,     8,   528,    21,    -1,   484,    -1,   560,    -1,
     559,    -1,   484,     8,   484,    -1,   484,     8,   560,    -1,
     560,     8,   484,    -1,   560,     8,   560,    -1,   559,     8,
     484,    -1,   559,     8,   560,    -1,   499,    -1,    15,   483,
      21,    -1,    15,   484,     8,   528,    21,    -1,    15,   560,
       8,   528,    21,    -1,    15,   559,     8,   528,    21,    -1,
      -1,    -1,   564,    -1,    15,   563,    21,    -1,   487,    -1,
     495,    -1,   563,   485,   563,    -1,   563,     5,   563,    -1,
     563,    37,   563,    -1,   563,     9,   563,    -1,   485,   563,
      -1,   563,    23,   563,    -1,   129,    26,   483,    -1,    -1,
     257,    -1,   568,    -1,   616,    -1,   591,    -1,   570,    -1,
     581,    -1,   576,    -1,   628,    -1,   631,    -1,   707,    -1,
     573,    -1,   582,    -1,   584,    -1,   586,    -1,   588,    -1,
     636,    -1,   642,    -1,   639,    -1,   767,    -1,   765,    -1,
     592,    -1,   617,    -1,   646,    -1,   696,    -1,   694,    -1,
     695,    -1,   697,    -1,   698,    -1,   699,    -1,   700,    -1,
     701,    -1,   709,    -1,   711,    -1,   716,    -1,   713,    -1,
     715,    -1,   719,    -1,   717,    -1,   718,    -1,   730,    -1,
     734,    -1,   735,    -1,   738,    -1,   737,    -1,   739,    -1,
     740,    -1,   741,    -1,   742,    -1,   645,    -1,   724,    -1,
     725,    -1,   726,    -1,   729,    -1,   743,    -1,   746,    -1,
     751,    -1,   756,    -1,   758,    -1,   759,    -1,   760,    -1,
     761,    -1,   763,    -1,   722,    -1,   766,    -1,    82,   477,
     569,    -1,   568,     8,   569,    -1,   355,   460,    -1,    94,
     477,   571,    -1,   572,    -1,   571,     8,   572,    -1,   355,
      -1,   138,   477,   574,    -1,   575,    -1,   574,     8,   575,
      -1,   355,    -1,   228,   477,   580,   577,    -1,    15,   578,
      21,    -1,   579,    -1,   578,     8,   579,    -1,   483,    -1,
     483,     7,   483,    -1,     7,    15,   482,    21,    -1,   355,
      -1,   259,   477,   355,   460,    -1,   303,   477,   355,   460,
      -1,   581,     8,   355,   460,    -1,   136,   477,   583,    -1,
     582,     8,   583,    -1,   355,    -1,   211,   477,   585,    -1,
     584,     8,   585,    -1,   355,    -1,   205,   477,   587,    -1,
     586,     8,   587,    -1,   355,    -1,    70,   477,   589,    -1,
     588,     8,   589,    -1,   355,    -1,   175,   355,   460,    -1,
      -1,    88,   477,   595,   598,   590,    -1,   204,   539,   595,
     599,   601,   590,    -1,   204,   539,   599,   601,   590,     7,
       7,   593,    -1,   594,    -1,   593,     8,   594,    -1,   595,
      -1,   596,    -1,   355,    -1,   355,    15,   482,    21,    -1,
     355,    -1,    -1,   599,   601,    -1,    15,   600,    21,    -1,
     601,   602,    -1,   600,     8,   601,   602,    -1,    -1,    58,
      -1,    58,    15,   561,   615,    21,    -1,   126,    15,   603,
      21,    -1,   258,    15,   603,     8,   483,    21,    -1,   165,
      15,   483,    21,    -1,     5,    -1,   137,    15,   603,    21,
      -1,    86,    15,   604,    21,    -1,   355,    -1,    15,   605,
      21,   362,   255,   607,    -1,   606,    -1,   605,     8,   606,
      -1,   483,    -1,   483,     7,   483,    -1,   608,    -1,   608,
      15,   609,    21,    -1,   355,    -1,   610,    -1,   609,     8,
     610,    -1,   483,    -1,   755,    -1,    40,   611,   612,    -1,
     355,    -1,    -1,   613,    -1,    17,   614,    -1,   612,    17,
     614,    -1,   483,    -1,   552,   483,    -1,   552,   483,     8,
     552,   483,    -1,    43,   477,   619,   621,    -1,   199,   539,
     620,   621,    -1,   199,   539,   621,     7,     7,   618,    -1,
     620,    -1,   618,     8,   620,    -1,   355,    -1,   486,    -1,
      15,   626,    21,   362,   255,   622,    -1,   625,    15,   623,
      21,    -1,   624,    -1,   623,     8,   624,    -1,   483,    -1,
       5,    -1,   501,    -1,   355,    -1,   627,    -1,   626,     8,
     627,    -1,   355,    -1,     5,    -1,     7,    -1,   629,     7,
       7,   477,   355,   460,    -1,   628,     8,   355,   460,    -1,
     630,    -1,   629,     8,   362,   630,    -1,    82,    -1,   259,
      -1,   303,    -1,    94,    -1,    87,    15,   461,    21,    -1,
     228,   577,    -1,    43,    15,   626,    21,   362,   255,   622,
      -1,    43,    -1,    88,   599,   601,   590,    -1,    88,    -1,
      67,    -1,   386,     8,   362,    93,   477,    15,   632,    21,
       7,     7,   634,    -1,    -1,   633,     7,    -1,   632,     8,
       7,    -1,   635,    -1,   634,     8,   635,    -1,   355,    -1,
     127,   477,   637,    -1,   638,    -1,   637,     8,   638,    -1,
     355,    -1,    74,   477,   640,    -1,   641,    -1,   640,     8,
     641,    -1,   355,    -1,    51,   477,   643,    -1,    51,   477,
       8,   362,    67,     7,     7,   643,    -1,   644,    -1,   643,
       8,   644,    -1,   355,   460,    -1,   168,   539,    -1,   182,
     539,    15,   647,    21,   649,   653,    -1,   648,    -1,   647,
       8,   648,    -1,   486,    -1,   601,   173,   650,    -1,   601,
      -1,   486,    15,   651,    21,    -1,   652,    -1,   651,     8,
     652,    -1,   483,    -1,     5,    -1,    -1,   654,    -1,   655,
      -1,   654,   655,    -1,   659,    -1,   679,    -1,   687,    -1,
     656,    -1,   664,    -1,   666,    -1,   665,    -1,   657,    -1,
     660,    -1,   661,    -1,     8,   362,   209,    15,   702,     7,
     703,    21,    -1,     8,   362,   209,    15,   703,    21,    -1,
       8,   362,    71,    15,   658,     7,   703,    21,    -1,     8,
     362,    71,    15,   703,    21,    -1,   355,    -1,     8,   362,
     169,    15,   663,    21,    -1,     8,   362,   282,    15,   663,
      21,    -1,     8,   362,   191,    15,   663,    21,    -1,     8,
     362,   320,    15,   662,    21,    -1,   483,    -1,   483,     8,
     483,    -1,   483,     8,   483,     8,   483,    -1,   487,    -1,
     663,     8,   487,    -1,     8,   362,   135,    15,   702,     7,
     720,    21,    -1,     8,   362,   135,    15,   720,    21,    -1,
       8,   362,   229,    15,   483,    21,    -1,     8,   362,    41,
     667,    -1,     8,   362,    41,   667,   667,    -1,    15,   601,
     668,   669,    21,    -1,   148,     7,    -1,   180,     7,    -1,
      -1,   670,    -1,   669,     8,   670,    -1,   692,    -1,   692,
      15,   671,    21,    -1,   692,    15,   671,    21,    15,   673,
      21,    -1,   672,    -1,   671,     8,   672,    -1,   483,     7,
     483,    -1,   674,    -1,   673,     8,   674,    -1,   675,     7,
     676,     7,   677,    -1,   675,     7,   676,    -1,   675,     7,
     677,    -1,   675,    -1,   676,     7,   677,    -1,   676,    -1,
     677,    -1,   362,   217,   678,    -1,   362,   157,   678,    -1,
     362,   128,   678,    -1,    15,   481,    21,    -1,     8,   362,
     208,    15,   680,   684,   681,     8,   683,    21,    -1,     8,
     362,   208,    15,   680,   684,   681,    21,    -1,     8,   362,
     208,    15,   680,   682,   681,     7,   683,    21,    -1,    -1,
      -1,   355,    -1,   362,   684,    -1,   683,     8,   362,   684,
      -1,   685,    15,   487,    21,    -1,   686,    15,   663,     8,
     483,    21,    -1,   234,    -1,   192,    -1,   162,    -1,   159,
      -1,    35,    -1,    22,    -1,    24,    -1,    33,    -1,   247,
      -1,   158,    -1,   161,    -1,     8,   362,   223,    15,   689,
      21,    -1,     8,   362,   224,   688,    -1,     8,   362,   226,
     688,    -1,     8,   362,   221,    -1,     8,   362,   221,    15,
     692,    15,   578,    21,    21,    -1,   355,    -1,   690,    -1,
     689,     8,   690,    -1,   692,    -1,   692,    15,   691,    78,
      21,    -1,   692,    15,   691,   578,    21,    -1,   692,    15,
     691,   578,    21,    15,   362,    78,    21,    -1,    -1,   486,
      -1,   692,    -1,   693,     8,   692,    -1,   225,   539,   688,
      -1,   224,   539,   688,    -1,   227,   539,   688,    -1,   226,
     539,   688,    -1,   222,   539,   688,    15,   689,    21,    -1,
     206,   539,   682,    -1,   207,   539,   682,    -1,    72,   539,
     658,    -1,    73,   539,   658,    -1,   210,   539,    15,   702,
       7,   703,    21,    -1,   210,   539,    15,   703,    21,    -1,
     355,    -1,   704,    -1,   703,     8,   704,    -1,   692,    15,
     705,    21,    -1,   692,    -1,   706,    -1,   705,     8,   706,
      -1,   483,    -1,     7,    -1,   237,   477,   708,    -1,   707,
       8,   708,    -1,   355,   460,    -1,   238,   539,   710,    -1,
     238,   539,   710,   679,    -1,   238,   539,   710,   657,    -1,
     238,   539,   710,   679,   657,    -1,   238,   539,   710,   657,
     679,    -1,   355,    -1,   111,   539,    -1,   710,    15,   483,
      21,    -1,   710,    15,   501,    21,    -1,   174,   539,   488,
     714,    -1,    -1,   659,    -1,   109,   539,    -1,   160,   539,
     712,   362,   175,   597,   460,    -1,   160,   539,   712,   362,
     321,   487,    -1,   189,   539,   702,    -1,   212,   539,   702,
      -1,   135,   539,    15,   702,     7,   720,    21,    -1,   135,
     539,    15,   720,    21,    -1,   721,    -1,   720,     8,   721,
      -1,   692,    -1,   692,    15,   482,    21,    -1,   134,   539,
      -1,   134,   539,   659,    -1,   134,   539,   723,    -1,   134,
     539,   659,   723,    -1,     8,   362,   208,    15,   663,    21,
      -1,    50,   539,   728,    -1,    99,   539,    -1,    52,   539,
     728,    -1,   355,    -1,   727,    -1,   727,    15,   482,    21,
      -1,   120,   539,   487,    26,   487,    -1,    83,   539,   733,
      -1,    83,   539,   733,    15,   731,    21,    -1,   561,   732,
      -1,   731,     8,   561,   732,    -1,   552,   483,    -1,   149,
      -1,   100,   539,   733,    -1,   146,   539,   736,    -1,    -1,
     483,    -1,   330,   539,   482,    -1,   101,   539,    -1,   241,
     539,    -1,   242,   539,    -1,    56,   539,    -1,    65,   539,
     561,    15,   536,    21,   396,   478,   663,    -1,   322,   539,
      15,   744,    21,    -1,   745,    -1,   744,     8,   745,    -1,
     362,   315,    -1,   362,   318,    -1,   362,   182,    -1,   220,
     539,    15,   747,    26,   614,    21,   601,   750,    -1,   486,
      15,   748,    21,    -1,   749,    -1,   748,     8,   749,    -1,
     604,    -1,   755,    -1,   132,   693,    -1,    -1,   153,   539,
      15,   486,    18,   752,    21,    -1,   486,    -1,   486,    15,
     753,    21,    -1,   754,    -1,   753,     8,   754,    -1,   755,
      -1,     7,    -1,     5,    -1,   323,   539,   483,     8,   362,
     328,    15,   663,    21,     8,   362,   327,    15,   482,    21,
     757,    -1,    -1,     8,   362,   182,    -1,     8,   362,   318,
      -1,   324,   539,   483,    -1,   325,   539,   483,    -1,   325,
     539,   483,     8,   362,   315,    -1,   326,   539,   483,     8,
     362,   329,    15,   486,    21,    -1,   331,   539,    15,   762,
      21,    -1,   490,    -1,   762,     8,   490,    -1,   332,   539,
      15,   764,    21,    -1,   486,    -1,   764,     8,   486,    -1,
     816,    -1,   769,    -1,   768,    -1,   786,    -1,   789,    -1,
     790,    -1,   791,    -1,   792,    -1,   798,    -1,   801,    -1,
     806,    -1,   807,    -1,   808,    -1,   811,    -1,   812,    -1,
     813,    -1,   814,    -1,   815,    -1,   817,    -1,   818,    -1,
     819,    -1,   820,    -1,   821,    -1,   822,    -1,   823,    -1,
     824,    -1,   825,    -1,   268,   539,    -1,   275,   539,    -1,
     291,   539,   601,   770,    -1,   291,   539,   601,    -1,   534,
     601,   771,   601,    -1,   770,   534,   601,   771,   601,    -1,
     773,    -1,   782,    -1,   777,    -1,   778,    -1,   774,    -1,
     775,    -1,   776,    -1,   780,    -1,   781,    -1,   828,    15,
     829,   827,    21,    -1,   191,   772,    -1,   282,   772,    -1,
     285,   772,    -1,   265,   772,    -1,   299,   772,    -1,    84,
      15,   362,   779,    21,    -1,   191,    -1,   299,    -1,   288,
      -1,   304,    15,   483,    21,    -1,   289,    15,   483,    21,
      -1,   208,    15,   783,    21,    -1,   601,   785,     7,   784,
      -1,   663,    -1,    17,    -1,    16,    -1,     5,    -1,    37,
      -1,   162,    -1,   159,    -1,    35,    -1,    22,    -1,    24,
      -1,    33,    -1,   305,    -1,   306,    -1,   307,    -1,   247,
      -1,   297,   539,   601,   787,    -1,   297,   539,   601,    -1,
     534,   601,   788,   601,    -1,   787,   534,   601,   788,   601,
      -1,   773,    -1,   782,    -1,   774,    -1,   775,    -1,   279,
     539,   601,   805,    -1,   279,   539,   601,    -1,   296,   539,
      -1,   269,   539,   601,   793,    -1,   269,   539,   601,    -1,
     272,   539,   601,   805,    -1,   272,   539,   601,    -1,   534,
     601,   794,   601,    -1,   793,   534,   601,   794,   601,    -1,
     773,    -1,   782,    -1,   774,    -1,   775,    -1,   796,    -1,
     795,    -1,   290,    -1,   298,    15,   362,   797,     8,   483,
      21,    -1,   298,    15,   362,   797,    21,    -1,   230,    -1,
      94,    -1,   284,    -1,   295,    -1,   300,   539,   601,   799,
      -1,   300,   539,   601,    -1,   534,   601,   800,   601,    -1,
     799,   534,   601,   800,   601,    -1,   773,    -1,   774,    -1,
     280,   539,   601,   802,    -1,   280,   539,   601,    -1,   534,
     601,   803,   601,    -1,   802,   534,   601,   803,   601,    -1,
     805,    -1,   804,    -1,   266,   772,    -1,   287,    -1,   302,
     539,    -1,   281,   539,   601,   805,    -1,   281,   539,   601,
      -1,   292,   539,   601,   809,    -1,   292,   539,   601,    -1,
     534,   601,   810,   601,    -1,   809,   534,   601,   810,   601,
      -1,   773,    -1,   782,    -1,   777,    -1,   778,    -1,   774,
      -1,   775,    -1,   776,    -1,   780,    -1,   781,    -1,   796,
      -1,   795,    -1,   276,   539,    -1,   293,   539,   601,   770,
      -1,   293,   539,   601,    -1,   277,   539,    -1,   294,   539,
     601,   770,    -1,   294,   539,   601,    -1,   278,   539,    -1,
     301,   477,   772,    -1,   286,   539,    -1,   273,   539,    -1,
     290,   539,    -1,   274,   539,    -1,   264,   539,    -1,   263,
     539,    -1,   283,   539,   772,    -1,   283,   539,    -1,   267,
     539,    15,   486,    21,    -1,   267,   539,    -1,   271,   539,
      15,   486,    21,    -1,   271,   539,    -1,    37,   355,   828,
      37,   829,    -1,   826,    -1,   486,    -1,   827,     8,   826,
      -1,   827,     8,   486,    -1,    -1,    -1,   831,    -1,   844,
      -1,   832,    -1,   845,    -1,   833,    -1,   834,    -1,   308,
     539,   835,    -1,   310,   539,    -1,   312,   539,    15,   841,
      21,    -1,   312,   539,    15,    21,    -1,   312,   539,    -1,
     313,   539,    15,   841,    21,    -1,   313,   539,    15,    21,
      -1,   313,   539,    -1,   362,   363,    -1,   836,    -1,   837,
      -1,   836,     8,   837,    -1,   362,   838,    -1,   362,   840,
      -1,   362,   839,    -1,   147,    15,   841,    21,    -1,   148,
      15,   841,    21,    -1,   180,    15,   841,    21,    -1,   318,
      15,   841,    21,    -1,   319,    15,   841,    21,    -1,   314,
      15,   842,    21,    -1,   315,    -1,   663,    -1,   843,    -1,
     842,     8,   843,    -1,   362,   316,    -1,   362,   317,    -1,
     309,    -1,   311,    -1,   847,    -1,   848,    -1,   849,    -1,
     850,    -1,   851,    -1,   333,    15,   852,    21,    -1,   334,
      15,   856,    21,    -1,   335,    15,   861,    21,    -1,   337,
     863,    -1,   338,    -1,   853,    -1,   852,     8,   853,    -1,
     854,    -1,   855,    -1,   362,   208,    15,   683,    21,    -1,
     362,   191,    15,   663,    21,    -1,   857,    -1,   856,     8,
     857,    -1,   858,    -1,   859,    -1,   860,    -1,   362,   228,
      15,   689,    21,    -1,   362,    41,    15,   689,    21,    -1,
     362,   209,    15,   703,    21,    -1,   862,    -1,   861,     8,
     862,    -1,   362,   336,    -1,   362,   340,    15,   647,    21,
      -1,   362,   339,    -1,   362,   339,    15,   647,    21,    -1,
     362,   341,    15,   864,    21,    -1,   355,    -1,   490,    -1,
     864,     8,   490,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   772,   772,   773,   777,   779,   793,   824,   833,   839,
     859,   868,   884,   896,   906,   913,   919,   924,   929,   953,
     980,   994,   996,   998,  1002,  1019,  1033,  1057,  1073,  1087,
    1105,  1107,  1114,  1118,  1119,  1126,  1127,  1135,  1136,  1138,
    1142,  1143,  1147,  1151,  1157,  1167,  1171,  1176,  1183,  1184,
    1185,  1186,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,
    1195,  1196,  1201,  1206,  1213,  1215,  1216,  1217,  1218,  1219,
    1220,  1221,  1222,  1223,  1224,  1225,  1226,  1229,  1233,  1241,
    1249,  1258,  1266,  1270,  1272,  1276,  1278,  1280,  1282,  1284,
    1286,  1288,  1290,  1292,  1294,  1296,  1298,  1300,  1302,  1304,
    1306,  1308,  1310,  1315,  1324,  1334,  1342,  1352,  1373,  1393,
    1394,  1396,  1400,  1402,  1406,  1410,  1412,  1416,  1422,  1426,
    1428,  1432,  1436,  1440,  1444,  1448,  1454,  1458,  1462,  1468,
    1473,  1480,  1491,  1504,  1515,  1528,  1538,  1551,  1556,  1563,
    1566,  1571,  1576,  1583,  1586,  1596,  1610,  1613,  1632,  1659,
    1661,  1673,  1681,  1682,  1683,  1684,  1685,  1686,  1687,  1692,
    1693,  1697,  1699,  1706,  1711,  1712,  1714,  1716,  1729,  1735,
    1741,  1750,  1759,  1772,  1773,  1776,  1780,  1795,  1810,  1828,
    1849,  1869,  1891,  1908,  1926,  1933,  1940,  1947,  1960,  1967,
    1974,  1985,  1989,  1991,  1996,  2014,  2025,  2037,  2049,  2063,
    2069,  2076,  2082,  2088,  2096,  2103,  2119,  2122,  2131,  2133,
    2137,  2141,  2161,  2165,  2167,  2171,  2172,  2175,  2177,  2179,
    2181,  2183,  2186,  2189,  2193,  2199,  2203,  2207,  2209,  2214,
    2215,  2219,  2223,  2225,  2229,  2231,  2233,  2238,  2242,  2244,
    2246,  2249,  2251,  2252,  2253,  2254,  2255,  2256,  2257,  2258,
    2261,  2262,  2268,  2271,  2272,  2274,  2278,  2279,  2282,  2283,
    2285,  2289,  2290,  2291,  2292,  2294,  2297,  2298,  2307,  2309,
    2316,  2323,  2330,  2339,  2341,  2343,  2347,  2349,  2353,  2362,
    2369,  2376,  2378,  2382,  2386,  2392,  2394,  2399,  2403,  2407,
    2414,  2421,  2431,  2433,  2437,  2449,  2452,  2461,  2474,  2480,
    2486,  2492,  2500,  2510,  2512,  2516,  2518,  2551,  2553,  2557,
    2596,  2597,  2601,  2601,  2606,  2610,  2618,  2627,  2636,  2646,
    2652,  2655,  2657,  2661,  2669,  2684,  2691,  2693,  2697,  2713,
    2713,  2717,  2719,  2731,  2733,  2737,  2743,  2755,  2767,  2784,
    2813,  2814,  2822,  2823,  2827,  2829,  2831,  2842,  2846,  2852,
    2854,  2858,  2860,  2862,  2866,  2868,  2872,  2874,  2876,  2878,
    2880,  2882,  2884,  2886,  2888,  2890,  2892,  2894,  2896,  2898,
    2900,  2902,  2904,  2906,  2908,  2910,  2912,  2914,  2916,  2920,
    2921,  2932,  3006,  3018,  3020,  3024,  3155,  3205,  3249,  3291,
    3349,  3351,  3353,  3392,  3435,  3446,  3447,  3451,  3456,  3457,
    3461,  3463,  3469,  3471,  3477,  3487,  3493,  3500,  3506,  3514,
    3522,  3538,  3548,  3561,  3568,  3570,  3593,  3595,  3597,  3599,
    3601,  3603,  3605,  3607,  3611,  3611,  3611,  3625,  3627,  3650,
    3652,  3654,  3670,  3672,  3674,  3688,  3691,  3693,  3701,  3703,
    3705,  3707,  3761,  3781,  3796,  3805,  3808,  3858,  3864,  3869,
    3887,  3889,  3891,  3893,  3895,  3898,  3904,  3906,  3908,  3911,
    3913,  3915,  3942,  3951,  3960,  3961,  3963,  3968,  3975,  3983,
    3985,  3989,  3992,  3994,  3998,  4004,  4006,  4008,  4010,  4014,
    4016,  4025,  4026,  4033,  4034,  4038,  4042,  4063,  4066,  4070,
    4072,  4079,  4084,  4085,  4096,  4113,  4136,  4161,  4162,  4169,
    4171,  4173,  4175,  4177,  4181,  4258,  4270,  4277,  4279,  4280,
    4282,  4291,  4298,  4305,  4313,  4318,  4323,  4326,  4329,  4332,
    4335,  4338,  4342,  4360,  4365,  4384,  4403,  4407,  4408,  4411,
    4415,  4420,  4427,  4429,  4431,  4435,  4436,  4447,  4462,  4466,
    4473,  4476,  4486,  4499,  4512,  4515,  4517,  4520,  4523,  4527,
    4536,  4539,  4543,  4545,  4551,  4555,  4557,  4559,  4566,  4570,
    4572,  4576,  4578,  4582,  4601,  4617,  4626,  4635,  4637,  4641,
    4667,  4682,  4697,  4714,  4722,  4731,  4739,  4744,  4749,  4771,
    4787,  4789,  4793,  4795,  4802,  4804,  4806,  4810,  4812,  4814,
    4816,  4818,  4820,  4824,  4827,  4830,  4836,  4842,  4851,  4855,
    4862,  4864,  4868,  4870,  4872,  4877,  4882,  4887,  4892,  4901,
    4906,  4912,  4913,  4928,  4929,  4930,  4931,  4932,  4933,  4934,
    4935,  4936,  4937,  4938,  4939,  4940,  4941,  4942,  4943,  4944,
    4945,  4946,  4949,  4950,  4951,  4952,  4953,  4954,  4955,  4956,
    4957,  4958,  4959,  4960,  4961,  4962,  4963,  4964,  4965,  4966,
    4967,  4968,  4969,  4970,  4971,  4972,  4973,  4974,  4975,  4976,
    4977,  4978,  4979,  4980,  4981,  4982,  4983,  4984,  4985,  4986,
    4987,  4988,  4989,  4990,  4991,  4992,  4996,  4998,  5009,  5030,
    5034,  5036,  5040,  5053,  5057,  5059,  5063,  5074,  5085,  5089,
    5091,  5095,  5097,  5099,  5114,  5126,  5146,  5166,  5188,  5194,
    5203,  5211,  5217,  5225,  5232,  5238,  5247,  5251,  5257,  5265,
    5279,  5293,  5298,  5314,  5329,  5357,  5359,  5363,  5365,  5369,
    5398,  5421,  5442,  5443,  5447,  5468,  5470,  5474,  5482,  5486,
    5491,  5493,  5495,  5497,  5503,  5505,  5509,  5519,  5523,  5525,
    5530,  5532,  5536,  5540,  5546,  5556,  5558,  5562,  5564,  5566,
    5573,  5591,  5592,  5596,  5598,  5602,  5609,  5619,  5648,  5663,
    5670,  5688,  5690,  5694,  5708,  5734,  5747,  5763,  5765,  5768,
    5770,  5776,  5780,  5808,  5810,  5814,  5822,  5828,  5831,  5889,
    5953,  5955,  5958,  5962,  5966,  5970,  5987,  5999,  6003,  6007,
    6017,  6022,  6027,  6034,  6043,  6043,  6054,  6065,  6067,  6071,
    6082,  6086,  6088,  6092,  6103,  6107,  6109,  6113,  6125,  6127,
    6134,  6136,  6140,  6156,  6164,  6175,  6177,  6181,  6184,  6187,
    6192,  6202,  6204,  6208,  6210,  6219,  6220,  6224,  6226,  6231,
    6232,  6233,  6234,  6235,  6236,  6237,  6238,  6239,  6240,  6243,
    6248,  6252,  6256,  6260,  6273,  6277,  6281,  6285,  6288,  6290,
    6292,  6296,  6298,  6302,  6307,  6311,  6315,  6317,  6321,  6329,
    6335,  6342,  6345,  6347,  6351,  6353,  6357,  6369,  6371,  6375,
    6379,  6381,  6385,  6387,  6389,  6391,  6393,  6395,  6397,  6401,
    6405,  6409,  6413,  6417,  6424,  6430,  6435,  6438,  6441,  6454,
    6456,  6460,  6462,  6467,  6473,  6479,  6485,  6491,  6497,  6503,
    6509,  6515,  6524,  6530,  6547,  6549,  6557,  6565,  6567,  6571,
    6575,  6577,  6581,  6583,  6591,  6595,  6607,  6610,  6628,  6630,
    6634,  6636,  6640,  6642,  6646,  6650,  6654,  6663,  6667,  6671,
    6676,  6680,  6692,  6694,  6698,  6703,  6707,  6709,  6713,  6715,
    6719,  6724,  6731,  6754,  6756,  6758,  6760,  6762,  6766,  6777,
    6781,  6796,  6803,  6810,  6811,  6815,  6819,  6827,  6831,  6835,
    6843,  6848,  6862,  6864,  6868,  6870,  6879,  6881,  6883,  6885,
    6921,  6925,  6929,  6933,  6937,  6949,  6951,  6955,  6958,  6960,
    6964,  6969,  6976,  6979,  6987,  6991,  6996,  6998,  7005,  7010,
    7014,  7018,  7022,  7026,  7030,  7033,  7035,  7039,  7041,  7043,
    7047,  7051,  7063,  7065,  7069,  7071,  7075,  7078,  7081,  7085,
    7091,  7103,  7105,  7109,  7111,  7115,  7123,  7135,  7136,  7138,
    7142,  7146,  7148,  7156,  7160,  7163,  7165,  7169,  7173,  7175,
    7178,  7180,  7181,  7182,  7183,  7184,  7185,  7186,  7187,  7188,
    7189,  7190,  7191,  7192,  7193,  7194,  7195,  7196,  7197,  7198,
    7199,  7200,  7201,  7202,  7203,  7204,  7205,  7208,  7214,  7220,
    7226,  7232,  7236,  7242,  7243,  7244,  7245,  7246,  7247,  7248,
    7249,  7250,  7253,  7258,  7263,  7269,  7275,  7281,  7286,  7292,
    7298,  7304,  7311,  7317,  7323,  7330,  7334,  7336,  7342,  7349,
    7355,  7361,  7367,  7373,  7379,  7385,  7391,  7397,  7403,  7409,
    7415,  7425,  7430,  7436,  7440,  7446,  7447,  7448,  7449,  7452,
    7460,  7466,  7472,  7477,  7483,  7490,  7496,  7500,  7506,  7507,
    7508,  7509,  7510,  7511,  7514,  7523,  7527,  7533,  7540,  7547,
    7554,  7563,  7569,  7575,  7579,  7585,  7586,  7589,  7595,  7601,
    7605,  7612,  7613,  7616,  7622,  7628,  7633,  7641,  7647,  7652,
    7659,  7663,  7669,  7670,  7671,  7672,  7673,  7674,  7675,  7676,
    7677,  7678,  7679,  7683,  7688,  7693,  7700,  7705,  7711,  7717,
    7722,  7727,  7732,  7736,  7741,  7746,  7750,  7755,  7759,  7765,
    7770,  7776,  7781,  7787,  7797,  7801,  7805,  7809,  7815,  7818,
    7822,  7823,  7824,  7825,  7826,  7827,  7830,  7834,  7838,  7840,
    7842,  7846,  7848,  7850,  7854,  7856,  7860,  7862,  7866,  7869,
    7872,  7877,  7879,  7881,  7883,  7885,  7889,  7893,  7898,  7902,
    7904,  7908,  7910,  7914,  7918,  7922,  7923,  7924,  7925,  7926,
    7929,  7933,  7937,  7941,  7945,  7949,  7951,  7955,  7956,  7959,
    7963,  7967,  7969,  7973,  7974,  7975,  7979,  7983,  7987,  7991,
    7993,  7997,  7999,  8001,  8003,  8006,  8010,  8014,  8016
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "PERCENT", "AMPERSAND", "ASTER",
  "CLUSTER", "COLON", "COMMA", "DASTER", "DEFINED_OPERATOR", "DOT",
  "DQUOTE", "GLOBAL_A", "LEFTAB", "LEFTPAR", "MINUS", "PLUS", "POINT_TO",
  "QUOTE", "RIGHTAB", "RIGHTPAR", "AND", "DSLASH", "EQV", "EQ", "EQUAL",
  "FFALSE", "GE", "GT", "LE", "LT", "NE", "NEQV", "NOT", "OR", "TTRUE",
  "SLASH", "XOR", "REFERENCE", "AT", "ACROSS", "ALIGN_WITH", "ALIGN",
  "ALLOCATABLE", "ALLOCATE", "ARITHIF", "ASSIGNMENT", "ASSIGN",
  "ASSIGNGOTO", "ASYNCHRONOUS", "ASYNCID", "ASYNCWAIT", "BACKSPACE",
  "BAD_CCONST", "BAD_SYMBOL", "BARRIER", "BLOCKDATA", "BLOCK",
  "BOZ_CONSTANT", "BYTE", "CALL", "CASE", "CHARACTER", "CHAR_CONSTANT",
  "CHECK", "CLOSE", "COMMON", "COMPLEX", "COMPGOTO", "CONSISTENT_GROUP",
  "CONSISTENT_SPEC", "CONSISTENT_START", "CONSISTENT_WAIT", "CONSISTENT",
  "CONSTRUCT_ID", "CONTAINS", "CONTINUE", "CORNER", "CYCLE", "DATA",
  "DEALLOCATE", "HPF_TEMPLATE", "DEBUG", "DEFAULT_CASE", "DEFINE",
  "DERIVED", "DIMENSION", "DISTRIBUTE", "DOWHILE", "DOUBLEPRECISION",
  "DOUBLECOMPLEX", "DP_CONSTANT", "DVM_POINTER", "DYNAMIC", "ELEMENTAL",
  "ELSE", "ELSEIF", "ELSEWHERE", "ENDASYNCHRONOUS", "ENDDEBUG",
  "ENDINTERVAL", "ENDUNIT", "ENDDO", "ENDFILE", "ENDFORALL", "ENDIF",
  "ENDINTERFACE", "ENDMODULE", "ENDON", "ENDSELECT", "ENDTASK_REGION",
  "ENDTYPE", "ENDWHERE", "ENTRY", "EXIT", "EOLN", "EQUIVALENCE", "ERROR",
  "EXTERNAL", "F90", "FIND", "FORALL", "FORMAT", "FUNCTION", "GATE",
  "GEN_BLOCK", "HEAP", "HIGH", "IDENTIFIER", "IMPLICIT", "IMPLICITNONE",
  "INCLUDE_TO", "INCLUDE", "INDEPENDENT", "INDIRECT_ACCESS",
  "INDIRECT_GROUP", "INDIRECT", "INHERIT", "INQUIRE",
  "INTERFACEASSIGNMENT", "INTERFACEOPERATOR", "INTERFACE", "INTRINSIC",
  "INTEGER", "INTENT", "INTERVAL", "INOUT", "IN", "INT_CONSTANT", "LABEL",
  "LABEL_DECLARE", "LET", "LOCALIZE", "LOGICAL", "LOGICALIF", "LOOP",
  "LOW", "MAXLOC", "MAX", "MAP", "MINLOC", "MIN", "MODULE_PROCEDURE",
  "MODULE", "MULT_BLOCK", "NAMEEQ", "NAMELIST", "NEW_VALUE", "NEW",
  "NULLIFY", "OCTAL_CONSTANT", "ONLY", "ON", "ON_DIR", "ONTO", "OPEN",
  "OPERATOR", "OPTIONAL", "OTHERWISE", "OUT", "OWN", "PARALLEL",
  "PARAMETER", "PAUSE", "PLAINDO", "PLAINGOTO", "POINTER", "POINTERLET",
  "PREFETCH", "PRINT", "PRIVATE", "PRODUCT", "PROGRAM", "PUBLIC", "PURE",
  "RANGE", "READ", "REALIGN_WITH", "REALIGN", "REAL", "REAL_CONSTANT",
  "RECURSIVE", "REDISTRIBUTE_NEW", "REDISTRIBUTE", "REDUCTION_GROUP",
  "REDUCTION_START", "REDUCTION_WAIT", "REDUCTION", "REMOTE_ACCESS_SPEC",
  "REMOTE_ACCESS", "REMOTE_GROUP", "RESET", "RESULT", "RETURN", "REWIND",
  "SAVE", "SECTION", "SELECT", "SEQUENCE", "SHADOW_ADD", "SHADOW_COMPUTE",
  "SHADOW_GROUP", "SHADOW_RENEW", "SHADOW_START_SPEC", "SHADOW_START",
  "SHADOW_WAIT_SPEC", "SHADOW_WAIT", "SHADOW", "STAGE", "STATIC", "STAT",
  "STOP", "SUBROUTINE", "SUM", "SYNC", "TARGET", "TASK", "TASK_REGION",
  "THEN", "TO", "TRACEON", "TRACEOFF", "TRUNC", "TYPE", "TYPE_DECL",
  "UNDER", "UNKNOWN", "USE", "VIRTUAL", "VARIABLE", "WAIT", "WHERE",
  "WHERE_ASSIGN", "WHILE", "WITH", "WRITE", "COMMENT", "WGT_BLOCK",
  "HPF_PROCESSORS", "IOSTAT", "ERR", "END", "OMPDVM_ATOMIC",
  "OMPDVM_BARRIER", "OMPDVM_COPYIN", "OMPDVM_COPYPRIVATE",
  "OMPDVM_CRITICAL", "OMPDVM_ONETHREAD", "OMPDVM_DO", "OMPDVM_DYNAMIC",
  "OMPDVM_ENDCRITICAL", "OMPDVM_ENDDO", "OMPDVM_ENDMASTER",
  "OMPDVM_ENDORDERED", "OMPDVM_ENDPARALLEL", "OMPDVM_ENDPARALLELDO",
  "OMPDVM_ENDPARALLELSECTIONS", "OMPDVM_ENDPARALLELWORKSHARE",
  "OMPDVM_ENDSECTIONS", "OMPDVM_ENDSINGLE", "OMPDVM_ENDWORKSHARE",
  "OMPDVM_FIRSTPRIVATE", "OMPDVM_FLUSH", "OMPDVM_GUIDED",
  "OMPDVM_LASTPRIVATE", "OMPDVM_MASTER", "OMPDVM_NOWAIT", "OMPDVM_NONE",
  "OMPDVM_NUM_THREADS", "OMPDVM_ORDERED", "OMPDVM_PARALLEL",
  "OMPDVM_PARALLELDO", "OMPDVM_PARALLELSECTIONS",
  "OMPDVM_PARALLELWORKSHARE", "OMPDVM_RUNTIME", "OMPDVM_SECTION",
  "OMPDVM_SECTIONS", "OMPDVM_SCHEDULE", "OMPDVM_SHARED", "OMPDVM_SINGLE",
  "OMPDVM_THREADPRIVATE", "OMPDVM_WORKSHARE", "OMPDVM_NODES", "OMPDVM_IF",
  "IAND", "IEOR", "IOR", "ACC_REGION", "ACC_END_REGION",
  "ACC_CHECKSECTION", "ACC_END_CHECKSECTION", "ACC_GET_ACTUAL",
  "ACC_ACTUAL", "ACC_TARGETS", "ACC_ASYNC", "ACC_HOST", "ACC_CUDA",
  "ACC_LOCAL", "ACC_INLOCAL", "ACC_CUDA_BLOCK", "BY", "IO_MODE",
  "CP_CREATE", "CP_LOAD", "CP_SAVE", "CP_WAIT", "FILES", "VARLIST",
  "STATUS", "EXITINTERVAL", "TEMPLATE_CREATE", "TEMPLATE_DELETE",
  "SPF_ANALYSIS", "SPF_PARALLEL", "SPF_TRANSFORM", "SPF_NOINLINE",
  "SPF_PARALLEL_REG", "SPF_END_PARALLEL_REG", "SPF_PRIVATES_EXPANSION",
  "SPF_FISSION", "SPF_SHRINK", "BINARY_OP", "UNARY_OP", "$accept",
  "program", "stat", "thislabel", "entry", "new_prog", "proc_attr",
  "procname", "funcname", "typedfunc", "opt_result_clause", "name",
  "progname", "blokname", "arglist", "args", "arg", "filename",
  "needkeyword", "keywordoff", "keyword_if_colon_follow", "spec",
  "interface", "defined_op", "operator", "intrinsic_op", "type_dcl",
  "end_type", "dcl", "options", "attr_spec_list", "attr_spec",
  "intent_spec", "access_spec", "intent", "optional", "static", "private",
  "private_attr", "sequence", "public", "public_attr", "type",
  "opt_key_hedr", "attrib", "att_type", "typespec", "typename", "lengspec",
  "proper_lengspec", "selector", "clause", "end_ioctl", "initial_value",
  "dimension", "allocatable", "pointer", "target", "common", "namelist",
  "namelist_group", "comblock", "var", "external", "intrinsic",
  "equivalence", "equivset", "equivlist", "equi_object", "data", "data1",
  "data_in", "in_data", "datapair", "datalvals", "datarvals", "datalval",
  "data_null", "d_name", "dataname", "datasubs", "datarange",
  "iconexprlist", "opticonexpr", "dataimplieddo", "dlist", "dataelt",
  "datarval", "datavalue", "BOZ_const", "int_const", "unsignedint",
  "real_const", "unsignedreal", "complex_const_data", "complex_part",
  "iconexpr", "iconterm", "iconfactor", "iconprimary", "savelist",
  "saveitem", "use_name_list", "use_key_word", "no_use_key_word",
  "use_name", "paramlist", "paramitem", "module_proc_stmt",
  "proc_name_list", "use_stat", "module_name", "only_list", "only_name",
  "rename_list", "rename_name", "dims", "dimlist", "@1", "dim", "ubound",
  "labellist", "label", "implicit", "implist", "impitem", "imptype", "@2",
  "type_implicit", "letgroups", "letgroup", "letter", "inside", "in_dcl",
  "opt_double_colon", "funarglist", "funarg", "funargs", "subscript_list",
  "expr", "uexpr", "addop", "ident", "lhs", "array_ele_substring_func_ref",
  "structure_component", "array_element", "asubstring", "opt_substring",
  "substring", "opt_expr", "simple_const", "numeric_bool_const",
  "integer_constant", "string_constant", "complex_const", "kind",
  "triplet", "vec", "@3", "@4", "allocate_object", "allocation_list",
  "allocate_object_list", "stat_spec", "pointer_name_list", "exec",
  "do_while", "opt_while", "plain_do", "case", "case_selector",
  "case_value_range", "case_value_range_list", "opt_construct_name",
  "opt_unit_name", "construct_name", "construct_name_colon", "logif",
  "forall", "forall_list", "forall_expr", "opt_forall_cond", "do_var",
  "dospec", "dotarget", "whereable", "iffable", "let", "goto", "opt_comma",
  "call", "callarglist", "callarg", "stop", "end_spec", "intonlyon",
  "intonlyoff", "io", "iofmove", "fmkwd", "iofctl", "ctlkwd", "inquire",
  "infmt", "ioctl", "ctllist", "ioclause", "nameeq", "read", "write",
  "print", "inlist", "inelt", "outlist", "out2", "other", "in_ioctl",
  "start_ioctl", "fexpr", "unpar_fexpr", "cmnt", "dvm_specification",
  "dvm_exec", "dvm_template", "template_obj", "dvm_dynamic",
  "dyn_array_name_list", "dyn_array_name", "dvm_inherit",
  "dummy_array_name_list", "dummy_array_name", "dvm_shadow",
  "shadow_attr_stuff", "sh_width_list", "sh_width", "sh_array_name",
  "dvm_processors", "dvm_indirect_group", "indirect_group_name",
  "dvm_remote_group", "remote_group_name", "dvm_reduction_group",
  "reduction_group_name", "dvm_consistent_group", "consistent_group_name",
  "opt_onto", "dvm_distribute", "dvm_redistribute", "dist_name_list",
  "distributee", "dist_name", "pointer_ar_elem", "processors_name",
  "opt_dist_format_clause", "dist_format_clause", "dist_format_list",
  "opt_key_word", "dist_format", "array_name", "derived_spec",
  "derived_elem_list", "derived_elem", "target_spec", "derived_target",
  "derived_subscript_list", "derived_subscript", "dummy_ident",
  "opt_plus_shadow", "plus_shadow", "shadow_id", "shadow_width",
  "dvm_align", "dvm_realign", "realignee_list", "alignee", "realignee",
  "align_directive_stuff", "align_base", "align_subscript_list",
  "align_subscript", "align_base_name", "dim_ident_list", "dim_ident",
  "dvm_combined_dir", "dvm_attribute_list", "dvm_attribute", "dvm_pointer",
  "dimension_list", "@5", "pointer_var_list", "pointer_var", "dvm_heap",
  "heap_array_name_list", "heap_array_name", "dvm_consistent",
  "consistent_array_name_list", "consistent_array_name", "dvm_asyncid",
  "async_id_list", "async_id", "dvm_new_value", "dvm_parallel_on",
  "loop_var_list", "loop_var", "opt_on", "distribute_cycles",
  "par_subscript_list", "par_subscript", "opt_spec", "spec_list",
  "par_spec", "remote_access_spec", "consistent_spec", "consistent_group",
  "new_spec", "private_spec", "cuda_block_spec", "sizelist",
  "variable_list", "indirect_access_spec", "stage_spec", "across_spec",
  "in_out_across", "opt_in_out", "dependent_array_list", "dependent_array",
  "dependence_list", "dependence", "section_spec_list", "section_spec",
  "ar_section", "low_section", "high_section", "section", "reduction_spec",
  "opt_key_word_r", "no_opt_key_word_r", "reduction_group",
  "reduction_list", "reduction", "reduction_op", "loc_op", "shadow_spec",
  "shadow_group_name", "shadow_list", "shadow", "opt_corner",
  "array_ident", "array_ident_list", "dvm_shadow_start", "dvm_shadow_wait",
  "dvm_shadow_group", "dvm_reduction_start", "dvm_reduction_wait",
  "dvm_consistent_start", "dvm_consistent_wait", "dvm_remote_access",
  "group_name", "remote_data_list", "remote_data", "remote_index_list",
  "remote_index", "dvm_task", "task_array", "dvm_task_region", "task_name",
  "dvm_end_task_region", "task", "dvm_on", "opt_new_spec", "dvm_end_on",
  "dvm_map", "dvm_prefetch", "dvm_reset", "dvm_indirect_access",
  "indirect_list", "indirect_reference", "hpf_independent",
  "hpf_reduction_spec", "dvm_asynchronous", "dvm_endasynchronous",
  "dvm_asyncwait", "async_ident", "async", "dvm_f90", "dvm_debug_dir",
  "debparamlist", "debparam", "fragment_number", "dvm_enddebug_dir",
  "dvm_interval_dir", "interval_number", "dvm_exit_interval_dir",
  "dvm_endinterval_dir", "dvm_traceon_dir", "dvm_traceoff_dir",
  "dvm_barrier_dir", "dvm_check", "dvm_io_mode_dir", "mode_list",
  "mode_spec", "dvm_shadow_add", "template_ref", "shadow_axis_list",
  "shadow_axis", "opt_include_to", "dvm_localize", "localize_target",
  "target_subscript_list", "target_subscript", "aster_expr",
  "dvm_cp_create", "opt_mode", "dvm_cp_load", "dvm_cp_save", "dvm_cp_wait",
  "dvm_template_create", "template_list", "dvm_template_delete",
  "template_ident_list", "omp_specification_directive",
  "omp_execution_directive", "ompdvm_onethread",
  "omp_parallel_end_directive", "omp_parallel_begin_directive",
  "parallel_clause_list", "parallel_clause", "omp_variable_list_in_par",
  "ompprivate_clause", "ompfirstprivate_clause", "omplastprivate_clause",
  "ompcopyin_clause", "ompshared_clause", "ompdefault_clause", "def_expr",
  "ompif_clause", "ompnumthreads_clause", "ompreduction_clause",
  "ompreduction", "ompreduction_vars", "ompreduction_op",
  "omp_sections_begin_directive", "sections_clause_list",
  "sections_clause", "omp_sections_end_directive", "omp_section_directive",
  "omp_do_begin_directive", "omp_do_end_directive", "do_clause_list",
  "do_clause", "ompordered_clause", "ompschedule_clause", "ompschedule_op",
  "omp_single_begin_directive", "single_clause_list", "single_clause",
  "omp_single_end_directive", "end_single_clause_list",
  "end_single_clause", "ompcopyprivate_clause", "ompnowait_clause",
  "omp_workshare_begin_directive", "omp_workshare_end_directive",
  "omp_parallel_do_begin_directive", "paralleldo_clause_list",
  "paralleldo_clause", "omp_parallel_do_end_directive",
  "omp_parallel_sections_begin_directive",
  "omp_parallel_sections_end_directive",
  "omp_parallel_workshare_begin_directive",
  "omp_parallel_workshare_end_directive", "omp_threadprivate_directive",
  "omp_master_begin_directive", "omp_master_end_directive",
  "omp_ordered_begin_directive", "omp_ordered_end_directive",
  "omp_barrier_directive", "omp_atomic_directive", "omp_flush_directive",
  "omp_critical_begin_directive", "omp_critical_end_directive",
  "omp_common_var", "omp_variable_list", "op_slash_1", "op_slash_0",
  "acc_directive", "acc_region", "acc_checksection", "acc_get_actual",
  "acc_actual", "opt_clause", "acc_clause_list", "acc_clause",
  "data_clause", "targets_clause", "async_clause", "acc_var_list",
  "computer_list", "computer", "acc_end_region", "acc_end_checksection",
  "spf_directive", "spf_analysis", "spf_parallel", "spf_transform",
  "spf_parallel_reg", "spf_end_parallel_reg", "analysis_spec_list",
  "analysis_spec", "analysis_reduction_spec", "analysis_private_spec",
  "parallel_spec_list", "parallel_spec", "parallel_shadow_spec",
  "parallel_across_spec", "parallel_remote_access_spec",
  "transform_spec_list", "transform_spec", "region_name",
  "array_element_list", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   340,   341,     1,     2,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   156,   157,
     158,   159,   160,   161,   162,   163,   164,   165,   166,   167,
     168,   169,   170,   171,   172,   173,   174,   175,   176,   177,
     178,   179,   180,   181,   182,   183,   184,   185,   186,   187,
     188,   189,   190,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,   201,   202,   203,   204,   205,   206,   207,
     208,   209,   210,   211,   212,   213,   214,   215,   216,   217,
     218,   219,   220,   221,   222,   223,   224,   225,   226,   227,
     228,   229,   230,   231,   232,   233,   234,   235,   236,   237,
     238,   239,   240,   241,   242,   243,   244,   245,   246,   247,
     248,   249,   250,   251,   252,   253,   254,   255,   256,   257,
     258,   259,   260,   261,   262,   263,   264,   265,   266,   267,
     268,   269,   270,   271,   272,   273,   274,   275,   276,   277,
     278,   279,   280,   281,   282,   283,   284,   285,   286,   287,
     288,   289,   290,   291,   292,   293,   294,   295,   296,   297,
     298,   299,   300,   301,   302,   303,   304,   305,   306,   307,
     308,   309,   310,   311,   312,   313,   314,   315,   316,   317,
     318,   319,   320,   321,   322,   323,   324,   325,   326,   327,
     328,   329,   330,   331,   332,   333,   334,   335,   336,   337,
     338,   339,   342,   343
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   344,   345,   345,   346,   346,   346,   346,   346,   346,
     346,   347,   348,   348,   348,   348,   348,   348,   348,   348,
     349,   350,   350,   350,   351,   352,   353,   353,   353,   353,
     354,   354,   355,   356,   356,   357,   357,   358,   358,   358,
     359,   359,   360,   360,   361,   362,   363,   364,   365,   365,
     365,   365,   365,   365,   365,   365,   365,   365,   365,   365,
     365,   365,   365,   365,   365,   365,   365,   365,   365,   365,
     365,   365,   365,   365,   365,   365,   365,   366,   366,   366,
     366,   366,   367,   368,   368,   369,   369,   369,   369,   369,
     369,   369,   369,   369,   369,   369,   369,   369,   369,   369,
     369,   369,   369,   370,   370,   371,   371,   372,   372,   373,
     373,   373,   374,   374,   375,   375,   375,   375,   375,   375,
     375,   375,   375,   375,   375,   375,   376,   376,   376,   377,
     377,   378,   378,   379,   379,   380,   380,   381,   381,   382,
     383,   384,   384,   385,   386,   386,   387,   388,   388,   389,
     389,   390,   391,   391,   391,   391,   391,   391,   391,   392,
     392,   393,   393,   393,   394,   394,   394,   394,   395,   395,
     395,   395,   396,   397,   397,   397,   398,   398,   399,   399,
     400,   400,   401,   401,   402,   402,   402,   402,   403,   403,
     403,   404,   405,   405,   406,   407,   407,   408,   408,   409,
     409,   410,   411,   411,   412,   412,   412,   413,   414,   414,
     415,   416,   417,   418,   418,   419,   419,   420,   420,   420,
     420,   420,   421,   422,   423,   424,   425,   426,   426,   427,
     427,   428,   429,   429,   430,   430,   430,   430,   431,   431,
     431,   432,   432,   432,   432,   432,   432,   432,   432,   432,
     432,   432,   433,   434,   434,   434,   435,   435,   436,   436,
     436,   437,   437,   437,   437,   438,   439,   439,   440,   440,
     440,   440,   440,   441,   441,   441,   442,   442,   443,   443,
     443,   444,   444,   445,   445,   446,   446,   447,   448,   449,
     449,   449,   450,   450,   451,   452,   453,   453,   454,   454,
     454,   454,   455,   456,   456,   457,   457,   458,   458,   459,
     460,   460,   462,   461,   461,   463,   463,   463,   463,   464,
     464,   465,   465,   466,   467,   467,   468,   468,   469,   471,
     470,   472,   472,   473,   473,   474,   474,   475,   476,   477,
     478,   478,   479,   479,   480,   480,   480,   481,   481,   482,
     482,   483,   483,   483,   484,   484,   484,   484,   484,   484,
     484,   484,   484,   484,   484,   484,   484,   484,   484,   484,
     484,   484,   484,   484,   484,   484,   484,   484,   484,   485,
     485,   486,   487,   487,   487,   488,   488,   488,   488,   489,
     490,   490,   490,   490,   491,   492,   492,   493,   494,   494,
     495,   495,   495,   495,   495,   496,   496,   496,   496,   497,
     498,   498,   498,   499,   500,   500,   501,   501,   501,   501,
     501,   501,   501,   501,   503,   504,   502,   505,   505,   506,
     506,   506,   507,   507,   507,   508,   509,   509,   510,   510,
     510,   510,   510,   510,   510,   510,   510,   510,   510,   510,
     510,   510,   510,   510,   510,   510,   510,   510,   510,   510,
     510,   510,   511,   511,   512,   512,   512,   513,   513,   514,
     514,   514,   514,   514,   515,   516,   516,   516,   516,   517,
     517,   518,   518,   519,   519,   520,   521,   522,   523,   524,
     524,   525,   526,   526,   527,   528,   528,   529,   529,   530,
     530,   530,   530,   530,   531,   531,   531,   531,   531,   531,
     531,   531,   531,   531,   531,   531,   531,   531,   531,   531,
     531,   531,   532,   533,   533,   533,   533,   534,   534,   535,
     536,   536,   537,   537,   537,   538,   538,   539,   540,   541,
     542,   542,   542,   542,   542,   542,   542,   542,   542,   542,
     542,   542,   542,   542,   543,   544,   544,   544,   545,   546,
     546,   547,   547,   548,   548,   549,   549,   550,   550,   551,
     551,   551,   551,   551,   551,   552,   553,   554,   555,   555,
     556,   556,   557,   557,   558,   558,   558,   559,   559,   559,
     559,   559,   559,   560,   560,   560,   560,   560,   561,   562,
     563,   563,   564,   564,   564,   564,   564,   564,   564,   564,
     564,   565,   565,   566,   566,   566,   566,   566,   566,   566,
     566,   566,   566,   566,   566,   566,   566,   566,   566,   566,
     566,   566,   567,   567,   567,   567,   567,   567,   567,   567,
     567,   567,   567,   567,   567,   567,   567,   567,   567,   567,
     567,   567,   567,   567,   567,   567,   567,   567,   567,   567,
     567,   567,   567,   567,   567,   567,   567,   567,   567,   567,
     567,   567,   567,   567,   567,   567,   568,   568,   569,   570,
     571,   571,   572,   573,   574,   574,   575,   576,   577,   578,
     578,   579,   579,   579,   580,   581,   581,   581,   582,   582,
     583,   584,   584,   585,   586,   586,   587,   588,   588,   589,
     590,   590,   591,   592,   592,   593,   593,   594,   594,   595,
     596,   597,   598,   598,   599,   600,   600,   601,   602,   602,
     602,   602,   602,   602,   602,   602,   603,   604,   605,   605,
     606,   606,   607,   607,   608,   609,   609,   610,   610,   610,
     611,   612,   612,   613,   613,   614,   615,   615,   616,   617,
     617,   618,   618,   619,   620,   621,   622,   623,   623,   624,
     624,   624,   625,   626,   626,   627,   627,   627,   628,   628,
     629,   629,   630,   630,   630,   630,   630,   630,   630,   630,
     630,   630,   630,   631,   633,   632,   632,   634,   634,   635,
     636,   637,   637,   638,   639,   640,   640,   641,   642,   642,
     643,   643,   644,   645,   646,   647,   647,   648,   649,   649,
     650,   651,   651,   652,   652,   653,   653,   654,   654,   655,
     655,   655,   655,   655,   655,   655,   655,   655,   655,   656,
     656,   657,   657,   658,   659,   659,   660,   661,   662,   662,
     662,   663,   663,   664,   664,   665,   666,   666,   667,   668,
     668,   668,   669,   669,   670,   670,   670,   671,   671,   672,
     673,   673,   674,   674,   674,   674,   674,   674,   674,   675,
     676,   677,   678,   679,   679,   679,   680,   681,   682,   683,
     683,   684,   684,   685,   685,   685,   685,   685,   685,   685,
     685,   685,   686,   686,   687,   687,   687,   687,   687,   688,
     689,   689,   690,   690,   690,   690,   691,   692,   693,   693,
     694,   694,   695,   695,   696,   697,   698,   699,   700,   701,
     701,   702,   703,   703,   704,   704,   705,   705,   706,   706,
     707,   707,   708,   709,   709,   709,   709,   709,   710,   711,
     712,   712,   713,   714,   714,   715,   716,   716,   717,   718,
     719,   719,   720,   720,   721,   721,   722,   722,   722,   722,
     723,   724,   725,   726,   727,   728,   728,   729,   730,   730,
     731,   731,   732,   733,   734,   735,   736,   736,   737,   738,
     739,   740,   741,   742,   743,   744,   744,   745,   745,   745,
     746,   747,   748,   748,   749,   749,   750,   750,   751,   752,
     752,   753,   753,   754,   754,   755,   756,   757,   757,   757,
     758,   759,   759,   760,   761,   762,   762,   763,   764,   764,
     765,   766,   766,   766,   766,   766,   766,   766,   766,   766,
     766,   766,   766,   766,   766,   766,   766,   766,   766,   766,
     766,   766,   766,   766,   766,   766,   766,   767,   768,   769,
     769,   770,   770,   771,   771,   771,   771,   771,   771,   771,
     771,   771,   772,   773,   774,   775,   776,   777,   778,   779,
     779,   779,   780,   781,   782,   783,   784,   785,   785,   785,
     785,   785,   785,   785,   785,   785,   785,   785,   785,   785,
     785,   786,   786,   787,   787,   788,   788,   788,   788,   789,
     789,   790,   791,   791,   792,   792,   793,   793,   794,   794,
     794,   794,   794,   794,   795,   796,   796,   797,   797,   797,
     797,   798,   798,   799,   799,   800,   800,   801,   801,   802,
     802,   803,   803,   804,   805,   806,   807,   807,   808,   808,
     809,   809,   810,   810,   810,   810,   810,   810,   810,   810,
     810,   810,   810,   811,   812,   812,   813,   814,   814,   815,
     816,   817,   818,   819,   820,   821,   822,   823,   823,   824,
     824,   825,   825,   826,   827,   827,   827,   827,   828,   829,
     830,   830,   830,   830,   830,   830,   831,   832,   833,   833,
     833,   834,   834,   834,   835,   835,   836,   836,   837,   837,
     837,   838,   838,   838,   838,   838,   839,   840,   841,   842,
     842,   843,   843,   844,   845,   846,   846,   846,   846,   846,
     847,   848,   849,   850,   851,   852,   852,   853,   853,   854,
     855,   856,   856,   857,   857,   857,   858,   859,   860,   861,
     861,   862,   862,   862,   862,   862,   863,   864,   864
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     3,     3,     3,     3,     3,     2,     1,
       1,     1,     3,     3,     4,     5,     5,     3,     4,     3,
       0,     2,     2,     2,     1,     1,     4,     5,     4,     5,
       2,     5,     1,     0,     1,     0,     1,     0,     2,     3,
       1,     3,     1,     1,     1,     0,     0,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     4,     2,     5,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     3,     5,
       5,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     4,     7,     2,     3,     7,     6,     0,
       2,     5,     1,     4,     1,     1,     1,     2,     1,     4,
       1,     1,     1,     1,     1,     1,     2,     2,     2,     1,
       1,     7,     3,     4,     3,     4,     3,     2,     5,     0,
       2,     2,     5,     0,     4,     5,     0,     2,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     5,     6,     6,     0,     1,     5,     9,     1,     1,
       2,     2,     0,     0,     2,     2,     5,     4,     4,     3,
       4,     3,     4,     3,     3,     4,     5,     3,     4,     5,
       3,     3,     1,     3,     2,     4,     3,     4,     3,     3,
       3,     3,     3,     3,     1,     4,     1,     1,     4,     3,
       0,     0,     4,     1,     3,     1,     3,     2,     3,     3,
       4,     2,     0,     1,     1,     3,     5,     1,     3,     0,
       1,     7,     1,     3,     2,     2,     3,     1,     1,     4,
       3,     2,     1,     1,     1,     1,     3,     1,     3,     1,
       1,     6,     1,     1,     2,     2,     1,     3,     1,     2,
       2,     1,     3,     1,     3,     5,     1,     1,     1,     2,
       2,     3,     3,     1,     3,     3,     1,     3,     1,     1,
       3,     1,     3,     1,     1,     3,     5,     0,     0,     1,
       4,     4,     1,     3,     3,     2,     1,     3,     3,     6,
       6,     7,     1,     1,     3,     1,     1,     1,     3,     3,
       0,     3,     0,     2,     3,     1,     1,     2,     3,     1,
       1,     1,     3,     1,     3,     1,     1,     3,     4,     0,
       2,     2,     1,     1,     3,     1,     3,     1,     0,     0,
       0,     2,     0,     1,     1,     1,     2,     2,     4,     1,
       3,     1,     3,     1,     1,     1,     1,     3,     3,     3,
       3,     3,     2,     2,     2,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     2,     3,     3,     1,
       1,     1,     1,     1,     1,     5,     6,     4,     5,     3,
       1,     1,     5,     4,     2,     0,     1,     5,     0,     1,
       1,     3,     1,     3,     2,     1,     1,     1,     1,     1,
       1,     3,     3,     5,     1,     1,     3,     2,     5,     4,
       4,     3,     2,     1,     0,     0,     6,     1,     1,     1,
       4,     5,     1,     4,     5,     0,     1,     3,     1,     1,
       1,     2,     3,     3,     2,     1,     2,     2,     2,     3,
       7,     3,     3,     1,     2,     2,     1,     2,     3,     1,
       1,     1,     5,     7,     0,     6,     4,    11,    13,     4,
       3,     3,     7,     8,     3,     1,     2,     2,     3,     1,
       3,     0,     1,     0,     1,     1,     2,     5,     6,     1,
       3,     3,     0,     2,     1,     5,     7,     0,     1,     3,
       3,     6,     5,     6,     4,     5,     5,     2,     1,     1,
      10,     1,     3,     4,     3,     3,     3,     3,     6,     6,
       5,     8,     2,     3,     3,     7,     7,     0,     1,     4,
       2,     4,     1,     2,     2,     1,     1,     0,     0,     0,
       2,     2,     2,     2,     2,     1,     2,     2,     3,     4,
       2,     3,     1,     3,     3,     1,     1,     1,     3,     1,
       1,     4,     5,     1,     1,     3,     3,     1,     4,     1,
       1,     1,     2,     2,     2,     1,     3,     3,     4,     4,
       1,     3,     1,     5,     1,     1,     1,     3,     3,     3,
       3,     3,     3,     1,     3,     5,     5,     5,     0,     0,
       1,     3,     1,     1,     3,     3,     3,     3,     2,     3,
       3,     0,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     3,     2,     3,
       1,     3,     1,     3,     1,     3,     1,     4,     3,     1,
       3,     1,     3,     4,     1,     4,     4,     4,     3,     3,
       1,     3,     3,     1,     3,     3,     1,     3,     3,     1,
       3,     0,     5,     6,     8,     1,     3,     1,     1,     1,
       4,     1,     0,     2,     3,     2,     4,     0,     1,     5,
       4,     6,     4,     1,     4,     4,     1,     6,     1,     3,
       1,     3,     1,     4,     1,     1,     3,     1,     1,     3,
       1,     0,     1,     2,     3,     1,     2,     5,     4,     4,
       6,     1,     3,     1,     1,     6,     4,     1,     3,     1,
       1,     1,     1,     1,     3,     1,     1,     1,     6,     4,
       1,     4,     1,     1,     1,     1,     4,     2,     7,     1,
       4,     1,     1,    11,     0,     2,     3,     1,     3,     1,
       3,     1,     3,     1,     3,     1,     3,     1,     3,     8,
       1,     3,     2,     2,     7,     1,     3,     1,     3,     1,
       4,     1,     3,     1,     1,     0,     1,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     8,
       6,     8,     6,     1,     6,     6,     6,     6,     1,     3,
       5,     1,     3,     8,     6,     6,     4,     5,     5,     2,
       2,     0,     1,     3,     1,     4,     7,     1,     3,     3,
       1,     3,     5,     3,     3,     1,     3,     1,     1,     3,
       3,     3,     3,    10,     8,    10,     0,     0,     1,     2,
       4,     4,     6,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     6,     4,     4,     3,     9,     1,
       1,     3,     1,     5,     5,     9,     0,     1,     1,     3,
       3,     3,     3,     3,     6,     3,     3,     3,     3,     7,
       5,     1,     1,     3,     4,     1,     1,     3,     1,     1,
       3,     3,     2,     3,     4,     4,     5,     5,     1,     2,
       4,     4,     4,     0,     1,     2,     7,     6,     3,     3,
       7,     5,     1,     3,     1,     4,     2,     3,     3,     4,
       6,     3,     2,     3,     1,     1,     4,     5,     3,     6,
       2,     4,     2,     1,     3,     3,     0,     1,     3,     2,
       2,     2,     2,     9,     5,     1,     3,     2,     2,     2,
       9,     4,     1,     3,     1,     1,     2,     0,     7,     1,
       4,     1,     3,     1,     1,     1,    16,     0,     3,     3,
       3,     3,     6,     9,     5,     1,     3,     5,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     2,     4,
       3,     4,     5,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     5,     2,     2,     2,     2,     2,     5,     1,
       1,     1,     4,     4,     4,     4,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     4,     3,     4,     5,     1,     1,     1,     1,     4,
       3,     2,     4,     3,     4,     3,     4,     5,     1,     1,
       1,     1,     1,     1,     1,     7,     5,     1,     1,     1,
       1,     4,     3,     4,     5,     1,     1,     4,     3,     4,
       5,     1,     1,     2,     1,     2,     4,     3,     4,     3,
       4,     5,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     4,     3,     2,     4,     3,     2,
       3,     2,     2,     2,     2,     2,     2,     3,     2,     5,
       2,     5,     2,     5,     1,     1,     3,     3,     0,     0,
       1,     1,     1,     1,     1,     1,     3,     2,     5,     4,
       2,     5,     4,     2,     2,     1,     1,     3,     2,     2,
       2,     4,     4,     4,     4,     4,     4,     1,     1,     1,
       3,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       4,     4,     4,     2,     1,     1,     3,     1,     1,     5,
       5,     1,     3,     1,     1,     1,     5,     5,     5,     1,
       3,     2,     5,     2,     5,     5,     1,     1,     3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     1,    10,    11,     9,     0,     0,     3,   150,
     149,   789,   339,   537,   537,   537,   537,   537,   339,   537,
     555,   537,    20,   537,   537,   158,   537,   560,   339,   154,
     537,   339,   537,   537,   339,   485,   537,   537,   537,   338,
     537,   782,   537,   537,   340,   791,   537,   155,   156,   785,
      45,   537,   537,   537,   537,   537,   537,   537,   537,   557,
     537,   537,   483,   483,   537,   537,   537,   339,   537,     0,
     537,   339,   339,   537,   537,   338,    20,   339,   339,   325,
       0,   537,   537,   339,   339,   537,   339,   339,   339,   339,
     152,   339,   537,   211,   537,   157,   537,   537,     0,    20,
     339,   537,   537,   537,   559,   339,   537,   339,   535,   537,
     537,   339,   537,   537,   537,   339,    20,   339,    45,   537,
     537,   153,    45,   537,   339,   537,   537,   537,   339,   537,
     537,   556,   339,   537,   339,   537,   537,   537,   537,   537,
     537,   339,   339,   536,    20,   339,   339,   537,   537,   537,
       0,   339,     8,   339,   537,   537,   537,   783,   537,   537,
     537,   537,   537,   537,   537,   537,   537,   537,   537,   537,
     537,   537,   537,   537,   537,   537,   537,   537,   537,   537,
     537,   537,   537,   537,   339,   537,   784,   537,  1223,   537,
    1224,   537,   537,   537,   537,   537,   537,   537,   537,   537,
     537,     0,     0,     0,     0,  1234,   611,     0,    37,   611,
      73,    48,    49,    50,    65,    66,    76,    68,    69,    67,
     109,    58,     0,   146,   151,    52,    70,    71,    72,    51,
      59,    54,    55,    56,    60,   207,    75,    74,    57,   611,
     445,   440,   453,     0,     0,     0,   456,   439,   438,     0,
     508,   511,   537,   509,     0,   537,     0,   537,   545,     0,
       0,   552,    53,   459,   613,   616,   622,   618,   617,   623,
     624,   625,   626,   615,   632,   614,   633,   619,     0,   780,
     620,   627,   629,   628,   660,   634,   636,   637,   635,   638,
     639,   640,   641,   642,   621,   643,   644,   646,   647,   645,
     649,   650,   648,   674,   661,   662,   663,   664,   651,   652,
     653,   655,   654,   656,   657,   658,   659,   665,   666,   667,
     668,   669,   670,   671,   672,   673,   631,   675,   630,  1032,
    1031,  1033,  1034,  1035,  1036,  1037,  1038,  1039,  1040,  1041,
    1042,  1043,  1044,  1045,  1046,  1047,  1030,  1048,  1049,  1050,
    1051,  1052,  1053,  1054,  1055,  1056,   460,  1190,  1192,  1194,
    1195,  1191,  1193,   461,  1225,  1226,  1227,  1228,  1229,     0,
       0,   340,     0,     0,     0,     0,     0,     0,     0,   992,
      35,     0,     0,   598,     0,     0,     0,     0,     0,     0,
     454,   507,   481,   210,     0,     0,     0,   481,     0,   312,
     339,   727,     0,   727,   538,     0,    23,   481,     0,   481,
     972,     0,   989,   483,   481,   481,   481,    32,   484,    81,
     444,   955,   481,   949,   105,   481,    37,   481,     0,   340,
       0,     0,    63,     0,     0,   329,    44,     7,   966,     0,
       0,     0,   599,     0,     0,    77,   340,     0,   986,   522,
       0,     0,     0,   296,   295,     0,     0,   813,     0,     0,
     340,     0,     0,   538,     0,   340,     0,     0,     0,   340,
      33,   340,    22,   599,     0,    21,     0,     0,     0,     0,
       0,     0,     0,   398,   340,    45,   140,     0,     0,     0,
       0,     0,     0,     0,     0,   787,   340,     0,   340,     0,
       0,   990,   991,     0,   339,   340,     0,     0,     0,   599,
       0,  1176,  1175,  1180,  1057,   727,  1182,   727,  1172,  1174,
    1058,  1163,  1166,  1169,   727,   727,   727,  1178,  1171,  1173,
     727,   727,   727,   727,  1111,   727,   727,  1188,  1145,     0,
      45,  1197,  1200,  1203,     0,     0,     0,     0,     0,     0,
       0,     0,    45,    45,    45,  1256,  1233,   612,     4,    20,
      20,     0,     0,    45,     5,     0,     0,     0,     0,     0,
      45,    20,     0,     0,     0,   147,   164,     0,     0,     0,
       0,   528,     0,   528,     0,     0,     0,     0,   528,   222,
       6,   486,   537,   537,   446,   441,     0,   457,   448,   447,
     455,    82,   172,     0,     0,     0,   406,     0,   405,   410,
     408,   409,   407,   381,     0,     0,   351,   382,   354,   384,
     383,   355,   400,   402,   395,   353,   356,   598,   398,   542,
     543,     0,   380,   379,    32,     0,   602,   603,   540,     0,
     600,   599,     0,   544,   599,   564,   547,   546,   600,   550,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    45,
       0,   776,   777,   775,     0,   773,   763,     0,     0,   435,
       0,   323,     0,   524,   974,   975,   971,    45,   310,   808,
     810,   973,    36,    13,   598,     0,   481,     0,   192,     0,
     310,     0,   184,     0,   709,   707,   843,   927,   928,   807,
     804,   805,   482,   516,   222,   435,   310,   676,   983,   978,
     470,   341,     0,     0,     0,     0,     0,   719,   722,   711,
       0,   497,   682,   679,   680,   451,     0,     0,   500,   984,
     442,   443,   458,   452,   471,   106,   499,    45,   517,     0,
     199,     0,   382,     0,     0,    37,    25,   803,   800,   801,
     324,   326,     0,     0,    45,   967,   968,     0,   700,   698,
     686,   683,   684,     0,     0,     0,    78,     0,    45,   987,
     985,     0,     0,   948,     0,    45,     0,    19,     0,     0,
       0,     0,   953,     0,     0,     0,   497,   523,     0,     0,
     931,   958,   599,     0,   599,   600,   139,    34,    12,   143,
     576,     0,   764,     0,     0,     0,   727,   706,   704,   888,
     925,   926,     0,   703,   701,   959,   399,   514,     0,     0,
       0,   909,     0,   921,   920,   923,   922,     0,   691,     0,
     689,   694,     0,     0,    37,    24,     0,   310,   940,   943,
       0,    45,     0,   302,   298,     0,     0,   577,   310,     0,
     527,     0,  1115,  1110,   527,  1147,  1177,     0,   527,   527,
     527,   527,   527,   527,  1170,   310,    46,  1196,  1205,  1206,
       0,     0,    45,     0,  1020,  1021,     0,   988,   349,     0,
       0,     0,     0,  1235,  1237,  1238,     0,     0,  1241,  1243,
    1244,  1245,     0,     0,  1249,     0,     0,    20,    43,    38,
      42,     0,    40,    17,    46,   310,   132,   134,   136,   110,
       0,     0,    20,   339,   148,   538,   598,   165,   146,   310,
     179,   181,   183,   187,   527,   190,   527,   196,   198,   200,
     209,     0,   213,     0,    45,     0,   449,   424,     0,   351,
     364,   363,   376,   362,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   598,     0,     0,   598,     0,     0,   398,
     404,   396,   512,     0,     0,   515,   570,   571,   575,     0,
     567,     0,   569,     0,   608,     0,     0,     0,     0,     0,
     554,   569,   558,     0,     0,   582,   548,   580,     0,     0,
     351,   353,   551,   586,   585,   553,   677,   310,   699,   702,
     705,   708,   310,   339,     0,   941,     0,    45,   758,   178,
       0,     0,     0,     0,     0,     0,   312,   812,     0,   529,
       0,   475,   479,     0,   469,   598,     0,   194,   185,     0,
     321,     0,   208,     0,   678,   598,     0,   786,   319,   316,
     313,   315,   320,   310,   727,   724,   733,   728,     0,     0,
       0,     0,     0,   725,   711,   727,     0,   790,     0,   498,
     539,     0,     0,     0,    18,   204,     0,     0,     0,   206,
     195,     0,   494,   492,   489,     0,    45,     0,   329,     0,
       0,   332,   330,     0,    45,   969,   381,   917,   964,     0,
       0,   962,     0,   561,     0,    87,    88,    86,    85,    91,
      90,   102,    95,    98,    97,   100,    99,    96,   101,    94,
      92,    89,    93,    83,     0,    84,   197,     0,     0,     0,
       0,     0,     0,   297,     0,   188,   436,     0,    45,   954,
     952,   133,   817,     0,   815,     0,     0,   292,   539,   180,
       0,   579,     0,   578,   287,   287,     0,   759,     0,   727,
     711,   935,     0,     0,   932,   284,   283,    62,   281,     0,
       0,     0,     0,     0,     0,     0,   688,   687,   135,    14,
     182,   942,    45,   945,   944,   146,     0,   103,    47,     0,
       0,   695,     0,   727,   527,     0,  1144,  1114,  1109,   727,
     527,  1146,  1189,   727,   527,   727,   527,   527,   527,   727,
     527,   727,   527,   696,     0,     0,     0,     0,  1217,     0,
       0,  1204,  1208,  1210,  1209,    45,  1199,   851,  1218,     0,
    1202,     0,     0,     0,   995,    45,    45,    45,     0,   390,
     391,  1025,     0,  1028,     0,     0,     0,    45,  1230,     0,
       0,     0,    45,  1231,  1251,  1253,     0,     0,    45,  1232,
      28,    37,     0,     0,    39,     0,    30,   159,   116,   310,
     339,   118,   120,     0,   121,   114,   122,   130,   129,   123,
     124,   125,     0,   112,   115,    26,     0,   310,     0,     0,
     144,   177,     0,     0,   222,   222,     0,   224,   217,   221,
       0,     0,     0,   352,     0,   359,   361,   358,   357,   375,
     377,   371,   365,   504,   368,   366,   369,   367,   370,   372,
     374,   360,   373,   378,   598,   411,   389,     0,   343,     0,
     414,   415,   401,   412,   403,     0,   598,   513,     0,   532,
     530,     0,   598,   566,   573,   574,   572,   601,   610,   605,
     607,   609,   606,   604,   565,   549,     0,     0,     0,   351,
       0,     0,     0,     0,     0,   697,   779,     0,   789,   792,
     782,     0,   791,   785,     0,   783,   784,   781,   774,     0,
     429,     0,     0,   506,     0,     0,     0,     0,   811,   477,
     476,     0,   474,     0,   193,     0,   527,   806,   427,   428,
     432,     0,     0,     0,   314,   317,   176,     0,   598,     0,
       0,     0,     0,     0,   712,   723,   310,   462,   727,   681,
       0,   481,     0,     0,   201,     0,   394,   977,     0,     0,
       0,    16,   802,   327,   337,     0,   333,   335,   331,     0,
       0,     0,     0,     0,     0,     0,   961,   685,   562,    80,
      79,   128,   126,   127,   340,     0,   487,   423,     0,     0,
       0,     0,   191,     0,   520,     0,     0,   727,     0,     0,
      64,   527,   505,   601,   138,     0,   142,    45,     0,   711,
       0,     0,     0,     0,   930,     0,     0,     0,     0,     0,
     910,   912,     0,   692,   690,     0,    45,   947,    45,   946,
     145,   340,     0,   502,     0,  1179,     0,   727,  1181,     0,
     727,     0,     0,   727,     0,   727,     0,   727,     0,   727,
       0,     0,     0,    45,     0,     0,     0,  1207,     0,  1198,
    1201,   999,   997,   998,    45,   994,     0,     0,     0,   350,
     598,   598,     0,  1024,     0,  1027,     0,    45,  1236,     0,
       0,     0,  1242,     0,     0,     0,  1250,    15,    29,    41,
       0,   173,   160,   117,     0,    45,     0,    45,    27,   159,
     539,   539,   169,   172,   168,     0,   186,   189,   214,     0,
       0,     0,   247,   245,   252,   249,   263,   256,   261,     0,
       0,   215,   238,   250,   242,   253,   243,   258,   244,     0,
     237,     0,   232,   229,   218,   219,     0,     0,   425,   351,
       0,   387,   598,   347,   344,   345,     0,   398,     0,   534,
     533,     0,     0,   581,   352,     0,     0,     0,   351,   588,
     351,   592,   351,   590,   310,     0,   598,   518,     0,     0,
     976,     0,   311,   478,   480,   172,   322,     0,   598,   519,
       0,   980,   598,   979,   318,   320,   726,     0,     0,     0,
     736,     0,     0,     0,     0,   710,   464,   481,   501,     0,
     203,   202,   381,   493,   490,   488,     0,   491,     0,   328,
       0,     0,     0,     0,     0,     0,   963,     0,  1009,     0,
       0,   422,   417,   950,   951,   721,   310,   957,   437,   816,
     819,   825,   294,   293,     0,   287,     0,     0,   289,   288,
       0,   760,   761,   713,     0,   939,   938,     0,   936,     0,
     933,   282,     0,  1015,  1004,     0,  1002,  1005,   755,     0,
       0,   924,   916,   693,     0,     0,     0,     0,     0,   300,
       0,   299,   307,     0,  1188,     0,  1188,  1188,  1124,     0,
    1118,  1120,  1121,  1119,   727,  1123,  1122,     0,  1188,   727,
    1142,  1141,     0,     0,  1185,  1184,     0,     0,  1188,     0,
    1188,     0,   727,  1063,  1067,  1068,  1069,  1065,  1066,  1070,
    1071,  1064,     0,  1152,  1156,  1157,  1158,  1154,  1155,  1159,
    1160,  1153,  1162,  1161,   727,     0,  1105,  1107,  1108,  1106,
     727,     0,  1135,  1136,   727,     0,     0,     0,     0,     0,
       0,  1219,     0,     0,   852,   996,     0,  1022,     0,   598,
       0,  1026,  1029,     0,     0,     0,     0,     0,     0,     0,
       0,  1257,     0,     0,     0,     0,   108,   794,     0,   111,
       0,   173,     0,   146,     0,   171,   170,   267,   253,   266,
       0,   255,   260,   254,   259,     0,     0,     0,     0,     0,
     222,   212,   223,   241,     0,   222,   234,   235,     0,     0,
       0,     0,   278,   223,   279,     0,     0,   227,   268,   273,
     276,   229,   220,     0,   503,     0,   413,   385,   388,     0,
     346,     0,   531,   568,   569,     0,     0,   351,     0,     0,
       0,   778,   772,   788,     0,     0,     0,   525,     0,   340,
     526,     0,   982,     0,     0,     0,   740,     0,   738,   735,
     730,   734,   732,     0,    45,     0,   463,   450,   205,   334,
     336,     0,     0,     0,   965,   960,   131,     0,  1008,   421,
       0,     0,   416,   956,     0,    45,   814,   826,   827,   832,
     836,   829,   837,   838,   833,   835,   834,   830,   831,     0,
       0,     0,     0,   285,     0,     0,     0,     0,   934,   929,
     472,     0,  1001,   727,   911,     0,     0,   886,   104,   306,
     301,   303,   305,     0,     0,     0,  1073,   727,  1074,  1075,
      45,  1116,   727,  1143,  1139,   727,  1188,     0,  1072,    45,
    1076,     0,  1077,     0,  1061,   727,  1150,   727,  1103,   727,
    1133,   727,  1211,  1212,  1213,  1221,  1222,    45,  1216,  1214,
    1215,     0,     0,     0,   393,  1240,   898,   899,   900,   897,
     902,   896,   903,   895,   894,   893,   901,   889,     0,     0,
      45,  1239,  1247,  1248,  1246,  1254,  1252,     0,  1255,    31,
     175,   174,     0,     0,   119,   113,   107,     0,     0,   161,
     598,   166,     0,   248,   246,   264,   257,   262,   216,   222,
     598,     0,   240,   236,   223,     0,   233,     0,   270,   269,
       0,   225,   229,     0,     0,     0,     0,     0,   230,     0,
     426,   386,   348,   397,     0,   583,   595,   597,   596,     0,
     430,     0,     0,   809,     0,   433,     0,   981,   756,   729,
       0,     0,    45,     0,     0,     0,   844,   970,   845,  1014,
       0,  1011,  1013,   420,   419,     0,     0,   818,     0,   828,
       0,   288,     0,     0,   765,   762,   719,   714,   715,   717,
     718,   937,  1003,  1007,     0,     0,   381,     0,     0,     0,
       0,   309,   308,   521,     0,     0,     0,  1117,  1140,     0,
    1187,  1186,     0,     0,     0,  1062,  1151,  1104,  1134,  1220,
       0,     0,   392,     0,     0,     0,  1258,     0,     0,   795,
     162,   163,     0,     0,   239,   598,   241,     0,   280,   228,
       0,   272,   271,   274,   275,   277,   473,     0,   770,   769,
     771,     0,   767,   431,     0,   993,   434,     0,   741,   739,
       0,   731,     0,     0,     0,  1010,   418,     0,     0,     0,
       0,     0,   907,     0,     0,     0,     0,     0,     0,   286,
     291,   290,     0,     0,     0,  1000,   913,   914,     0,   842,
     887,   887,   304,  1089,  1088,  1087,  1094,  1095,  1096,  1093,
    1090,  1092,  1091,  1100,  1097,  1098,  1099,     0,  1084,  1128,
    1127,  1129,  1130,     0,  1189,  1079,  1081,  1080,     0,  1083,
    1082,     0,  1023,     0,     0,   890,   796,     0,   172,   265,
       0,     0,   227,   226,     0,     0,   766,   510,     0,     0,
       0,   466,  1012,   824,   823,     0,   821,   727,   856,     0,
       0,     0,     0,     0,   905,   906,     0,     0,     0,     0,
     716,   918,  1006,    45,     0,     0,     0,     0,     0,  1126,
    1183,  1078,    45,   891,     0,     0,     0,   251,   231,   495,
     768,   757,   744,   737,   742,     0,     0,   820,   861,   857,
       0,     0,     0,     0,     0,     0,     0,     0,   848,     0,
     467,   720,     0,     0,   841,    45,    45,   884,  1086,  1085,
       0,     0,     0,   354,   799,   793,   797,   167,     0,     0,
     465,   822,     0,     0,     0,     0,   854,   846,     0,   840,
       0,   904,   855,     0,   847,     0,   919,     0,     0,     0,
    1125,     0,   892,     0,   496,     0,   747,     0,   745,   748,
     859,   860,     0,   862,   864,     0,     0,     0,   849,   468,
     915,   885,   883,     0,   798,   750,   751,     0,   743,     0,
     858,     0,   853,   839,     0,     0,     0,     0,   749,   752,
     746,   863,     0,     0,   867,   908,   850,  1017,   753,     0,
       0,     0,   865,    45,  1016,   754,   869,   868,    45,     0,
       0,     0,   870,   875,   877,   878,  1018,  1019,     0,     0,
       0,    45,   866,    45,    45,   598,   881,   880,   879,   871,
       0,   873,   874,     0,   876,     0,    45,   882,   872
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     6,     7,   206,   380,   207,   834,   745,   208,
     903,   613,   798,   683,   563,   901,   902,   437,   904,  1221,
    1502,   209,   210,   614,  1124,  1125,   211,   212,   213,   573,
    1282,  1283,  1128,  1284,   214,   215,   216,   217,  1154,   218,
     219,  1155,   220,   576,   221,   222,   223,   224,  1561,  1562,
     918,  1573,   937,  1836,   225,   226,   227,   228,   229,   230,
     779,  1165,  1166,   231,   232,   233,   740,  1076,  1077,   234,
     235,   704,   449,   930,   931,  1589,   932,   933,  1874,  1599,
    1604,  1605,  1875,  1876,  1600,  1601,  1602,  1591,  1592,  1593,
    1594,  1848,  1596,  1597,  1598,  1850,  2088,  1878,  1879,  1880,
    1167,  1168,  1474,  1475,  1963,  1709,  1146,  1147,   236,   454,
     237,   844,  1980,  1981,  1741,  1982,  1027,   712,   713,  1050,
    1051,  1039,  1040,   238,   750,   751,   752,   753,  1092,  1435,
    1436,  1437,   393,   370,   400,  1327,  1613,  1328,   877,   999,
     616,   635,   617,   618,   619,   620,  1241,  1079,   970,  1888,
     817,   621,   622,   623,   624,   625,  1332,  1615,   626,  1302,
    1885,  1400,  1381,  1401,  1020,  1137,   239,   240,  1926,   241,
     242,   686,  1032,  1033,   703,   419,   243,   244,   245,   246,
    1083,  1084,  1429,  1895,  1896,  1070,   247,   248,   249,   250,
    1203,   251,   973,  1340,   252,   372,   721,  1418,   253,   254,
     255,   256,   257,   258,   646,   638,   979,   980,   981,   259,
     260,   261,   996,   997,  1002,  1003,  1004,  1329,   763,   639,
     795,   558,   262,   263,   264,   707,   265,   723,   724,   266,
     761,   762,   267,   495,   829,   830,   832,   268,   269,   759,
     270,   814,   271,   808,   272,   695,  1067,   273,   274,  2137,
    2138,  2139,  2140,  1696,  1064,   403,   715,   716,  1063,  1661,
    1724,  1917,  1918,  2333,  2334,  2397,  2398,  2416,  2428,  2429,
    1729,  1915,   275,   276,  1711,   667,   803,   804,  1903,  2201,
    2202,  1904,   664,   665,   277,   278,   279,   280,  2052,  2053,
    2365,  2366,   281,   748,   749,   282,   700,   701,   283,   679,
     680,   284,   285,  1143,  1144,  1701,  2127,  2295,  2296,  1946,
    1947,  1948,  1949,  1950,   697,  1951,  1952,  1953,  2349,  1228,
    1954,  1955,  1956,  2298,  2374,  2402,  2403,  2433,  2434,  2451,
    2452,  2453,  2454,  2455,  2466,  1957,  2149,  2315,   810,  1825,
    2037,  2038,  2039,  1958,   822,  1489,  1490,  1975,  1161,  2312,
     286,   287,   288,   289,   290,   291,   292,   293,   791,  1163,
    1164,  1717,  1718,   294,   838,   295,   774,   296,   775,   297,
    1140,   298,   299,   300,   301,   302,  1100,  1101,   303,   756,
     304,   305,   306,   675,   676,   307,   308,  1403,  1651,   709,
     309,   310,   770,   311,   312,   313,   314,   315,   316,   317,
    1233,  1234,   318,  1171,  1725,  1726,  2235,   319,  1689,  2120,
    2121,  1727,   320,  2444,   321,   322,   323,   324,  1242,   325,
    1244,   326,   327,   328,   329,   330,  1204,  1772,   856,  1750,
    1751,  1752,  1776,  1777,  1778,  2268,  1779,  1780,  1753,  2155,
    2359,  2257,   331,  1210,  1800,   332,   333,   334,   335,  1194,
    1754,  1755,  1756,  2263,   336,  1212,  1804,   337,  1200,  1759,
    1760,  1761,   338,   339,   340,  1206,  1794,   341,   342,   343,
     344,   345,   346,   347,   348,   349,   350,   351,   352,   353,
     354,   355,  1765,  1766,   857,  1511,   356,   357,   358,   359,
     360,   867,   868,   869,  1222,  1223,  1224,  1229,  1810,  1811,
     361,   362,   363,   364,   365,   366,   367,   368,   882,   883,
     884,   885,   887,   888,   889,   890,   891,   893,   894,   556,
    1832
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -1925
static const yytype_int16 yypact[] =
{
   -1925,    87, -1925, -1925, -1925, -1925,   194,  4567, -1925, -1925,
   -1925,   133, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,   470, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925,    90, -1925, -1925,   625,   152, -1925, -1925, -1925,    90,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925,   278,   278, -1925, -1925, -1925, -1925, -1925,   278,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
     242, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,   278, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925,   306, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
     543,   640, -1925, -1925, -1925, -1925, -1925,    90, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925,    90, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925,   686,   697,   706,   278, -1925,   436,  1469,   717,   436,
   -1925, -1925, -1925,   732,   738,   749,   778, -1925, -1925, -1925,
     590,   793,   278, -1925, -1925,   801,   837,   841,   879,   458,
     174,   886,   906,   930, -1925,   142, -1925, -1925, -1925,   436,
   -1925, -1925, -1925,   947,    32,  1733,  2579, -1925, -1925,  2422,
   -1925,   790, -1925, -1925,  1099, -1925,  1015, -1925, -1925,  1574,
    1015,   955, -1925, -1925,   971, -1925, -1925, -1925,  1020,  1028,
    1034,  1050,  1057, -1925, -1925, -1925, -1925,  1069,   535, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925,  1078, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,   126,
     278,  1084,  1080,  1082,   951,   278,   278,   151,   278, -1925,
     278,   278,  1094, -1925,   107,  1127,   278,   278,   278,   278,
   -1925, -1925,   278, -1925,  1147,   278,   979,   278,  1133, -1925,
   -1925, -1925,   278, -1925,  1159,   278, -1925,   278,  1161,   175,
   -1925,   979, -1925,   278,   278,   278,   278, -1925, -1925, -1925,
   -1925, -1925,   278, -1925,   278,   278,   717,   278,  1167,  1084,
     278,  1181, -1925,   278,   278, -1925, -1925, -1925,  1122,  1183,
     278,   278, -1925,  1192,  1200,   278,  1084,  1203,  2422, -1925,
    1220,  1232,   278, -1925,  1229,   278,  1226, -1925,  1251,   278,
    1084,  1253,  1273, -1925,   951,  1084,   278,   278,  1865,   135,
     278,   148, -1925, -1925,   178, -1925,   190,   278,   278,   278,
    1275,   278,   278,  2422,   153, -1925, -1925,  1287,   278,   278,
     278,   278,   278,  2692,   278, -1925,  1084,   278,  1084,   278,
     278, -1925, -1925,   278, -1925,  1084,   278,  1296,  1307, -1925,
     278, -1925, -1925,  1339, -1925, -1925,  1355, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925,  1357, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,   278,
   -1925, -1925,  1363,  1366,  1373,  2422,  2422,  2422,  2422,  2422,
    1381,  1383, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925,  1265,   118, -1925, -1925,   278,   278,   278,   278,  1392,
   -1925, -1925,  1277,   278,   278, -1925,   300,   278,   278,   278,
     278,   278,   447,   278,  1226,   278,   278,  1167, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925,  1166, -1925, -1925, -1925,
   -1925, -1925, -1925,  2422,  2422,  2422, -1925,  2422, -1925, -1925,
   -1925, -1925, -1925, -1925,  2422,  3376, -1925,    68,  1397, -1925,
    1391, -1925,  1164,  1168,  1400, -1925, -1925,  1386,  2422, -1925,
   -1925,  1377, -1925, -1925,  1390,  1102,  1397, -1925, -1925,   983,
     -31, -1925,  1377, -1925, -1925, -1925,  1404,   285,   177,  3105,
    3105,   278,   278,   278,   278,   278,   278,   278,  1411, -1925,
     278, -1925, -1925, -1925,   199, -1925, -1925,  1406,   278, -1925,
    2422, -1925,  1179,   687, -1925,  1407, -1925, -1925,  1409,  1418,
   -1925, -1925, -1925, -1925, -1925,  2854,   278,  1413, -1925,   278,
    1409,   278, -1925,   951, -1925, -1925, -1925, -1925, -1925, -1925,
    1421, -1925, -1925, -1925, -1925, -1925,  1409, -1925, -1925,  1419,
   -1925, -1925,   559,  1475,   278,   602,    88, -1925,  1420,  1263,
    2422,  1290, -1925,  1428, -1925, -1925,  2422,  2422, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,   278,
   -1925,   278,  1425,   140,   278,   717, -1925, -1925,  1435, -1925,
    1436, -1925,  1430,  2098, -1925,  1441, -1925,   278, -1925, -1925,
   -1925,  1444, -1925,  1015,  1427,  1038, -1925,   278, -1925,  5466,
   -1925,   278,  2422, -1925,  1440, -1925,   278, -1925,   278,   278,
     278,  1397,   885,   278,   278,   278,  1290, -1925,   278,   210,
   -1925, -1925, -1925,  1102,   983, -1925, -1925, -1925, -1925, -1925,
   -1925,   126, -1925,  1406,  1449,  1420, -1925, -1925, -1925, -1925,
   -1925, -1925,   278, -1925, -1925, -1925,  5466, -1925,   107,  1396,
     278, -1925,  1445, -1925, -1925, -1925, -1925,  1448,  3427,   621,
   -1925, -1925,   306,   278,   717, -1925,   278,  1409, -1925,  1451,
    1443, -1925,   278, -1925,  1457,  2422,  2422, -1925,  1409,   278,
     223,   278,  1180,  1180,   339,  1180, -1925,  1453,   376,   378,
     401,   453,   456,   459, -1925,  1409,   865, -1925,  1458, -1925,
     150,   159, -1925,  3472,  5466,  3506,  3580,  1464,  5466,   278,
     278,   404,   636, -1925, -1925, -1925,    54,   642, -1925, -1925,
   -1925, -1925,   831,   652, -1925,   278,   278, -1925, -1925, -1925,
   -1925,   660, -1925, -1925,  1264,  1409, -1925, -1925, -1925, -1925,
    3180,   278, -1925, -1925, -1925, -1925, -1925, -1925, -1925,  1409,
   -1925, -1925, -1925, -1925,  1467, -1925,  1467, -1925, -1925, -1925,
   -1925,   474, -1925,   370, -1925,  1463, -1925, -1925,  3649,  1473,
    1474,  1474,  2919, -1925,  2422,  2422,  2422,  2422,  2422,  2422,
    2422,  2422,  2422,  2422,  2422,  2422,  2422,  2422,  2422,  2422,
    2422,  2422,  2422, -1925,  1422,  1358,  1472,    27,   284,  2422,
   -1925, -1925, -1925,   670,  1592, -1925, -1925, -1925, -1925,   675,
   -1925,  1835,  1374,  2422,  1479,  1102,  1102,  1102,  1102,  1102,
   -1925,  1613, -1925,   285,   285,  1397,  1486, -1925,  3105,  5466,
      92,   186, -1925,  1487,  1490, -1925, -1925,  1409, -1925, -1925,
   -1925, -1925,  1409, -1925,   798, -1925,   126, -1925, -1925, -1925,
     278,  3700,   278,  1484,  2422,  1433, -1925, -1925,   278, -1925,
    2422,  3734, -1925,   742, -1925, -1925,  1466, -1925, -1925,   757,
   -1925,   278, -1925,   278, -1925, -1925,  1475, -1925, -1925, -1925,
   -1925, -1925,  3776,  1409, -1925, -1925, -1925,  1489,  1497,  1498,
    1501,  1503,  1505, -1925,  1263, -1925,   278, -1925,  3811, -1925,
   -1925,   278,  3849,  3901, -1925,  1506,   766,  1514,  1400, -1925,
   -1925,   278, -1925,  1515, -1925,  1502, -1925,   278, -1925,  1376,
    1110, -1925, -1925,   416, -1925, -1925,  1520, -1925,  1516,  1523,
     767, -1925,   278,  3105,  1519, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925,  1524, -1925, -1925,    41,  1526,  1517,
    3932,  3037,   -79, -1925,  1504, -1925, -1925,   792, -1925, -1925,
   -1925, -1925, -1925,   794, -1925,  1518,   811, -1925, -1925, -1925,
    2422, -1925,  1656, -1925, -1925, -1925,   821, -1925,  1541, -1925,
    1263,  1534,  1543,   825, -1925, -1925, -1925,  1547, -1925,  1546,
    1548,  1530,   278,  2422,  2422,  2692, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925,  1550,  1557, -1925,   746, -1925, -1925,  3963,
    3997, -1925,  1545, -1925,   460,  1551, -1925, -1925, -1925, -1925,
     465, -1925, -1925, -1925,   471, -1925,   477,   478,   504, -1925,
     510, -1925,   553, -1925,  1553,  1554,  1556,  1561, -1925,  1562,
    1565, -1925, -1925, -1925, -1925, -1925, -1925,  1397,  1573,  1564,
   -1925,  1571,   -66,   827, -1925, -1925, -1925, -1925,  2422,   656,
     708, -1925,   839, -1925,   843,  1567,  1579, -1925, -1925,  1581,
    1583,  1584, -1925, -1925, -1925,  1585,  1588,  1590, -1925, -1925,
   -1925,   717,   278,   110, -1925,  1596, -1925,  1607, -1925,  1409,
   -1925, -1925, -1925,  1599, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925,   816, -1925, -1925, -1925,   278,  1409,   105,  1749,
   -1925, -1925,   278,   278, -1925,  1415,   370, -1925,  1605, -1925,
    1559,  2422,  3105, -1925,  2422,  1474,  1474,   616,   616,  2919,
     992,  5497,  4282,  5466,  4282,  4282,  4282,  4282,  4282,  5497,
    1976,  1474,  1976,  3537,  1472, -1925, -1925,  1604,  1578,  1946,
   -1925, -1925, -1925, -1925, -1925,  1620, -1925, -1925,   951,  5466,
   -1925,  2422, -1925, -1925, -1925, -1925,  5466,   160,  5466,  1479,
    1479,  1073,  1479,   647, -1925,  1486,  1623,   285,  4028,  1624,
    1625,  1636,  3105,  3105,  3105, -1925, -1925,   278,  1630, -1925,
   -1925,  1631,  1420, -1925,   306, -1925, -1925, -1925, -1925,  1394,
   -1925,   846,   951, -1925,   951,   849,  1644,   861, -1925,  5466,
    2422,  2854, -1925,   868, -1925,   951,  1467, -1925,   768,   791,
   -1925,   869,  1488,   875, -1925,  2106, -1925,    88, -1925,  1638,
     278,   278,  2422,   278, -1925, -1925,  1409, -1925, -1925, -1925,
    1416,   278,  2422,   278, -1925,   278, -1925,  1397,  2422,  1637,
    3037, -1925, -1925, -1925, -1925,   889, -1925,  1641, -1925,  1647,
    1648,  1649,  1459,  2422,   278,   278, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925,  1084,   278, -1925,  3062,  3220,  1654,
     278,   278, -1925,   278, -1925,   -34,   278, -1925,  2422,   278,
   -1925,  1467,  5466, -1925,  1662,    81,  1662, -1925,   278,  1263,
    1671,  3097,   278,   278, -1925,   107,  2422,   454,  2422,   908,
   -1925,  1666,   910,  5466, -1925,    30, -1925, -1925, -1925, -1925,
   -1925,  1084,     9, -1925,   278, -1925,   581, -1925, -1925,   324,
   -1925,   103,   613, -1925,  1172, -1925,   -55, -1925,   -22, -1925,
     278,   278,   278, -1925,   278,   278,   865, -1925,   278, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925,  1354,  1368,  1356,  5466,
   -1925,  1472,   278, -1925,   278, -1925,   278, -1925, -1925,   278,
     278,   278, -1925,   278,   278,   278, -1925, -1925, -1925, -1925,
     278,   668, -1925, -1925,  1673, -1925,  1683, -1925, -1925,  1607,
   -1925, -1925, -1925, -1925,  5466,  2298, -1925, -1925, -1925,   690,
     440,   440,  1446,  1450, -1925, -1925,  1452,  1454,  1455,   500,
     278, -1925, -1925, -1925, -1925,  1686, -1925, -1925, -1925,  1605,
   -1925,  1687, -1925,   598,  1679, -1925,  1690,  4078, -1925,  1685,
    1688,  1400, -1925, -1925,  4118, -1925,  2422,  2422,  1592, -1925,
    5466,  1377,   285, -1925,   192,  3105,  3105,  3105,   215, -1925,
     283, -1925,   408, -1925,  1409,   278, -1925, -1925,  1699,   915,
   -1925,  1703, -1925,  5466, -1925, -1925, -1925,  2422, -1925, -1925,
    2422, -1925, -1925, -1925, -1925,  5466, -1925,  1488,  2422,  1691,
   -1925,  1693,  1695,  4174,  1709, -1925,   100,   278, -1925,   931,
   -1925, -1925,  1692,  5466, -1925, -1925,  4118, -1925,  1376, -1925,
    1376,   278,   278,   278,   934,   948, -1925,   278,  1704,  1701,
    2422,  4208,  3168, -1925, -1925, -1925,  1409,  1397, -1925, -1925,
    1552,  1716,  5466, -1925,   278, -1925,  1711,  1712, -1925, -1925,
    1476,  1720, -1925, -1925,  1723, -1925,  5466,   960, -1925,   964,
   -1925, -1925,  4247, -1925, -1925,   966, -1925, -1925,  5466,  1715,
     278, -1925, -1925, -1925,  1717,  1718,  1532,  1672,   278,   278,
    1724,  1736, -1925,   544, -1925,  1730, -1925, -1925, -1925,  1731,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925,   581, -1925, -1925,
   -1925, -1925,   324,   278, -1925, -1925,   969,  1735, -1925,  1737,
   -1925,  1740, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925,   613, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925,  1172, -1925, -1925, -1925, -1925,
   -1925,   -55, -1925, -1925, -1925,   -22,  1727,  1739,  1741,   678,
     990, -1925,  1746,  1747,  1397, -1925,  1742, -1925,  1756,  1472,
    1751, -1925, -1925,   995,  1117,   997,  1013,  1017,  1019,  1031,
    1066, -1925,  1090,  1752,  2422,  2422, -1925, -1925,  1753, -1925,
    2049,   668,  2389, -1925,  1104, -1925,  5466, -1925, -1925, -1925,
    1748, -1925, -1925, -1925, -1925,    27,    27,    27,    27,    27,
    1415, -1925,  1762,  1775,  1772,  1415,  1679, -1925,   370,   598,
     112,   112, -1925, -1925, -1925,  1113,  1754,  1086,   187, -1925,
    1779,   598, -1925,  2422, -1925,  1769, -1925,  1400, -1925,  1946,
    5466,  1771, -1925, -1925,   983,  1764,  1774,  1115,  1776,  1777,
    1782, -1925, -1925, -1925,  1781,    35,   951, -1925,   278,  1084,
    5466,    35,  5466,  1488,  2422,  1783,  4320,  1116, -1925, -1925,
   -1925, -1925, -1925,  2422, -1925,  1785, -1925, -1925, -1925, -1925,
   -1925,  1123,  1125,  1137, -1925, -1925, -1925,   772, -1925,  5466,
    2422,  2422,  4569, -1925,   278, -1925, -1925,  1716, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,  1780,
      81,  1789,  1038, -1925,   278,   278,   278,  3097, -1925, -1925,
   -1925,   454, -1925, -1925, -1925,  2351,   278, -1925, -1925,  1724,
    1797, -1925, -1925,   278,   278,  2422, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925,   103, -1925, -1925,
   -1925,  2422, -1925,  2422, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925,   278,   278,  1787,   919, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,  1796,  1805,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925,   278, -1925, -1925,
    5466,  5466,  1160,  1814, -1925, -1925, -1925,  1802,  4898, -1925,
   -1925, -1925,   690, -1925, -1925, -1925, -1925, -1925, -1925,  1415,
   -1925,   278, -1925, -1925,  1809,  1800, -1925,   809,   187,   187,
     598, -1925,   598,   112,   112,   112,   112,   112,  1131,  4936,
   -1925, -1925, -1925, -1925,  2422, -1925, -1925, -1925, -1925,  2038,
   -1925,   278,  1820,  1418,   278, -1925,   278, -1925,  4967, -1925,
    2422,  2422, -1925,  4998,  1576,  2422, -1925, -1925, -1925, -1925,
    1184, -1925, -1925,  5466,  5466,  2422,  1816, -1925,   629, -1925,
    2422, -1925,  1812,  1813, -1925, -1925,  1823,  1827, -1925, -1925,
   -1925, -1925, -1925,  1707,  1821,  1185,  1836,  1839,  1191,  1061,
     278, -1925, -1925,  5466,   614,  1832,   377, -1925, -1925,  1817,
   -1925, -1925,   -59,  5029,  5060, -1925, -1925, -1925, -1925, -1925,
    1195,  1834,   929,   278,   278,  1117, -1925,  1850,  1851, -1925,
   -1925, -1925,  1749,  1838, -1925,  1472, -1925,   598, -1925,  1131,
    1840,   187,   187, -1925, -1925, -1925, -1925,  5091, -1925,  4118,
   -1925,  1196, -1925, -1925,   951,  1573, -1925,  1488,  5466, -1925,
    1608, -1925,  1849,  5122,   772, -1925,  5466,  2707,  1853,  1858,
    1860,  1861,  1862,  1864,   278,   278,  1868,  1869,  5153, -1925,
   -1925, -1925,  2422,   278,   278, -1925, -1925,  1871,   278, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925,  1859, -1925, -1925,
   -1925, -1925, -1925,  1205, -1925, -1925, -1925, -1925,  1866, -1925,
   -1925,  1880, -1925,   201,  1881, -1925, -1925,  1883, -1925, -1925,
    1870,  1206,  1131, -1925,  2422,  2038, -1925, -1925,  2422,   278,
    2422, -1925, -1925, -1925,  5466,  1221, -1925, -1925,  1853,   278,
     278,   278,   278,   278, -1925, -1925,  2422,  2422,  2422,  1222,
   -1925, -1925,  1885, -1925,  1224,  1887,  1225,   278,  2422, -1925,
   -1925, -1925, -1925, -1925,  2422,   278,  1875, -1925, -1925,  5184,
   -1925,  5466, -1925, -1925,  1882,  5215,  2707, -1925,   317, -1925,
    1893,  1250,  1259,  1895,  1261,  1889,  1262,  5246,  5277,  1884,
    5308, -1925,   278,  1828, -1925, -1925, -1925, -1925,  1573, -1925,
    5339,  1586,  5370,   970, -1925,  1902, -1925, -1925,  2422,  2073,
   -1925, -1925,  1904,  1909,   278,   278, -1925, -1925,   278, -1925,
    2692, -1925, -1925,  2422, -1925,  2422, -1925,  1897,  1266,  1276,
   -1925,  1905, -1925,   278,  5466,   278,  5466,  1283, -1925, -1925,
   -1925, -1925,  1285, -1925,  1907,  1286,  1291,  1297,  5401,  5466,
   -1925, -1925, -1925,  2422, -1925, -1925,  1908,  2073, -1925,   278,
   -1925,  2422, -1925, -1925,  1912,  2422,  1334,  2422,  1911, -1925,
   -1925, -1925,  5435,  1335, -1925, -1925,  5466,  1918, -1925,  2422,
    2422,  2422,  1919, -1925, -1925, -1925,  5466, -1925, -1925,   -53,
     449,  1336, -1925,  1928,  1930, -1925, -1925, -1925,  1923,  1923,
    1923, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
     411,  1932, -1925,  1818, -1925,  1344, -1925, -1925, -1925
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1925, -1925, -1925, -1925, -1925,     3,  1721,  1046, -1925, -1925,
    -684,   -52, -1925, -1925,  -393, -1925,   681, -1925,   -50,  1041,
   -1925, -1925, -1925,  1973,   -10, -1925, -1925, -1925, -1925, -1925,
   -1925,   115,   393,   773, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925,  -171,  -900, -1925, -1925, -1925,   876,   399,  1393,
   -1925,  -212, -1519,   130, -1925, -1925, -1925, -1925, -1925, -1925,
    1395,  -295,  -335, -1925, -1925, -1925,  1387, -1925,  -477, -1925,
   -1925, -1925, -1925,  1271, -1925, -1925,   682, -1274, -1552,  1044,
     384, -1524,  -209,   -95,  1055, -1925,   123,   143, -1771, -1925,
   -1521, -1214, -1512,  -416, -1925,   -62, -1542, -1364,  -766, -1925,
   -1925,   517,   854,   307,  -116,    56, -1925,   548, -1925, -1925,
   -1925, -1925, -1925,  -132, -1925, -1454,  -507,   994, -1925,   975,
     617,   639,  -372, -1925, -1925,   936, -1925, -1925, -1925, -1925,
     347,   360,  1952,  2457,  -347, -1255,   155,  -424, -1011,   379,
    -546,  -608,  1477,   654,  1587,  -852, -1006, -1925, -1925,  -615,
    -600,  -234, -1925,  -897, -1925,  -594,  -952, -1108, -1925, -1925,
   -1925,   131, -1925, -1925,  1342, -1925, -1925,  1806, -1925,  1807,
   -1925, -1925,   658, -1925,  -375,    10, -1925, -1925,  1815,  1819,
   -1925,   628, -1925,  -729,  -259,  1272, -1925,  1068, -1925, -1925,
     -51, -1925,  1022,   443, -1925,  4370,  -419, -1082, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925,  -197, -1925,   446,  -909, -1925,
   -1925, -1925,   323, -1271,  -620,  1070,  -888,  -371,  -332,  -433,
     797,   -36, -1925, -1925, -1925,  1426, -1925, -1925,   993, -1925,
   -1925,   968, -1925,  1241, -1924,   901, -1925, -1925, -1925,  1429,
   -1925,  1431, -1925,  1424, -1925,  1438,  -988, -1925, -1925, -1925,
    -153,   -82, -1925, -1925, -1925,  -359, -1925,  -258,   674,  -389,
     677, -1925,   -27, -1925, -1925, -1925,  -326, -1925, -1925, -1925,
   -1599, -1925, -1925, -1925, -1925, -1925, -1414,  -533,   128, -1925,
    -189, -1925,  1298,  1085, -1925, -1925,  1089, -1925, -1925, -1925,
   -1925,  -289, -1925, -1925,  1018, -1925, -1925,  1065, -1925,   200,
    1091, -1925, -1925,  -207,   644, -1925, -1925, -1925,  -222, -1925,
   -1925,   170, -1925,  -765,  -381,  -361, -1925, -1925, -1925, -1486,
   -1925, -1925, -1925,  -180, -1925, -1925,  -294, -1925,  -315, -1925,
    -334, -1925,  -331, -1809, -1110,  -761, -1925,  -113,  -478,  -981,
   -1532, -1925, -1925, -1925,  -486, -1506,   409, -1925,  -715, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,  -474, -1445,
     646, -1925,   164, -1925,  1478, -1925,  1634, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1425,   696, -1925,  1388,
   -1925, -1925, -1925, -1925,  1766, -1925, -1925, -1925,   232,  1738,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925,   612, -1925, -1925, -1925,   179, -1925, -1925, -1925, -1925,
     -63, -1846, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925,   524,   366,  -508, -1222,
   -1217, -1405, -1430, -1424, -1417, -1925, -1416, -1408, -1351, -1925,
   -1925, -1925, -1925, -1925,   351, -1925, -1925, -1925, -1925, -1925,
     396, -1400, -1389, -1925, -1925, -1925,   349, -1925, -1925,   394,
   -1925,  -637, -1925, -1925, -1925, -1925,   362, -1925, -1925, -1925,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,
   -1925, -1925,   158, -1925,   163,  -104, -1925, -1925, -1925, -1925,
   -1925, -1925, -1925,   937, -1925, -1925, -1925,  -845, -1925,   146,
   -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925, -1925,   917,
   -1925, -1925, -1925,   925, -1925, -1925, -1925, -1925,   911, -1925,
   -1925
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1189
static const yytype_int16 yytable[] =
{
     406,   811,   672,   823,   824,   825,   826,   698,   815,   971,
     418,   418,   687,  1385,  1380,  1085,  1334,   426,  1290,  1685,
     637,  1590,   710,  1459,   668,   637,  1231,  1240,   975,   864,
    1005,   989,   725,   737,   728,   794,   561,  1719,  1863,   731,
     732,   733,  1098,  1826,   786,  1828,   453,   734,  1742,   692,
     736,  2145,   738,  1074,  1844,  1001,  1001,   939,  1847,   643,
    1823,  1877,   647,   649,  1712,  1341,  1471,  1849,   472,  1610,
    1331,  1331,   475,   420,  1183,  1867,  1414,   755,  1184,   433,
    1882,  1595,   741,   963,  1786,  -541,  1623,     2,     3,   691,
    1787,  2122,   787,  1056,  2072,  1249,  1460,  1788,  1789,   767,
    1362,  1734,   455,  1000,  1000,  1827,  1790,  1775,  1924,  1785,
    1361,  1798,  -584,   783,  1792,   898,  1531,   806,   788,   470,
    1570,    46,   796,   898,   799,  1793,  1909,  1869,  1706,  2456,
     688,   661,  2265,   662,  1018,  1439,  1744,   818,   417,   899,
    1763,   800,   398,   965,   689,   719,  1057,   497,   369,   833,
     588,   836,   555,  1745,    74,   398,   417,  -527,   842,   677,
     398,  1781,  1492,  1791,   417,  1799,  1081,   401,  1240,  1744,
     575,  1226,  1480,   564,  1058,  -565,   611,   847,   582,   584,
    1230,  1739,   583,  1037,   589,  -563,   989,    96,  1451,  1452,
     727,  1399,  2085,   801,  -593,  1931,  1932,  1933,   982,  1044,
    -594,   978,   984,   590,   965,   401,  -593,  1016,  -584,   991,
     417,  -527,  -594,   965,  1059,  1197,  1198,   109,  1201,  -339,
    1017,  1453,  2323,  -587,  2086,  1060,  -541,  1746,  1150,  2266,
    1747,   588,   417,   908,   637,  -587,   417,     4,  1735,   417,
    2267,   417,  1461,   920,   921,   922,   923,   417,  1441,  1532,
     592,  -137,  1533,  1061,   611,   417,   974,   850,  1707,   852,
    1746,  1872,  -339,  1250,  -141,  2457,   853,   854,   855,   -61,
    1157,  -527,   858,   859,   860,   861,  -565,   862,   863,   417,
     417,  -339,  1251,  1099,   593,  1500,  1820,   924,   417,  -565,
    1773,  -591,  1783,  -563,  1796,  1774,  1802,  1784,  2184,  1797,
     994,  1803,  -593,  -591,   417,   915,   436,   417,  -594,   990,
       8,  1034,   992,  1029,   964,   916,  2075,   663,   666,   417,
     718,   493,  1677,   673,   674,   678,   674,  2077,   682,   684,
    1181,  -587,   690,  1019,   694,   696,   696,   699,  1162, -1113,
     702,  1191,  2073,   706,     5,   702,  1062,   588,  1333,  -584,
     717,  1623,  1086,   722,  1925,   702,  1038,   702,  1213,  1065,
    1152,   418,   702,   702,   702,  1786,  1851,  1853,  2122,  1335,
     702,  1787,   735,   702,   989,   702,   989,  1775,  1788,  1789,
    1575,   746,   747,   989,   588,  1296,   588,  1790,   758,   760,
    1785,  1571,  -137,   766,   805,  1792,  1798,   637,  1267,  -591,
     773,   637,  1431,   777,  1001,  -141,  1793,   417,   637,   588,
     -61,  1669,  1291,   417,   417,   790,  -589,  -565,   797,  1499,
    1616,  1139,  1497,   730,   717,   807,   809,   809,  -589,   813,
     790,  1781,  1684,   611,  -563,   819,   821,   821,   821,   821,
     821,  1179,   831,  -593,  1791,   835,  1159,   837,   773,  -594,
    1799,   840,  1359,  1149,   843, -1138,  2407,  1491,   848,  1723,
    1151,   588,  1153,  1426,   588,  2372,   581,   588,   588,  1658,
     688,  2259,  -587,   588,  1629,  1631,  1633,  -792,  -792,   588,
   -1113,  -527,  1294,  1448,   689,   588,   588,   865,  1842,  1843,
     866,  1713, -1060,  1650, -1149,  -527,  1288,  2373,  1178,   417,
    1365,  1180,   881,   886,   892,  1366,  2078,  2079,  1860,  1001,
     900,  1295,   588,   905,   906,   907,   690, -1165,   588,  2186,
     910,   913,   914,  2399,  -589,   919,   690,   690,   690,   690,
    2152,  2148,  1586,   927,   928,  2170,  1821,  1861,  2189,  2458,
    -591,  1847,   658,   659,   989,  1289,  1406,   965,  1160,  1831,
    1849,  2135,  1349,  1350,  1351,  1352,  1353,  1000,   503,   637,
    1773,   588,   895,   896,  2023,  1774,  1103,  1046,  2459, -1168,
    1985,  2399, -1102,  1783,   911, -1132, -1112,  2458,  1784,  1796,
    1047, -1137,  1091,  1802,  1797,  1439,  1590, -1059,  1803,  1587,
    1758,  2071,  1324, -1148, -1164,  1245, -1138,   569,   570,   706,
    1007,   758,   813,   807,   694,  1012,  2459,  2260,   837,  1014,
    1054,  1196,  1246,  1869,  1870,  1871,   690,  2241,  2205,  2243,
   -1167,   944,  1023,  1055,  1440,   945, -1101,  1025,   615,  1175,
    2244,  2245,   398, -1060,   702, -1149,  2246,  1036,  2247,   690,
     399,  1588,  1176,  2275,  1247,  2282,  1595,  2248,   504,  2249,
    1252,  2250,   985,   960,  2472,  2474,   986,  1248, -1165,  -382,
    1258,  2261,  1053,  1253,   974,  -589,  2460,  2478,  1263, -1131,
    2218,  1540,  2262,  1259,  1402,  1806,  1807,  1808,  1336,  1812,
    1813,  1264,  1608,  1342,   988,    50,  1834,  1075,  2274,  1080,
    1240,  1337,  1082,   557,  1835,   588,  1343,  1767,  1441,  1085,
    1734,   552,  -527,  1240,  1093,  1096,  1580,  1581,  1001,  1341,
   -1168,  -383,   553, -1102,   571,  1126, -1132, -1112,  1127,  2191,
    2192,   554, -1137,  1541,  1133,  1132,  1134,   417, -1059,  1098,
    1098,  1141,   562,  1145, -1148, -1164,   690,  1629,  1631,  1633,
     565,   989,   989,   989,   989,   989,   566,  1872,  1914,   663,
    1391,   637,   637,   637,   637,   637,  1000,   567,  1609,  2326,
    1096, -1167,  1563,  1392,  2219,  1395,   690, -1101,  1001,  1001,
    1001,  -382,  1744,  2251,  1423,  1445,  2252,  1723,  1396,  2119,
    1569,   690,  1586,   963,   690,   118,   568,  1424,  1446,  1745,
    1187,  1186,   122,  2314,  -383,  2071,  1407,  2346,  1439,  1193,
    1463,   574,  1466,  1199,  1744,   627,   966,  1415,  1205,   577,
   -1131,  1209,  1211,  1464,  2342,  1467,  1628,  1630,  1632,  1469,
    2220,  1745,  1232,  1566,  1567,  2083,  2084,   769,  2438,  1016,
    2188,  2358,  1470,  1483,  1491,  1534,  1491,  1735,  2221,  1587,
    2445,  1368,  1477,  1260,   835,   578,  1484,  1542,  1535,   579,
    2222,  1544,  2223,  2224,  1636,  2225,  2344,  1238,  2226,  1285,
    1543,  2253,   816,  1746,  1545,  1369,  1747,  1637,  1557,  1046,
    1640,  1748,   828,  1292,  2341,  1293,  1336,  1648,  1768,  1749,
    1370,  1297,  1642,  1652,  1300,  1371,  1372,   580,  -384,  1645,
    1649,  1588,  1373,  1138,   585,  1746,  1653,  1678,  1747,  2100,
    1262,  1479,  1769,  2063,  2064,  2065,  2066,  2067,   636,  1665,
    1679,  1441,  1770,   636,   586,  1286,  1730,  1771,  1238,  2254,
    2255,  2256,  -387,  1395,   873,   874,   875,   876,   878,  1731,
    2280,  1733,  -385,  2406,   969,  1506,  1907,  1277,   587,  1238,
    1278,  1509,  1238,  2059,   969,  1512,  1670,  1514,  1671,  2227,
    2405,  1516,  1928,  1518,   591,  1934,  1445,  1576,  1331,  1331,
    1331,  1331,  1331,   650,   663,  1618,  1619,  1379,  1967,  1935,
    1383,  1621,  1483,   965,  1971,  1959,   678,  1997,  -852,   651,
    1616,  1968,   938,   940,   941,  1969,   942,  1972,   985,   699,
    1998,  2200,   986,   943,  2015,  2016,  2101,   944,  2017,   632,
     633,   945,  2106,  1528,  1650,  2040,   987,   816,   946,   947,
    1638,  2018,  1214,  1215,  1416,  1491,  2025,  1891,  2041,   722,
     988,  1730,  1662,  1646,  1664,  1483,  1374,  1730,   652,   960,
     642,  1001,  1001,  1001,  2042,   747,   653,  1657,  2043,  1466,
    2044,  2176,   654,  1105,  1442,  1216,  1668,  1106,   601,  1021,
     760,   640,  2045,  1240,  1107,  1108,   648,  1375,   655,  1399,
    1109,  1110,  1111,  1112,  1031,   656,  1113,  1114,  1115,  1116,
    1117,  1118,  1119,  1120,  1466,  1121,  1122,   657,   985,  1897,
    1630,  1632,   986,  2026,   743,  2027,   660,  2046,  1465,   632,
     633,   398,  1052,  -230,  2028,   669,  2029,   670,  2047,  1068,
     671,  1376,  2083,  2084,   629,  1072,  1073,  1687,   630,   685,
     988,  2048,  2060,   781,   631,   632,   633,   793,   632,   633,
     789,  2080,   636,  -587,  2111,  2061,   606,  1901,   708,   606,
     754,  1528,  1495,  1528,  2081,   608,  1886,  2112,   608,  2026,
     711,  2027,   693,  1507,  2116,  1528,  2117,  2083,  2084,  1510,
    2028,  1130,  2029,  1513,  1738,  1515,  1513,  1513,  2118,  1517,
    1666,  1519,   705,   609,  1852,  1854,   609,  1254,  2177,  1819,
    1255,  1256,  1257,    25,   720,  1526,   726,  2200,    29,  1217,
    1218,  2178,   739,  1219,  1220,  1536,  1537,  1538,  1894,  1943,
     417,   610,  2214,  1175,   610,  1240,   744,   881,   757,  1483,
      47,    48,   886,  1528,  2285,  2215,  2237,   764,   892,  1700,
    1558,   900,  2239,  2318,  2080,   765,  2271,  2286,   768,  2030,
    2031,  2309,  2032,  2033,  1189,  1190,  2319,  2328,   634,  2336,
    1238,   634,  1483,  2356,  1568,   771,  1986,   776,  1988,  1989,
     690,  1889,  2337,  2351,  1297,  2354,  2357,   772,   611,  1757,
    1993,   611,  1762,  2034,    90,  1782,  1767,  1795,  1445,  1801,
    2000,  1805,  2002,   778,    95,  1905,   780,  1528,   784,  1483,
    1730,  2376,  2091,  1575,  2040,  2030,  2031,  1911,  2032,  2033,
    2377,  1913,  2379,  2381,  2040,   636,   989,  2411,   785,   636,
     812,  2417,  1927,  2419,  1445,  2035,   636,  2412,  2288,  1483,
     612,   995,   820,   612,  2418,  1175,  2420,  2422,  2036,  2034,
     121,   845,  2423,   599,   600,  1634,  1355,  1356,  2424,  2193,
    2194,  2195,   846,  1305,  1306,  1307,  1308,  1309,  1310,  1311,
    1312,  1313,  1314,  1315,  1316,  1317,  1318,  1319,  1320,  1321,
    1322,  1323,  1238,  2441,  2461,  1647,  1829,  1830,   816,  2467,
    2468,  2035,  1612,  1339,   849,  2437,  2442,  2462,  1660,  1660,
    1346,  1660,  1348,  1744,  2036,  2477,  1898,  1899,  1900,   702,
     851,  1075, -1188,  1075,  2388,  2389,  1672,  1358,   870,   985,
    1745,   871,   976,   986,  1207,  1208,   977,   637,   872,   897,
     632,   633,   793,   632,   633,  1347,   879,   987,   880,   909,
     965,   912,  2426,   878,   606,   936,   966,   972,  1695,  1389,
     967,   988,   993,   608,   968,   969,   983,  1145,  1013,  1022,
    1704,   801,  1024,  1708,  1026,  1052,  1028,  1710,  1035,  1041,
    1579,  1580,  1581,   690,  1045,   401,  1071,  1768,  1066,  1069,
     963,   609,  1582,  1087,  1088,  1089,  1736,   636,  1737,  1094,
    1740,  1583,  1102,  1104,  1746,  1131,  1158,  1747,  1169,  1182,
    1172,  1769,  1748,  1173,  1185,  1188,  1225,  1196,  1202,   610,
    1749,  1770,  1238,  1809,  1584,   588,  1771,  1265,  1301,  1585,
    1048,  1304,  1049,   945,  1232,   601,  1325,  1326,   986,   602,
     603,   604,   605,  -342,  1357,  1363,  1991,  1824,  1364,  1384,
    1386,  1994,   606,  1394,  1408,  1434,   634,  1586,  1833,   607,
    1458,   608,  1409,  1410,  2004,  1127,  1411,  1840,  1412,  2311,
    1413,  1422,  1425,  1428,  1227,  1227,   611,  -931,  1430,  1472,
    1444,  1443,    25,   781,  2102,  1455,  2006,    29,  1862,   609,
    1449,  1462,  2008,   978,  1468,  1450,  2010,  1454,  1478,  1481,
    1482,  1873,   878,  1493,   828,  1485,  1488,  2091,  1496,    47,
      48,  1486,  2104,  1487,  1587,  1498,  1505,   610,  1520,  1521,
    1672,  1522,  1508,  1672,  1672,  1672,  1523,  1524,   612,   645,
    1525,  1528,  1546,  1902,  1098,  1529,  1612,  2345,  1491,   631,
     632,   633,  1530,   559,  1547,  2147,  1549,  1338,  1550,  1551,
    1553,   606,   601,  1554,   417,  1555,   602,   603,   604,   605,
     608,  1560,   915,    90,  1565,   702,  1588,  1539,   985,   606,
    1603,  1606,   986,    95,   611,  1611,   607,  1617,   608,   632,
     633,  1622,  1625,  1626,  1354,  1936,   987,  2386,   609,   636,
     636,   636,   636,   636,  1627,   369,   399,   995,   995,  1635,
     988,  1641,  1082,  1658,   978,  1667,   609,  1680,  1675,  2404,
    1098,   985,  1681,  1682,  1683,   986,   610,  1440,  1574,   121,
    1705,  2240,   632,   633,   781,  1694,   612,  1473,  1714,   987,
    1607,  1732,  1816,  1817,   610,  1818,  1978,  1979,  1837,  2182,
    1839,  1865,  1855,   988,  1881,  1868,  1856,   781,  1857,  2185,
    1858,  1859,   560,   634,  2404,  1883,  1886,  1906,  1614,  1887,
    1908,  1996,  1919,   150,  1920,  2143,  1921,  1923,  -494,  1937,
    1620,   417,  1938,   611,  1945,  1944,  1961,  1962,  1965,  2154,
    1966,  1964,  1976,  1977,  2157,  1427,  1973,  2158,  2304,  2305,
    1735,   611,  1983,  1734,  1984,  1987,  1990,  2165,  2012,  2166,
    1999,  2167,  2001,  2168,  1572,  2003,  2062,  2021,   978,   601,
    2013,  2082,  2014,   602,   603,   604,   605,  2019,  2020,  1643,
    1031,  2022,  2024,  2049,  2054,   612,   606,  -381,    13,    14,
    2069,    15,    16,   607,  1655,   608,    20,  2070,  2087,  2090,
    2094,  1663,  2093,   612,    23,  2095,  2099,  2096,  2097,    27,
    2115,   878,    30,  2098,  2109,  2150,  2130,  1673,  2172,  1676,
      37,  2173,    38,   609,    40,  2132,  2074,  1873,  1873,  1873,
    2174,  2179,   878,  2180,  -224,  2340,  2187,  2343,  2204,  1873,
    2212,  2217,  2287,  2230,  2231,  2233,  1691,    59,  2232,  2234,
    1344,   610,  2236,  -843,  1345,   601,  2238,  1702,    70,   602,
     603,   604,   605,  2258,  2264,  2272,   678,  2276,  2277,  2279,
    1716,  2283,   606,  2289,  2290,  1722,  2317,  1728,  2297,   607,
     792,   608,    85,  2299,  2114,  2300,  2301,  2302,   417,  2303,
     793,   632,   633,  2306,  2307,    93,  2313,  2321,  2322,  2324,
    2325,  2327,   606,  2352,  2355,  2128,  2367,  2369,   611,   609,
    2375,   608,  2378,   102,  2380,  2384,  2387,   742,  1708,   104,
    2393,  2400,  1902,  2391,  2136,   978,  2401,   108,  2410,   110,
    2413,   112,  2421,   114,  2146,  2427,  2443,   610,  2439,   609,
     119,  2151,  1740,  2435,  2448,  2463,   742,  2464,  2465,  2476,
    2156,   572,  1261,   742,  1559,  1266,  2458,   130,   131,  2162,
     612,   802,  2133,  1457,  1846,  2055,   601,   610,  1838,  1501,
     602,   603,   604,   605,   417,   143,  1438,  1809,  1841,   917,
    2278,  2056,   598,   606,   929,  1042,  1578,  1298,  2281,   926,
     607,   944,   608,  1866,   611,   945,   155,  2190,  1299,   156,
    2175,  2076,   946,   947,   634,  1890,   816,  1339,   948,   949,
    2183,   951,  1721,  2068,   953,   954,   955,   956,   957,  1476,
     609,   995,  1960,   960,   611,  2229,  2131,  1703,  2242,  1862,
    1387,  1404,  1654,  1639,  1433,  1929,  1910,   432,  1873,  1912,
    1873,  1873,  1873,  1873,  1873,  1873,   612,  1916,   610,  2338,
    1930,  2475,  2105,  2198,  2092,  1457,   782,  1043,   601,  1644,
     594,   595,   602,   603,   604,   605,  1674,  1393,  1148,   596,
     925,  1892,  2210,   597,  1419,   606,   612,  1893,  1360,  1939,
    1447,  1942,   607,  1177,   608,   417,  1494,  1006,  1723,  1010,
    2310,  1656,  1008,   601,  2209,  1009,  1659,   602,   603,   604,
     605,  2430,  2134,  1268,  1011,   611,  2330,   809,  1979,  1156,
     606,  1378,   609,  1377,  2414,  1432,  1397,   607,  2103,   608,
    1699,  1048,   978,  2395,  2371,  1697,   601,  2129,  2339,  1388,
     602,   603,   604,   605,   742,  2431,  2447,  2469,  2316,  1720,
     610,  2141,  2471,   606,   839,  1873,  1269,   609,  1015,  1974,
     607,  1686,   608,  1095,   681,  2107,  1815,   612,  2005,   729,
    2142,  2292,  2009,  1992,  2011,  2161,  1995,  2007,  1743,  2159,
    2320,    25,  1527,  2169,  1548,   610,    29,   417,  1271,  1556,
     609,     0,   821,   821,  1227,  1227,  1227,  1552,  1227,  1227,
       0,  2136,  1814,     0,     0,     0,     0,   611,    47,    48,
       0,     0,  1272,     0,  1273,     0,   781,     0,   610,     0,
    1227,     0,   417,     0,     0,     0,     0,     0,     0,   781,
       0,     0,     0,  2050,  2051,     0,  1078,     0,     0,     0,
       0,  2058,   611,     0,     0,     0,     0,  1274,     0,     0,
       0,     0,  1275,     0,  1097,   417,  1276,  2332,     0,   612,
    1277,     0,    90,  1278,     0,     0,     0,  1096,  1129,  1096,
       0,     0,    95,     0,     0,   611,  1135,  1136,     0,     0,
       0,  1142,  2089,  2353,     0,  1279,     0,     0,  1614,     0,
       0,     0,  2361,  2364,   612,   636,   995,     0,     0,  1280,
       0,     0,     0,     0,     0,  1281,     0,     0,     0,  1097,
       0,     0,     0,  2108,     0,     0,     0,  1170,   121,     0,
       0,     0,  2113,  1845,     0,  1824,  1824,   612,   601,     0,
       0,     0,   602,   603,   604,   605,     0,     0,     0,  2123,
    2124,     0,     0,     0,     0,   606,  1192,     0,  1195,  1090,
       0,     0,   607,     0,   608,  1227,  1227,  1227,     0,     0,
       0,  2364,   150,  2415,     0,     0,  1716,   742,   742,     0,
       0,     0,     0,     0,   828,     0,  1239,  1243,   827,     0,
       0,   601,   609,     0,  2153,   602,   603,   604,   605,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   606,     0,
    2163,     0,  2164,     0,     0,   607,     0,   608,     0,     0,
     610,     0,     0,  2449,  2057,     0,     0,     0,  2450,   601,
       0,     0,     0,   602,   603,   604,   605,     0,     0,     0,
       0,  2450,     0,  2470,  2473,   609,   606,     0,     0,     0,
       0,     0,     0,   607,     0,   608,  2473,   417,     0,  2144,
       0,     0,   601,     0,     0,     0,   602,   603,   604,   605,
       0,     0,     0,   610,  1330,  1330,     0,   611,     0,   606,
       0,     0,     0,   609,     0,     0,   607,     0,   608,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   371,
     742,   742,     0,  2197,     0,   377,     0,     0,  2199,     0,
     417,   610,     0,     0,     0,   384,   609,     0,   386,  2208,
    1916,   389,     0,     0,  2213,     0,     0,  1239,   395,   612,
     611,     0,   402,     0,  2216,     0,   405,     0,     0,  2228,
       0,     0,     0,     0,   610,     0,     0,     0,   417,     0,
    1398,     0,     0,     0,   424,     0,     0,     0,   428,   429,
       0,     0,     0,     0,   434,   435,     0,     0,   611,     0,
     440,   441,     0,   443,   444,   445,   446,     0,   447,     0,
       0,   417,   612,     0,     0,     0,     0,   456,   742,   781,
       0,  1574,   460,     0,   462,   781,     0,     0,   465,     0,
       0,   611,   469,     0,   471,     0,     0,     0,     0,     0,
       0,   477,     0,     0,     0,   481,     0,     0,   962,   484,
     612,   486,     0,     0,     0,     0,  2294,     0,   494,   496,
       0,     0,   498,   499,     0,     0,     0,     0,   505,     0,
     506,   878,     0,     0,   510,     0,     0,     0,     0,     0,
       0,     0,     0,   612,    13,    14,     0,    15,    16,     0,
       0,     0,    20,     0,     0,     0,     0,     0,     0,     0,
      23,   537,     0,   539,     0,    27,     0,     0,    30,  1097,
       0,     0,     0,     0,     0,     0,    37,     0,    38,     0,
      40,     0,     0,  2329,  2199,     0,     0,  2331,     0,  2335,
       0,     0,     0,     0,     0,  1227,     0,     0,     0,     0,
       0,     0,     0,    59,     0,  2347,  2348,  2350,     0,     0,
       0,     0,     0,     0,    70,     0,     0,  2360,     0,   827,
       0,   781,   601,  2362,     0,     0,   602,   603,   604,   605,
       0,     0,  2293,     0,     0,  2294,     0,   601,    85,   606,
       0,   602,   603,   604,   605,     0,   607,     0,   608,     0,
       0,    93,     0,     0,   606,     0,     0,     0,  1123,     0,
       0,   607,   962,   608,     0,     0,     0,  2394,  2396,   102,
       0,     0,     0,     0,     0,   104,   609,     0,  1227,   828,
       0,     0,  2408,   108,  2409,   110,     0,   112,     0,   114,
    1577,   609,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,   610,     0,     0,     0,     0,   962,
       0,     0,   878,   130,   131,     0,  2396,     0,     0,   610,
    2432,   962,     0,     0,  2436,     0,  1728,     0,     0,     0,
       0,   143,     0,     0,     0,     0,     0,     0,  1728,  2446,
    2432,   417,     0,     0,     0,     0,     0,  2273,  1227,     0,
       0,     0,   155,     0,   742,   156,   417,     0,     0,     0,
       0,   611,     0,     0,     0,     0,   962,   962,   962,   962,
       0,   962,     0,     0,     0,     0,   611,   714,     0,     0,
       0,  1030,     0,     0,   601,     0,     0,     0,   602,   603,
     604,   605,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   606,     0,     0,     0,     0,     0,     0,   607,     0,
     608,     0,     0,   612,     0,     0,     0,     0,     0,     0,
    1078,     0,  1078,     0,     0,     0,     0,     0,   612,     0,
       0,   962,     0,   962,   962,   962,   962,     0,   609,     0,
       0,  1097,  1097,     0,   944,     0,     0,     0,   945,     0,
       0,     0,  1688,     0,     0,   946,   947,     0,   742,     0,
    1698,     0,   949,  1142,   951,     0,   610,   953,   954,   955,
     956,   957,     0,     0,  1227,   802,   960,     0,     0,  1097,
    1097,   841,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1227,   962,     0,     0,     0,     0,     0,  2363,     0,
       0,   742,     0,   417,     0,     0,     0,     0,  1764,     0,
       0,     0,     0,     0,   962,     0,     0,   742,   742,   742,
       0,   742,   742,   611,   962,   742,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1239,
       0,  1822,     0,   742,     0,   962,  1097,  1097,  1097,     0,
    1142,  1142,  1239,     0,     0,     0,     0,     0,     0,     0,
       0,   962,     0,     0,  1457,   962,   962,   601,     0,     0,
       0,   602,   603,   604,   605,   612,     0,     0,     0,     0,
       0,     0,     0,     0,   606,     0,     0,  1864,     0,  1690,
       0,   607,   601,   608,     0,     0,   602,   603,   604,   605,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   606,
       0,     0,     0,     0,     0,     0,   607,     0,   608,   742,
       0,   609,     0,   962,  1715,     0,     0,   601,     0,     0,
       0,   602,   603,   604,   605,   601,     0,     0,     0,   602,
     998,   604,   605,     0,   606,     0,   609,     0,     0,   610,
       0,   607,   606,   608,     0,     0,     0,     0,     0,   607,
       0,   608,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   610,     0,     0,     0,   742,   742,
     742,   609,   962,   962,     0,     0,   417,     0,     0,   609,
       0,     0,     0,     0,     0,  1941,     0,     0,   601,     0,
       0,     0,   602,   603,   604,   605,   611,     0,     0,   610,
       0,   417,     0,     0,     0,   606,     0,   610,     0,     0,
       0,     0,   607,     0,   608,     0,     0,  1097,     0,     0,
       0,   611,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1268,   944,   417,  1692,     0,   945,
     601,     0,   609,     0,   417,     0,   946,   947,   612,     0,
       0,  1693,   948,   949,   950,   951,   611,     0,   953,   954,
     955,   956,   957,   958,   611,   959,     0,   960,   961,     0,
     610,     0,     0,   612,     0,     0,     0,  1269,     0,     0,
       0,     0,     0,  1270,     0,     0,     0,     0,   962,   962,
     962,   962,   962,   962,   962,   962,   962,   962,   962,   962,
     962,   962,   962,   962,   962,   962,   962,   417,   612,  1271,
       0,     0,     0,     0,     0,     0,   612,     0,     0,     0,
       0,     0,   962,     0,     0,     0,     0,   611,     0,   962,
       0,   962,     0,  1272,     0,  1273,     0,     0,     0,     0,
       0,   962,  1330,  1330,  1330,  1330,  1330,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1274,     0,
       0,     0,   962,  1275,     0,     0,     0,  1276,     0,   612,
    1287,  1277,     0,     0,  1278,     0,     0,     0,     0,     0,
       0,   944,  1239,     0,     0,   945,   601,     0,  1398,     0,
       0,     0,   946,   947,     0,     0,  1279,     0,   948,   949,
     950,   951,   952,     0,   953,   954,   955,   956,   957,   958,
    1280,   959,     0,   960,   961,     0,  1281,     0,     0,     0,
       0,  2126,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   962,   944,     0,  1174,     0,   945,   601,     0,     0,
       0,     0,   802,   946,   947,   962,     0,     0,     0,   948,
     949,   950,   951,  1097,     0,   953,   954,   955,   956,   957,
     958,     0,   959,     0,   960,   961,   962,     0,     0,     0,
    1367,     0,     0,     0,  2160,     0,     0,   944,     0,     0,
    1235,   945,   601,     0,     0,     0,     0,     0,   946,   947,
       0,     0,     0,     0,   948,   949,   950,   951,   742,  2171,
     953,   954,   955,   956,   957,   958,     0,   959,     0,   960,
     961,   944,   962,     0,  1236,   945,   601,     0,     0,     0,
       0,     0,   946,   947,  1239,     0,     0,     0,   948,   949,
     950,   951,     0,     0,   953,   954,   955,   956,   957,   958,
       0,   959,   944,   960,   961,     0,   945,   962,  1864,     0,
       0,     0,     0,   946,   947,     0,     0,     0,     0,   948,
     949,   950,   951,     0,     0,   953,   954,   955,   956,   957,
     958,     0,   959,     0,   960,   961,     0,     0,  2203,     0,
     962,   742,     0,  2206,     0,   944,     0,   962,  1237,   945,
     601,     0,     0,   962,     0,     0,   946,   947,     0,     0,
       0,     0,   948,   949,   950,   951,     0,     0,   953,   954,
     955,   956,   957,   958,     0,   959,   962,   960,   961,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   962,     0,
       0,     0,     0,     0,     0,     0,   962,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   962,     0,     0,   962,
     742,   742,     0,     0,   944,     0,     0,     0,   945,   601,
       0,     0,     0,     0,   962,   946,   947,     0,     0,     0,
    1303,   948,   949,   950,   951,   962,     0,   953,   954,   955,
     956,   957,   958,     0,   959,     0,   960,   961,     0,   962,
       0,     0,     0,     0,     0,   962,     0,     0,     0,     0,
       0,   962,     0,     0,     0,   944,     0,     0,     0,   945,
     601,  1097,     0,     0,     0,  1097,   946,   947,     0,     0,
       0,  1382,   948,   949,   950,   951,     0,  1564,   953,   954,
     955,   956,   957,   958,     0,   959,     0,   960,   961,   944,
       0,  1390,     0,   945,   601,     0,     0,     0,     0,     0,
     946,   947,     0,     0,     0,     0,   948,   949,   950,   951,
       0,     0,   953,   954,   955,   956,   957,   958,     0,   959,
       0,   960,   961,     0,     0,     0,  1097,   742,  1097,  1097,
    1097,   944,     0,  1405,     0,   945,   601,     0,     0,     0,
       0,     0,   946,   947,   742,     0,     0,     0,   948,   949,
     950,   951,     0,     0,   953,   954,   955,   956,   957,   958,
       0,   959,     0,   960,   961,     0,   944,     0,     0,   962,
     945,   601,     0,     0,     0,     0,     0,   946,   947,  1097,
       0,     0,  1417,   948,   949,   950,   951,     0,     0,   953,
     954,   955,   956,   957,   958,     0,   959,     0,   960,   961,
       0,  1097,  1097,     0,   944,  1097,     0,     0,   945,   601,
       0,     0,     0,   962,     0,   946,   947,     0,     0,     0,
    1420,   948,   949,   950,   951,     0,     0,   953,   954,   955,
     956,   957,   958,   962,   959,   962,   960,   961,     0,   962,
       0,     0,     0,     0,     0,     0,  1097,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   944,     0,     0,     0,
     945,   601,   962,     0,     0,   962,     0,   946,   947,     0,
       0,     0,  1421,   948,   949,   950,   951,     0,     0,   953,
     954,   955,   956,   957,   958,  1123,   959,   944,   960,   961,
       0,   945,   601,     0,     0,     0,     0,     0,   946,   947,
       0,     0,     0,  1456,   948,   949,   950,   951,     0,     0,
     953,   954,   955,   956,   957,   958,     0,   959,   944,   960,
     961,     0,   945,   601,     0,     0,     0,     0,     0,   946,
     947,     0,     0,     0,  1503,   948,   949,   950,   951,     0,
       0,   953,   954,   955,   956,   957,   958,     0,   959,     0,
     960,   961,   944,     0,     0,     0,   945,   601,     0,     0,
       0,     0,     0,   946,   947,     0,     0,     0,  1504,   948,
     949,   950,   951,   962,   962,   953,   954,   955,   956,   957,
     958,   962,   959,   944,   960,   961,     0,   945,   601,     0,
       0,     0,     0,     0,   946,   947,     0,     0,     0,  1624,
     948,   949,   950,   951,     0,     0,   953,   954,   955,   956,
     957,   958,   962,   959,     0,   960,   961,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   962,     0,   944,     0,     0,   962,   945,   601,     0,
       0,     0,     0,     0,   946,   947,   962,   962,     0,  1884,
     948,   949,   950,   951,     0,     0,   953,   954,   955,   956,
     957,   958,     0,   959,     0,   960,   961,     0,     0,     0,
       0,     0,     0,   944,     0,  1692,   962,   945,   601,     0,
       0,     0,     0,     0,   946,   947,   962,   962,     0,     0,
     948,   949,   950,   951,     0,     0,   953,   954,   955,   956,
     957,   958,     0,   959,     0,   960,   961,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     962,     0,   962,     0,     0,     0,     0,     0,     0,   944,
       0,   962,     0,   945,   601,     0,   962,     0,     0,   962,
     946,   947,     0,     0,     0,  1922,   948,   949,   950,   951,
       0,   962,   953,   954,   955,   956,   957,   958,     0,   959,
       0,   960,   961,   944,     0,  1940,     0,   945,   601,     0,
       0,     0,     0,     0,   946,   947,     0,     0,     0,     0,
     948,   949,   950,   951,     0,     0,   953,   954,   955,   956,
     957,   958,     0,   959,     0,   960,   961,     0,     0,     0,
       0,     0,   944,     0,     0,     0,   945,   601,     0,     0,
       0,     0,     0,   946,   947,     0,     0,   962,  1970,   948,
     949,   950,   951,     0,     0,   953,   954,   955,   956,   957,
     958,     0,   959,     0,   960,   961,     0,   944,     0,     0,
       0,   945,     0,     0,     0,     0,     0,     0,   946,   947,
       0,     0,   962,     0,   962,   949,     0, -1189,   962,     0,
   -1189, -1189, -1189, -1189, -1189,     0,     0,     0,     0,   960,
     962,   962,     0,   962,     0,   944,     0,  2110,     0,   945,
     601,     0,     0,   962,     0,   962,   946,   947,     0,     0,
       0,     0,   948,   949,   950,   951,     0,     0,   953,   954,
     955,   956,   957,   958,     0,   959,     0,   960,   961,     0,
       0,     0,     0,     0,     0,     0,     0,   962,     0,   962,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   962,   962,     0,   373,   374,   375,   376,     0,   378,
       0,   379,     0,   381,   382,     0,   383,     0,     0,     0,
     385,     0,   387,   388,     0,   962,   390,   391,   392,   962,
     394,     0,   396,   397,     0,     0,   404,     0,     0,   962,
       0,   407,   408,   409,   410,   411,   412,   413,   414,     0,
     415,   416,     0,     0,   421,   422,   423,     0,   425,     0,
     427,     0,     0,   430,   431,     0,     0,     0,     0,     0,
       0,   438,   439,     0,     0,   442,     0,     0,     0,     0,
       0,     0,   448,     0,   450,     0,   451,   452,     0,     0,
       0,   457,   458,   459,     0,     0,   461,     0,     0,   463,
     464,     0,   466,   467,   468,     0,     0,     0,     0,   473,
     474,     0,     0,   476,     0,   478,   479,   480,     0,   482,
     483,     0,     0,   485,     0,   487,   488,   489,   490,   491,
     492,     0,     0,     0,     0,     0,     0,   500,   501,   502,
       0,     0,     0,     0,   507,   508,   509,     0,   511,   512,
     513,   514,   515,   516,   517,   518,   519,   520,   521,   522,
     523,   524,   525,   526,   527,   528,   529,   530,   531,   532,
     533,   534,   535,   536,     0,   538,     0,   540,     0,   541,
       0,   542,   543,   544,   545,   546,   547,   548,   549,   550,
     551,     0,     0,     9,   944,     0,  2125,     0,   945,   601,
      10,     0,     0,     0,     0,   946,   947,     0,     0,     0,
       0,   948,   949,   950,   951,     0,     0,   953,   954,   955,
     956,   957,   958,     0,   959,     0,   960,   961,     0,     0,
      11,    12,    13,    14,     0,    15,    16,    17,    18,    19,
      20,     0,   628,    21,    22,   641,     0,   644,    23,    24,
      25,     0,    26,    27,    28,    29,    30,    31,     0,    32,
      33,    34,    35,    36,    37,     0,    38,    39,    40,    41,
      42,    43,     0,     0,    44,    45,    46,    47,    48,     0,
       0,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,    71,     0,    72,    73,     0,    74,
      75,    76,     0,     0,    77,     0,     0,    78,    79,     0,
      80,    81,    82,    83,     0,    84,    85,    86,    87,    88,
      89,    90,    91,    92,     0,     0,     0,     0,     0,    93,
      94,    95,    96,     0,     0,     0,     0,    97,     0,     0,
      98,    99,     0,     0,   100,   101,     0,   102,     0,     0,
       0,   103,     0,   104,     0,   105,     0,     0,     0,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,     0,
     116,   117,   118,     0,   119,     0,   120,   121,     0,   122,
       0,   123,   124,   125,   126,     0,     0,   127,   128,   129,
       0,   130,   131,   132,     0,   133,   134,   135,     0,   136,
       0,   137,   138,   139,   140,   141,     0,   142,     0,   143,
     144,     0,     0,   145,   146,   147,     0,     0,   148,   149,
       0,   150,   151,     0,   152,   153,     0,     0,     0,   154,
     155,     0,     0,   156,     0,     0,   157,     0,     0,     0,
     158,   159,     0,     0,   160,   161,   162,     0,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
     174,     0,     0,   175,     0,     0,     0,   176,   177,   178,
     179,   180,     0,   181,   182,     0,     0,   183,   184,   185,
     186,     0,     0,     0,     0,   187,   188,   189,   190,   191,
     192,     0,     0,     0,     0,     0,     0,     0,     0,   193,
     194,   195,   196,   197,     0,     0,     0,   198,   199,   200,
     201,   202,   203,   944,   204,   205,     0,   945,   601,     0,
       0,     0,     0,     0,   946,   947,     0,     0,     0,  2181,
     948,   949,   950,   951,     0,     0,   953,   954,   955,   956,
     957,   958,     0,   959,     0,   960,   961,     0,     0,     0,
       0,   944,     0,     0,     0,   945,   601,     0,     0,     0,
       0,     0,   946,   947,     0,     0,     0,  2196,   948,   949,
     950,   951,   934,   935,   953,   954,   955,   956,   957,   958,
       0,   959,   944,   960,   961,  2207,   945,   601,     0,     0,
       0,     0,     0,   946,   947,     0,     0,     0,     0,   948,
     949,   950,   951,     0,     0,   953,   954,   955,   956,   957,
     958,     0,   959,   944,   960,   961,     0,   945,   601,     0,
       0,     0,     0,     0,   946,   947,     0,     0,     0,  2211,
     948,   949,   950,   951,     0,     0,   953,   954,   955,   956,
     957,   958,     0,   959,   944,   960,   961,     0,   945,   601,
       0,     0,     0,     0,     0,   946,   947,     0,     0,     0,
    2269,   948,   949,   950,   951,     0,     0,   953,   954,   955,
     956,   957,   958,     0,   959,   944,   960,   961,     0,   945,
     601,     0,     0,     0,     0,     0,   946,   947,     0,     0,
       0,  2270,   948,   949,   950,   951,     0,     0,   953,   954,
     955,   956,   957,   958,     0,   959,   944,   960,   961,  2284,
     945,   601,     0,     0,     0,     0,     0,   946,   947,     0,
       0,     0,     0,   948,   949,   950,   951,     0,     0,   953,
     954,   955,   956,   957,   958,     0,   959,   944,   960,   961,
       0,   945,   601,     0,     0,     0,     0,     0,   946,   947,
       0,     0,     0,  2291,   948,   949,   950,   951,     0,     0,
     953,   954,   955,   956,   957,   958,     0,   959,   944,   960,
     961,  2308,   945,   601,     0,     0,     0,     0,     0,   946,
     947,     0,     0,     0,     0,   948,   949,   950,   951,     0,
       0,   953,   954,   955,   956,   957,   958,     0,   959,   944,
     960,   961,  2368,   945,   601,     0,     0,     0,     0,     0,
     946,   947,     0,     0,     0,     0,   948,   949,   950,   951,
       0,     0,   953,   954,   955,   956,   957,   958,     0,   959,
     944,   960,   961,     0,   945,   601,     0,     0,     0,     0,
       0,   946,   947,     0,     0,     0,  2370,   948,   949,   950,
     951,     0,     0,   953,   954,   955,   956,   957,   958,     0,
     959,   944,   960,   961,     0,   945,   601,     0,     0,     0,
       0,     0,   946,   947,     0,     0,     0,  2382,   948,   949,
     950,   951,     0,     0,   953,   954,   955,   956,   957,   958,
       0,   959,   944,   960,   961,  2383,   945,   601,     0,     0,
       0,     0,     0,   946,   947,     0,     0,     0,     0,   948,
     949,   950,   951,     0,     0,   953,   954,   955,   956,   957,
     958,     0,   959,   944,   960,   961,  2385,   945,   601,     0,
       0,     0,     0,     0,   946,   947,     0,     0,     0,     0,
     948,   949,   950,   951,     0,     0,   953,   954,   955,   956,
     957,   958,     0,   959,   944,   960,   961,     0,   945,   601,
       0,     0,     0,     0,     0,   946,   947,     0,     0,     0,
    2390,   948,   949,   950,   951,     0,     0,   953,   954,   955,
     956,   957,   958,     0,   959,   944,   960,   961,     0,   945,
     601,     0,     0,     0,     0,     0,   946,   947,     0,     0,
       0,  2392,   948,   949,   950,   951,     0,     0,   953,   954,
     955,   956,   957,   958,     0,   959,   944,   960,   961,  2425,
     945,   601,     0,     0,     0,     0,     0,   946,   947,     0,
       0,     0,     0,   948,   949,   950,   951,     0,     0,   953,
     954,   955,   956,   957,   958,     0,   959,     0,   960,   961,
     944,     0,  2440,     0,   945,   601,     0,     0,     0,     0,
       0,   946,   947,     0,     0,     0,     0,   948,   949,   950,
     951,     0,     0,   953,   954,   955,   956,   957,   958,     0,
     959,   944,   960,   961,     0,   945,   601,     0,     0,     0,
       0,     0,   946,   947,     0,     0,     0,     0,   948,   949,
     950,   951,     0,     0,   953,   954,   955,   956,   957,   958,
       0,   959,   944,   960,   961,     0,   945,     0,     0,     0,
       0,     0,     0,   946,   947,     0,     0,     0,     0,   948,
     949,     0,   951,     0,     0,   953,   954,   955,   956,   957,
       0,     0,   959,     0,   960,   961
};

static const yytype_int16 yycheck[] =
{
      50,   479,   374,   489,   490,   491,   492,   388,   482,   624,
      62,    63,   383,  1024,  1020,   744,   968,    69,   918,  1444,
     254,  1295,   397,  1131,   371,   259,   871,   879,   628,   537,
     650,   639,   407,   426,   409,   468,   207,  1482,  1590,   414,
     415,   416,   757,  1549,   463,  1551,    98,   422,  1502,   384,
     425,  1975,   427,   737,  1573,   649,   650,   603,  1579,   256,
    1546,  1603,   259,   260,  1478,   974,  1148,  1579,   118,  1324,
     967,   968,   122,    63,   839,  1599,  1064,   438,   839,    76,
    1604,  1295,   429,    15,  1514,   116,  1357,     0,     1,   384,
    1514,  1937,   464,     5,  1865,    41,   175,  1514,  1514,   446,
       8,    71,    99,   649,   650,  1550,  1514,  1512,     8,  1514,
     998,  1516,    20,   460,  1514,     5,   182,   476,   465,   116,
      15,    89,   469,     5,   471,  1514,  1645,    15,    47,   182,
      23,     5,   191,     7,   667,   169,   191,   484,   129,    21,
      37,   473,     7,     3,    37,   403,    58,   144,    15,   496,
       8,   498,   204,   208,   122,     7,   129,    15,   505,     8,
       7,  1512,  1173,  1514,   129,  1516,    26,    15,  1020,   191,
     222,    21,  1160,   209,    86,    15,   149,   509,   229,   230,
      21,   172,     8,   690,   235,     8,   794,   155,   147,   148,
      15,  1043,     5,    15,     8,  1681,  1682,  1683,   631,   706,
       8,   166,   635,   239,     3,    15,    20,     8,   116,   642,
     129,    37,    20,     3,   126,   852,   853,   185,   855,   129,
      21,   180,    21,     8,    37,   137,   257,   282,    18,   288,
     285,     8,   129,   568,   468,    20,   129,   150,   208,   129,
     299,   129,   321,   578,   579,   580,   581,   129,   282,   315,
     218,   116,   318,   165,   149,   129,   627,   515,   177,   517,
     282,   149,   129,   209,   116,   318,   524,   525,   526,   116,
     803,   129,   530,   531,   532,   533,   116,   535,   536,   129,
     129,   129,   228,   757,   252,  1185,  1541,   582,   129,   129,
    1512,     8,  1514,   116,  1516,  1512,  1518,  1514,  2069,  1516,
      15,  1518,   116,    20,   129,     5,    64,   129,   116,   641,
     116,   686,   644,   684,   246,    15,  1868,   369,   370,   129,
     402,    15,  1430,   375,   376,   377,   378,  1869,   380,   381,
     837,   116,   384,   668,   386,   387,   388,   389,   812,   116,
     392,   848,  1866,   395,   257,   397,   258,     8,    64,   257,
     402,  1622,   745,   405,   254,   407,   691,   409,   865,   718,
     793,   413,   414,   415,   416,  1795,  1580,  1581,  2214,   969,
     422,  1795,   424,   425,   982,   427,   984,  1782,  1795,  1795,
    1289,   433,   434,   991,     8,    15,     8,  1795,   440,   441,
    1795,  1288,   257,   445,   476,  1795,  1801,   631,   905,   116,
     452,   635,  1086,   455,   998,   257,  1795,   129,   642,     8,
     257,  1422,   919,   129,   129,   467,     8,   257,   470,  1184,
    1329,   782,  1183,   413,   476,   477,   478,   479,    20,   481,
     482,  1782,  1443,   149,   257,   485,   488,   489,   490,   491,
     492,   834,   494,   257,  1795,   497,   805,   499,   500,   257,
    1801,   503,   998,   788,   506,   116,  2380,  1172,   510,     5,
     792,     8,   794,  1078,     8,   148,     8,     8,     8,    15,
      23,    94,   257,     8,  1362,  1363,  1364,     7,     8,     8,
     257,    23,     8,  1103,    37,     8,     8,   539,  1570,  1571,
     540,  1479,   116,  1402,   116,    37,   915,   180,   833,   129,
    1007,   836,   552,   553,   554,  1012,  1870,  1871,     8,  1103,
     562,    37,     8,   565,   566,   567,   568,   116,     8,  2071,
     570,   573,   574,  2369,   116,   577,   578,   579,   580,   581,
    1984,  1976,    92,   585,   586,  2021,  1542,    37,  2080,   128,
     257,  2062,     7,     8,  1152,   916,  1053,     3,   806,  1555,
    2062,  1965,   985,   986,   987,   988,   989,  1103,    15,   793,
    1782,     8,   559,   560,  1819,  1782,   763,     8,   157,   116,
      26,  2417,   116,  1795,   571,   116,   116,   128,  1795,  1801,
      21,   116,   753,  1805,  1801,   169,  1860,   116,  1805,   149,
     266,  1865,   963,   116,   116,   191,   257,     7,     8,   651,
     652,   653,   654,   655,   656,   657,   157,   230,   660,   659,
       8,   287,   208,    15,    16,    17,   668,  2149,  2104,     5,
     116,     5,   673,    21,   208,     9,   116,   677,   249,     8,
      16,    17,     7,   257,   686,   257,    22,   689,    24,   691,
      15,   201,    21,  2175,     8,  2187,  1860,    33,     8,    35,
       8,    37,     5,    37,  2463,  2464,     9,    21,   257,     3,
       8,   284,   714,    21,  1035,   257,   217,  2476,     8,   116,
      41,    15,   295,    21,  1045,  1520,  1521,  1522,     8,  1524,
    1525,    21,  1302,     8,    37,    95,    18,   739,  2174,   741,
    1542,    21,   744,   257,    26,     8,    21,    84,   282,  1428,
      71,    15,    15,  1555,   754,   757,    16,    17,  1302,  1618,
     257,     3,    15,   257,   124,   767,   257,   257,   768,  2083,
    2084,    15,   257,    15,   776,   775,   778,   129,   257,  1444,
    1445,   783,    15,   785,   257,   257,   788,  1625,  1626,  1627,
       8,  1349,  1350,  1351,  1352,  1353,     8,   149,  1657,   801,
       8,   985,   986,   987,   988,   989,  1302,     8,  1304,  2278,
     812,   257,  1269,    21,   135,     8,   818,   257,  1362,  1363,
    1364,     3,   191,   159,     8,     8,   162,     5,    21,     7,
    1287,   833,    92,    15,   836,   195,     8,    21,    21,   208,
     842,   841,   202,  2238,     3,  2069,  1054,  2303,   169,   850,
       8,     8,     8,   854,   191,    15,    15,  1065,   859,     8,
     257,   862,   863,    21,  2300,    21,  1362,  1363,  1364,     8,
     191,   208,   872,     7,     8,    16,    17,   448,  2427,     8,
      21,  2317,    21,     8,  1549,     8,  1551,   208,   209,   149,
    2439,    43,    21,   895,   896,     8,    21,     8,    21,     8,
     221,     8,   223,   224,     8,   226,  2301,     8,   229,   911,
      21,   247,   483,   282,    21,    67,   285,    21,  1261,     8,
      21,   290,   493,   924,  2299,   926,     8,     8,   265,   298,
      82,   933,    21,     8,   934,    87,    88,     8,     3,    21,
      21,   201,    94,     8,     8,   282,    21,     8,   285,  1905,
     897,  1159,   289,  1855,  1856,  1857,  1858,  1859,   254,  1416,
      21,   282,   299,   259,     8,   912,     8,   304,     8,   305,
     306,   307,     3,     8,   545,   546,   547,   548,   549,    21,
    2185,    21,     3,  2378,    15,  1193,    21,   191,     8,     8,
     194,  1199,     8,  1843,    15,  1203,  1423,  1205,  1425,   320,
    2375,  1209,    21,  1211,     7,    21,     8,  1292,  1855,  1856,
    1857,  1858,  1859,     8,  1016,  1336,  1338,  1017,     8,    21,
    1022,  1342,     8,     3,     8,  1704,  1028,     8,     8,     8,
    1889,    21,   603,   604,   605,    21,   607,    21,     5,  1041,
      21,  2099,     9,   614,   316,   317,  1905,     5,     8,    16,
      17,     9,  1911,     8,  1913,     8,    23,   628,    16,    17,
    1382,    21,   147,   148,  1066,  1730,    21,  1617,    21,  1071,
      37,     8,  1411,  1395,  1413,     8,   228,     8,     8,    37,
      15,  1625,  1626,  1627,    21,  1087,     8,  1408,    21,     8,
      21,  2047,     8,     5,  1094,   180,  1421,     9,    10,   670,
    1102,   254,    21,  1905,    16,    17,   259,   259,     8,  1911,
      22,    23,    24,    25,   685,     8,    28,    29,    30,    31,
      32,    33,    34,    35,     8,    37,    38,     8,     5,  1625,
    1626,  1627,     9,    22,   430,    24,     8,    21,  1138,    16,
      17,     7,   713,     7,    33,    15,    35,    15,     8,   720,
     149,   303,    16,    17,     5,   726,   727,  1454,     9,    15,
      37,    21,     8,   459,    15,    16,    17,    15,    16,    17,
     466,     8,   468,     8,     8,    21,    27,  1634,   149,    27,
       8,     8,  1182,     8,    21,    36,    21,    21,    36,    22,
       7,    24,    15,  1194,    21,     8,    21,    16,    17,  1200,
      33,   772,    35,  1204,  1501,  1206,  1207,  1208,    21,  1210,
    1418,  1212,    15,    64,  1580,  1581,    64,   336,     8,  1540,
     339,   340,   341,    63,    15,  1225,    15,  2285,    68,   314,
     315,    21,    15,   318,   319,  1235,  1236,  1237,  1621,  1696,
     129,    92,     8,     8,    92,  2047,    15,  1247,    15,     8,
      90,    91,  1252,     8,     8,    21,    21,    15,  1258,  1467,
    1262,  1263,    21,     8,     8,    15,    21,    21,    15,   158,
     159,  2232,   161,   162,   845,   846,    21,    21,   129,     8,
       8,   129,     8,     8,  1286,    15,  1744,     8,  1746,  1747,
    1292,  1612,    21,    21,  1296,    21,    21,    15,   149,  1507,
    1758,   149,  1510,   192,   144,  1513,    84,  1515,     8,  1517,
    1768,  1519,  1770,    37,   154,  1636,    15,     8,    15,     8,
       8,    21,  1887,  2182,     8,   158,   159,  1648,   161,   162,
      21,  1652,    21,    21,     8,   631,  1894,    21,    15,   635,
      15,     8,  1667,     8,     8,   234,   642,    21,  2207,     8,
     201,   647,    15,   201,    21,     8,    21,    21,   247,   192,
     200,    15,    21,   245,   246,  1367,   993,   994,    21,  2085,
    2086,  2087,    15,   944,   945,   946,   947,   948,   949,   950,
     951,   952,   953,   954,   955,   956,   957,   958,   959,   960,
     961,   962,     8,     8,     8,  1396,  1553,  1554,   969,  2459,
    2460,   234,     8,   974,    15,    21,    21,    21,  1410,  1411,
     981,  1413,   983,   191,   247,    21,  1625,  1626,  1627,  1421,
      15,  1423,    15,  1425,  2355,  2356,  1428,   998,    15,     5,
     208,    15,     5,     9,   860,   861,     9,  1621,    15,   124,
      16,    17,    15,    16,    17,    21,    15,    23,    15,     7,
       3,   124,  2413,  1024,    27,   239,    15,    21,  1460,  1030,
     246,    37,     8,    36,   246,    15,    26,  1469,     7,   240,
    1471,    15,    15,  1475,    15,  1046,     8,  1477,    15,     8,
      15,    16,    17,  1485,    15,    15,     8,   265,   175,   149,
      15,    64,    27,     8,     8,    15,  1496,   793,  1498,     8,
    1502,    36,     8,    26,   282,    15,     7,   285,    62,     8,
      15,   289,   290,    15,    21,     8,     8,   287,    15,    92,
     298,   299,     8,  1523,    59,     8,   304,   213,    15,    64,
       5,     8,     7,     9,  1534,    10,    64,   129,     9,    14,
      15,    16,    17,    21,     8,     8,  1754,  1547,     8,    15,
      67,  1759,    27,    37,    15,   129,   129,    92,  1560,    34,
    1131,    36,    15,    15,  1772,  1565,    15,  1567,    15,  2234,
      15,    15,     8,     8,   870,   871,   149,     7,    26,  1150,
       7,    15,    63,   879,  1906,    18,  1794,    68,  1590,    64,
      21,    37,  1800,   166,    26,    21,  1804,    21,     7,    15,
       7,  1603,  1173,  1174,  1175,     8,    26,  2172,     8,    90,
      91,    15,  1909,    15,   149,     8,    21,    92,    15,    15,
    1622,    15,    21,  1625,  1626,  1627,    15,    15,   201,     5,
      15,     8,    15,  1635,  2299,    21,     8,  2302,  2303,    15,
      16,    17,    21,   124,    15,  1976,    15,     5,    15,    15,
      15,    27,    10,    15,   129,    15,    14,    15,    16,    17,
      36,    15,     5,   144,    15,  1667,   201,  1238,     5,    27,
      15,    62,     9,   154,   149,    21,    34,     7,    36,    16,
      17,     8,     8,     8,    21,  1687,    23,  2352,    64,   985,
     986,   987,   988,   989,     8,    15,    15,   993,   994,   255,
      37,     7,  1704,    15,   166,   239,    64,    16,    21,  2374,
    2375,     5,    15,    15,    15,     9,    92,   208,  1289,   200,
       8,  2149,    16,    17,  1020,    21,   201,    21,     7,    23,
    1301,    15,   328,   315,    92,   329,  1738,  1739,    15,  2060,
       7,     5,   246,    37,    15,     8,   246,  1043,   246,  2070,
     246,   246,   233,   129,  2419,    15,    21,     8,  1329,    21,
       7,  1763,    21,   244,    21,  1973,    21,     8,    26,    15,
    1341,   129,    21,   149,     8,   173,    15,    15,     8,  1987,
       7,   255,    15,    15,  1992,  1081,    21,  1995,  2224,  2225,
     208,   149,    18,    71,     8,    15,    15,  2005,    21,  2007,
      15,  2009,    15,  2011,     5,    15,     8,    15,   166,    10,
      21,     7,    21,    14,    15,    16,    17,    21,    21,  1390,
    1391,    15,    21,    21,    21,   201,    27,    15,    45,    46,
       5,    48,    49,    34,  1405,    36,    53,    15,     9,    20,
      26,  1412,    21,   201,    61,    21,    15,    21,    21,    66,
      15,  1422,    69,    21,    21,     8,    26,  1428,    21,  1430,
      77,    15,    79,    64,    81,    26,  1868,  1869,  1870,  1871,
      15,     7,  1443,    21,    15,  2299,    26,  2301,     8,  1881,
     254,    15,  2204,    21,    21,     8,  1457,   104,    15,   132,
       5,    92,    21,     7,     9,    10,     7,  1468,   115,    14,
      15,    16,    17,    21,    37,    21,  1908,     7,     7,    21,
    1481,    21,    27,   255,    15,  1486,     7,  1488,    15,    34,
       5,    36,   139,    15,  1924,    15,    15,    15,   129,    15,
      15,    16,    17,    15,    15,   152,    15,    21,     8,     8,
       7,    21,    27,     8,     7,  1945,    21,    15,   149,    64,
       7,    36,     7,   170,    15,    21,    78,   430,  1960,   176,
       8,     7,  1964,   327,  1966,   166,     7,   184,    21,   186,
      15,   188,    15,   190,  1976,    17,     8,    92,    17,    64,
     197,  1983,  1984,    21,    15,     7,   459,     7,    15,     7,
    1990,   220,   896,   466,  1263,   904,   128,   214,   215,  1999,
     201,   474,  1962,     7,  1575,  1840,    10,    92,  1565,  1186,
      14,    15,    16,    17,   129,   232,  1090,  2017,  1569,   576,
    2182,  1841,   239,    27,   587,   704,  1294,   933,  2187,   584,
      34,     5,    36,  1599,   149,     9,   253,  2082,   933,   256,
    2040,  1868,    16,    17,   129,  1616,  1617,  1618,    22,    23,
    2062,    25,  1485,  1860,    28,    29,    30,    31,    32,  1155,
      64,  1357,  1705,    37,   149,  2131,  1960,  1469,  2150,  2071,
    1026,  1046,  1405,  1384,  1088,  1678,  1647,    75,  2080,  1650,
    2082,  2083,  2084,  2085,  2086,  2087,   201,  1658,    92,  2297,
    1680,  2465,  1911,     5,  1889,     7,   459,   705,    10,  1391,
     244,   244,    14,    15,    16,    17,  1428,  1035,   786,   244,
     583,  1618,  2112,   244,  1071,    27,   201,  1621,   998,  1690,
    1102,  1692,    34,   832,    36,   129,  1175,   651,     5,   655,
    2233,  1407,   653,    10,  2111,   654,  1409,    14,    15,    16,
      17,  2417,  1964,    44,   656,   149,  2285,  2149,  2150,   801,
      27,  1016,    64,  1014,  2393,  1087,  1041,    34,  1908,    36,
    1466,     5,   166,    40,  2336,  1461,    10,  1947,  2298,  1028,
      14,    15,    16,    17,   647,  2419,  2441,  2461,  2241,  1483,
      92,  1967,  2463,    27,   500,  2187,    87,    64,   660,  1730,
      34,  1445,    36,   755,   378,  1913,  1534,   201,  1782,   411,
    1971,  2214,  1801,  1757,  1805,  1997,  1762,  1795,  1504,  1996,
    2264,    63,  1225,  2017,  1247,    92,    68,   129,   119,  1258,
      64,    -1,  2224,  2225,  1520,  1521,  1522,  1252,  1524,  1525,
      -1,  2233,  1528,    -1,    -1,    -1,    -1,   149,    90,    91,
      -1,    -1,   143,    -1,   145,    -1,  1542,    -1,    92,    -1,
    1546,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,  1555,
      -1,    -1,    -1,  1834,  1835,    -1,   739,    -1,    -1,    -1,
      -1,  1842,   149,    -1,    -1,    -1,    -1,   178,    -1,    -1,
      -1,    -1,   183,    -1,   757,   129,   187,  2289,    -1,   201,
     191,    -1,   144,   194,    -1,    -1,    -1,  2299,   771,  2301,
      -1,    -1,   154,    -1,    -1,   149,   779,   780,    -1,    -1,
      -1,   784,  1883,  2313,    -1,   216,    -1,    -1,  1889,    -1,
      -1,    -1,  2322,  2325,   201,  1621,  1622,    -1,    -1,   230,
      -1,    -1,    -1,    -1,    -1,   236,    -1,    -1,    -1,   812,
      -1,    -1,    -1,  1914,    -1,    -1,    -1,   820,   200,    -1,
      -1,    -1,  1923,     5,    -1,  2355,  2356,   201,    10,    -1,
      -1,    -1,    14,    15,    16,    17,    -1,    -1,    -1,  1940,
    1941,    -1,    -1,    -1,    -1,    27,   849,    -1,   851,   231,
      -1,    -1,    34,    -1,    36,  1681,  1682,  1683,    -1,    -1,
      -1,  2393,   244,  2395,    -1,    -1,  1967,   870,   871,    -1,
      -1,    -1,    -1,    -1,  1975,    -1,   879,   880,     7,    -1,
      -1,    10,    64,    -1,  1985,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    27,    -1,
    2001,    -1,  2003,    -1,    -1,    34,    -1,    36,    -1,    -1,
      92,    -1,    -1,  2443,     5,    -1,    -1,    -1,  2448,    10,
      -1,    -1,    -1,    14,    15,    16,    17,    -1,    -1,    -1,
      -1,  2461,    -1,  2463,  2464,    64,    27,    -1,    -1,    -1,
      -1,    -1,    -1,    34,    -1,    36,  2476,   129,    -1,    78,
      -1,    -1,    10,    -1,    -1,    -1,    14,    15,    16,    17,
      -1,    -1,    -1,    92,   967,   968,    -1,   149,    -1,    27,
      -1,    -1,    -1,    64,    -1,    -1,    34,    -1,    36,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
     993,   994,    -1,  2094,    -1,    18,    -1,    -1,  2099,    -1,
     129,    92,    -1,    -1,    -1,    28,    64,    -1,    31,  2110,
    2111,    34,    -1,    -1,  2115,    -1,    -1,  1020,    41,   201,
     149,    -1,    45,    -1,  2125,    -1,    49,    -1,    -1,  2130,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,   129,    -1,
    1043,    -1,    -1,    -1,    67,    -1,    -1,    -1,    71,    72,
      -1,    -1,    -1,    -1,    77,    78,    -1,    -1,   149,    -1,
      83,    84,    -1,    86,    87,    88,    89,    -1,    91,    -1,
      -1,   129,   201,    -1,    -1,    -1,    -1,   100,  1081,  1905,
      -1,  2182,   105,    -1,   107,  1911,    -1,    -1,   111,    -1,
      -1,   149,   115,    -1,   117,    -1,    -1,    -1,    -1,    -1,
      -1,   124,    -1,    -1,    -1,   128,    -1,    -1,   615,   132,
     201,   134,    -1,    -1,    -1,    -1,  2217,    -1,   141,   142,
      -1,    -1,   145,   146,    -1,    -1,    -1,    -1,   151,    -1,
     153,  2232,    -1,    -1,   157,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   201,    45,    46,    -1,    48,    49,    -1,
      -1,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      61,   184,    -1,   186,    -1,    66,    -1,    -1,    69,  1172,
      -1,    -1,    -1,    -1,    -1,    -1,    77,    -1,    79,    -1,
      81,    -1,    -1,  2284,  2285,    -1,    -1,  2288,    -1,  2290,
      -1,    -1,    -1,    -1,    -1,  2021,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,  2306,  2307,  2308,    -1,    -1,
      -1,    -1,    -1,    -1,   115,    -1,    -1,  2318,    -1,     7,
      -1,  2047,    10,  2324,    -1,    -1,    14,    15,    16,    17,
      -1,    -1,     5,    -1,    -1,  2336,    -1,    10,   139,    27,
      -1,    14,    15,    16,    17,    -1,    34,    -1,    36,    -1,
      -1,   152,    -1,    -1,    27,    -1,    -1,    -1,   765,    -1,
      -1,    34,   769,    36,    -1,    -1,    -1,  2368,  2369,   170,
      -1,    -1,    -1,    -1,    -1,   176,    64,    -1,  2104,  2380,
      -1,    -1,  2383,   184,  2385,   186,    -1,   188,    -1,   190,
    1293,    64,    -1,    -1,    -1,    -1,   197,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,   816,
      -1,    -1,  2413,   214,   215,    -1,  2417,    -1,    -1,    92,
    2421,   828,    -1,    -1,  2425,    -1,  2427,    -1,    -1,    -1,
      -1,   232,    -1,    -1,    -1,    -1,    -1,    -1,  2439,  2440,
    2441,   129,    -1,    -1,    -1,    -1,    -1,  2173,  2174,    -1,
      -1,    -1,   253,    -1,  1357,   256,   129,    -1,    -1,    -1,
      -1,   149,    -1,    -1,    -1,    -1,   873,   874,   875,   876,
      -1,   878,    -1,    -1,    -1,    -1,   149,   400,    -1,    -1,
      -1,     7,    -1,    -1,    10,    -1,    -1,    -1,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    27,    -1,    -1,    -1,    -1,    -1,    -1,    34,    -1,
      36,    -1,    -1,   201,    -1,    -1,    -1,    -1,    -1,    -1,
    1423,    -1,  1425,    -1,    -1,    -1,    -1,    -1,   201,    -1,
      -1,   938,    -1,   940,   941,   942,   943,    -1,    64,    -1,
      -1,  1444,  1445,    -1,     5,    -1,    -1,    -1,     9,    -1,
      -1,    -1,  1455,    -1,    -1,    16,    17,    -1,  1461,    -1,
    1463,    -1,    23,  1466,    25,    -1,    92,    28,    29,    30,
      31,    32,    -1,    -1,  2300,  1478,    37,    -1,    -1,  1482,
    1483,   504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2317,   999,    -1,    -1,    -1,    -1,    -1,  2324,    -1,
      -1,  1504,    -1,   129,    -1,    -1,    -1,    -1,  1511,    -1,
      -1,    -1,    -1,    -1,  1021,    -1,    -1,  1520,  1521,  1522,
      -1,  1524,  1525,   149,  1031,  1528,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1542,
      -1,  1544,    -1,  1546,    -1,  1052,  1549,  1550,  1551,    -1,
    1553,  1554,  1555,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1068,    -1,    -1,     7,  1072,  1073,    10,    -1,    -1,
      -1,    14,    15,    16,    17,   201,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    27,    -1,    -1,  1590,    -1,     7,
      -1,    34,    10,    36,    -1,    -1,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    27,
      -1,    -1,    -1,    -1,    -1,    -1,    34,    -1,    36,  1622,
      -1,    64,    -1,  1130,     7,    -1,    -1,    10,    -1,    -1,
      -1,    14,    15,    16,    17,    10,    -1,    -1,    -1,    14,
      15,    16,    17,    -1,    27,    -1,    64,    -1,    -1,    92,
      -1,    34,    27,    36,    -1,    -1,    -1,    -1,    -1,    34,
      -1,    36,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,  1681,  1682,
    1683,    64,  1189,  1190,    -1,    -1,   129,    -1,    -1,    64,
      -1,    -1,    -1,    -1,    -1,     7,    -1,    -1,    10,    -1,
      -1,    -1,    14,    15,    16,    17,   149,    -1,    -1,    92,
      -1,   129,    -1,    -1,    -1,    27,    -1,    92,    -1,    -1,
      -1,    -1,    34,    -1,    36,    -1,    -1,  1730,    -1,    -1,
      -1,   149,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    44,     5,   129,     7,    -1,     9,
      10,    -1,    64,    -1,   129,    -1,    16,    17,   201,    -1,
      -1,    21,    22,    23,    24,    25,   149,    -1,    28,    29,
      30,    31,    32,    33,   149,    35,    -1,    37,    38,    -1,
      92,    -1,    -1,   201,    -1,    -1,    -1,    87,    -1,    -1,
      -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,  1305,  1306,
    1307,  1308,  1309,  1310,  1311,  1312,  1313,  1314,  1315,  1316,
    1317,  1318,  1319,  1320,  1321,  1322,  1323,   129,   201,   119,
      -1,    -1,    -1,    -1,    -1,    -1,   201,    -1,    -1,    -1,
      -1,    -1,  1339,    -1,    -1,    -1,    -1,   149,    -1,  1346,
      -1,  1348,    -1,   143,    -1,   145,    -1,    -1,    -1,    -1,
      -1,  1358,  1855,  1856,  1857,  1858,  1859,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   178,    -1,
      -1,    -1,  1389,   183,    -1,    -1,    -1,   187,    -1,   201,
     913,   191,    -1,    -1,   194,    -1,    -1,    -1,    -1,    -1,
      -1,     5,  1905,    -1,    -1,     9,    10,    -1,  1911,    -1,
      -1,    -1,    16,    17,    -1,    -1,   216,    -1,    22,    23,
      24,    25,    26,    -1,    28,    29,    30,    31,    32,    33,
     230,    35,    -1,    37,    38,    -1,   236,    -1,    -1,    -1,
      -1,  1944,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1458,     5,    -1,     7,    -1,     9,    10,    -1,    -1,
      -1,    -1,  1965,    16,    17,  1472,    -1,    -1,    -1,    22,
      23,    24,    25,  1976,    -1,    28,    29,    30,    31,    32,
      33,    -1,    35,    -1,    37,    38,  1493,    -1,    -1,    -1,
    1013,    -1,    -1,    -1,  1997,    -1,    -1,     5,    -1,    -1,
       8,     9,    10,    -1,    -1,    -1,    -1,    -1,    16,    17,
      -1,    -1,    -1,    -1,    22,    23,    24,    25,  2021,  2022,
      28,    29,    30,    31,    32,    33,    -1,    35,    -1,    37,
      38,     5,  1539,    -1,     8,     9,    10,    -1,    -1,    -1,
      -1,    -1,    16,    17,  2047,    -1,    -1,    -1,    22,    23,
      24,    25,    -1,    -1,    28,    29,    30,    31,    32,    33,
      -1,    35,     5,    37,    38,    -1,     9,  1574,  2071,    -1,
      -1,    -1,    -1,    16,    17,    -1,    -1,    -1,    -1,    22,
      23,    24,    25,    -1,    -1,    28,    29,    30,    31,    32,
      33,    -1,    35,    -1,    37,    38,    -1,    -1,  2101,    -1,
    1607,  2104,    -1,  2106,    -1,     5,    -1,  1614,     8,     9,
      10,    -1,    -1,  1620,    -1,    -1,    16,    17,    -1,    -1,
      -1,    -1,    22,    23,    24,    25,    -1,    -1,    28,    29,
      30,    31,    32,    33,    -1,    35,  1643,    37,    38,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1655,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1663,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1673,    -1,    -1,  1676,
    2173,  2174,    -1,    -1,     5,    -1,    -1,    -1,     9,    10,
      -1,    -1,    -1,    -1,  1691,    16,    17,    -1,    -1,    -1,
      21,    22,    23,    24,    25,  1702,    -1,    28,    29,    30,
      31,    32,    33,    -1,    35,    -1,    37,    38,    -1,  1716,
      -1,    -1,    -1,    -1,    -1,  1722,    -1,    -1,    -1,    -1,
      -1,  1728,    -1,    -1,    -1,     5,    -1,    -1,    -1,     9,
      10,  2234,    -1,    -1,    -1,  2238,    16,    17,    -1,    -1,
      -1,    21,    22,    23,    24,    25,    -1,  1270,    28,    29,
      30,    31,    32,    33,    -1,    35,    -1,    37,    38,     5,
      -1,     7,    -1,     9,    10,    -1,    -1,    -1,    -1,    -1,
      16,    17,    -1,    -1,    -1,    -1,    22,    23,    24,    25,
      -1,    -1,    28,    29,    30,    31,    32,    33,    -1,    35,
      -1,    37,    38,    -1,    -1,    -1,  2299,  2300,  2301,  2302,
    2303,     5,    -1,     7,    -1,     9,    10,    -1,    -1,    -1,
      -1,    -1,    16,    17,  2317,    -1,    -1,    -1,    22,    23,
      24,    25,    -1,    -1,    28,    29,    30,    31,    32,    33,
      -1,    35,    -1,    37,    38,    -1,     5,    -1,    -1,  1846,
       9,    10,    -1,    -1,    -1,    -1,    -1,    16,    17,  2352,
      -1,    -1,    21,    22,    23,    24,    25,    -1,    -1,    28,
      29,    30,    31,    32,    33,    -1,    35,    -1,    37,    38,
      -1,  2374,  2375,    -1,     5,  2378,    -1,    -1,     9,    10,
      -1,    -1,    -1,  1890,    -1,    16,    17,    -1,    -1,    -1,
      21,    22,    23,    24,    25,    -1,    -1,    28,    29,    30,
      31,    32,    33,  1910,    35,  1912,    37,    38,    -1,  1916,
      -1,    -1,    -1,    -1,    -1,    -1,  2419,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,    -1,
       9,    10,  1939,    -1,    -1,  1942,    -1,    16,    17,    -1,
      -1,    -1,    21,    22,    23,    24,    25,    -1,    -1,    28,
      29,    30,    31,    32,    33,  1962,    35,     5,    37,    38,
      -1,     9,    10,    -1,    -1,    -1,    -1,    -1,    16,    17,
      -1,    -1,    -1,    21,    22,    23,    24,    25,    -1,    -1,
      28,    29,    30,    31,    32,    33,    -1,    35,     5,    37,
      38,    -1,     9,    10,    -1,    -1,    -1,    -1,    -1,    16,
      17,    -1,    -1,    -1,    21,    22,    23,    24,    25,    -1,
      -1,    28,    29,    30,    31,    32,    33,    -1,    35,    -1,
      37,    38,     5,    -1,    -1,    -1,     9,    10,    -1,    -1,
      -1,    -1,    -1,    16,    17,    -1,    -1,    -1,    21,    22,
      23,    24,    25,  2050,  2051,    28,    29,    30,    31,    32,
      33,  2058,    35,     5,    37,    38,    -1,     9,    10,    -1,
      -1,    -1,    -1,    -1,    16,    17,    -1,    -1,    -1,    21,
      22,    23,    24,    25,    -1,    -1,    28,    29,    30,    31,
      32,    33,  2089,    35,    -1,    37,    38,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2108,    -1,     5,    -1,    -1,  2113,     9,    10,    -1,
      -1,    -1,    -1,    -1,    16,    17,  2123,  2124,    -1,    21,
      22,    23,    24,    25,    -1,    -1,    28,    29,    30,    31,
      32,    33,    -1,    35,    -1,    37,    38,    -1,    -1,    -1,
      -1,    -1,    -1,     5,    -1,     7,  2153,     9,    10,    -1,
      -1,    -1,    -1,    -1,    16,    17,  2163,  2164,    -1,    -1,
      22,    23,    24,    25,    -1,    -1,    28,    29,    30,    31,
      32,    33,    -1,    35,    -1,    37,    38,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2197,    -1,  2199,    -1,    -1,    -1,    -1,    -1,    -1,     5,
      -1,  2208,    -1,     9,    10,    -1,  2213,    -1,    -1,  2216,
      16,    17,    -1,    -1,    -1,    21,    22,    23,    24,    25,
      -1,  2228,    28,    29,    30,    31,    32,    33,    -1,    35,
      -1,    37,    38,     5,    -1,     7,    -1,     9,    10,    -1,
      -1,    -1,    -1,    -1,    16,    17,    -1,    -1,    -1,    -1,
      22,    23,    24,    25,    -1,    -1,    28,    29,    30,    31,
      32,    33,    -1,    35,    -1,    37,    38,    -1,    -1,    -1,
      -1,    -1,     5,    -1,    -1,    -1,     9,    10,    -1,    -1,
      -1,    -1,    -1,    16,    17,    -1,    -1,  2294,    21,    22,
      23,    24,    25,    -1,    -1,    28,    29,    30,    31,    32,
      33,    -1,    35,    -1,    37,    38,    -1,     5,    -1,    -1,
      -1,     9,    -1,    -1,    -1,    -1,    -1,    -1,    16,    17,
      -1,    -1,  2329,    -1,  2331,    23,    -1,    25,  2335,    -1,
      28,    29,    30,    31,    32,    -1,    -1,    -1,    -1,    37,
    2347,  2348,    -1,  2350,    -1,     5,    -1,     7,    -1,     9,
      10,    -1,    -1,  2360,    -1,  2362,    16,    17,    -1,    -1,
      -1,    -1,    22,    23,    24,    25,    -1,    -1,    28,    29,
      30,    31,    32,    33,    -1,    35,    -1,    37,    38,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2394,    -1,  2396,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2408,  2409,    -1,    14,    15,    16,    17,    -1,    19,
      -1,    21,    -1,    23,    24,    -1,    26,    -1,    -1,    -1,
      30,    -1,    32,    33,    -1,  2432,    36,    37,    38,  2436,
      40,    -1,    42,    43,    -1,    -1,    46,    -1,    -1,  2446,
      -1,    51,    52,    53,    54,    55,    56,    57,    58,    -1,
      60,    61,    -1,    -1,    64,    65,    66,    -1,    68,    -1,
      70,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,
      -1,    81,    82,    -1,    -1,    85,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    -1,    94,    -1,    96,    97,    -1,    -1,
      -1,   101,   102,   103,    -1,    -1,   106,    -1,    -1,   109,
     110,    -1,   112,   113,   114,    -1,    -1,    -1,    -1,   119,
     120,    -1,    -1,   123,    -1,   125,   126,   127,    -1,   129,
     130,    -1,    -1,   133,    -1,   135,   136,   137,   138,   139,
     140,    -1,    -1,    -1,    -1,    -1,    -1,   147,   148,   149,
      -1,    -1,    -1,    -1,   154,   155,   156,    -1,   158,   159,
     160,   161,   162,   163,   164,   165,   166,   167,   168,   169,
     170,   171,   172,   173,   174,   175,   176,   177,   178,   179,
     180,   181,   182,   183,    -1,   185,    -1,   187,    -1,   189,
      -1,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,    -1,    -1,     6,     5,    -1,     7,    -1,     9,    10,
      13,    -1,    -1,    -1,    -1,    16,    17,    -1,    -1,    -1,
      -1,    22,    23,    24,    25,    -1,    -1,    28,    29,    30,
      31,    32,    33,    -1,    35,    -1,    37,    38,    -1,    -1,
      43,    44,    45,    46,    -1,    48,    49,    50,    51,    52,
      53,    -1,   252,    56,    57,   255,    -1,   257,    61,    62,
      63,    -1,    65,    66,    67,    68,    69,    70,    -1,    72,
      73,    74,    75,    76,    77,    -1,    79,    80,    81,    82,
      83,    84,    -1,    -1,    87,    88,    89,    90,    91,    -1,
      -1,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,    -1,   119,   120,    -1,   122,
     123,   124,    -1,    -1,   127,    -1,    -1,   130,   131,    -1,
     133,   134,   135,   136,    -1,   138,   139,   140,   141,   142,
     143,   144,   145,   146,    -1,    -1,    -1,    -1,    -1,   152,
     153,   154,   155,    -1,    -1,    -1,    -1,   160,    -1,    -1,
     163,   164,    -1,    -1,   167,   168,    -1,   170,    -1,    -1,
      -1,   174,    -1,   176,    -1,   178,    -1,    -1,    -1,   182,
     183,   184,   185,   186,   187,   188,   189,   190,   191,    -1,
     193,   194,   195,    -1,   197,    -1,   199,   200,    -1,   202,
      -1,   204,   205,   206,   207,    -1,    -1,   210,   211,   212,
      -1,   214,   215,   216,    -1,   218,   219,   220,    -1,   222,
      -1,   224,   225,   226,   227,   228,    -1,   230,    -1,   232,
     233,    -1,    -1,   236,   237,   238,    -1,    -1,   241,   242,
      -1,   244,   245,    -1,   247,   248,    -1,    -1,    -1,   252,
     253,    -1,    -1,   256,    -1,    -1,   259,    -1,    -1,    -1,
     263,   264,    -1,    -1,   267,   268,   269,    -1,   271,   272,
     273,   274,   275,   276,   277,   278,   279,   280,   281,    -1,
     283,    -1,    -1,   286,    -1,    -1,    -1,   290,   291,   292,
     293,   294,    -1,   296,   297,    -1,    -1,   300,   301,   302,
     303,    -1,    -1,    -1,    -1,   308,   309,   310,   311,   312,
     313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   322,
     323,   324,   325,   326,    -1,    -1,    -1,   330,   331,   332,
     333,   334,   335,     5,   337,   338,    -1,     9,    10,    -1,
      -1,    -1,    -1,    -1,    16,    17,    -1,    -1,    -1,    21,
      22,    23,    24,    25,    -1,    -1,    28,    29,    30,    31,
      32,    33,    -1,    35,    -1,    37,    38,    -1,    -1,    -1,
      -1,     5,    -1,    -1,    -1,     9,    10,    -1,    -1,    -1,
      -1,    -1,    16,    17,    -1,    -1,    -1,    21,    22,    23,
      24,    25,   592,   593,    28,    29,    30,    31,    32,    33,
      -1,    35,     5,    37,    38,     8,     9,    10,    -1,    -1,
      -1,    -1,    -1,    16,    17,    -1,    -1,    -1,    -1,    22,
      23,    24,    25,    -1,    -1,    28,    29,    30,    31,    32,
      33,    -1,    35,     5,    37,    38,    -1,     9,    10,    -1,
      -1,    -1,    -1,    -1,    16,    17,    -1,    -1,    -1,    21,
      22,    23,    24,    25,    -1,    -1,    28,    29,    30,    31,
      32,    33,    -1,    35,     5,    37,    38,    -1,     9,    10,
      -1,    -1,    -1,    -1,    -1,    16,    17,    -1,    -1,    -1,
      21,    22,    23,    24,    25,    -1,    -1,    28,    29,    30,
      31,    32,    33,    -1,    35,     5,    37,    38,    -1,     9,
      10,    -1,    -1,    -1,    -1,    -1,    16,    17,    -1,    -1,
      -1,    21,    22,    23,    24,    25,    -1,    -1,    28,    29,
      30,    31,    32,    33,    -1,    35,     5,    37,    38,     8,
       9,    10,    -1,    -1,    -1,    -1,    -1,    16,    17,    -1,
      -1,    -1,    -1,    22,    23,    24,    25,    -1,    -1,    28,
      29,    30,    31,    32,    33,    -1,    35,     5,    37,    38,
      -1,     9,    10,    -1,    -1,    -1,    -1,    -1,    16,    17,
      -1,    -1,    -1,    21,    22,    23,    24,    25,    -1,    -1,
      28,    29,    30,    31,    32,    33,    -1,    35,     5,    37,
      38,     8,     9,    10,    -1,    -1,    -1,    -1,    -1,    16,
      17,    -1,    -1,    -1,    -1,    22,    23,    24,    25,    -1,
      -1,    28,    29,    30,    31,    32,    33,    -1,    35,     5,
      37,    38,     8,     9,    10,    -1,    -1,    -1,    -1,    -1,
      16,    17,    -1,    -1,    -1,    -1,    22,    23,    24,    25,
      -1,    -1,    28,    29,    30,    31,    32,    33,    -1,    35,
       5,    37,    38,    -1,     9,    10,    -1,    -1,    -1,    -1,
      -1,    16,    17,    -1,    -1,    -1,    21,    22,    23,    24,
      25,    -1,    -1,    28,    29,    30,    31,    32,    33,    -1,
      35,     5,    37,    38,    -1,     9,    10,    -1,    -1,    -1,
      -1,    -1,    16,    17,    -1,    -1,    -1,    21,    22,    23,
      24,    25,    -1,    -1,    28,    29,    30,    31,    32,    33,
      -1,    35,     5,    37,    38,     8,     9,    10,    -1,    -1,
      -1,    -1,    -1,    16,    17,    -1,    -1,    -1,    -1,    22,
      23,    24,    25,    -1,    -1,    28,    29,    30,    31,    32,
      33,    -1,    35,     5,    37,    38,     8,     9,    10,    -1,
      -1,    -1,    -1,    -1,    16,    17,    -1,    -1,    -1,    -1,
      22,    23,    24,    25,    -1,    -1,    28,    29,    30,    31,
      32,    33,    -1,    35,     5,    37,    38,    -1,     9,    10,
      -1,    -1,    -1,    -1,    -1,    16,    17,    -1,    -1,    -1,
      21,    22,    23,    24,    25,    -1,    -1,    28,    29,    30,
      31,    32,    33,    -1,    35,     5,    37,    38,    -1,     9,
      10,    -1,    -1,    -1,    -1,    -1,    16,    17,    -1,    -1,
      -1,    21,    22,    23,    24,    25,    -1,    -1,    28,    29,
      30,    31,    32,    33,    -1,    35,     5,    37,    38,     8,
       9,    10,    -1,    -1,    -1,    -1,    -1,    16,    17,    -1,
      -1,    -1,    -1,    22,    23,    24,    25,    -1,    -1,    28,
      29,    30,    31,    32,    33,    -1,    35,    -1,    37,    38,
       5,    -1,     7,    -1,     9,    10,    -1,    -1,    -1,    -1,
      -1,    16,    17,    -1,    -1,    -1,    -1,    22,    23,    24,
      25,    -1,    -1,    28,    29,    30,    31,    32,    33,    -1,
      35,     5,    37,    38,    -1,     9,    10,    -1,    -1,    -1,
      -1,    -1,    16,    17,    -1,    -1,    -1,    -1,    22,    23,
      24,    25,    -1,    -1,    28,    29,    30,    31,    32,    33,
      -1,    35,     5,    37,    38,    -1,     9,    -1,    -1,    -1,
      -1,    -1,    -1,    16,    17,    -1,    -1,    -1,    -1,    22,
      23,    -1,    25,    -1,    -1,    28,    29,    30,    31,    32,
      -1,    -1,    35,    -1,    37,    38
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   345,     0,     1,   150,   257,   346,   347,   116,     6,
      13,    43,    44,    45,    46,    48,    49,    50,    51,    52,
      53,    56,    57,    61,    62,    63,    65,    66,    67,    68,
      69,    70,    72,    73,    74,    75,    76,    77,    79,    80,
      81,    82,    83,    84,    87,    88,    89,    90,    91,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   117,   119,   120,   122,   123,   124,   127,   130,   131,
     133,   134,   135,   136,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   152,   153,   154,   155,   160,   163,   164,
     167,   168,   170,   174,   176,   178,   182,   183,   184,   185,
     186,   187,   188,   189,   190,   191,   193,   194,   195,   197,
     199,   200,   202,   204,   205,   206,   207,   210,   211,   212,
     214,   215,   216,   218,   219,   220,   222,   224,   225,   226,
     227,   228,   230,   232,   233,   236,   237,   238,   241,   242,
     244,   245,   247,   248,   252,   253,   256,   259,   263,   264,
     267,   268,   269,   271,   272,   273,   274,   275,   276,   277,
     278,   279,   280,   281,   283,   286,   290,   291,   292,   293,
     294,   296,   297,   300,   301,   302,   303,   308,   309,   310,
     311,   312,   313,   322,   323,   324,   325,   326,   330,   331,
     332,   333,   334,   335,   337,   338,   348,   350,   353,   365,
     366,   370,   371,   372,   378,   379,   380,   381,   383,   384,
     386,   388,   389,   390,   391,   398,   399,   400,   401,   402,
     403,   407,   408,   409,   413,   414,   452,   454,   467,   510,
     511,   513,   514,   520,   521,   522,   523,   530,   531,   532,
     533,   535,   538,   542,   543,   544,   545,   546,   547,   553,
     554,   555,   566,   567,   568,   570,   573,   576,   581,   582,
     584,   586,   588,   591,   592,   616,   617,   628,   629,   630,
     631,   636,   639,   642,   645,   646,   694,   695,   696,   697,
     698,   699,   700,   701,   707,   709,   711,   713,   715,   716,
     717,   718,   719,   722,   724,   725,   726,   729,   730,   734,
     735,   737,   738,   739,   740,   741,   742,   743,   746,   751,
     756,   758,   759,   760,   761,   763,   765,   766,   767,   768,
     769,   786,   789,   790,   791,   792,   798,   801,   806,   807,
     808,   811,   812,   813,   814,   815,   816,   817,   818,   819,
     820,   821,   822,   823,   824,   825,   830,   831,   832,   833,
     834,   844,   845,   846,   847,   848,   849,   850,   851,    15,
     477,   477,   539,   539,   539,   539,   539,   477,   539,   539,
     349,   539,   539,   539,   477,   539,   477,   539,   539,   477,
     539,   539,   539,   476,   539,   477,   539,   539,     7,    15,
     478,    15,   477,   599,   539,   477,   362,   539,   539,   539,
     539,   539,   539,   539,   539,   539,   539,   129,   355,   519,
     519,   539,   539,   539,   477,   539,   355,   539,   477,   477,
     539,   539,   476,   349,   477,   477,    64,   361,   539,   539,
     477,   477,   539,   477,   477,   477,   477,   477,   539,   416,
     539,   539,   539,   355,   453,   349,   477,   539,   539,   539,
     477,   539,   477,   539,   539,   477,   539,   539,   539,   477,
     349,   477,   362,   539,   539,   362,   539,   477,   539,   539,
     539,   477,   539,   539,   477,   539,   477,   539,   539,   539,
     539,   539,   539,    15,   477,   577,   477,   349,   477,   477,
     539,   539,   539,    15,     8,   477,   477,   539,   539,   539,
     477,   539,   539,   539,   539,   539,   539,   539,   539,   539,
     539,   539,   539,   539,   539,   539,   539,   539,   539,   539,
     539,   539,   539,   539,   539,   539,   539,   477,   539,   477,
     539,   539,   539,   539,   539,   539,   539,   539,   539,   539,
     539,   539,    15,    15,    15,   355,   863,   257,   565,   124,
     233,   386,    15,   358,   565,     8,     8,     8,     8,     7,
       8,   124,   350,   373,     8,   355,   387,     8,     8,     8,
       8,     8,   534,     8,   534,     8,     8,     8,     8,   534,
     565,     7,   218,   252,   511,   513,   522,   523,   239,   531,
     531,    10,    14,    15,    16,    17,    27,    34,    36,    64,
      92,   149,   201,   355,   367,   483,   484,   486,   487,   488,
     489,   495,   496,   497,   498,   499,   502,    15,   539,     5,
       9,    15,    16,    17,   129,   485,   487,   495,   549,   563,
     564,   539,    15,   549,   539,     5,   548,   549,   564,   549,
       8,     8,     8,     8,     8,     8,     8,     8,     7,     8,
       8,     5,     7,   355,   626,   627,   355,   619,   478,    15,
      15,   149,   466,   355,   355,   727,   728,     8,   355,   643,
     644,   728,   355,   357,   355,    15,   515,   561,    23,    37,
     355,   405,   406,    15,   355,   589,   355,   658,   658,   355,
     640,   641,   355,   518,   415,    15,   355,   569,   149,   733,
     518,     7,   461,   462,   477,   600,   601,   355,   595,   601,
      15,   540,   355,   571,   572,   518,    15,    15,   518,   733,
     519,   518,   518,   518,   518,   355,   518,   358,   518,    15,
     410,   478,   486,   487,    15,   352,   355,   355,   637,   638,
     468,   469,   470,   471,     8,   659,   723,    15,   355,   583,
     355,   574,   575,   562,    15,    15,   355,   478,    15,   483,
     736,    15,    15,   355,   710,   712,     8,   355,    37,   404,
      15,   487,   488,   478,    15,    15,   540,   466,   478,   487,
     355,   702,     5,    15,   563,   564,   478,   355,   356,   478,
     562,    15,   486,   620,   621,   595,   599,   355,   587,   355,
     682,   682,    15,   355,   585,   702,   483,   494,   478,   362,
      15,   355,   688,   688,   688,   688,   688,     7,   483,   578,
     579,   355,   580,   478,   351,   355,   478,   355,   708,   710,
     355,   477,   478,   355,   455,    15,    15,   562,   355,    15,
     601,    15,   601,   601,   601,   601,   772,   828,   601,   601,
     601,   601,   601,   601,   772,   355,   362,   835,   836,   837,
      15,    15,    15,   483,   483,   483,   483,   482,   483,    15,
      15,   362,   852,   853,   854,   855,   362,   856,   857,   858,
     859,   860,   362,   861,   862,   349,   349,   124,     5,    21,
     355,   359,   360,   354,   362,   355,   355,   355,   406,     7,
     362,   349,   124,   355,   355,     5,    15,   393,   394,   355,
     406,   406,   406,   406,   405,   486,   404,   355,   355,   410,
     417,   418,   420,   421,   539,   539,   239,   396,   483,   484,
     483,   483,   483,   483,     5,     9,    16,    17,    22,    23,
      24,    25,    26,    28,    29,    30,    31,    32,    33,    35,
      37,    38,   367,    15,   246,     3,    15,   246,   246,    15,
     492,   493,    21,   536,   561,   494,     5,     9,   166,   550,
     551,   552,   563,    26,   563,     5,     9,    23,    37,   485,
     562,   563,   562,     8,    15,   487,   556,   557,    15,   483,
     484,   499,   558,   559,   560,   558,   569,   355,   583,   585,
     587,   589,   355,     7,   362,   708,     8,    21,   621,   406,
     508,   483,   240,   534,    15,   362,    15,   460,     8,   561,
       7,   483,   516,   517,   518,    15,   355,   460,   406,   465,
     466,     8,   417,   508,   460,    15,     8,    21,     5,     7,
     463,   464,   483,   355,     8,    21,     5,    58,    86,   126,
     137,   165,   258,   602,   598,   599,   175,   590,   483,   149,
     529,     8,   483,   483,   354,   355,   411,   412,   486,   491,
     355,    26,   355,   524,   525,   527,   358,     8,     8,    15,
     231,   386,   472,   362,     8,   723,   355,   486,   692,   702,
     720,   721,     8,   549,    26,     5,     9,    16,    17,    22,
      23,    24,    25,    28,    29,    30,    31,    32,    33,    34,
      35,    37,    38,   367,   368,   369,   355,   362,   376,   486,
     483,    15,   362,   355,   355,   486,   486,   509,     8,   659,
     714,   355,   486,   647,   648,   355,   450,   451,   529,   406,
      18,   562,   563,   562,   382,   385,   626,   621,     7,   599,
     601,   692,   702,   703,   704,   405,   406,   444,   445,    62,
     486,   747,    15,    15,     7,     8,    21,   577,   406,   358,
     406,   460,     8,   657,   679,    21,   362,   355,     8,   483,
     483,   460,   486,   534,   793,   486,   287,   805,   805,   534,
     802,   805,    15,   534,   770,   534,   809,   770,   770,   534,
     787,   534,   799,   460,   147,   148,   180,   314,   315,   318,
     319,   363,   838,   839,   840,     8,    21,   487,   663,   841,
      21,   841,   362,   744,   745,     8,     8,     8,     8,   486,
     489,   490,   762,   486,   764,   191,   208,     8,    21,    41,
     209,   228,     8,    21,   336,   339,   340,   341,     8,    21,
     355,   351,   349,     8,    21,   213,   363,   460,    44,    87,
      93,   119,   143,   145,   178,   183,   187,   191,   194,   216,
     230,   236,   374,   375,   377,   355,   349,   477,   540,   561,
     387,   460,   534,   534,     8,    37,    15,   355,   423,   428,
     362,    15,   503,    21,     8,   483,   483,   483,   483,   483,
     483,   483,   483,   483,   483,   483,   483,   483,   483,   483,
     483,   483,   483,   483,   561,    64,   129,   479,   481,   561,
     486,   497,   500,    64,   500,   494,     8,    21,     5,   483,
     537,   552,     8,    21,     5,     9,   483,    21,   483,   563,
     563,   563,   563,   563,    21,   556,   556,     8,   483,   484,
     559,   560,     8,     8,     8,   460,   460,   477,    43,    67,
      82,    87,    88,    94,   228,   259,   303,   630,   627,   362,
     490,   506,    21,   355,    15,   482,    67,   461,   644,   483,
       7,     8,    21,   536,    37,     8,    21,   641,   486,   489,
     505,   507,   561,   731,   463,     7,   460,   601,    15,    15,
      15,    15,    15,    15,   590,   601,   355,    21,   541,   572,
      21,    21,    15,     8,    21,     8,   493,   487,     8,   526,
      26,   354,   638,   469,   129,   473,   474,   475,   391,   169,
     208,   282,   362,    15,     7,     8,    21,   575,   558,    21,
      21,   147,   148,   180,    21,    18,    21,     7,   483,   501,
     175,   321,    37,     8,    21,   362,     8,    21,    26,     8,
      21,   541,   483,    21,   446,   447,   446,    21,     7,   601,
     590,    15,     7,     8,    21,     8,    15,    15,    26,   689,
     690,   692,   482,   483,   579,   362,     8,   679,     8,   657,
     387,   377,   364,    21,    21,    21,   601,   534,    21,   601,
     534,   829,   601,   534,   601,   534,   601,   534,   601,   534,
      15,    15,    15,    15,    15,    15,   362,   837,     8,    21,
      21,   182,   315,   318,     8,    21,   362,   362,   362,   483,
      15,    15,     8,    21,     8,    21,    15,    15,   853,    15,
      15,    15,   857,    15,    15,    15,   862,   358,   355,   360,
      15,   392,   393,   460,   477,    15,     7,     8,   355,   460,
      15,   497,     5,   395,   483,   552,   406,   486,   420,    15,
      16,    17,    27,    36,    59,    64,    92,   149,   201,   419,
     421,   431,   432,   433,   434,   435,   436,   437,   438,   423,
     428,   429,   430,    15,   424,   425,    62,   483,   558,   484,
     479,    21,     8,   480,   483,   501,   552,     7,   561,   466,
     483,   561,     8,   557,    21,     8,     8,     8,   484,   560,
     484,   560,   484,   560,   355,   255,     8,    21,   466,   465,
      21,     7,    21,   483,   516,    21,   466,   534,     8,    21,
     552,   732,     8,    21,   464,   483,   602,   561,    15,   604,
     355,   603,   603,   483,   603,   460,   601,   239,   518,   482,
     412,   412,   355,   483,   525,    21,   483,   501,     8,    21,
      16,    15,    15,    15,   482,   720,   721,   478,   486,   752,
       7,   483,     7,    21,    21,   355,   597,   487,   486,   648,
     601,   649,   483,   451,   534,     8,    47,   177,   355,   449,
     362,   618,   620,   590,     7,     7,   483,   705,   706,   703,
     704,   445,   483,     5,   604,   748,   749,   755,   483,   614,
       8,    21,    15,    21,    71,   208,   362,   362,   478,   172,
     355,   458,   459,   487,   191,   208,   282,   285,   290,   298,
     773,   774,   775,   782,   794,   795,   796,   601,   266,   803,
     804,   805,   601,    37,   486,   826,   827,    84,   265,   289,
     299,   304,   771,   773,   774,   775,   776,   777,   778,   780,
     781,   782,   601,   773,   774,   775,   776,   777,   778,   780,
     781,   782,   795,   796,   810,   601,   773,   774,   775,   782,
     788,   601,   773,   774,   800,   601,   841,   841,   841,   362,
     842,   843,   841,   841,   487,   745,   328,   315,   329,   561,
     479,   490,   486,   663,   362,   683,   689,   703,   689,   647,
     647,   490,   864,   355,    18,    26,   397,    15,   376,     7,
     362,   392,   541,   541,   396,     5,   483,   434,   435,   436,
     439,   435,   437,   435,   437,   246,   246,   246,   246,   246,
       8,    37,   355,   422,   486,     5,   424,   425,     8,    15,
      16,    17,   149,   355,   422,   426,   427,   440,   441,   442,
     443,    15,   425,    15,    21,   504,    21,    21,   493,   561,
     483,   494,   537,   551,   563,   527,   528,   484,   528,   528,
     528,   460,   355,   622,   625,   561,     8,    21,     7,   396,
     483,   561,   483,   561,   552,   615,   483,   605,   606,    21,
      21,    21,    21,     8,     8,   254,   512,   518,    21,   474,
     475,   663,   663,   663,    21,    21,   355,    15,    21,   483,
       7,     7,   483,   460,   173,     8,   653,   654,   655,   656,
     657,   659,   660,   661,   664,   665,   666,   679,   687,   527,
     447,    15,    15,   448,   255,     8,     7,     8,    21,    21,
      21,     8,    21,    21,   690,   691,    15,    15,   355,   355,
     456,   457,   459,    18,     8,    26,   772,    15,   772,   772,
      15,   601,   794,   772,   601,   803,   355,     8,    21,    15,
     772,    15,   772,    15,   601,   771,   601,   810,   601,   788,
     601,   800,    21,    21,    21,   316,   317,     8,    21,    21,
      21,    15,    15,   479,    21,    21,    22,    24,    33,    35,
     158,   159,   161,   162,   192,   234,   247,   684,   685,   686,
       8,    21,    21,    21,    21,    21,    21,     8,    21,    21,
     483,   483,   632,   633,    21,   375,   397,     5,   483,   387,
       8,    21,     8,   500,   500,   500,   500,   500,   431,     5,
      15,   421,   432,   425,   355,   422,   430,   440,   441,   441,
       8,    21,     7,    16,    17,     5,    37,     9,   440,   483,
      20,   493,   480,    21,    26,    21,    21,    21,    21,    15,
     490,   552,   466,   643,   478,   505,   552,   732,   483,    21,
       7,     8,    21,   483,   362,    15,    21,    21,    21,     7,
     753,   754,   755,   483,   483,     7,   486,   650,   362,   655,
      26,   449,    26,   368,   622,   620,   355,   593,   594,   595,
     596,   706,   749,   601,    78,   578,   355,   658,   703,   680,
       8,   355,   459,   483,   601,   783,   362,   601,   601,   828,
     486,   826,   362,   483,   483,   601,   601,   601,   601,   843,
     663,   486,    21,    15,    15,   362,   490,     8,    21,     7,
      21,    21,   561,   439,   432,   561,   422,    26,    21,   440,
     427,   441,   441,   442,   442,   442,    21,   483,     5,   483,
     501,   623,   624,   486,     8,   663,   486,     8,   483,   606,
     362,    21,   254,   483,     8,    21,   483,    15,    41,   135,
     191,   209,   221,   223,   224,   226,   229,   320,   483,   448,
      21,    21,    15,     8,   132,   750,    21,    21,     7,    21,
     682,   684,   457,     5,    16,    17,    22,    24,    33,    35,
      37,   159,   162,   247,   305,   306,   307,   785,    21,    94,
     230,   284,   295,   797,    37,   191,   288,   299,   779,    21,
      21,    21,    21,   487,   663,   684,     7,     7,   395,    21,
     479,   426,   440,    21,     8,     8,    21,   466,   552,   255,
      15,    21,   754,     5,   483,   651,   652,    15,   667,    15,
      15,    15,    15,    15,   688,   688,    15,    15,     8,   482,
     594,   692,   693,    15,   703,   681,   681,     7,     8,    21,
     829,    21,     8,    21,     8,     7,   396,    21,    21,   483,
     624,   483,   355,   607,   608,   483,     8,    21,   601,   667,
     702,   720,   663,   702,   703,   692,   689,   483,   483,   662,
     483,    21,     8,   362,    21,     7,     8,    21,   663,   784,
     483,   362,   483,   487,   355,   634,   635,    21,     8,    15,
      21,   652,   148,   180,   668,     7,    21,    21,     7,    21,
      15,    21,    21,     8,    21,     8,   692,    78,   683,   683,
      21,   327,    21,     8,   483,    40,   483,   609,   610,   755,
       7,     7,   669,   670,   692,   720,   703,   578,   483,   483,
      21,    21,    21,    15,   635,   355,   611,     8,    21,     8,
      21,    15,    21,    21,    21,     8,   482,    17,   612,   613,
     610,   670,   483,   671,   672,    21,   483,    21,   614,    17,
       7,     8,    21,     8,   757,   614,   483,   672,    15,   362,
     362,   673,   674,   675,   676,   677,   182,   318,   128,   157,
     217,     8,    21,     7,     7,    15,   678,   678,   678,   674,
     362,   676,   677,   362,   677,   481,     7,    21,   677
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 772 "gram1.y"
    { (yyval.bf_node) = BFNULL; ;}
    break;

  case 3:
#line 774 "gram1.y"
    { (yyval.bf_node) = set_stat_list((yyvsp[(1) - (3)].bf_node),(yyvsp[(2) - (3)].bf_node)); ;}
    break;

  case 4:
#line 778 "gram1.y"
    { lastwasbranch = NO;  (yyval.bf_node) = BFNULL; ;}
    break;

  case 5:
#line 780 "gram1.y"
    {
	       if ((yyvsp[(2) - (3)].bf_node) != BFNULL) 
               {	    
	          (yyvsp[(2) - (3)].bf_node)->label = (yyvsp[(1) - (3)].label);
	          (yyval.bf_node) = (yyvsp[(2) - (3)].bf_node);
	 	  if (is_openmp_stmt) {            /*OMP*/
			is_openmp_stmt = 0;
			if((yyvsp[(2) - (3)].bf_node)) {                        /*OMP*/
				if ((yyvsp[(2) - (3)].bf_node)->decl_specs != -BIT_OPENMP) (yyvsp[(2) - (3)].bf_node)->decl_specs = BIT_OPENMP; /*OMP*/
			}                               /*OMP*/
		  }                                       /*OMP*/
               }
	    ;}
    break;

  case 6:
#line 794 "gram1.y"
    { PTR_BFND p;

	     if(lastwasbranch && ! thislabel)
               /*if (warn_all)
		 warn("statement cannot be reached", 36);*/
	     lastwasbranch = thiswasbranch;
	     thiswasbranch = NO;
	     if((yyvsp[(2) - (3)].bf_node)) (yyvsp[(2) - (3)].bf_node)->label = (yyvsp[(1) - (3)].label);
	     if((yyvsp[(1) - (3)].label) && (yyvsp[(2) - (3)].bf_node)) (yyvsp[(1) - (3)].label)->statbody = (yyvsp[(2) - (3)].bf_node); /*8.11.06 podd*/
	     if((yyvsp[(1) - (3)].label)) {
		/*$1->statbody = $2;*/ /*8.11.06 podd*/
		if((yyvsp[(1) - (3)].label)->labtype == LABFORMAT)
		  err("label already that of a format",39);
		else
		  (yyvsp[(1) - (3)].label)->labtype = LABEXEC;
	     }
	     if (is_openmp_stmt) {            /*OMP*/
			is_openmp_stmt = 0;
			if((yyvsp[(2) - (3)].bf_node)) {                        /*OMP*/
				if ((yyvsp[(2) - (3)].bf_node)->decl_specs != -BIT_OPENMP) (yyvsp[(2) - (3)].bf_node)->decl_specs = BIT_OPENMP; /*OMP*/
			}                               /*OMP*/
	     }                                       /*OMP*/
             for (p = pred_bfnd; (yyvsp[(1) - (3)].label) && 
		  ((p->variant == FOR_NODE)||(p->variant == WHILE_NODE)) &&
                  (p->entry.for_node.doend) &&
		  (p->entry.for_node.doend->stateno == (yyvsp[(1) - (3)].label)->stateno);
		  p = p->control_parent)
                ++end_group;
	     (yyval.bf_node) = (yyvsp[(2) - (3)].bf_node);
     ;}
    break;

  case 7:
#line 825 "gram1.y"
    { /* PTR_LLND p; */
			doinclude( (yyvsp[(3) - (3)].charp) );
/*			p = make_llnd(fi, STRING_VAL, LLNULL, LLNULL, SMNULL);
			p->entry.string_val = $3;
			p->type = global_string;
			$$ = get_bfnd(fi, INCLUDE_STAT, SMNULL, p, LLNULL); */
			(yyval.bf_node) = BFNULL;
		;}
    break;

  case 8:
#line 834 "gram1.y"
    {
	      err("Unclassifiable statement", 10);
	      flline();
	      (yyval.bf_node) = BFNULL;
	    ;}
    break;

  case 9:
#line 840 "gram1.y"
    { PTR_CMNT p;
              PTR_BFND bif; 
	    
              if (last_bfnd && last_bfnd->control_parent &&((last_bfnd->control_parent->variant == LOGIF_NODE)
	         ||(last_bfnd->control_parent->variant == FORALL_STAT)))
  	         bif = last_bfnd->control_parent;
              else
                 bif = last_bfnd;
              p=bif->entry.Template.cmnt_ptr;
              if(p)
                 p->string = StringConcatenation(p->string,commentbuf);
              else
              {
                 p = make_comment(fi,commentbuf, FULL);
                 bif->entry.Template.cmnt_ptr = p;
              }
 	      (yyval.bf_node) = BFNULL;         
            ;}
    break;

  case 10:
#line 860 "gram1.y"
    { 
	      flline();	 needkwd = NO;	inioctl = NO;
/*!!!*/
              opt_kwd_ = NO; intonly = NO; opt_kwd_hedr = NO; opt_kwd_r = NO; as_op_kwd_= NO; optcorner = NO;
	      yyerrok; yyclearin;  (yyval.bf_node) = BFNULL;
	    ;}
    break;

  case 11:
#line 869 "gram1.y"
    {
	    if(yystno)
	      {
	      (yyval.label) = thislabel =	make_label_node(fi,yystno);
	      thislabel->scope = cur_scope();
	      if (thislabel->labdefined && (thislabel->scope == cur_scope()))
		 errstr("Label %s already defined",convic(thislabel->stateno),40);
	      else
		 thislabel->labdefined = YES;
	      }
	    else
	      (yyval.label) = thislabel = LBNULL;
	    ;}
    break;

  case 12:
#line 885 "gram1.y"
    { PTR_BFND p;

	        if (pred_bfnd != global_bfnd)
		    err("Misplaced PROGRAM statement", 33);
		p = get_bfnd(fi,PROG_HEDR, (yyvsp[(3) - (3)].symbol), LLNULL, LLNULL, LLNULL);
		(yyvsp[(3) - (3)].symbol)->entry.prog_decl.prog_hedr=p;
 		set_blobs(p, global_bfnd, NEW_GROUP1);
	        add_scope_level(p, NO);
	        position = IN_PROC;
	    ;}
    break;

  case 13:
#line 897 "gram1.y"
    {  PTR_BFND q = BFNULL;

	      (yyvsp[(3) - (3)].symbol)->variant = PROCEDURE_NAME;
	      (yyvsp[(3) - (3)].symbol)->decl = YES;   /* variable declaration has been seen. */
	      q = get_bfnd(fi,BLOCK_DATA, (yyvsp[(3) - (3)].symbol), LLNULL, LLNULL, LLNULL);
	      set_blobs(q, global_bfnd, NEW_GROUP1);
              add_scope_level(q, NO);
	    ;}
    break;

  case 14:
#line 907 "gram1.y"
    { 
              install_param_list((yyvsp[(3) - (4)].symbol), (yyvsp[(4) - (4)].symbol), LLNULL, PROCEDURE_NAME); 
	      /* if there is only a control end the control parent is not set */
              
	     ;}
    break;

  case 15:
#line 914 "gram1.y"
    { install_param_list((yyvsp[(4) - (5)].symbol), (yyvsp[(5) - (5)].symbol), LLNULL, PROCEDURE_NAME); 
              if((yyvsp[(1) - (5)].ll_node)->variant == RECURSIVE_OP) 
                   (yyvsp[(4) - (5)].symbol)->attr = (yyvsp[(4) - (5)].symbol)->attr | RECURSIVE_BIT;
              pred_bfnd->entry.Template.ll_ptr3 = (yyvsp[(1) - (5)].ll_node);
            ;}
    break;

  case 16:
#line 920 "gram1.y"
    {
              install_param_list((yyvsp[(3) - (5)].symbol), (yyvsp[(4) - (5)].symbol), (yyvsp[(5) - (5)].ll_node), FUNCTION_NAME);  
  	      pred_bfnd->entry.Template.ll_ptr1 = (yyvsp[(5) - (5)].ll_node);
            ;}
    break;

  case 17:
#line 925 "gram1.y"
    {
              install_param_list((yyvsp[(1) - (3)].symbol), (yyvsp[(2) - (3)].symbol), (yyvsp[(3) - (3)].ll_node), FUNCTION_NAME); 
	      pred_bfnd->entry.Template.ll_ptr1 = (yyvsp[(3) - (3)].ll_node);
	    ;}
    break;

  case 18:
#line 930 "gram1.y"
    {PTR_BFND p, bif;
	     PTR_SYMB q = SMNULL;
             PTR_LLND l = LLNULL;

	     if(parstate==OUTSIDE || procclass==CLMAIN || procclass==CLBLOCK)
	        err("Misplaced ENTRY statement", 35);

	     bif = cur_scope();
	     if (bif->variant == FUNC_HEDR) {
	        q = make_function((yyvsp[(2) - (4)].hash_entry), bif->entry.Template.symbol->type, YES);
	        l = construct_entry_list(q, (yyvsp[(3) - (4)].symbol), FUNCTION_NAME); 
             }
             else if ((bif->variant == PROC_HEDR) || 
                      (bif->variant == PROS_HEDR) || /* added for FORTRAN M */
                      (bif->variant == PROG_HEDR)) {
	             q = make_procedure((yyvsp[(2) - (4)].hash_entry), YES);
  	             l = construct_entry_list(q, (yyvsp[(3) - (4)].symbol), PROCEDURE_NAME); 
             }
	     p = get_bfnd(fi,ENTRY_STAT, q, l, (yyvsp[(4) - (4)].ll_node), LLNULL);
	     set_blobs(p, pred_bfnd, SAME_GROUP);
             q->decl = YES;   /*4.02.03*/
             q->entry.proc_decl.proc_hedr = p; /*5.02.03*/
	    ;}
    break;

  case 19:
#line 954 "gram1.y"
    { PTR_SYMB s;
	      PTR_BFND p;
/*
	      s = make_global_entity($3, MODULE_NAME, global_default, NO);
	      s->decl = YES;  
	      p = get_bfnd(fi, MODULE_STMT, s, LLNULL, LLNULL, LLNULL);
	      s->entry.Template.func_hedr = p;
	      set_blobs(p, pred_bfnd, NEW_GROUP1);
              add_scope_level(p, NO);
*/
	      /*position = IN_MODULE;*/


               s = make_module((yyvsp[(3) - (3)].hash_entry));
	       s->decl = YES;   /* variable declaration has been seen. */
	        if (pred_bfnd != global_bfnd)
		    err("Misplaced MODULE statement", 33);
              p = get_bfnd(fi, MODULE_STMT, s, LLNULL, LLNULL, LLNULL);
	      s->entry.Template.func_hedr = p; /* !!!????*/
	      set_blobs(p, global_bfnd, NEW_GROUP1);
	      add_scope_level(p, NO);	
	      position =  IN_MODULE;    /*IN_PROC*/
              privateall = 0;
            ;}
    break;

  case 20:
#line 980 "gram1.y"
    { newprog(); 
	      if (position == IN_OUTSIDE)
	           position = IN_PROC;
              else if (position != IN_INTERNAL_PROC){ 
                if(!is_interface_stat(pred_bfnd))
	           position--;
              }
              else {
                if(!is_interface_stat(pred_bfnd))
                  err("Internal procedures can not contain procedures",304);
              }
	    ;}
    break;

  case 21:
#line 995 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, RECURSIVE_OP, LLNULL, LLNULL, SMNULL); ;}
    break;

  case 22:
#line 997 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, PURE_OP, LLNULL, LLNULL, SMNULL); ;}
    break;

  case 23:
#line 999 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, ELEMENTAL_OP, LLNULL, LLNULL, SMNULL); ;}
    break;

  case 24:
#line 1003 "gram1.y"
    { PTR_BFND p;

	      (yyval.symbol) = make_procedure((yyvsp[(1) - (1)].hash_entry), LOCAL);
	      (yyval.symbol)->decl = YES;   /* variable declaration has been seen. */
             /* if (pred_bfnd != global_bfnd)
		 {
	         err("Misplaced SUBROUTINE statement", 34);
		 }  
              */
	      p = get_bfnd(fi,PROC_HEDR, (yyval.symbol), LLNULL, LLNULL, LLNULL);
              (yyval.symbol)->entry.proc_decl.proc_hedr = p;
	      set_blobs(p, pred_bfnd, NEW_GROUP1);
              add_scope_level(p, NO);
            ;}
    break;

  case 25:
#line 1020 "gram1.y"
    { PTR_BFND p;

	      (yyval.symbol) = make_function((yyvsp[(1) - (1)].hash_entry), TYNULL, LOCAL);
	      (yyval.symbol)->decl = YES;   /* variable declaration has been seen. */
             /* if (pred_bfnd != global_bfnd)
	         err("Misplaced FUNCTION statement", 34); */
	      p = get_bfnd(fi,FUNC_HEDR, (yyval.symbol), LLNULL, LLNULL, LLNULL);
              (yyval.symbol)->entry.func_decl.func_hedr = p;
	      set_blobs(p, pred_bfnd, NEW_GROUP1);
              add_scope_level(p, NO);
            ;}
    break;

  case 26:
#line 1034 "gram1.y"
    { PTR_BFND p;
             PTR_LLND l;

	      (yyval.symbol) = make_function((yyvsp[(4) - (4)].hash_entry), (yyvsp[(1) - (4)].data_type), LOCAL);
	      (yyval.symbol)->decl = YES;   /* variable declaration has been seen. */
              l = make_llnd(fi, TYPE_OP, LLNULL, LLNULL, SMNULL);
              l->type = (yyvsp[(1) - (4)].data_type);
	      p = get_bfnd(fi,FUNC_HEDR, (yyval.symbol), LLNULL, l, LLNULL);
              (yyval.symbol)->entry.func_decl.func_hedr = p;
            /*  if (pred_bfnd != global_bfnd)
	         err("Misplaced FUNCTION statement", 34);*/
	      set_blobs(p, pred_bfnd, NEW_GROUP1);
              add_scope_level(p, NO);
/*
	      $$ = make_function($4, $1, LOCAL);
	      $$->decl = YES;
	      p = get_bfnd(fi,FUNC_HEDR, $$, LLNULL, LLNULL, LLNULL);
              if (pred_bfnd != global_bfnd)
	         errstr("cftn.gram: misplaced SUBROUTINE statement.");
	      set_blobs(p, pred_bfnd, NEW_GROUP1);
              add_scope_level(p, NO);
*/
           ;}
    break;

  case 27:
#line 1058 "gram1.y"
    { PTR_BFND p;
             PTR_LLND l;
	      (yyval.symbol) = make_function((yyvsp[(5) - (5)].hash_entry), (yyvsp[(1) - (5)].data_type), LOCAL);
	      (yyval.symbol)->decl = YES;   /* variable declaration has been seen. */
              if((yyvsp[(2) - (5)].ll_node)->variant == RECURSIVE_OP)
	         (yyval.symbol)->attr = (yyval.symbol)->attr | RECURSIVE_BIT;
              l = make_llnd(fi, TYPE_OP, LLNULL, LLNULL, SMNULL);
              l->type = (yyvsp[(1) - (5)].data_type);
             /* if (pred_bfnd != global_bfnd)
	         err("Misplaced FUNCTION statement", 34);*/
	      p = get_bfnd(fi,FUNC_HEDR, (yyval.symbol), LLNULL, l, (yyvsp[(2) - (5)].ll_node));
              (yyval.symbol)->entry.func_decl.func_hedr = p;
	      set_blobs(p, pred_bfnd, NEW_GROUP1);
              add_scope_level(p, NO);
            ;}
    break;

  case 28:
#line 1074 "gram1.y"
    { PTR_BFND p;

	      (yyval.symbol) = make_function((yyvsp[(4) - (4)].hash_entry), TYNULL, LOCAL);
	      (yyval.symbol)->decl = YES;   /* variable declaration has been seen. */
              if((yyvsp[(1) - (4)].ll_node)->variant == RECURSIVE_OP)
	        (yyval.symbol)->attr = (yyval.symbol)->attr | RECURSIVE_BIT;
              /*if (pred_bfnd != global_bfnd)
	         err("Misplaced FUNCTION statement",34);*/
	      p = get_bfnd(fi,FUNC_HEDR, (yyval.symbol), LLNULL, LLNULL, (yyvsp[(1) - (4)].ll_node));
              (yyval.symbol)->entry.func_decl.func_hedr = p;
	      set_blobs(p, pred_bfnd, NEW_GROUP1);
              add_scope_level(p, NO);
            ;}
    break;

  case 29:
#line 1088 "gram1.y"
    { PTR_BFND p;
              PTR_LLND l;
	      (yyval.symbol) = make_function((yyvsp[(5) - (5)].hash_entry), (yyvsp[(2) - (5)].data_type), LOCAL);
	      (yyval.symbol)->decl = YES;   /* variable declaration has been seen. */
              if((yyvsp[(1) - (5)].ll_node)->variant == RECURSIVE_OP)
	        (yyval.symbol)->attr = (yyval.symbol)->attr | RECURSIVE_BIT;
              l = make_llnd(fi, TYPE_OP, LLNULL, LLNULL, SMNULL);
              l->type = (yyvsp[(2) - (5)].data_type);
             /* if (pred_bfnd != global_bfnd)
	          err("Misplaced FUNCTION statement",34);*/
	      p = get_bfnd(fi,FUNC_HEDR, (yyval.symbol), LLNULL, l, (yyvsp[(1) - (5)].ll_node));
              (yyval.symbol)->entry.func_decl.func_hedr = p;
	      set_blobs(p, pred_bfnd, NEW_GROUP1);
              add_scope_level(p, NO);
            ;}
    break;

  case 30:
#line 1106 "gram1.y"
    { (yyval.ll_node) = LLNULL; ;}
    break;

  case 31:
#line 1108 "gram1.y"
    { PTR_SYMB s;
              s = make_scalar((yyvsp[(4) - (5)].hash_entry), TYNULL, LOCAL);
              (yyval.ll_node) = make_llnd(fi, VAR_REF, LLNULL, LLNULL, s);
            ;}
    break;

  case 32:
#line 1115 "gram1.y"
    { (yyval.hash_entry) = look_up_sym(yytext); ;}
    break;

  case 33:
#line 1118 "gram1.y"
    { (yyval.symbol) = make_program(look_up_sym("_MAIN")); ;}
    break;

  case 34:
#line 1120 "gram1.y"
    {
              (yyval.symbol) = make_program((yyvsp[(1) - (1)].hash_entry));
	      (yyval.symbol)->decl = YES;   /* variable declaration has been seen. */
            ;}
    break;

  case 35:
#line 1126 "gram1.y"
    { (yyval.symbol) = make_program(look_up_sym("_BLOCK")); ;}
    break;

  case 36:
#line 1128 "gram1.y"
    {
              (yyval.symbol) = make_program((yyvsp[(1) - (1)].hash_entry)); 
	      (yyval.symbol)->decl = YES;   /* variable declaration has been seen. */
	    ;}
    break;

  case 37:
#line 1135 "gram1.y"
    { (yyval.symbol) = SMNULL; ;}
    break;

  case 38:
#line 1137 "gram1.y"
    { (yyval.symbol) = SMNULL; ;}
    break;

  case 39:
#line 1139 "gram1.y"
    { (yyval.symbol) = (yyvsp[(2) - (3)].symbol); ;}
    break;

  case 41:
#line 1144 "gram1.y"
    { (yyval.symbol) = set_id_list((yyvsp[(1) - (3)].symbol), (yyvsp[(3) - (3)].symbol)); ;}
    break;

  case 42:
#line 1148 "gram1.y"
    {
	      (yyval.symbol) = make_scalar((yyvsp[(1) - (1)].hash_entry), TYNULL, IO);
            ;}
    break;

  case 43:
#line 1152 "gram1.y"
    { (yyval.symbol) = make_scalar(look_up_sym("*"), TYNULL, IO); ;}
    break;

  case 44:
#line 1158 "gram1.y"
    { char *s;

	      s = copyn(yyleng+1, yytext);
	      s[yyleng] = '\0';
	      (yyval.charp) = s;
	    ;}
    break;

  case 45:
#line 1167 "gram1.y"
    { needkwd = 1; ;}
    break;

  case 46:
#line 1171 "gram1.y"
    { needkwd = NO; ;}
    break;

  case 47:
#line 1176 "gram1.y"
    { colon_flag = YES; ;}
    break;

  case 61:
#line 1197 "gram1.y"
    {
	      saveall = YES;
	      (yyval.bf_node) = get_bfnd(fi,SAVE_DECL, SMNULL, LLNULL, LLNULL, LLNULL);
	    ;}
    break;

  case 62:
#line 1202 "gram1.y"
    {
	      (yyval.bf_node) = get_bfnd(fi,SAVE_DECL, SMNULL, (yyvsp[(4) - (4)].ll_node), LLNULL, LLNULL);
            ;}
    break;

  case 63:
#line 1207 "gram1.y"
    { PTR_LLND p;

	      p = make_llnd(fi,STMT_STR, LLNULL, LLNULL, SMNULL);
	      p->entry.string_val = copys(stmtbuf);
	      (yyval.bf_node) = get_bfnd(fi,FORMAT_STAT, SMNULL, p, LLNULL, LLNULL);
             ;}
    break;

  case 64:
#line 1214 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,PARAM_DECL, SMNULL, (yyvsp[(4) - (5)].ll_node), LLNULL, LLNULL); ;}
    break;

  case 77:
#line 1230 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, INTERFACE_STMT, SMNULL, LLNULL, LLNULL, LLNULL); 
             /* add_scope_level($$, NO); */    
            ;}
    break;

  case 78:
#line 1234 "gram1.y"
    { PTR_SYMB s;

	      s = make_procedure((yyvsp[(3) - (3)].hash_entry), LOCAL);
	      s->variant = INTERFACE_NAME;
	      (yyval.bf_node) = get_bfnd(fi, INTERFACE_STMT, s, LLNULL, LLNULL, LLNULL);
              /*add_scope_level($$, NO);*/
	    ;}
    break;

  case 79:
#line 1242 "gram1.y"
    { PTR_SYMB s;

	      s = make_function((yyvsp[(4) - (5)].hash_entry), global_default, LOCAL);
	      s->variant = INTERFACE_NAME;
	      (yyval.bf_node) = get_bfnd(fi, INTERFACE_OPERATOR, s, LLNULL, LLNULL, LLNULL);
              /*add_scope_level($$, NO);*/
	    ;}
    break;

  case 80:
#line 1250 "gram1.y"
    { PTR_SYMB s;


	      s = make_procedure(look_up_sym("="), LOCAL);
	      s->variant = INTERFACE_NAME;
	      (yyval.bf_node) = get_bfnd(fi, INTERFACE_ASSIGNMENT, s, LLNULL, LLNULL, LLNULL);
              /*add_scope_level($$, NO);*/
	    ;}
    break;

  case 81:
#line 1259 "gram1.y"
    { parstate = INDCL;
              (yyval.bf_node) = get_bfnd(fi, CONTROL_END, SMNULL, LLNULL, LLNULL, LLNULL); 
	      /*process_interface($$);*/ /*podd 01.02.03*/
              /*delete_beyond_scope_level(pred_bfnd);*/
	    ;}
    break;

  case 82:
#line 1267 "gram1.y"
    { (yyval.hash_entry) = look_up_sym(yytext); ;}
    break;

  case 83:
#line 1271 "gram1.y"
    { (yyval.hash_entry) = (yyvsp[(1) - (1)].hash_entry); ;}
    break;

  case 84:
#line 1273 "gram1.y"
    { (yyval.hash_entry) = (yyvsp[(1) - (1)].hash_entry); ;}
    break;

  case 85:
#line 1277 "gram1.y"
    { (yyval.hash_entry) = look_up_op(PLUS); ;}
    break;

  case 86:
#line 1279 "gram1.y"
    { (yyval.hash_entry) = look_up_op(MINUS); ;}
    break;

  case 87:
#line 1281 "gram1.y"
    { (yyval.hash_entry) = look_up_op(ASTER); ;}
    break;

  case 88:
#line 1283 "gram1.y"
    { (yyval.hash_entry) = look_up_op(DASTER); ;}
    break;

  case 89:
#line 1285 "gram1.y"
    { (yyval.hash_entry) = look_up_op(SLASH); ;}
    break;

  case 90:
#line 1287 "gram1.y"
    { (yyval.hash_entry) = look_up_op(DSLASH); ;}
    break;

  case 91:
#line 1289 "gram1.y"
    { (yyval.hash_entry) = look_up_op(AND); ;}
    break;

  case 92:
#line 1291 "gram1.y"
    { (yyval.hash_entry) = look_up_op(OR); ;}
    break;

  case 93:
#line 1293 "gram1.y"
    { (yyval.hash_entry) = look_up_op(XOR); ;}
    break;

  case 94:
#line 1295 "gram1.y"
    { (yyval.hash_entry) = look_up_op(NOT); ;}
    break;

  case 95:
#line 1297 "gram1.y"
    { (yyval.hash_entry) = look_up_op(EQ); ;}
    break;

  case 96:
#line 1299 "gram1.y"
    { (yyval.hash_entry) = look_up_op(NE); ;}
    break;

  case 97:
#line 1301 "gram1.y"
    { (yyval.hash_entry) = look_up_op(GT); ;}
    break;

  case 98:
#line 1303 "gram1.y"
    { (yyval.hash_entry) = look_up_op(GE); ;}
    break;

  case 99:
#line 1305 "gram1.y"
    { (yyval.hash_entry) = look_up_op(LT); ;}
    break;

  case 100:
#line 1307 "gram1.y"
    { (yyval.hash_entry) = look_up_op(LE); ;}
    break;

  case 101:
#line 1309 "gram1.y"
    { (yyval.hash_entry) = look_up_op(NEQV); ;}
    break;

  case 102:
#line 1311 "gram1.y"
    { (yyval.hash_entry) = look_up_op(EQV); ;}
    break;

  case 103:
#line 1316 "gram1.y"
    {
             PTR_SYMB s;
         
             type_var = s = make_derived_type((yyvsp[(4) - (4)].hash_entry), TYNULL, LOCAL);	
             (yyval.bf_node) = get_bfnd(fi, STRUCT_DECL, s, LLNULL, LLNULL, LLNULL);
             add_scope_level((yyval.bf_node), NO);
	   ;}
    break;

  case 104:
#line 1325 "gram1.y"
    { PTR_SYMB s;
         
             type_var = s = make_derived_type((yyvsp[(7) - (7)].hash_entry), TYNULL, LOCAL);	
	     s->attr = s->attr | type_opt;
             (yyval.bf_node) = get_bfnd(fi, STRUCT_DECL, s, (yyvsp[(5) - (7)].ll_node), LLNULL, LLNULL);
             add_scope_level((yyval.bf_node), NO);
	   ;}
    break;

  case 105:
#line 1335 "gram1.y"
    {
	     (yyval.bf_node) = get_bfnd(fi, CONTROL_END, SMNULL, LLNULL, LLNULL, LLNULL);
	     if (type_var != SMNULL)
               process_type(type_var, (yyval.bf_node));
             type_var = SMNULL;
	     delete_beyond_scope_level(pred_bfnd);
           ;}
    break;

  case 106:
#line 1343 "gram1.y"
    {
             (yyval.bf_node) = get_bfnd(fi, CONTROL_END, SMNULL, LLNULL, LLNULL, LLNULL);
	     if (type_var != SMNULL)
               process_type(type_var, (yyval.bf_node));
             type_var = SMNULL;
	     delete_beyond_scope_level(pred_bfnd);	
           ;}
    break;

  case 107:
#line 1353 "gram1.y"
    { 
	      PTR_LLND q, r, l;
	     /* PTR_SYMB s;*/
	      PTR_TYPE t;
	      int type_opts;

	      vartype = (yyvsp[(1) - (7)].data_type);
              if((yyvsp[(6) - (7)].ll_node) && vartype->variant != T_STRING)
                errstr("Non character entity  %s  has length specification",(yyvsp[(3) - (7)].hash_entry)->ident,41);
              t = make_type_node(vartype, (yyvsp[(6) - (7)].ll_node));
	      type_opts = type_options;
	      if ((yyvsp[(5) - (7)].ll_node)) type_opts = type_opts | DIMENSION_BIT;
	      if ((yyvsp[(5) - (7)].ll_node))
		 q = deal_with_options((yyvsp[(3) - (7)].hash_entry), t, type_opts, (yyvsp[(5) - (7)].ll_node), ndim, (yyvsp[(7) - (7)].ll_node), (yyvsp[(5) - (7)].ll_node));
	      else q = deal_with_options((yyvsp[(3) - (7)].hash_entry), t, type_opts, attr_dims, attr_ndim, (yyvsp[(7) - (7)].ll_node), (yyvsp[(5) - (7)].ll_node));
	      r = make_llnd(fi, EXPR_LIST, q, LLNULL, SMNULL);
	      l = make_llnd(fi, TYPE_OP, LLNULL, LLNULL, SMNULL);
	      l->type = vartype;
	      (yyval.bf_node) = get_bfnd(fi,VAR_DECL, SMNULL, r, l, (yyvsp[(2) - (7)].ll_node));
	    ;}
    break;

  case 108:
#line 1374 "gram1.y"
    { 
	      PTR_LLND q, r;
	    /*  PTR_SYMB s;*/
              PTR_TYPE t;
	      int type_opts;
              if((yyvsp[(5) - (6)].ll_node) && vartype->variant != T_STRING)
                errstr("Non character entity  %s  has length specification",(yyvsp[(3) - (6)].hash_entry)->ident,41);
              t = make_type_node(vartype, (yyvsp[(5) - (6)].ll_node));
	      type_opts = type_options;
	      if ((yyvsp[(4) - (6)].ll_node)) type_opts = type_opts | DIMENSION_BIT;
	      if ((yyvsp[(4) - (6)].ll_node))
		 q = deal_with_options((yyvsp[(3) - (6)].hash_entry), t, type_opts, (yyvsp[(4) - (6)].ll_node), ndim, (yyvsp[(6) - (6)].ll_node), (yyvsp[(4) - (6)].ll_node));
	      else q = deal_with_options((yyvsp[(3) - (6)].hash_entry), t, type_opts, attr_dims, attr_ndim, (yyvsp[(6) - (6)].ll_node), (yyvsp[(4) - (6)].ll_node));
	      r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	      add_to_lowLevelList(r, (yyvsp[(1) - (6)].bf_node)->entry.Template.ll_ptr1);
       	    ;}
    break;

  case 109:
#line 1393 "gram1.y"
    { (yyval.ll_node) = LLNULL; ;}
    break;

  case 110:
#line 1395 "gram1.y"
    { (yyval.ll_node) = LLNULL; ;}
    break;

  case 111:
#line 1397 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(3) - (5)].ll_node); ;}
    break;

  case 112:
#line 1401 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 113:
#line 1403 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (4)].ll_node), (yyvsp[(4) - (4)].ll_node), EXPR_LIST); ;}
    break;

  case 114:
#line 1407 "gram1.y"
    { type_options = type_options | PARAMETER_BIT; 
              (yyval.ll_node) = make_llnd(fi, PARAMETER_OP, LLNULL, LLNULL, SMNULL);
            ;}
    break;

  case 115:
#line 1411 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 116:
#line 1413 "gram1.y"
    { type_options = type_options | ALLOCATABLE_BIT;
              (yyval.ll_node) = make_llnd(fi, ALLOCATABLE_OP, LLNULL, LLNULL, SMNULL);
	    ;}
    break;

  case 117:
#line 1417 "gram1.y"
    { type_options = type_options | DIMENSION_BIT;
	      attr_ndim = ndim;
	      attr_dims = (yyvsp[(2) - (2)].ll_node);
              (yyval.ll_node) = make_llnd(fi, DIMENSION_OP, (yyvsp[(2) - (2)].ll_node), LLNULL, SMNULL);
            ;}
    break;

  case 118:
#line 1423 "gram1.y"
    { type_options = type_options | EXTERNAL_BIT;
              (yyval.ll_node) = make_llnd(fi, EXTERNAL_OP, LLNULL, LLNULL, SMNULL);
            ;}
    break;

  case 119:
#line 1427 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(3) - (4)].ll_node); ;}
    break;

  case 120:
#line 1429 "gram1.y"
    { type_options = type_options | INTRINSIC_BIT;
              (yyval.ll_node) = make_llnd(fi, INTRINSIC_OP, LLNULL, LLNULL, SMNULL);
            ;}
    break;

  case 121:
#line 1433 "gram1.y"
    { type_options = type_options | OPTIONAL_BIT;
              (yyval.ll_node) = make_llnd(fi, OPTIONAL_OP, LLNULL, LLNULL, SMNULL);
            ;}
    break;

  case 122:
#line 1437 "gram1.y"
    { type_options = type_options | POINTER_BIT;
              (yyval.ll_node) = make_llnd(fi, POINTER_OP, LLNULL, LLNULL, SMNULL);
            ;}
    break;

  case 123:
#line 1441 "gram1.y"
    { type_options = type_options | SAVE_BIT; 
              (yyval.ll_node) = make_llnd(fi, SAVE_OP, LLNULL, LLNULL, SMNULL);
            ;}
    break;

  case 124:
#line 1445 "gram1.y"
    { type_options = type_options | SAVE_BIT; 
              (yyval.ll_node) = make_llnd(fi, STATIC_OP, LLNULL, LLNULL, SMNULL);
            ;}
    break;

  case 125:
#line 1449 "gram1.y"
    { type_options = type_options | TARGET_BIT; 
              (yyval.ll_node) = make_llnd(fi, TARGET_OP, LLNULL, LLNULL, SMNULL);
            ;}
    break;

  case 126:
#line 1455 "gram1.y"
    { type_options = type_options | IN_BIT;  type_opt = IN_BIT; 
              (yyval.ll_node) = make_llnd(fi, IN_OP, LLNULL, LLNULL, SMNULL);
            ;}
    break;

  case 127:
#line 1459 "gram1.y"
    { type_options = type_options | OUT_BIT;  type_opt = OUT_BIT; 
              (yyval.ll_node) = make_llnd(fi, OUT_OP, LLNULL, LLNULL, SMNULL);
            ;}
    break;

  case 128:
#line 1463 "gram1.y"
    { type_options = type_options | INOUT_BIT;  type_opt = INOUT_BIT;
              (yyval.ll_node) = make_llnd(fi, INOUT_OP, LLNULL, LLNULL, SMNULL);
            ;}
    break;

  case 129:
#line 1469 "gram1.y"
    { type_options = type_options | PUBLIC_BIT; 
              type_opt = PUBLIC_BIT;
              (yyval.ll_node) = make_llnd(fi, PUBLIC_OP, LLNULL, LLNULL, SMNULL);
            ;}
    break;

  case 130:
#line 1474 "gram1.y"
    { type_options =  type_options | PRIVATE_BIT;
               type_opt = PRIVATE_BIT;
              (yyval.ll_node) = make_llnd(fi, PRIVATE_OP, LLNULL, LLNULL, SMNULL);
            ;}
    break;

  case 131:
#line 1481 "gram1.y"
    { 
	      PTR_LLND q, r;
	      PTR_SYMB s;

              s = make_scalar((yyvsp[(7) - (7)].hash_entry), TYNULL, LOCAL);
	      s->attr = s->attr | type_opt;	
	      q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, s);
	      r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	      (yyval.bf_node) = get_bfnd(fi, INTENT_STMT, SMNULL, r, (yyvsp[(4) - (7)].ll_node), LLNULL);
	    ;}
    break;

  case 132:
#line 1492 "gram1.y"
    { 
	      PTR_LLND q, r;
	      PTR_SYMB s;

              s = make_scalar((yyvsp[(3) - (3)].hash_entry), TYNULL, LOCAL);	
	      s->attr = s->attr | type_opt;
	      q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, s);
	      r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	      add_to_lowLevelList(r, (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
  	    ;}
    break;

  case 133:
#line 1505 "gram1.y"
    { 
	      PTR_LLND q, r;
	      PTR_SYMB s;

              s = make_scalar((yyvsp[(4) - (4)].hash_entry), TYNULL, LOCAL);	
	      s->attr = s->attr | OPTIONAL_BIT;
	      q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, s);
	      r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	      (yyval.bf_node) = get_bfnd(fi, OPTIONAL_STMT, SMNULL, r, LLNULL, LLNULL);
	    ;}
    break;

  case 134:
#line 1516 "gram1.y"
    { 
	      PTR_LLND q, r;
	      PTR_SYMB s;

              s = make_scalar((yyvsp[(3) - (3)].hash_entry), TYNULL, LOCAL);	
	      s->attr = s->attr | OPTIONAL_BIT;
	      q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, s);
	      r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	      add_to_lowLevelList(r, (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
  	    ;}
    break;

  case 135:
#line 1529 "gram1.y"
    { 
	      PTR_LLND r;
	      PTR_SYMB s;

              s = (yyvsp[(4) - (4)].ll_node)->entry.Template.symbol; 
              s->attr = s->attr | SAVE_BIT;
	      r = make_llnd(fi,EXPR_LIST, (yyvsp[(4) - (4)].ll_node), LLNULL, SMNULL);
	      (yyval.bf_node) = get_bfnd(fi, STATIC_STMT, SMNULL, r, LLNULL, LLNULL);
	    ;}
    break;

  case 136:
#line 1539 "gram1.y"
    { 
	      PTR_LLND r;
	      PTR_SYMB s;

              s = (yyvsp[(3) - (3)].ll_node)->entry.Template.symbol;
              s->attr = s->attr | SAVE_BIT;
	      r = make_llnd(fi,EXPR_LIST, (yyvsp[(3) - (3)].ll_node), LLNULL, SMNULL);
	      add_to_lowLevelList(r, (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
  	    ;}
    break;

  case 137:
#line 1552 "gram1.y"
    {
	      privateall = 1;
	      (yyval.bf_node) = get_bfnd(fi, PRIVATE_STMT, SMNULL, LLNULL, LLNULL, LLNULL);
	    ;}
    break;

  case 138:
#line 1557 "gram1.y"
    {
	      /*type_options = type_options | PRIVATE_BIT;*/
	      (yyval.bf_node) = get_bfnd(fi, PRIVATE_STMT, SMNULL, (yyvsp[(5) - (5)].ll_node), LLNULL, LLNULL);
            ;}
    break;

  case 139:
#line 1563 "gram1.y"
    {type_opt = PRIVATE_BIT;;}
    break;

  case 140:
#line 1567 "gram1.y"
    { 
	      (yyval.bf_node) = get_bfnd(fi, SEQUENCE_STMT, SMNULL, LLNULL, LLNULL, LLNULL);
            ;}
    break;

  case 141:
#line 1572 "gram1.y"
    {
	      /*saveall = YES;*/ /*14.03.03*/
	      (yyval.bf_node) = get_bfnd(fi, PUBLIC_STMT, SMNULL, LLNULL, LLNULL, LLNULL);
	    ;}
    break;

  case 142:
#line 1577 "gram1.y"
    {
	      /*type_options = type_options | PUBLIC_BIT;*/
	      (yyval.bf_node) = get_bfnd(fi, PUBLIC_STMT, SMNULL, (yyvsp[(5) - (5)].ll_node), LLNULL, LLNULL);
            ;}
    break;

  case 143:
#line 1583 "gram1.y"
    {type_opt = PUBLIC_BIT;;}
    break;

  case 144:
#line 1587 "gram1.y"
    {
	      type_options = 0;
              /* following block added by dbg */
	      ndim = 0;
	      attr_ndim = 0;
	      attr_dims = LLNULL;
	      /* end section added by dbg */
              (yyval.data_type) = make_type_node((yyvsp[(1) - (4)].data_type), (yyvsp[(3) - (4)].ll_node));
            ;}
    break;

  case 145:
#line 1597 "gram1.y"
    { PTR_TYPE t;

	      type_options = 0;
	      ndim = 0;
	      attr_ndim = 0;
	      attr_dims = LLNULL;
              t = lookup_type((yyvsp[(3) - (5)].hash_entry));
	      vartype = t;
	      (yyval.data_type) = make_type_node(t, LLNULL);
            ;}
    break;

  case 146:
#line 1610 "gram1.y"
    {opt_kwd_hedr = YES;;}
    break;

  case 147:
#line 1615 "gram1.y"
    { PTR_TYPE p;
	      PTR_LLND q;
	      PTR_SYMB s;
              s = (yyvsp[(2) - (2)].hash_entry)->id_attr;
	      if (s)
		   s->attr = (yyvsp[(1) - (2)].token);
	      else {
		p = undeftype ? global_unknown : impltype[*(yyvsp[(2) - (2)].hash_entry)->ident - 'a'];
                s = install_entry((yyvsp[(2) - (2)].hash_entry), SOFT);
		s->attr = (yyvsp[(1) - (2)].token);
                set_type(s, p, LOCAL);
	      }
	      q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, (yyvsp[(2) - (2)].hash_entry)->id_attr);
	      q = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	      (yyval.bf_node) = get_bfnd(fi,ATTR_DECL, SMNULL, q, LLNULL, LLNULL);
	    ;}
    break;

  case 148:
#line 1634 "gram1.y"
    { PTR_TYPE p;
	      PTR_LLND q, r;
	      PTR_SYMB s;
	      int att;

	      att = (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1->entry.Template.ll_ptr1->
		    entry.Template.symbol->attr;
              s = (yyvsp[(3) - (3)].hash_entry)->id_attr;
	      if (s)
		   s->attr = att;
	      else {
		p = undeftype ? global_unknown : impltype[*(yyvsp[(3) - (3)].hash_entry)->ident - 'a'];
                s = install_entry((yyvsp[(3) - (3)].hash_entry), SOFT);
		s->attr = att;
                set_type(s, p, LOCAL);
	      }
	      q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, (yyvsp[(3) - (3)].hash_entry)->id_attr);
	      for (r = (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1;
		   r->entry.list.next;
		   r = r->entry.list.next) ;
	      r->entry.list.next = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);

	    ;}
    break;

  case 149:
#line 1660 "gram1.y"
    { (yyval.token) = ATT_GLOBAL; ;}
    break;

  case 150:
#line 1662 "gram1.y"
    { (yyval.token) = ATT_CLUSTER; ;}
    break;

  case 151:
#line 1674 "gram1.y"
    {
/*		  varleng = ($1<0 || $1==TYLONG ? 0 : typesize[$1]); */
		  vartype = (yyvsp[(1) - (1)].data_type);
		;}
    break;

  case 152:
#line 1681 "gram1.y"
    { (yyval.data_type) = global_int; ;}
    break;

  case 153:
#line 1682 "gram1.y"
    { (yyval.data_type) = global_float; ;}
    break;

  case 154:
#line 1683 "gram1.y"
    { (yyval.data_type) = global_complex; ;}
    break;

  case 155:
#line 1684 "gram1.y"
    { (yyval.data_type) = global_double; ;}
    break;

  case 156:
#line 1685 "gram1.y"
    { (yyval.data_type) = global_dcomplex; ;}
    break;

  case 157:
#line 1686 "gram1.y"
    { (yyval.data_type) = global_bool; ;}
    break;

  case 158:
#line 1687 "gram1.y"
    { (yyval.data_type) = global_string; ;}
    break;

  case 159:
#line 1692 "gram1.y"
    { (yyval.ll_node) = LLNULL; ;}
    break;

  case 160:
#line 1694 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 161:
#line 1698 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, LEN_OP, (yyvsp[(3) - (5)].ll_node), LLNULL, SMNULL); ;}
    break;

  case 162:
#line 1700 "gram1.y"
    { PTR_LLND l;

                 l = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL); 
                 l->entry.string_val = (char *)"*";
                 (yyval.ll_node) = make_llnd(fi, LEN_OP, l,l, SMNULL);
                ;}
    break;

  case 163:
#line 1707 "gram1.y"
    {(yyval.ll_node) = make_llnd(fi, LEN_OP, (yyvsp[(5) - (6)].ll_node), (yyvsp[(5) - (6)].ll_node), SMNULL);;}
    break;

  case 164:
#line 1711 "gram1.y"
    { (yyval.ll_node) = LLNULL; ;}
    break;

  case 165:
#line 1713 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 166:
#line 1715 "gram1.y"
    { /*$$ = make_llnd(fi, PAREN_OP, $2, LLNULL, SMNULL);*/  (yyval.ll_node) = (yyvsp[(3) - (5)].ll_node);  ;}
    break;

  case 167:
#line 1723 "gram1.y"
    { if((yyvsp[(7) - (9)].ll_node)->variant==LENGTH_OP && (yyvsp[(3) - (9)].ll_node)->variant==(yyvsp[(7) - (9)].ll_node)->variant)
                (yyvsp[(7) - (9)].ll_node)->variant=KIND_OP;
                (yyval.ll_node) = make_llnd(fi, CONS, (yyvsp[(3) - (9)].ll_node), (yyvsp[(7) - (9)].ll_node), SMNULL); 
            ;}
    break;

  case 168:
#line 1730 "gram1.y"
    { if(vartype->variant == T_STRING)
                (yyval.ll_node) = make_llnd(fi,LENGTH_OP,(yyvsp[(1) - (1)].ll_node),LLNULL,SMNULL);
              else
                (yyval.ll_node) = make_llnd(fi,KIND_OP,(yyvsp[(1) - (1)].ll_node),LLNULL,SMNULL);
            ;}
    break;

  case 169:
#line 1736 "gram1.y"
    { PTR_LLND l;
	      l = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
	      l->entry.string_val = (char *)"*";
              (yyval.ll_node) = make_llnd(fi,LENGTH_OP,l,LLNULL,SMNULL);
            ;}
    break;

  case 170:
#line 1742 "gram1.y"
    { /* $$ = make_llnd(fi, SPEC_PAIR, $2, LLNULL, SMNULL); */
	     char *q;
             q = (yyvsp[(1) - (2)].ll_node)->entry.string_val;
  	     if (strcmp(q, "len") == 0)
               (yyval.ll_node) = make_llnd(fi,LENGTH_OP,(yyvsp[(2) - (2)].ll_node),LLNULL,SMNULL);
             else
                (yyval.ll_node) = make_llnd(fi,KIND_OP,(yyvsp[(2) - (2)].ll_node),LLNULL,SMNULL);              
            ;}
    break;

  case 171:
#line 1751 "gram1.y"
    { PTR_LLND l;
	      l = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
	      l->entry.string_val = (char *)"*";
              (yyval.ll_node) = make_llnd(fi,LENGTH_OP,l,LLNULL,SMNULL);
            ;}
    break;

  case 172:
#line 1759 "gram1.y"
    {endioctl();;}
    break;

  case 173:
#line 1772 "gram1.y"
    { (yyval.ll_node) = LLNULL; ;}
    break;

  case 174:
#line 1774 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(2) - (2)].ll_node); ;}
    break;

  case 175:
#line 1777 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, POINTST_OP, LLNULL, (yyvsp[(2) - (2)].ll_node), SMNULL); ;}
    break;

  case 176:
#line 1781 "gram1.y"
    { PTR_SYMB s;
	      PTR_LLND q, r;
	      if(! (yyvsp[(5) - (5)].ll_node)) {
		err("No dimensions in DIMENSION statement", 42);
	      }
              if(statement_kind == 1) /*DVM-directive*/
                err("No shape specification", 65);                
	      s = make_array((yyvsp[(4) - (5)].hash_entry), TYNULL, (yyvsp[(5) - (5)].ll_node), ndim, LOCAL);
	      s->attr = s->attr | DIMENSION_BIT;
	      q = make_llnd(fi,ARRAY_REF, (yyvsp[(5) - (5)].ll_node), LLNULL, s);
	      s->type->entry.ar_decl.ranges = (yyvsp[(5) - (5)].ll_node);
	      r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	      (yyval.bf_node) = get_bfnd(fi,DIM_STAT, SMNULL, r, LLNULL, LLNULL);
	    ;}
    break;

  case 177:
#line 1796 "gram1.y"
    {  PTR_SYMB s;
	      PTR_LLND q, r;
	      if(! (yyvsp[(4) - (4)].ll_node)) {
		err("No dimensions in DIMENSION statement", 42);
	      }
	      s = make_array((yyvsp[(3) - (4)].hash_entry), TYNULL, (yyvsp[(4) - (4)].ll_node), ndim, LOCAL);
	      s->attr = s->attr | DIMENSION_BIT;
	      q = make_llnd(fi,ARRAY_REF, (yyvsp[(4) - (4)].ll_node), LLNULL, s);
	      s->type->entry.ar_decl.ranges = (yyvsp[(4) - (4)].ll_node);
	      r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	      add_to_lowLevelList(r, (yyvsp[(1) - (4)].bf_node)->entry.Template.ll_ptr1);
	;}
    break;

  case 178:
#line 1812 "gram1.y"
    {/* PTR_SYMB s;*/
	      PTR_LLND r;

	         /*if(!$5) {
		   err("No dimensions in ALLOCATABLE statement",305);		
	           }
	          s = make_array($4, TYNULL, $5, ndim, LOCAL);
	          s->attr = s->attr | ALLOCATABLE_BIT;
	          q = make_llnd(fi,ARRAY_REF, $5, LLNULL, s);
	          s->type->entry.ar_decl.ranges = $5;
                  r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
                */
              (yyvsp[(4) - (4)].ll_node)->entry.Template.symbol->attr = (yyvsp[(4) - (4)].ll_node)->entry.Template.symbol->attr | ALLOCATABLE_BIT;
	      r = make_llnd(fi,EXPR_LIST, (yyvsp[(4) - (4)].ll_node), LLNULL, SMNULL);
	      (yyval.bf_node) = get_bfnd(fi, ALLOCATABLE_STMT, SMNULL, r, LLNULL, LLNULL);
	    ;}
    break;

  case 179:
#line 1830 "gram1.y"
    {  /*PTR_SYMB s;*/
	      PTR_LLND r;

	        /*  if(! $4) {
		      err("No dimensions in ALLOCATABLE statement",305);
		
	            }
	           s = make_array($3, TYNULL, $4, ndim, LOCAL);
	           s->attr = s->attr | ALLOCATABLE_BIT;
	           q = make_llnd(fi,ARRAY_REF, $4, LLNULL, s);
	           s->type->entry.ar_decl.ranges = $4;
	           r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
                */
              (yyvsp[(3) - (3)].ll_node)->entry.Template.symbol->attr = (yyvsp[(3) - (3)].ll_node)->entry.Template.symbol->attr | ALLOCATABLE_BIT;
              r = make_llnd(fi,EXPR_LIST, (yyvsp[(3) - (3)].ll_node), LLNULL, SMNULL);
	      add_to_lowLevelList(r, (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
	;}
    break;

  case 180:
#line 1850 "gram1.y"
    { PTR_SYMB s;
	      PTR_LLND  r;
           
	          /*  if(! $5) {
		      err("No dimensions in POINTER statement",306);	    
	              } 
	             s = make_array($4, TYNULL, $5, ndim, LOCAL);
	             s->attr = s->attr | POINTER_BIT;
	             q = make_llnd(fi,ARRAY_REF, $5, LLNULL, s);
	             s->type->entry.ar_decl.ranges = $5;
                   */

                  /*s = make_pointer( $4->entry.Template.symbol->parent, TYNULL, LOCAL);*/ /*17.02.03*/
                 /*$4->entry.Template.symbol->attr = $4->entry.Template.symbol->attr | POINTER_BIT;*/
              s = (yyvsp[(4) - (4)].ll_node)->entry.Template.symbol; /*17.02.03*/
              s->attr = s->attr | POINTER_BIT;
	      r = make_llnd(fi,EXPR_LIST, (yyvsp[(4) - (4)].ll_node), LLNULL, SMNULL);
	      (yyval.bf_node) = get_bfnd(fi, POINTER_STMT, SMNULL, r, LLNULL, LLNULL);
	    ;}
    break;

  case 181:
#line 1870 "gram1.y"
    {  PTR_SYMB s;
	      PTR_LLND r;

     	        /*  if(! $4) {
	        	err("No dimensions in POINTER statement",306);
	            }
	           s = make_array($3, TYNULL, $4, ndim, LOCAL);
	           s->attr = s->attr | POINTER_BIT;
	           q = make_llnd(fi,ARRAY_REF, $4, LLNULL, s);
	           s->type->entry.ar_decl.ranges = $4;
                */

                /*s = make_pointer( $3->entry.Template.symbol->parent, TYNULL, LOCAL);*/ /*17.02.03*/
                /*$3->entry.Template.symbol->attr = $3->entry.Template.symbol->attr | POINTER_BIT;*/
              s = (yyvsp[(3) - (3)].ll_node)->entry.Template.symbol; /*17.02.03*/
              s->attr = s->attr | POINTER_BIT;
	      r = make_llnd(fi,EXPR_LIST, (yyvsp[(3) - (3)].ll_node), LLNULL, SMNULL);
	      add_to_lowLevelList(r, (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
	;}
    break;

  case 182:
#line 1892 "gram1.y"
    {/* PTR_SYMB s;*/
	      PTR_LLND r;


	     /* if(! $5) {
		err("No dimensions in TARGET statement",307);
	      }
	      s = make_array($4, TYNULL, $5, ndim, LOCAL);
	      s->attr = s->attr | TARGET_BIT;
	      q = make_llnd(fi,ARRAY_REF, $5, LLNULL, s);
	      s->type->entry.ar_decl.ranges = $5;
             */
              (yyvsp[(4) - (4)].ll_node)->entry.Template.symbol->attr = (yyvsp[(4) - (4)].ll_node)->entry.Template.symbol->attr | TARGET_BIT;
	      r = make_llnd(fi,EXPR_LIST, (yyvsp[(4) - (4)].ll_node), LLNULL, SMNULL);
	      (yyval.bf_node) = get_bfnd(fi, TARGET_STMT, SMNULL, r, LLNULL, LLNULL);
	    ;}
    break;

  case 183:
#line 1909 "gram1.y"
    {  /*PTR_SYMB s;*/
	      PTR_LLND r;

	     /* if(! $4) {
		err("No dimensions in TARGET statement",307);
	      }
	      s = make_array($3, TYNULL, $4, ndim, LOCAL);
	      s->attr = s->attr | TARGET_BIT;
	      q = make_llnd(fi,ARRAY_REF, $4, LLNULL, s);
	      s->type->entry.ar_decl.ranges = $4;
              */
              (yyvsp[(3) - (3)].ll_node)->entry.Template.symbol->attr = (yyvsp[(3) - (3)].ll_node)->entry.Template.symbol->attr | TARGET_BIT;
	      r = make_llnd(fi,EXPR_LIST, (yyvsp[(3) - (3)].ll_node), LLNULL, SMNULL);
	      add_to_lowLevelList(r, (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
	;}
    break;

  case 184:
#line 1927 "gram1.y"
    { PTR_LLND p, q;

              p = make_llnd(fi,EXPR_LIST, (yyvsp[(3) - (3)].ll_node), LLNULL, SMNULL);
	      q = make_llnd(fi,COMM_LIST, p, LLNULL, SMNULL);
	      (yyval.bf_node) = get_bfnd(fi,COMM_STAT, SMNULL, q, LLNULL, LLNULL);
	    ;}
    break;

  case 185:
#line 1934 "gram1.y"
    { PTR_LLND p, q;

              p = make_llnd(fi,EXPR_LIST, (yyvsp[(4) - (4)].ll_node), LLNULL, SMNULL);
	      q = make_llnd(fi,COMM_LIST, p, LLNULL, (yyvsp[(3) - (4)].symbol));
	      (yyval.bf_node) = get_bfnd(fi,COMM_STAT, SMNULL, q, LLNULL, LLNULL);
	    ;}
    break;

  case 186:
#line 1941 "gram1.y"
    { PTR_LLND p, q;

              p = make_llnd(fi,EXPR_LIST, (yyvsp[(5) - (5)].ll_node), LLNULL, SMNULL);
	      q = make_llnd(fi,COMM_LIST, p, LLNULL, (yyvsp[(3) - (5)].symbol));
	      add_to_lowList(q, (yyvsp[(1) - (5)].bf_node)->entry.Template.ll_ptr1);
	    ;}
    break;

  case 187:
#line 1948 "gram1.y"
    { PTR_LLND p, r;

              p = make_llnd(fi,EXPR_LIST, (yyvsp[(3) - (3)].ll_node), LLNULL, SMNULL);
	      /*q = make_llnd(fi,COMM_LIST, p, LLNULL, SMNULL);*/
	      for (r = (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1;
		   r->entry.list.next;
		   r = r->entry.list.next);
	      add_to_lowLevelList(p, r->entry.Template.ll_ptr1);
	    ;}
    break;

  case 188:
#line 1961 "gram1.y"
    { PTR_LLND q, r;

              q = make_llnd(fi,EXPR_LIST, (yyvsp[(4) - (4)].ll_node), LLNULL, SMNULL);
	      r = make_llnd(fi,NAMELIST_LIST, q, LLNULL, (yyvsp[(3) - (4)].symbol));
	      (yyval.bf_node) = get_bfnd(fi,NAMELIST_STAT, SMNULL, r, LLNULL, LLNULL);
	    ;}
    break;

  case 189:
#line 1968 "gram1.y"
    { PTR_LLND q, r;

              q = make_llnd(fi,EXPR_LIST, (yyvsp[(5) - (5)].ll_node), LLNULL, SMNULL);
	      r = make_llnd(fi,NAMELIST_LIST, q, LLNULL, (yyvsp[(3) - (5)].symbol));
	      add_to_lowList(r, (yyvsp[(1) - (5)].bf_node)->entry.Template.ll_ptr1);
	    ;}
    break;

  case 190:
#line 1975 "gram1.y"
    { PTR_LLND q, r;

              q = make_llnd(fi,EXPR_LIST, (yyvsp[(3) - (3)].ll_node), LLNULL, SMNULL);
	      for (r = (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1;
		   r->entry.list.next;
		   r = r->entry.list.next);
	      add_to_lowLevelList(q, r->entry.Template.ll_ptr1);
	    ;}
    break;

  case 191:
#line 1986 "gram1.y"
    { (yyval.symbol) =  make_local_entity((yyvsp[(2) - (3)].hash_entry), NAMELIST_NAME,global_default,LOCAL); ;}
    break;

  case 192:
#line 1990 "gram1.y"
    { (yyval.symbol) = NULL; /*make_common(look_up_sym("*"));*/ ;}
    break;

  case 193:
#line 1992 "gram1.y"
    { (yyval.symbol) = make_common((yyvsp[(2) - (3)].hash_entry)); ;}
    break;

  case 194:
#line 1997 "gram1.y"
    {  PTR_SYMB s;
	
	      if((yyvsp[(2) - (2)].ll_node)) {
		s = make_array((yyvsp[(1) - (2)].hash_entry), TYNULL, (yyvsp[(2) - (2)].ll_node), ndim, LOCAL);
                s->attr = s->attr | DIMENSION_BIT;
		s->type->entry.ar_decl.ranges = (yyvsp[(2) - (2)].ll_node);
		(yyval.ll_node) = make_llnd(fi,ARRAY_REF, (yyvsp[(2) - (2)].ll_node), LLNULL, s);
	      }
	      else {
		s = make_scalar((yyvsp[(1) - (2)].hash_entry), TYNULL, LOCAL);	
		(yyval.ll_node) = make_llnd(fi,VAR_REF, LLNULL, LLNULL, s);
	      }

          ;}
    break;

  case 195:
#line 2015 "gram1.y"
    { PTR_LLND p, q;
              PTR_SYMB s;

	      s = make_external((yyvsp[(4) - (4)].hash_entry), TYNULL);
	      s->attr = s->attr | EXTERNAL_BIT;
              q = make_llnd(fi, VAR_REF, LLNULL, LLNULL, s);
	      p = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	      (yyval.bf_node) = get_bfnd(fi,EXTERN_STAT, SMNULL, p, LLNULL, LLNULL);
	    ;}
    break;

  case 196:
#line 2026 "gram1.y"
    { PTR_LLND p, q;
              PTR_SYMB s;

	      s = make_external((yyvsp[(3) - (3)].hash_entry), TYNULL);
	      s->attr = s->attr | EXTERNAL_BIT;
              p = make_llnd(fi, VAR_REF, LLNULL, LLNULL, s);
	      q = make_llnd(fi,EXPR_LIST, p, LLNULL, SMNULL);
	      add_to_lowLevelList(q, (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
	    ;}
    break;

  case 197:
#line 2038 "gram1.y"
    { PTR_LLND p, q;
              PTR_SYMB s;

	      s = make_intrinsic((yyvsp[(4) - (4)].hash_entry), TYNULL); /*make_function($3, TYNULL, NO);*/
	      s->attr = s->attr | INTRINSIC_BIT;
              q = make_llnd(fi, VAR_REF, LLNULL, LLNULL, s);
	      p = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	      (yyval.bf_node) = get_bfnd(fi,INTRIN_STAT, SMNULL, p,
			     LLNULL, LLNULL);
	    ;}
    break;

  case 198:
#line 2050 "gram1.y"
    { PTR_LLND p, q;
              PTR_SYMB s;

	      s = make_intrinsic((yyvsp[(3) - (3)].hash_entry), TYNULL); /* make_function($3, TYNULL, NO);*/
	      s->attr = s->attr | INTRINSIC_BIT;
              p = make_llnd(fi, VAR_REF, LLNULL, LLNULL, s);
	      q = make_llnd(fi,EXPR_LIST, p, LLNULL, SMNULL);
	      add_to_lowLevelList(q, (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
	    ;}
    break;

  case 199:
#line 2064 "gram1.y"
    {
	      (yyval.bf_node) = get_bfnd(fi,EQUI_STAT, SMNULL, (yyvsp[(3) - (3)].ll_node),
			     LLNULL, LLNULL);
	    ;}
    break;

  case 200:
#line 2070 "gram1.y"
    { 
	      add_to_lowLevelList((yyvsp[(3) - (3)].ll_node), (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
	    ;}
    break;

  case 201:
#line 2077 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,EQUI_LIST, (yyvsp[(2) - (3)].ll_node), LLNULL, SMNULL);
           ;}
    break;

  case 202:
#line 2083 "gram1.y"
    { PTR_LLND p;
	      p = make_llnd(fi,EXPR_LIST, (yyvsp[(3) - (3)].ll_node), LLNULL, SMNULL);
	      (yyval.ll_node) = make_llnd(fi,EXPR_LIST, (yyvsp[(1) - (3)].ll_node), p, SMNULL);
	    ;}
    break;

  case 203:
#line 2089 "gram1.y"
    { PTR_LLND p;

	      p = make_llnd(fi,EXPR_LIST, (yyvsp[(3) - (3)].ll_node), LLNULL, SMNULL);
	      add_to_lowLevelList(p, (yyvsp[(1) - (3)].ll_node));
	    ;}
    break;

  case 204:
#line 2097 "gram1.y"
    {  PTR_SYMB s;
           s=make_scalar((yyvsp[(1) - (1)].hash_entry),TYNULL,LOCAL);
           (yyval.ll_node) = make_llnd(fi,VAR_REF, LLNULL, LLNULL, s);
           s->attr = s->attr | EQUIVALENCE_BIT;
            /*$$=$1; $$->entry.Template.symbol->attr = $$->entry.Template.symbol->attr | EQUIVALENCE_BIT; */
        ;}
    break;

  case 205:
#line 2104 "gram1.y"
    {  PTR_SYMB s;
           s=make_array((yyvsp[(1) - (4)].hash_entry),TYNULL,LLNULL,0,LOCAL);
           (yyval.ll_node) = make_llnd(fi,ARRAY_REF, (yyvsp[(3) - (4)].ll_node), LLNULL, s);
           s->attr = s->attr | EQUIVALENCE_BIT;
            /*$$->entry.Template.symbol->attr = $$->entry.Template.symbol->attr | EQUIVALENCE_BIT; */
        ;}
    break;

  case 207:
#line 2123 "gram1.y"
    { PTR_LLND p;
              data_stat = NO;
	      p = make_llnd(fi,STMT_STR, LLNULL, LLNULL,
			    SMNULL);
              p->entry.string_val = copys(stmtbuf);
	      (yyval.bf_node) = get_bfnd(fi,DATA_DECL, SMNULL, p, LLNULL, LLNULL);
            ;}
    break;

  case 210:
#line 2137 "gram1.y"
    {data_stat = YES;;}
    break;

  case 211:
#line 2141 "gram1.y"
    {
	      if (parstate == OUTSIDE)
	         { PTR_BFND p;

		   p = get_bfnd(fi,PROG_HEDR,
                                make_program(look_up_sym("_MAIN")),
                                LLNULL, LLNULL, LLNULL);
		   set_blobs(p, global_bfnd, NEW_GROUP1);
	           add_scope_level(p, NO);
		   position = IN_PROC; 
	  	   /*parstate = INDCL;*/
                 }
	      if(parstate < INDCL)
		{
		  /* enddcl();*/
		  parstate = INDCL;
		}
	    ;}
    break;

  case 222:
#line 2186 "gram1.y"
    {;;}
    break;

  case 223:
#line 2190 "gram1.y"
    { (yyval.symbol)= make_scalar((yyvsp[(1) - (1)].hash_entry), TYNULL, LOCAL);;}
    break;

  case 224:
#line 2194 "gram1.y"
    { (yyval.symbol)= make_scalar((yyvsp[(1) - (1)].hash_entry), TYNULL, LOCAL); 
              (yyval.symbol)->attr = (yyval.symbol)->attr | DATA_BIT; 
            ;}
    break;

  case 225:
#line 2200 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, DATA_SUBS, (yyvsp[(2) - (3)].ll_node), LLNULL, SMNULL); ;}
    break;

  case 226:
#line 2204 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, DATA_RANGE, (yyvsp[(2) - (5)].ll_node), (yyvsp[(4) - (5)].ll_node), SMNULL); ;}
    break;

  case 227:
#line 2208 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 228:
#line 2210 "gram1.y"
    { (yyval.ll_node) = add_to_lowLevelList((yyvsp[(3) - (3)].ll_node), (yyvsp[(1) - (3)].ll_node)); ;}
    break;

  case 229:
#line 2214 "gram1.y"
    { (yyval.ll_node) = LLNULL; ;}
    break;

  case 230:
#line 2216 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 231:
#line 2220 "gram1.y"
    {(yyval.ll_node)= make_llnd(fi, DATA_IMPL_DO, (yyvsp[(2) - (7)].ll_node), (yyvsp[(6) - (7)].ll_node), (yyvsp[(4) - (7)].symbol)); ;}
    break;

  case 232:
#line 2224 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 233:
#line 2226 "gram1.y"
    { (yyval.ll_node) = add_to_lowLevelList((yyvsp[(3) - (3)].ll_node), (yyvsp[(1) - (3)].ll_node)); ;}
    break;

  case 234:
#line 2230 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, DATA_ELT, (yyvsp[(2) - (2)].ll_node), LLNULL, (yyvsp[(1) - (2)].symbol)); ;}
    break;

  case 235:
#line 2232 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, DATA_ELT, (yyvsp[(2) - (2)].ll_node), LLNULL, (yyvsp[(1) - (2)].symbol)); ;}
    break;

  case 236:
#line 2234 "gram1.y"
    {
              (yyvsp[(2) - (3)].ll_node)->entry.Template.ll_ptr2 = (yyvsp[(3) - (3)].ll_node);
              (yyval.ll_node) = make_llnd(fi, DATA_ELT, (yyvsp[(2) - (3)].ll_node), LLNULL, (yyvsp[(1) - (3)].symbol)); 
            ;}
    break;

  case 237:
#line 2239 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, DATA_ELT, (yyvsp[(1) - (1)].ll_node), LLNULL, SMNULL); ;}
    break;

  case 251:
#line 2263 "gram1.y"
    {if((yyvsp[(2) - (6)].ll_node)->entry.Template.symbol->variant != TYPE_NAME)
               errstr("Undefined type %s",(yyvsp[(2) - (6)].ll_node)->entry.Template.symbol->ident,319); 
           ;}
    break;

  case 268:
#line 2308 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ICON_EXPR, (yyvsp[(1) - (1)].ll_node), LLNULL, SMNULL); ;}
    break;

  case 269:
#line 2310 "gram1.y"
    {
              PTR_LLND p;

              p = intrinsic_op_node("+", UNARY_ADD_OP, (yyvsp[(2) - (2)].ll_node), LLNULL);
              (yyval.ll_node) = make_llnd(fi,ICON_EXPR, p, LLNULL, SMNULL);
            ;}
    break;

  case 270:
#line 2317 "gram1.y"
    {
              PTR_LLND p;
 
              p = intrinsic_op_node("-", MINUS_OP, (yyvsp[(2) - (2)].ll_node), LLNULL);
              (yyval.ll_node) = make_llnd(fi,ICON_EXPR, p, LLNULL, SMNULL);
            ;}
    break;

  case 271:
#line 2324 "gram1.y"
    {
              PTR_LLND p;
 
              p = intrinsic_op_node("+", ADD_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node));
              (yyval.ll_node) = make_llnd(fi,ICON_EXPR, p, LLNULL, SMNULL);
            ;}
    break;

  case 272:
#line 2331 "gram1.y"
    {
              PTR_LLND p;
 
              p = intrinsic_op_node("-", SUBT_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node));
              (yyval.ll_node) = make_llnd(fi,ICON_EXPR, p, LLNULL, SMNULL);
            ;}
    break;

  case 273:
#line 2340 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 274:
#line 2342 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node("*", MULT_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 275:
#line 2344 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node("/", DIV_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 276:
#line 2348 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 277:
#line 2350 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node("**", EXP_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 278:
#line 2354 "gram1.y"
    {
              PTR_LLND p;

              p = make_llnd(fi,INT_VAL, LLNULL, LLNULL, SMNULL);
              p->entry.ival = atoi(yytext);
              p->type = global_int;
              (yyval.ll_node) = make_llnd(fi,EXPR_LIST, p, LLNULL, SMNULL);
            ;}
    break;

  case 279:
#line 2363 "gram1.y"
    {
              PTR_LLND p;
 
              p = make_llnd(fi,VAR_REF, LLNULL, LLNULL, (yyvsp[(1) - (1)].symbol));
              (yyval.ll_node) = make_llnd(fi,EXPR_LIST, p, LLNULL, SMNULL);
            ;}
    break;

  case 280:
#line 2370 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,EXPR_LIST, (yyvsp[(2) - (3)].ll_node), LLNULL, SMNULL);
            ;}
    break;

  case 281:
#line 2377 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,EXPR_LIST, (yyvsp[(1) - (1)].ll_node), LLNULL, SMNULL); ;}
    break;

  case 282:
#line 2379 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); ;}
    break;

  case 283:
#line 2383 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);
             (yyval.ll_node)->entry.Template.symbol->attr = (yyval.ll_node)->entry.Template.symbol->attr | SAVE_BIT;
           ;}
    break;

  case 284:
#line 2387 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,COMM_LIST, LLNULL, LLNULL, (yyvsp[(1) - (1)].symbol)); 
            (yyval.ll_node)->entry.Template.symbol->attr = (yyval.ll_node)->entry.Template.symbol->attr | SAVE_BIT;
          ;}
    break;

  case 285:
#line 2393 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(2) - (3)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 286:
#line 2395 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (5)].ll_node), (yyvsp[(4) - (5)].ll_node), EXPR_LIST); ;}
    break;

  case 287:
#line 2399 "gram1.y"
    { as_op_kwd_ = YES; ;}
    break;

  case 288:
#line 2403 "gram1.y"
    { as_op_kwd_ = NO; ;}
    break;

  case 289:
#line 2408 "gram1.y"
    { 
             PTR_SYMB s; 
             s = make_scalar((yyvsp[(1) - (1)].hash_entry), TYNULL, LOCAL);	
	     s->attr = s->attr | type_opt;
	     (yyval.ll_node) = make_llnd(fi,VAR_REF, LLNULL, LLNULL, s);
            ;}
    break;

  case 290:
#line 2415 "gram1.y"
    { PTR_SYMB s;
	      s = make_function((yyvsp[(3) - (4)].hash_entry), global_default, LOCAL);
	      s->variant = INTERFACE_NAME;
              s->attr = s->attr | type_opt;
              (yyval.ll_node) = make_llnd(fi,OPERATOR_OP, LLNULL, LLNULL, s);
	    ;}
    break;

  case 291:
#line 2422 "gram1.y"
    { PTR_SYMB s;
	      s = make_procedure(look_up_sym("="), LOCAL);
	      s->variant = INTERFACE_NAME;
              s->attr = s->attr | type_opt;
              (yyval.ll_node) = make_llnd(fi,ASSIGNMENT_OP, LLNULL, LLNULL, s);
	    ;}
    break;

  case 292:
#line 2432 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 293:
#line 2434 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); ;}
    break;

  case 294:
#line 2438 "gram1.y"
    { PTR_SYMB p;

                /* The check if name and expr have compatible types has
                   not been done yet. */ 
		p = make_constant((yyvsp[(1) - (3)].hash_entry), TYNULL);
 	        p->attr = p->attr | PARAMETER_BIT;
                p->entry.const_value = (yyvsp[(3) - (3)].ll_node);
		(yyval.ll_node) = make_llnd(fi,CONST_REF, LLNULL, LLNULL, p);
	    ;}
    break;

  case 295:
#line 2450 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, MODULE_PROC_STMT, SMNULL, (yyvsp[(2) - (2)].ll_node), LLNULL, LLNULL); ;}
    break;

  case 296:
#line 2453 "gram1.y"
    { PTR_SYMB s;
 	      PTR_LLND q;

	      s = make_function((yyvsp[(1) - (1)].hash_entry), TYNULL, LOCAL);
	      s->variant = ROUTINE_NAME;
              q = make_llnd(fi, VAR_REF, LLNULL, LLNULL, s);
	      (yyval.ll_node) = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	    ;}
    break;

  case 297:
#line 2462 "gram1.y"
    { PTR_LLND p, q;
              PTR_SYMB s;

	      s = make_function((yyvsp[(3) - (3)].hash_entry), TYNULL, LOCAL);
	      s->variant = ROUTINE_NAME;
              p = make_llnd(fi, VAR_REF, LLNULL, LLNULL, s);
	      q = make_llnd(fi,EXPR_LIST, p, LLNULL, SMNULL);
	      add_to_lowLevelList(q, (yyvsp[(1) - (3)].ll_node));
	    ;}
    break;

  case 298:
#line 2475 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, USE_STMT, (yyvsp[(3) - (3)].symbol), LLNULL, LLNULL, LLNULL);
              /*add_scope_level($3->entry.Template.func_hedr, YES);*/ /*17.06.01*/
              copy_module_scope((yyvsp[(3) - (3)].symbol),LLNULL); /*17.03.03*/
              colon_flag = NO;
            ;}
    break;

  case 299:
#line 2481 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, USE_STMT, (yyvsp[(3) - (6)].symbol), (yyvsp[(6) - (6)].ll_node), LLNULL, LLNULL); 
              /*add_scope_level(module_scope, YES); *//* 17.06.01*/
              copy_module_scope((yyvsp[(3) - (6)].symbol),(yyvsp[(6) - (6)].ll_node)); /*17.03.03 */
              colon_flag = NO;
            ;}
    break;

  case 300:
#line 2487 "gram1.y"
    { PTR_LLND l;

	      l = make_llnd(fi, ONLY_NODE, LLNULL, LLNULL, SMNULL);
              (yyval.bf_node) = get_bfnd(fi, USE_STMT, (yyvsp[(3) - (6)].symbol), l, LLNULL, LLNULL);
            ;}
    break;

  case 301:
#line 2493 "gram1.y"
    { PTR_LLND l;

	      l = make_llnd(fi, ONLY_NODE, (yyvsp[(7) - (7)].ll_node), LLNULL, SMNULL);
              (yyval.bf_node) = get_bfnd(fi, USE_STMT, (yyvsp[(3) - (7)].symbol), l, LLNULL, LLNULL);
            ;}
    break;

  case 302:
#line 2501 "gram1.y"
    {
              if ((yyvsp[(1) - (1)].hash_entry)->id_attr == SMNULL)
	         warn1("Unknown module %s", (yyvsp[(1) - (1)].hash_entry)->ident,308);
              (yyval.symbol) = make_global_entity((yyvsp[(1) - (1)].hash_entry), MODULE_NAME, global_default, NO);
	      module_scope = (yyval.symbol)->entry.Template.func_hedr;
           
            ;}
    break;

  case 303:
#line 2511 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 304:
#line 2513 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); ;}
    break;

  case 305:
#line 2517 "gram1.y"
    {  (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 306:
#line 2519 "gram1.y"
    {  PTR_HASH oldhash,copyhash;
	       PTR_SYMB oldsym, newsym;
	       PTR_LLND m;

	       oldhash = just_look_up_sym_in_scope(module_scope, (yyvsp[(1) - (1)].hash_entry)->ident);
	       if (oldhash == HSNULL) {
                  errstr("Unknown identifier %s.", (yyvsp[(1) - (1)].hash_entry)->ident,309);
	          (yyval.ll_node)= LLNULL;
	       }
	       else {
                 oldsym = oldhash->id_attr;
                 copyhash=just_look_up_sym_in_scope(cur_scope(), (yyvsp[(1) - (1)].hash_entry)->ident);
	         if( copyhash && copyhash->id_attr && copyhash->id_attr->entry.Template.tag==module_scope->id)
                 {
                   newsym = copyhash->id_attr;
                   newsym->entry.Template.tag = 0;
                 }
                 else
                 {
	           newsym = make_local_entity((yyvsp[(1) - (1)].hash_entry), oldsym->variant, oldsym->type,LOCAL);
	           /* copies data in entry.Template structure and attr */
	           copy_sym_data(oldsym, newsym);	         
	             /*newsym->entry.Template.base_name = oldsym;*//*19.03.03*/
                 }
	  	/* l = make_llnd(fi, VAR_REF, LLNULL, LLNULL, oldsym);*/
		 m = make_llnd(fi, VAR_REF, LLNULL, LLNULL, newsym);
		 (yyval.ll_node) = make_llnd(fi, RENAME_NODE, m, LLNULL, oldsym);
 	      }
            ;}
    break;

  case 307:
#line 2552 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 308:
#line 2554 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); ;}
    break;

  case 309:
#line 2558 "gram1.y"
    {  PTR_HASH oldhash,copyhash;
	       PTR_SYMB oldsym, newsym;
	       PTR_LLND l, m;

	       oldhash = just_look_up_sym_in_scope(module_scope, (yyvsp[(3) - (3)].hash_entry)->ident);
	       if (oldhash == HSNULL) {
                  errstr("Unknown identifier %s", (yyvsp[(3) - (3)].hash_entry)->ident,309);
	          (yyval.ll_node)= LLNULL;
	       }
	       else {
                 oldsym = oldhash->id_attr;
                 copyhash = just_look_up_sym_in_scope(cur_scope(), (yyvsp[(3) - (3)].hash_entry)->ident);
	         if(copyhash && copyhash->id_attr && copyhash->id_attr->entry.Template.tag==module_scope->id)
                 {
                    delete_symbol(copyhash->id_attr);
                    copyhash->id_attr == SMNULL;
                 }
                   newsym = make_local_entity((yyvsp[(1) - (3)].hash_entry), oldsym->variant, oldsym->type, LOCAL);
	           /* copies data in entry.Template structure and attr */
	           copy_sym_data(oldsym, newsym);	
                         
	           /*newsym->entry.Template.base_name = oldsym;*//*19.03.03*/
	  	 l  = make_llnd(fi, VAR_REF, LLNULL, LLNULL, oldsym);
		 m  = make_llnd(fi, VAR_REF, LLNULL, LLNULL, newsym);
		 (yyval.ll_node) = make_llnd(fi, RENAME_NODE, m, l, SMNULL);
 	      }
            ;}
    break;

  case 310:
#line 2596 "gram1.y"
    { ndim = 0;	explicit_shape = 1; (yyval.ll_node) = LLNULL; ;}
    break;

  case 311:
#line 2598 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(2) - (3)].ll_node); ;}
    break;

  case 312:
#line 2601 "gram1.y"
    { ndim = 0; explicit_shape = 1;;}
    break;

  case 313:
#line 2602 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,EXPR_LIST, (yyvsp[(2) - (2)].ll_node), LLNULL, SMNULL);
	      (yyval.ll_node)->type = global_default;
	    ;}
    break;

  case 314:
#line 2607 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); ;}
    break;

  case 315:
#line 2611 "gram1.y"
    {
	      if(ndim == maxdim)
		err("Too many dimensions", 43);
	      else if(ndim < maxdim)
		(yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);
	      ++ndim;
	    ;}
    break;

  case 316:
#line 2619 "gram1.y"
    {
	      if(ndim == maxdim)
		err("Too many dimensions", 43);
	      else if(ndim < maxdim)
		(yyval.ll_node) = make_llnd(fi, DDOT, LLNULL, LLNULL, SMNULL);
	      ++ndim;
              explicit_shape = 0;
	    ;}
    break;

  case 317:
#line 2628 "gram1.y"
    {
	      if(ndim == maxdim)
		err("Too many dimensions", 43);
	      else if(ndim < maxdim)
		(yyval.ll_node) = make_llnd(fi,DDOT, (yyvsp[(1) - (2)].ll_node), LLNULL, SMNULL);
	      ++ndim;
              explicit_shape = 0;
	    ;}
    break;

  case 318:
#line 2637 "gram1.y"
    {
	      if(ndim == maxdim)
		err("Too many dimensions", 43);
	      else if(ndim < maxdim)
		(yyval.ll_node) = make_llnd(fi,DDOT, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), SMNULL);
	      ++ndim;
	    ;}
    break;

  case 319:
#line 2647 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,STAR_RANGE, LLNULL, LLNULL, SMNULL);
	      (yyval.ll_node)->type = global_default;
              explicit_shape = 0;
	    ;}
    break;

  case 321:
#line 2656 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 322:
#line 2658 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); ;}
    break;

  case 323:
#line 2662 "gram1.y"
    {PTR_LABEL p;
	     p = make_label_node(fi,convci(yyleng, yytext));
	     p->scope = cur_scope();
	     (yyval.ll_node) = make_llnd_label(fi,LABEL_REF, p);
	  ;}
    break;

  case 324:
#line 2670 "gram1.y"
    { /*PTR_LLND l;*/

          /*   l = make_llnd(fi, EXPR_LIST, $3, LLNULL, SMNULL);*/
             (yyval.bf_node) = get_bfnd(fi,IMPL_DECL, SMNULL, (yyvsp[(3) - (3)].ll_node), LLNULL, LLNULL);
             redefine_func_arg_type();
           ;}
    break;

  case 325:
#line 2685 "gram1.y"
    { /*undeftype = YES;
	    setimpl(TYNULL, (int)'a', (int)'z'); FB COMMENTED---> NOT QUITE RIGHT BUT AVOID PB WITH COMMON*/
	    (yyval.bf_node) = get_bfnd(fi,IMPL_DECL, SMNULL, LLNULL, LLNULL, LLNULL);
	  ;}
    break;

  case 326:
#line 2692 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 327:
#line 2694 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); ;}
    break;

  case 328:
#line 2698 "gram1.y"
    { 

            (yyval.ll_node) = make_llnd(fi, IMPL_TYPE, (yyvsp[(3) - (4)].ll_node), LLNULL, SMNULL);
            (yyval.ll_node)->type = vartype;
          ;}
    break;

  case 329:
#line 2713 "gram1.y"
    { implkwd = YES; ;}
    break;

  case 330:
#line 2714 "gram1.y"
    { vartype = (yyvsp[(2) - (2)].data_type); ;}
    break;

  case 331:
#line 2718 "gram1.y"
    { (yyval.data_type) = (yyvsp[(2) - (2)].data_type); ;}
    break;

  case 332:
#line 2720 "gram1.y"
    { (yyval.data_type) = (yyvsp[(1) - (1)].data_type);;}
    break;

  case 333:
#line 2732 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 334:
#line 2734 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); ;}
    break;

  case 335:
#line 2738 "gram1.y"
    {
	      setimpl(vartype, (int)(yyvsp[(1) - (1)].charv), (int)(yyvsp[(1) - (1)].charv));
	      (yyval.ll_node) = make_llnd(fi,CHAR_VAL, LLNULL, LLNULL, SMNULL);
	      (yyval.ll_node)->entry.cval = (yyvsp[(1) - (1)].charv);
	    ;}
    break;

  case 336:
#line 2744 "gram1.y"
    { PTR_LLND p,q;
	      
	      setimpl(vartype, (int)(yyvsp[(1) - (3)].charv), (int)(yyvsp[(3) - (3)].charv));
	      p = make_llnd(fi,CHAR_VAL, LLNULL, LLNULL, SMNULL);
	      p->entry.cval = (yyvsp[(1) - (3)].charv);
	      q = make_llnd(fi,CHAR_VAL, LLNULL, LLNULL, SMNULL);
	      q->entry.cval = (yyvsp[(3) - (3)].charv);
	      (yyval.ll_node)= make_llnd(fi,DDOT, p, q, SMNULL);
	    ;}
    break;

  case 337:
#line 2756 "gram1.y"
    {
	      if(yyleng!=1 || yytext[0]<'a' || yytext[0]>'z')
		{
		  err("IMPLICIT item must be single letter", 37);
		  (yyval.charv) = '\0';
		}
	      else (yyval.charv) = yytext[0];
	    ;}
    break;

  case 338:
#line 2767 "gram1.y"
    {
	      if (parstate == OUTSIDE)
	         { PTR_BFND p;

		   p = get_bfnd(fi,PROG_HEDR,
                                make_program(look_up_sym("_MAIN")),
                                LLNULL, LLNULL, LLNULL);
		   set_blobs(p, global_bfnd, NEW_GROUP1);
	           add_scope_level(p, NO);
		   position = IN_PROC; 
	  	   parstate = INSIDE;
                 }
	  
	    ;}
    break;

  case 339:
#line 2784 "gram1.y"
    { switch(parstate)
		{
                case OUTSIDE:  
			{ PTR_BFND p;

			  p = get_bfnd(fi,PROG_HEDR,
                                       make_program(look_up_sym("_MAIN")),
                                       LLNULL, LLNULL, LLNULL);
			  set_blobs(p, global_bfnd, NEW_GROUP1);
			  add_scope_level(p, NO);
			  position = IN_PROC; 
	  		  parstate = INDCL; }
	                  break;
                case INSIDE:    parstate = INDCL;
                case INDCL:     break;

                case INDATA:
                         /*  err(
                     "Statement order error: declaration after DATA or function statement", 
                                 29);*/
                              break;

                default:
                           err("Declaration among executables", 30);
                }
        ;}
    break;

  case 342:
#line 2822 "gram1.y"
    { (yyval.ll_node) = LLNULL; endioctl(); ;}
    break;

  case 343:
#line 2824 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);  endioctl();;}
    break;

  case 344:
#line 2828 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 345:
#line 2830 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 346:
#line 2832 "gram1.y"
    { PTR_LLND l;
	      l = make_llnd(fi, KEYWORD_ARG, (yyvsp[(1) - (2)].ll_node), (yyvsp[(2) - (2)].ll_node), SMNULL);
	      l->type = (yyvsp[(2) - (2)].ll_node)->type;
              (yyval.ll_node) = l; 
	    ;}
    break;

  case 347:
#line 2843 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(2) - (2)].ll_node), LLNULL, EXPR_LIST);
              endioctl(); 
            ;}
    break;

  case 348:
#line 2847 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (4)].ll_node), (yyvsp[(4) - (4)].ll_node), EXPR_LIST);
              endioctl(); 
            ;}
    break;

  case 349:
#line 2853 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 350:
#line 2855 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); ;}
    break;

  case 351:
#line 2859 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 352:
#line 2861 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(2) - (3)].ll_node); ;}
    break;

  case 353:
#line 2863 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 354:
#line 2867 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 355:
#line 2869 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 356:
#line 2873 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 357:
#line 2875 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node("+", ADD_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 358:
#line 2877 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node("-", SUBT_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 359:
#line 2879 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node("*", MULT_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 360:
#line 2881 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node("/", DIV_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 361:
#line 2883 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node("**", EXP_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 362:
#line 2885 "gram1.y"
    { (yyval.ll_node) = defined_op_node((yyvsp[(1) - (2)].hash_entry), (yyvsp[(2) - (2)].ll_node), LLNULL); ;}
    break;

  case 363:
#line 2887 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node("+", UNARY_ADD_OP, (yyvsp[(2) - (2)].ll_node), LLNULL); ;}
    break;

  case 364:
#line 2889 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node("-", MINUS_OP, (yyvsp[(2) - (2)].ll_node), LLNULL); ;}
    break;

  case 365:
#line 2891 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node(".eq.", EQ_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 366:
#line 2893 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node(".gt.", GT_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 367:
#line 2895 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node(".lt.", LT_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 368:
#line 2897 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node(".ge.", GTEQL_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 369:
#line 2899 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node(".ge.", LTEQL_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 370:
#line 2901 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node(".ne.", NOTEQL_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 371:
#line 2903 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node(".eqv.", EQV_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 372:
#line 2905 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node(".neqv.", NEQV_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 373:
#line 2907 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node(".xor.", XOR_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 374:
#line 2909 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node(".or.", OR_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 375:
#line 2911 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node(".and.", AND_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 376:
#line 2913 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node(".not.", NOT_OP, (yyvsp[(2) - (2)].ll_node), LLNULL); ;}
    break;

  case 377:
#line 2915 "gram1.y"
    { (yyval.ll_node) = intrinsic_op_node("//", CONCAT_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 378:
#line 2917 "gram1.y"
    { (yyval.ll_node) = defined_op_node((yyvsp[(2) - (3)].hash_entry), (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node)); ;}
    break;

  case 379:
#line 2920 "gram1.y"
    { (yyval.token) = ADD_OP; ;}
    break;

  case 380:
#line 2921 "gram1.y"
    { (yyval.token) = SUBT_OP; ;}
    break;

  case 381:
#line 2933 "gram1.y"
    { PTR_SYMB s;
	      PTR_TYPE t;
	     /* PTR_LLND l;*/

       	      if (!(s = (yyvsp[(1) - (1)].hash_entry)->id_attr))
              {
	         s = make_scalar((yyvsp[(1) - (1)].hash_entry), TYNULL, LOCAL);
	     	 s->decl = SOFT;
	      } 
	
	      switch (s->variant)
              {
	      case CONST_NAME:
		   (yyval.ll_node) = make_llnd(fi,CONST_REF,LLNULL,LLNULL, s);
		   t = s->type;
	           if ((t != TYNULL) &&
                       ((t->variant == T_ARRAY) ||  (t->variant == T_STRING) ))
                                 (yyval.ll_node)->variant = ARRAY_REF;

                   (yyval.ll_node)->type = t;
	           break;
	      case DEFAULT:   /* if common region with same name has been
                                 declared. */
		   s = make_scalar((yyvsp[(1) - (1)].hash_entry), TYNULL, LOCAL);
	     	   s->decl = SOFT;

	      case VARIABLE_NAME:
                   (yyval.ll_node) = make_llnd(fi,VAR_REF,LLNULL,LLNULL, s);
	           t = s->type;
	           if (t != TYNULL) {
                     if ((t->variant == T_ARRAY) ||  (t->variant == T_STRING) ||
                         ((t->variant == T_POINTER) && (t->entry.Template.base_type->variant == T_ARRAY) ) )
                         (yyval.ll_node)->variant = ARRAY_REF;

/*  	              if (t->variant == T_DERIVED_TYPE)
                         $$->variant = RECORD_REF; */
	           }
                   (yyval.ll_node)->type = t;
	           break;
	      case TYPE_NAME:
  	           (yyval.ll_node) = make_llnd(fi,TYPE_REF,LLNULL,LLNULL, s);
	           (yyval.ll_node)->type = s->type;
	           break;
	      case INTERFACE_NAME:
  	           (yyval.ll_node) = make_llnd(fi, INTERFACE_REF,LLNULL,LLNULL, s);
	           (yyval.ll_node)->type = s->type;
	           break;
              case FUNCTION_NAME:
                   if(isResultVar(s)) {
                     (yyval.ll_node) = make_llnd(fi,VAR_REF,LLNULL,LLNULL, s);
	             t = s->type;
	             if (t != TYNULL) {
                       if ((t->variant == T_ARRAY) ||  (t->variant == T_STRING) ||
                         ((t->variant == T_POINTER) && (t->entry.Template.base_type->variant == T_ARRAY) ) )
                         (yyval.ll_node)->variant = ARRAY_REF;
	             }
                     (yyval.ll_node)->type = t;
	             break;
                   }                                        
	      default:
  	           (yyval.ll_node) = make_llnd(fi,VAR_REF,LLNULL,LLNULL, s);
	           (yyval.ll_node)->type = s->type;
	           break;
	      }
             /* if ($$->variant == T_POINTER) {
	         l = $$;
	         $$ = make_llnd(fi, DEREF_OP, l, LLNULL, SMNULL);
	         $$->type = l->type->entry.Template.base_type;
	      }
              */ /*11.02.03*/
           ;}
    break;

  case 382:
#line 3007 "gram1.y"
    { PTR_SYMB  s;
	      (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); 
              s= (yyval.ll_node)->entry.Template.symbol;
              if ((((yyvsp[(1) - (1)].ll_node)->variant == VAR_REF) || ((yyvsp[(1) - (1)].ll_node)->variant == ARRAY_REF))  && (s->scope !=cur_scope()))  /*global_bfnd*/
              {
	          if(((s->variant == FUNCTION_NAME) && (!isResultVar(s))) || (s->variant == PROCEDURE_NAME) || (s->variant == ROUTINE_NAME))
                  { s = (yyval.ll_node)->entry.Template.symbol =  make_scalar(s->parent, TYNULL, LOCAL);
		    (yyval.ll_node)->type = s->type;  
		  }
              }
            ;}
    break;

  case 383:
#line 3019 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 384:
#line 3021 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 385:
#line 3025 "gram1.y"
    { int num_triplets;
	      PTR_SYMB s;  /*, sym;*/
	      /* PTR_LLND l; */
	      PTR_TYPE tp;
	      /* l = $1; */
	      s = (yyvsp[(1) - (5)].ll_node)->entry.Template.symbol;
            
	      /* Handle variable to function conversion. */
	      if (((yyvsp[(1) - (5)].ll_node)->variant == VAR_REF) && 
	          (((s->variant == VARIABLE_NAME) && (s->type) &&
                    (s->type->variant != T_ARRAY)) ||
  	            (s->variant == ROUTINE_NAME))) {
	        s = (yyvsp[(1) - (5)].ll_node)->entry.Template.symbol =  make_function(s->parent, TYNULL, NO);
	        (yyvsp[(1) - (5)].ll_node)->variant = FUNC_CALL;
              }
	      if (((yyvsp[(1) - (5)].ll_node)->variant == VAR_REF) && (s->variant == FUNCTION_NAME)) { 
                if(isResultVar(s))
	          (yyvsp[(1) - (5)].ll_node)->variant = ARRAY_REF;
                else
                  (yyvsp[(1) - (5)].ll_node)->variant = FUNC_CALL;
              }
	      if (((yyvsp[(1) - (5)].ll_node)->variant == VAR_REF) && (s->variant == PROGRAM_NAME)) {
                 errstr("The name '%s' is invalid in this context",s->ident,285);
                 (yyvsp[(1) - (5)].ll_node)->variant = FUNC_CALL;
              }
              /* l = $1; */
	      num_triplets = is_array_section_ref((yyvsp[(4) - (5)].ll_node));
	      switch ((yyvsp[(1) - (5)].ll_node)->variant)
              {
	      case TYPE_REF:
                   (yyvsp[(1) - (5)].ll_node)->variant = STRUCTURE_CONSTRUCTOR;                  
                   (yyvsp[(1) - (5)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (5)].ll_node);
                   (yyval.ll_node) = (yyvsp[(1) - (5)].ll_node);
                   (yyval.ll_node)->type =  lookup_type(s->parent); 
	          /* $$ = make_llnd(fi, STRUCTURE_CONSTRUCTOR, $1, $4, SMNULL);
	           $$->type = $1->type;*//*18.02.03*/
	           break;
	      case INTERFACE_REF:
	       /*  sym = resolve_overloading(s, $4);
	           if (sym != SMNULL)
	  	   {
	              l = make_llnd(fi, FUNC_CALL, $4, LLNULL, sym);
	              l->type = sym->type;
	              $$ = $1; $$->variant = OVERLOADED_CALL;
	              $$->entry.Template.ll_ptr1 = l;
	              $$->type = sym->type;
	           }
	           else {
	             errstr("can't resolve call %s", s->ident,310);
	           }
	           break;
                 */ /*podd 01.02.03*/

                   (yyvsp[(1) - (5)].ll_node)->variant = FUNC_CALL;

	      case FUNC_CALL:
                   (yyvsp[(1) - (5)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (5)].ll_node);
                   (yyval.ll_node) = (yyvsp[(1) - (5)].ll_node);
                   if(s->type) 
                     (yyval.ll_node)->type = s->type;
                   else
                     (yyval.ll_node)->type = global_default;
	           late_bind_if_needed((yyval.ll_node));
	           break;
	      case DEREF_OP:
              case ARRAY_REF:
	           /* array element */
	           if (num_triplets == 0) {
                       if ((yyvsp[(4) - (5)].ll_node) == LLNULL) {
                           s = (yyvsp[(1) - (5)].ll_node)->entry.Template.symbol = make_function(s->parent, TYNULL, NO);
                           s->entry.func_decl.num_output = 1;
                           (yyvsp[(1) - (5)].ll_node)->variant = FUNC_CALL;
                           (yyval.ll_node) = (yyvsp[(1) - (5)].ll_node);
                       } else if ((yyvsp[(1) - (5)].ll_node)->type->variant == T_STRING) {
                           PTR_LLND temp = (yyvsp[(4) - (5)].ll_node);
                           int num_input = 0;

                           while (temp) {
                             ++num_input;
                             temp = temp->entry.Template.ll_ptr2;
                           }
                           (yyvsp[(1) - (5)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (5)].ll_node);
                           s = (yyvsp[(1) - (5)].ll_node)->entry.Template.symbol = make_function(s->parent, TYNULL, NO);
                           s->entry.func_decl.num_output = 1;
                           s->entry.func_decl.num_input = num_input;
                           (yyvsp[(1) - (5)].ll_node)->variant = FUNC_CALL;
                           (yyval.ll_node) = (yyvsp[(1) - (5)].ll_node);
                       } else {
       	                   (yyvsp[(1) - (5)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (5)].ll_node);
	                   (yyval.ll_node) = (yyvsp[(1) - (5)].ll_node);
                           (yyval.ll_node)->type = (yyvsp[(1) - (5)].ll_node)->type->entry.ar_decl.base_type;
                       }
                   }
                   /* substring */
	           else if ((num_triplets == 1) && 
                            ((yyvsp[(1) - (5)].ll_node)->type->variant == T_STRING)) {
    	           /*
                     $1->entry.Template.ll_ptr1 = $4;
	             $$ = $1; $$->type = global_string;
                   */
	                  (yyval.ll_node) = make_llnd(fi, 
			  ARRAY_OP, LLNULL, LLNULL, SMNULL);
    	                  (yyval.ll_node)->entry.Template.ll_ptr1 = (yyvsp[(1) - (5)].ll_node);
       	                  (yyval.ll_node)->entry.Template.ll_ptr2 = (yyvsp[(4) - (5)].ll_node)->entry.Template.ll_ptr1;
	                  (yyval.ll_node)->type = global_string;
                   }           
                   /* array section */
                   else {
    	             (yyvsp[(1) - (5)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (5)].ll_node);
	             (yyval.ll_node) = (yyvsp[(1) - (5)].ll_node); tp = make_type(fi, T_ARRAY);     /**18.03.17*/
                     tp->entry.ar_decl.base_type = (yyvsp[(1) - (5)].ll_node)->type->entry.ar_decl.base_type; /**18.03.17 $1->type */
	             tp->entry.ar_decl.num_dimensions = num_triplets;
	             (yyval.ll_node)->type = tp;
                   }
	           break;
	      default:
                    if((yyvsp[(1) - (5)].ll_node)->entry.Template.symbol)
                      errstr("Can't subscript %s",(yyvsp[(1) - (5)].ll_node)->entry.Template.symbol->ident, 44);
                    else
	              err("Can't subscript",44);
             }
             /*if ($$->variant == T_POINTER) {
	        l = $$;
	        $$ = make_llnd(fi, DEREF_OP, l, LLNULL, SMNULL);
	        $$->type = l->type->entry.Template.base_type;
	     }
              */  /*11.02.03*/

	     endioctl(); 
           ;}
    break;

  case 386:
#line 3156 "gram1.y"
    { int num_triplets;
	      PTR_SYMB s;
	      PTR_LLND l;

	      s = (yyvsp[(1) - (6)].ll_node)->entry.Template.symbol;
/*              if ($1->type->variant == T_POINTER) {
	         l = $1;
	         $1 = make_llnd(fi, DEREF_OP, l, LLNULL, SMNULL);
	         $1->type = l->type->entry.Template.base_type;
	      } */
	      if (((yyvsp[(1) - (6)].ll_node)->type->variant != T_ARRAY) ||
                  ((yyvsp[(1) - (6)].ll_node)->type->entry.ar_decl.base_type->variant != T_STRING)) {
	         errstr("Can't take substring of %s", s->ident, 45);
              }
              else {
  	        num_triplets = is_array_section_ref((yyvsp[(4) - (6)].ll_node));
	           /* array element */
                if (num_triplets == 0) {
                   (yyvsp[(1) - (6)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (6)].ll_node);
                  /* $1->entry.Template.ll_ptr2 = $6;*/
	          /* $$ = $1;*/
                   l=(yyvsp[(1) - (6)].ll_node);
                   /*$$->type = $1->type->entry.ar_decl.base_type;*/
                   l->type = global_string;  /**18.03.17* $1->type->entry.ar_decl.base_type;*/
                }
                /* array section */
                else {
    	           (yyvsp[(1) - (6)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (6)].ll_node);
    	           /*$1->entry.Template.ll_ptr2 = $6;
	           $$ = $1; $$->type = make_type(fi, T_ARRAY);
                   $$->type->entry.ar_decl.base_type = $1->type;
	           $$->type->entry.ar_decl.num_dimensions = num_triplets;
                  */
                   l = (yyvsp[(1) - (6)].ll_node); l->type = make_type(fi, T_ARRAY);
                   l->type->entry.ar_decl.base_type = global_string;   /**18.03.17* $1->type*/
	           l->type->entry.ar_decl.num_dimensions = num_triplets;
               }
                (yyval.ll_node) = make_llnd(fi, ARRAY_OP, l, (yyvsp[(6) - (6)].ll_node), SMNULL);
	        (yyval.ll_node)->type = l->type;
              
              /* if ($$->variant == T_POINTER) {
	          l = $$;
	          $$ = make_llnd(fi, DEREF_OP, l, LLNULL, SMNULL);
	          $$->type = l->type->entry.Template.base_type;
	       }
               */  /*11.02.03*/
             }
             endioctl();
          ;}
    break;

  case 387:
#line 3206 "gram1.y"
    {  int num_triplets;
	      PTR_LLND l,l1,l2;
              PTR_TYPE tp;

         /*   if ($1->variant == T_POINTER) {
	         l = $1;
	         $1 = make_llnd(fi, DEREF_OP, l, LLNULL, SMNULL);
	         $1->type = l->type->entry.Template.base_type;
	      } */

              num_triplets = is_array_section_ref((yyvsp[(3) - (4)].ll_node));
              (yyval.ll_node) = (yyvsp[(1) - (4)].ll_node);
              l2 = (yyvsp[(1) - (4)].ll_node)->entry.Template.ll_ptr2;  
              l1 = (yyvsp[(1) - (4)].ll_node)->entry.Template.ll_ptr1;                
              if(l2 && l2->type->variant == T_STRING)/*substring*/
                if(num_triplets == 1){
	           l = make_llnd(fi, ARRAY_OP, LLNULL, LLNULL, SMNULL);
    	           l->entry.Template.ll_ptr1 = l2;
       	           l->entry.Template.ll_ptr2 = (yyvsp[(3) - (4)].ll_node)->entry.Template.ll_ptr1;
	           l->type = global_string; 
                   (yyval.ll_node)->entry.Template.ll_ptr2 = l;                                          
                } else
                   err("Can't subscript",44);
              else if (l2 && l2->type->variant == T_ARRAY) {
                 if(num_triplets > 0) { /*array section*/
                   tp = make_type(fi,T_ARRAY);
                   tp->entry.ar_decl.base_type = (yyvsp[(1) - (4)].ll_node)->type->entry.ar_decl.base_type;
                   tp->entry.ar_decl.num_dimensions = num_triplets;
                   (yyval.ll_node)->type = tp;
                   l2->entry.Template.ll_ptr1 = (yyvsp[(3) - (4)].ll_node);
                   l2->type = (yyval.ll_node)->type;   
                  }                 
                 else {  /*array element*/
                   l2->type = l2->type->entry.ar_decl.base_type;
                   l2->entry.Template.ll_ptr1 = (yyvsp[(3) - (4)].ll_node);   
                   if(l1->type->variant != T_ARRAY)  
                     (yyval.ll_node)->type = l2->type;
                 }
              } else 
                   {err("Can't subscript",44); /*fprintf(stderr,"%d  %d",$1->variant,l2);*/}
                   /*errstr("Can't subscript %s",l2->entry.Template.symbol->ident,441);*/
         ;}
    break;

  case 388:
#line 3250 "gram1.y"
    { int num_triplets;
	      PTR_LLND l,q;

          /*     if ($1->variant == T_POINTER) {
	         l = $1;
	         $1 = make_llnd(fi, DEREF_OP, l, LLNULL, SMNULL);
	         $1->type = l->type->entry.Template.base_type;
	      } */

              (yyval.ll_node) = (yyvsp[(1) - (5)].ll_node);
	      if (((yyvsp[(1) - (5)].ll_node)->type->variant != T_ARRAY) &&
                  ((yyvsp[(1) - (5)].ll_node)->type->entry.ar_decl.base_type->variant != T_STRING)) {
	         err("Can't take substring",45);
              }
              else {
  	        num_triplets = is_array_section_ref((yyvsp[(3) - (5)].ll_node));
                l = (yyvsp[(1) - (5)].ll_node)->entry.Template.ll_ptr2;
                if(l) {
                /* array element */
	        if (num_triplets == 0) {
                   l->entry.Template.ll_ptr1 = (yyvsp[(3) - (5)].ll_node);       	           
                   l->type = global_string;
                }
                /* array section */
                else {	
    	             l->entry.Template.ll_ptr1 = (yyvsp[(3) - (5)].ll_node);
	             l->type = make_type(fi, T_ARRAY);
                     l->type->entry.ar_decl.base_type = global_string;
	             l->type->entry.ar_decl.num_dimensions = num_triplets;
                }
	        q = make_llnd(fi, ARRAY_OP, l, (yyvsp[(5) - (5)].ll_node), SMNULL);
	        q->type = l->type;
                (yyval.ll_node)->entry.Template.ll_ptr2 = q;
                if((yyvsp[(1) - (5)].ll_node)->entry.Template.ll_ptr1->type->variant != T_ARRAY)  
                     (yyval.ll_node)->type = q->type;
               }
             }
          ;}
    break;

  case 389:
#line 3292 "gram1.y"
    { PTR_TYPE t;
	      PTR_SYMB  field;
	    /*  PTR_BFND at_scope;*/
              PTR_LLND l;


/*              if ($1->variant == T_POINTER) {
	         l = $1;
	         $1 = make_llnd(fi, DEREF_OP, l, LLNULL, SMNULL);
	         $1->type = l->type->entry.Template.base_type;
	      } */

	      t = (yyvsp[(1) - (3)].ll_node)->type; 
	      
	      if (( ( ((yyvsp[(1) - (3)].ll_node)->variant == VAR_REF) 
	          ||  ((yyvsp[(1) - (3)].ll_node)->variant == CONST_REF) 
                  ||  ((yyvsp[(1) - (3)].ll_node)->variant == ARRAY_REF)
                  ||  ((yyvsp[(1) - (3)].ll_node)->variant == RECORD_REF)) && (t->variant == T_DERIVED_TYPE)) 
	          ||((((yyvsp[(1) - (3)].ll_node)->variant == ARRAY_REF) || ((yyvsp[(1) - (3)].ll_node)->variant == RECORD_REF)) && (t->variant == T_ARRAY) &&
                      (t = t->entry.ar_decl.base_type) && (t->variant == T_DERIVED_TYPE))) 
                {
                 t->name = lookup_type_symbol(t->name);
	         if ((field = component(t->name, yytext))) {                   
	            l =  make_llnd(fi, VAR_REF, LLNULL, LLNULL, field);
                    l->type = field->type;
                    if(field->type->variant == T_ARRAY || field->type->variant == T_STRING)
                      l->variant = ARRAY_REF; 
                    (yyval.ll_node) = make_llnd(fi, RECORD_REF, (yyvsp[(1) - (3)].ll_node), l, SMNULL);
                    if((yyvsp[(1) - (3)].ll_node)->type->variant != T_ARRAY)
                       (yyval.ll_node)->type = field->type;
                    else {
                       (yyval.ll_node)->type = make_type(fi,T_ARRAY);
                       if(field->type->variant != T_ARRAY) 
	                 (yyval.ll_node)->type->entry.ar_decl.base_type = field->type;
                       else
                         (yyval.ll_node)->type->entry.ar_decl.base_type = field->type->entry.ar_decl.base_type;
	               (yyval.ll_node)->type->entry.ar_decl.num_dimensions = t->entry.ar_decl.num_dimensions;
                       }
                 }
                  else  
                    errstr("Illegal component  %s", yytext,311);
              }                     
               else 
                    errstr("Can't take component  %s", yytext,311);
             ;}
    break;

  case 390:
#line 3350 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 391:
#line 3352 "gram1.y"
    {(yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 392:
#line 3354 "gram1.y"
    {  int num_triplets;
               PTR_TYPE tp;
              /* PTR_LLND l;*/
	      if ((yyvsp[(1) - (5)].ll_node)->type->variant == T_ARRAY)
              {
  	         num_triplets = is_array_section_ref((yyvsp[(4) - (5)].ll_node));
	         /* array element */
	         if (num_triplets == 0) {
       	            (yyvsp[(1) - (5)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (5)].ll_node);
       	            (yyval.ll_node) = (yyvsp[(1) - (5)].ll_node);
                    (yyval.ll_node)->type = (yyvsp[(1) - (5)].ll_node)->type->entry.ar_decl.base_type;
                 }
                 /* substring */
	       /*  else if ((num_triplets == 1) && 
                          ($1->type->variant == T_STRING)) {
    	                  $1->entry.Template.ll_ptr1 = $4;
	                  $$ = $1; $$->type = global_string;
                 }   */ /*podd*/        
                 /* array section */
                 else {
    	             (yyvsp[(1) - (5)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (5)].ll_node);
	             (yyval.ll_node) = (yyvsp[(1) - (5)].ll_node); tp = make_type(fi, T_ARRAY);
                     tp->entry.ar_decl.base_type = (yyvsp[(1) - (5)].ll_node)->type->entry.ar_decl.base_type;  /**18.03.17* $1->type */
	             tp->entry.ar_decl.num_dimensions = num_triplets;
                     (yyval.ll_node)->type = tp;
                 }
             } 
             else err("can't subscript",44);

            /* if ($$->variant == T_POINTER) {
	        l = $$;
	        $$ = make_llnd(fi, DEREF_OP, l, LLNULL, SMNULL);
	        $$->type = l->type->entry.Template.base_type;
	     }
             */  /*11.02.03*/

            endioctl();
           ;}
    break;

  case 393:
#line 3394 "gram1.y"
    {  int num_triplets;
	      PTR_LLND l,l1,l2;

         /*   if ($1->variant == T_POINTER) {
	         l = $1;
	         $1 = make_llnd(fi, DEREF_OP, l, LLNULL, SMNULL);
	         $1->type = l->type->entry.Template.base_type;
	      } */

              num_triplets = is_array_section_ref((yyvsp[(3) - (4)].ll_node));
              (yyval.ll_node) = (yyvsp[(1) - (4)].ll_node);
              l2 = (yyvsp[(1) - (4)].ll_node)->entry.Template.ll_ptr2;  
              l1 = (yyvsp[(1) - (4)].ll_node)->entry.Template.ll_ptr1;                
              if(l2 && l2->type->variant == T_STRING)/*substring*/
                if(num_triplets == 1){
	           l = make_llnd(fi, ARRAY_OP, LLNULL, LLNULL, SMNULL);
    	           l->entry.Template.ll_ptr1 = l2;
       	           l->entry.Template.ll_ptr2 = (yyvsp[(3) - (4)].ll_node)->entry.Template.ll_ptr1;
	           l->type = global_string; 
                   (yyval.ll_node)->entry.Template.ll_ptr2 = l;                                          
                } else
                   err("Can't subscript",44);
              else if (l2 && l2->type->variant == T_ARRAY) {
                 if(num_triplets > 0) { /*array section*/
                   (yyval.ll_node)->type = make_type(fi,T_ARRAY);
                   (yyval.ll_node)->type->entry.ar_decl.base_type = l2->type->entry.ar_decl.base_type;
                   (yyval.ll_node)->type->entry.ar_decl.num_dimensions = num_triplets;
                   l2->entry.Template.ll_ptr1 = (yyvsp[(3) - (4)].ll_node);
                   l2->type = (yyval.ll_node)->type;   
                  }                 
                 else {  /*array element*/
                   l2->type = l2->type->entry.ar_decl.base_type;
                   l2->entry.Template.ll_ptr1 = (yyvsp[(3) - (4)].ll_node);   
                   if(l1->type->variant != T_ARRAY)  
                     (yyval.ll_node)->type = l2->type;
                 }
              } else 
                   err("Can't subscript",44);
         ;}
    break;

  case 394:
#line 3436 "gram1.y"
    { 
	      if ((yyvsp[(1) - (2)].ll_node)->type->variant == T_STRING) {
                 (yyvsp[(1) - (2)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(2) - (2)].ll_node);
                 (yyval.ll_node) = (yyvsp[(1) - (2)].ll_node); (yyval.ll_node)->type = global_string;
              }
              else errstr("can't subscript of %s", (yyvsp[(1) - (2)].ll_node)->entry.Template.symbol->ident,44);
            ;}
    break;

  case 395:
#line 3446 "gram1.y"
    { (yyval.ll_node) = LLNULL; ;}
    break;

  case 396:
#line 3448 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 397:
#line 3452 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, DDOT, (yyvsp[(2) - (5)].ll_node), (yyvsp[(4) - (5)].ll_node), SMNULL); ;}
    break;

  case 398:
#line 3456 "gram1.y"
    { (yyval.ll_node) = LLNULL; ;}
    break;

  case 399:
#line 3458 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 400:
#line 3462 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 401:
#line 3464 "gram1.y"
    { PTR_TYPE t;
               t = make_type_node((yyvsp[(1) - (3)].ll_node)->type, (yyvsp[(3) - (3)].ll_node));
               (yyval.ll_node) = (yyvsp[(1) - (3)].ll_node);
               (yyval.ll_node)->type = t;
             ;}
    break;

  case 402:
#line 3470 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 403:
#line 3472 "gram1.y"
    { PTR_TYPE t;
               t = make_type_node((yyvsp[(1) - (3)].ll_node)->type, (yyvsp[(3) - (3)].ll_node));
               (yyval.ll_node) = (yyvsp[(1) - (3)].ll_node);
               (yyval.ll_node)->type = t;
             ;}
    break;

  case 404:
#line 3478 "gram1.y"
    {
              if ((yyvsp[(2) - (2)].ll_node) != LLNULL)
		 (yyval.ll_node) = make_llnd(fi, ARRAY_OP, (yyvsp[(1) - (2)].ll_node), (yyvsp[(2) - (2)].ll_node), SMNULL); 
	      else 
                 (yyval.ll_node) = (yyvsp[(1) - (2)].ll_node);
             ;}
    break;

  case 405:
#line 3488 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,BOOL_VAL, LLNULL, LLNULL, SMNULL);
	      (yyval.ll_node)->entry.bval = 1;
	      (yyval.ll_node)->type = global_bool;
	    ;}
    break;

  case 406:
#line 3494 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,BOOL_VAL, LLNULL, LLNULL, SMNULL);
	      (yyval.ll_node)->entry.bval = 0;
	      (yyval.ll_node)->type = global_bool;
	    ;}
    break;

  case 407:
#line 3501 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,FLOAT_VAL, LLNULL, LLNULL, SMNULL);
	      (yyval.ll_node)->entry.string_val = copys(yytext);
	      (yyval.ll_node)->type = global_float;
	    ;}
    break;

  case 408:
#line 3507 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,DOUBLE_VAL, LLNULL, LLNULL, SMNULL);
	      (yyval.ll_node)->entry.string_val = copys(yytext);
	      (yyval.ll_node)->type = global_double;
	    ;}
    break;

  case 409:
#line 3515 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,INT_VAL, LLNULL, LLNULL, SMNULL);
	      (yyval.ll_node)->entry.ival = atoi(yytext);
	      (yyval.ll_node)->type = global_int;
	    ;}
    break;

  case 410:
#line 3523 "gram1.y"
    { PTR_TYPE t;
	      PTR_LLND p,q;
	      (yyval.ll_node) = make_llnd(fi,STRING_VAL, LLNULL, LLNULL, SMNULL);
	      (yyval.ll_node)->entry.string_val = copys(yytext);
              if(yyquote=='\"') 
	        t = global_string_2;
              else
	        t = global_string;

	      p = make_llnd(fi,INT_VAL, LLNULL, LLNULL, SMNULL);
	      p->entry.ival = yyleng;
	      p->type = global_int;
              q = make_llnd(fi, LEN_OP, p, LLNULL, SMNULL); 
              (yyval.ll_node)->type = make_type_node(t, q);
	    ;}
    break;

  case 411:
#line 3539 "gram1.y"
    { PTR_TYPE t;
	      (yyval.ll_node) = make_llnd(fi,STRING_VAL, LLNULL, LLNULL, SMNULL);
	      (yyval.ll_node)->entry.string_val = copys(yytext);
              if(yyquote=='\"') 
	        t = global_string_2;
              else
	        t = global_string;
	      (yyval.ll_node)->type = make_type_node(t, (yyvsp[(1) - (3)].ll_node));
            ;}
    break;

  case 412:
#line 3549 "gram1.y"
    { PTR_TYPE t;
	      (yyval.ll_node) = make_llnd(fi,STRING_VAL, LLNULL, LLNULL, SMNULL);
	      (yyval.ll_node)->entry.string_val = copys(yytext);
              if(yyquote=='\"') 
	        t = global_string_2;
              else
	        t = global_string;
	      (yyval.ll_node)->type = make_type_node(t, (yyvsp[(1) - (3)].ll_node));
            ;}
    break;

  case 413:
#line 3562 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,COMPLEX_VAL, (yyvsp[(2) - (5)].ll_node), (yyvsp[(4) - (5)].ll_node), SMNULL);
	      (yyval.ll_node)->type = global_complex;
	    ;}
    break;

  case 414:
#line 3569 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 415:
#line 3571 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 416:
#line 3594 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,(yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),SMNULL); ;}
    break;

  case 417:
#line 3596 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,(yyvsp[(1) - (2)].ll_node),LLNULL,SMNULL); ;}
    break;

  case 418:
#line 3598 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,make_llnd(fi,DDOT,(yyvsp[(1) - (5)].ll_node),(yyvsp[(3) - (5)].ll_node),SMNULL),(yyvsp[(5) - (5)].ll_node),SMNULL); ;}
    break;

  case 419:
#line 3600 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,make_llnd(fi,DDOT,(yyvsp[(1) - (4)].ll_node),LLNULL,SMNULL),(yyvsp[(4) - (4)].ll_node),SMNULL); ;}
    break;

  case 420:
#line 3602 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT, make_llnd(fi,DDOT,LLNULL,(yyvsp[(2) - (4)].ll_node),SMNULL),(yyvsp[(4) - (4)].ll_node),SMNULL); ;}
    break;

  case 421:
#line 3604 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,make_llnd(fi,DDOT,LLNULL,LLNULL,SMNULL),(yyvsp[(3) - (3)].ll_node),SMNULL); ;}
    break;

  case 422:
#line 3606 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,LLNULL,(yyvsp[(2) - (2)].ll_node),SMNULL); ;}
    break;

  case 423:
#line 3608 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,LLNULL,LLNULL,SMNULL); ;}
    break;

  case 424:
#line 3611 "gram1.y"
    {in_vec=YES;;}
    break;

  case 425:
#line 3611 "gram1.y"
    {in_vec=NO;;}
    break;

  case 426:
#line 3612 "gram1.y"
    { PTR_TYPE array_type;
             (yyval.ll_node) = make_llnd (fi,CONSTRUCTOR_REF,(yyvsp[(4) - (6)].ll_node),LLNULL,SMNULL); 
             /*$$->type = $2->type;*/ /*28.02.03*/
             array_type = make_type(fi, T_ARRAY);
	     array_type->entry.ar_decl.num_dimensions = 1;
             if((yyvsp[(4) - (6)].ll_node)->type->variant == T_ARRAY)
	       array_type->entry.ar_decl.base_type = (yyvsp[(4) - (6)].ll_node)->type->entry.ar_decl.base_type;
             else
               array_type->entry.ar_decl.base_type = (yyvsp[(4) - (6)].ll_node)->type;
             (yyval.ll_node)->type = array_type;
           ;}
    break;

  case 427:
#line 3626 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 428:
#line 3628 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 429:
#line 3651 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 430:
#line 3653 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (4)].ll_node), (yyvsp[(4) - (4)].ll_node), EXPR_LIST); endioctl(); ;}
    break;

  case 431:
#line 3655 "gram1.y"
    { stat_alloc = make_llnd(fi, SPEC_PAIR, (yyvsp[(4) - (5)].ll_node), (yyvsp[(5) - (5)].ll_node), SMNULL);
                  endioctl();
                ;}
    break;

  case 432:
#line 3671 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 433:
#line 3673 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (4)].ll_node), (yyvsp[(4) - (4)].ll_node), EXPR_LIST); endioctl(); ;}
    break;

  case 434:
#line 3675 "gram1.y"
    { stat_alloc = make_llnd(fi, SPEC_PAIR, (yyvsp[(4) - (5)].ll_node), (yyvsp[(5) - (5)].ll_node), SMNULL);
             endioctl();
           ;}
    break;

  case 435:
#line 3688 "gram1.y"
    {stat_alloc = LLNULL;;}
    break;

  case 436:
#line 3692 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 437:
#line 3694 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); ;}
    break;

  case 438:
#line 3702 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(1) - (1)].bf_node); ;}
    break;

  case 439:
#line 3704 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(1) - (1)].bf_node); ;}
    break;

  case 440:
#line 3706 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(1) - (1)].bf_node); ;}
    break;

  case 441:
#line 3708 "gram1.y"
    {
              (yyval.bf_node) = (yyvsp[(2) - (2)].bf_node);
              (yyval.bf_node)->entry.Template.ll_ptr3 = (yyvsp[(1) - (2)].ll_node);
            ;}
    break;

  case 442:
#line 3762 "gram1.y"
    { PTR_BFND biff;

	      (yyval.bf_node) = get_bfnd(fi,CONTROL_END, SMNULL, LLNULL, LLNULL, LLNULL); 
	      bind(); 
	      biff = cur_scope();
	      if ((biff->variant == FUNC_HEDR) || (biff->variant == PROC_HEDR)
		  || (biff->variant == PROS_HEDR) 
	          || (biff->variant == PROG_HEDR)
                  || (biff->variant == BLOCK_DATA)) {
                if(biff->control_parent == global_bfnd) position = IN_OUTSIDE;
		else if(!is_interface_stat(biff->control_parent)) position++;
              } else if  (biff->variant == MODULE_STMT)
                position = IN_OUTSIDE;
	      else err("Unexpected END statement read", 52);
             /* FB ADDED set the control parent so the empty function unparse right*/
              if ((yyval.bf_node))
                (yyval.bf_node)->control_parent = biff;
              delete_beyond_scope_level(pred_bfnd);
            ;}
    break;

  case 443:
#line 3784 "gram1.y"
    {
              make_extend((yyvsp[(3) - (3)].symbol));
              (yyval.bf_node) = BFNULL; 
              /* delete_beyond_scope_level(pred_bfnd); */
             ;}
    break;

  case 444:
#line 3797 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,CONTROL_END, SMNULL, LLNULL, LLNULL, LLNULL); 
	    bind(); 
	    delete_beyond_scope_level(pred_bfnd);
	    position = IN_OUTSIDE;
          ;}
    break;

  case 445:
#line 3806 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(1) - (1)].bf_node); ;}
    break;

  case 446:
#line 3809 "gram1.y"
    {
              (yyval.bf_node) = (yyvsp[(2) - (2)].bf_node);
              (yyval.bf_node)->entry.Template.ll_ptr3 = (yyvsp[(1) - (2)].ll_node);
            ;}
    break;

  case 447:
#line 3859 "gram1.y"
    { thiswasbranch = NO;
              (yyvsp[(1) - (2)].bf_node)->variant = LOGIF_NODE;
              (yyval.bf_node) = make_logif((yyvsp[(1) - (2)].bf_node), (yyvsp[(2) - (2)].bf_node));
	      set_blobs((yyvsp[(1) - (2)].bf_node), pred_bfnd, SAME_GROUP);
	    ;}
    break;

  case 448:
#line 3865 "gram1.y"
    {
              (yyval.bf_node) = (yyvsp[(1) - (2)].bf_node);
	      set_blobs((yyval.bf_node), pred_bfnd, NEW_GROUP1); 
            ;}
    break;

  case 449:
#line 3870 "gram1.y"
    {
              (yyval.bf_node) = (yyvsp[(2) - (3)].bf_node);
              (yyval.bf_node)->entry.Template.ll_ptr3 = (yyvsp[(1) - (3)].ll_node);
	      set_blobs((yyval.bf_node), pred_bfnd, NEW_GROUP1); 
            ;}
    break;

  case 450:
#line 3888 "gram1.y"
    { make_elseif((yyvsp[(4) - (7)].ll_node),(yyvsp[(7) - (7)].symbol)); lastwasbranch = NO; (yyval.bf_node) = BFNULL;;}
    break;

  case 451:
#line 3890 "gram1.y"
    { make_else((yyvsp[(3) - (3)].symbol)); lastwasbranch = NO; (yyval.bf_node) = BFNULL; ;}
    break;

  case 452:
#line 3892 "gram1.y"
    { make_endif((yyvsp[(3) - (3)].symbol)); (yyval.bf_node) = BFNULL; ;}
    break;

  case 453:
#line 3894 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(1) - (1)].bf_node); ;}
    break;

  case 454:
#line 3896 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, CONTAINS_STMT, SMNULL, LLNULL, LLNULL, LLNULL); ;}
    break;

  case 455:
#line 3899 "gram1.y"
    { thiswasbranch = NO;
              (yyvsp[(1) - (2)].bf_node)->variant = FORALL_STAT;
              (yyval.bf_node) = make_logif((yyvsp[(1) - (2)].bf_node), (yyvsp[(2) - (2)].bf_node));
	      set_blobs((yyvsp[(1) - (2)].bf_node), pred_bfnd, SAME_GROUP);
	    ;}
    break;

  case 456:
#line 3905 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(1) - (1)].bf_node); ;}
    break;

  case 457:
#line 3907 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(2) - (2)].bf_node); (yyval.bf_node)->entry.Template.ll_ptr3 = (yyvsp[(1) - (2)].ll_node);;}
    break;

  case 458:
#line 3909 "gram1.y"
    { make_endforall((yyvsp[(3) - (3)].symbol)); (yyval.bf_node) = BFNULL; ;}
    break;

  case 459:
#line 3912 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(1) - (1)].bf_node); ;}
    break;

  case 460:
#line 3914 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(1) - (1)].bf_node); ;}
    break;

  case 461:
#line 3916 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(1) - (1)].bf_node); ;}
    break;

  case 462:
#line 3943 "gram1.y"
    { 	     
	     /*  if($5 && $5->labdefined)
		 execerr("no backward DO loops", (char *)NULL); */
	       (yyval.bf_node) = make_do(WHILE_NODE, LBNULL, SMNULL, (yyvsp[(4) - (5)].ll_node), LLNULL, LLNULL);
	       /*$$->entry.Template.ll_ptr3 = $1;*/	     
           ;}
    break;

  case 463:
#line 3952 "gram1.y"
    {
               if( (yyvsp[(4) - (7)].label) && (yyvsp[(4) - (7)].label)->labdefined)
		  err("No backward DO loops", 46);
	        (yyval.bf_node) = make_do(WHILE_NODE, (yyvsp[(4) - (7)].label), SMNULL, (yyvsp[(7) - (7)].ll_node), LLNULL, LLNULL);            
	    ;}
    break;

  case 464:
#line 3960 "gram1.y"
    { (yyval.ll_node) = LLNULL; ;}
    break;

  case 465:
#line 3962 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(5) - (6)].ll_node);;}
    break;

  case 466:
#line 3964 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(3) - (4)].ll_node);;}
    break;

  case 467:
#line 3969 "gram1.y"
    {  
               if( (yyvsp[(4) - (11)].label) && (yyvsp[(4) - (11)].label)->labdefined)
		  err("No backward DO loops", 46);
	        (yyval.bf_node) = make_do(FOR_NODE, (yyvsp[(4) - (11)].label), (yyvsp[(7) - (11)].symbol), (yyvsp[(9) - (11)].ll_node), (yyvsp[(11) - (11)].ll_node), LLNULL);            
	    ;}
    break;

  case 468:
#line 3976 "gram1.y"
    {
               if( (yyvsp[(4) - (13)].label) && (yyvsp[(4) - (13)].label)->labdefined)
		  err("No backward DO loops", 46);
	        (yyval.bf_node) = make_do(FOR_NODE, (yyvsp[(4) - (13)].label), (yyvsp[(7) - (13)].symbol), (yyvsp[(9) - (13)].ll_node), (yyvsp[(11) - (13)].ll_node), (yyvsp[(13) - (13)].ll_node));            
	    ;}
    break;

  case 469:
#line 3984 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, CASE_NODE, (yyvsp[(4) - (4)].symbol), (yyvsp[(3) - (4)].ll_node), LLNULL, LLNULL); ;}
    break;

  case 470:
#line 3986 "gram1.y"
    { /*PTR_LLND p;*/
	     /* p = make_llnd(fi, DEFAULT, LLNULL, LLNULL, SMNULL); */
	      (yyval.bf_node) = get_bfnd(fi, DEFAULT_NODE, (yyvsp[(3) - (3)].symbol), LLNULL, LLNULL, LLNULL); ;}
    break;

  case 471:
#line 3990 "gram1.y"
    { make_endselect((yyvsp[(3) - (3)].symbol)); (yyval.bf_node) = BFNULL; ;}
    break;

  case 472:
#line 3993 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, SWITCH_NODE, SMNULL, (yyvsp[(6) - (7)].ll_node), LLNULL, LLNULL) ; ;}
    break;

  case 473:
#line 3995 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, SWITCH_NODE, SMNULL, (yyvsp[(7) - (8)].ll_node), LLNULL, (yyvsp[(1) - (8)].ll_node)) ; ;}
    break;

  case 474:
#line 3999 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(2) - (3)].ll_node); ;}
    break;

  case 475:
#line 4005 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 476:
#line 4007 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, DDOT, (yyvsp[(1) - (2)].ll_node), LLNULL, SMNULL); ;}
    break;

  case 477:
#line 4009 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, DDOT, LLNULL, (yyvsp[(2) - (2)].ll_node), SMNULL); ;}
    break;

  case 478:
#line 4011 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, DDOT, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), SMNULL); ;}
    break;

  case 479:
#line 4015 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, EXPR_LIST, (yyvsp[(1) - (1)].ll_node), LLNULL, SMNULL); ;}
    break;

  case 480:
#line 4017 "gram1.y"
    { PTR_LLND p;
	      
	      p = make_llnd(fi, EXPR_LIST, (yyvsp[(3) - (3)].ll_node), LLNULL, SMNULL);
	      add_to_lowLevelList(p, (yyvsp[(1) - (3)].ll_node));
	    ;}
    break;

  case 481:
#line 4025 "gram1.y"
    { (yyval.symbol) = SMNULL; ;}
    break;

  case 482:
#line 4027 "gram1.y"
    { (yyval.symbol) = make_local_entity((yyvsp[(1) - (1)].hash_entry), CONSTRUCT_NAME, global_default,
                                     LOCAL); ;}
    break;

  case 483:
#line 4033 "gram1.y"
    {(yyval.hash_entry) = HSNULL;;}
    break;

  case 484:
#line 4035 "gram1.y"
    { (yyval.hash_entry) = (yyvsp[(1) - (1)].hash_entry);;}
    break;

  case 485:
#line 4039 "gram1.y"
    {(yyval.hash_entry) = look_up_sym(yytext);;}
    break;

  case 486:
#line 4043 "gram1.y"
    { PTR_SYMB s;
	             s = make_local_entity( (yyvsp[(1) - (2)].hash_entry), CONSTRUCT_NAME, global_default, LOCAL);             
                    (yyval.ll_node) = make_llnd(fi, VAR_REF, LLNULL, LLNULL, s);
                   ;}
    break;

  case 487:
#line 4064 "gram1.y"
    { (yyval.bf_node) = make_if((yyvsp[(4) - (5)].ll_node)); ;}
    break;

  case 488:
#line 4067 "gram1.y"
    { (yyval.bf_node) = make_forall((yyvsp[(4) - (6)].ll_node),(yyvsp[(5) - (6)].ll_node)); ;}
    break;

  case 489:
#line 4071 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, EXPR_LIST, (yyvsp[(1) - (1)].ll_node), LLNULL, SMNULL); ;}
    break;

  case 490:
#line 4073 "gram1.y"
    { PTR_LLND p;	      
	      p = make_llnd(fi, EXPR_LIST, (yyvsp[(3) - (3)].ll_node), LLNULL, SMNULL);
	      add_to_lowLevelList(p, (yyvsp[(1) - (3)].ll_node));
	    ;}
    break;

  case 491:
#line 4080 "gram1.y"
    {(yyval.ll_node) = make_llnd(fi, FORALL_OP, (yyvsp[(3) - (3)].ll_node), LLNULL, (yyvsp[(1) - (3)].symbol)); ;}
    break;

  case 492:
#line 4084 "gram1.y"
    { (yyval.ll_node)=LLNULL;;}
    break;

  case 493:
#line 4086 "gram1.y"
    { (yyval.ll_node)=(yyvsp[(2) - (2)].ll_node);;}
    break;

  case 494:
#line 4102 "gram1.y"
    { PTR_SYMB  s;
      	      if (!(s = (yyvsp[(1) - (1)].hash_entry)->id_attr))
              {
	         s = make_scalar((yyvsp[(1) - (1)].hash_entry), TYNULL, LOCAL);
	     	 s->decl = SOFT;
	      }
              (yyval.symbol) = s; 
	 ;}
    break;

  case 495:
#line 4115 "gram1.y"
    { PTR_SYMB s;
              PTR_LLND l;
              int vrnt;

            /*  s = make_scalar($1, TYNULL, LOCAL);*/ /*16.02.03*/
              s = (yyvsp[(1) - (5)].symbol);
	      if (s->variant != CONST_NAME) {
                if(in_vec) 
                   vrnt=SEQ;
                else
                   vrnt=DDOT;     
                l = make_llnd(fi, SEQ, make_llnd(fi, vrnt, (yyvsp[(3) - (5)].ll_node), (yyvsp[(5) - (5)].ll_node), SMNULL),
                              LLNULL, SMNULL);
		(yyval.ll_node) = make_llnd(fi,IOACCESS, LLNULL, l, s);
		do_name_err = NO;
	      }
	      else {
		err("Symbolic constant not allowed as DO variable", 47);
		do_name_err = YES;
	      }
	    ;}
    break;

  case 496:
#line 4138 "gram1.y"
    { PTR_SYMB s;
              PTR_LLND l;
              int vrnt;
              /*s = make_scalar($1, TYNULL, LOCAL);*/ /*16.02.03*/
              s = (yyvsp[(1) - (7)].symbol);
	      if( s->variant != CONST_NAME ) {
                if(in_vec) 
                   vrnt=SEQ;
                else
                   vrnt=DDOT;     
                l = make_llnd(fi, SEQ, make_llnd(fi, vrnt, (yyvsp[(3) - (7)].ll_node), (yyvsp[(5) - (7)].ll_node), SMNULL), (yyvsp[(7) - (7)].ll_node),
                              SMNULL);
		(yyval.ll_node) = make_llnd(fi,IOACCESS, LLNULL, l, s);
		do_name_err = NO;
	      }
	      else {
		err("Symbolic constant not allowed as DO variable", 47);
		do_name_err = YES;
	      }
	    ;}
    break;

  case 497:
#line 4161 "gram1.y"
    { (yyval.label) = LBNULL; ;}
    break;

  case 498:
#line 4163 "gram1.y"
    {
	       (yyval.label)  = make_label_node(fi,convci(yyleng, yytext));
	       (yyval.label)->scope = cur_scope();
	    ;}
    break;

  case 499:
#line 4170 "gram1.y"
    { make_endwhere((yyvsp[(3) - (3)].symbol)); (yyval.bf_node) = BFNULL; ;}
    break;

  case 500:
#line 4172 "gram1.y"
    { make_elsewhere((yyvsp[(3) - (3)].symbol)); lastwasbranch = NO; (yyval.bf_node) = BFNULL; ;}
    break;

  case 501:
#line 4174 "gram1.y"
    { make_elsewhere_mask((yyvsp[(4) - (6)].ll_node),(yyvsp[(6) - (6)].symbol)); lastwasbranch = NO; (yyval.bf_node) = BFNULL; ;}
    break;

  case 502:
#line 4176 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, WHERE_BLOCK_STMT, SMNULL, (yyvsp[(4) - (5)].ll_node), LLNULL, LLNULL); ;}
    break;

  case 503:
#line 4178 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, WHERE_BLOCK_STMT, SMNULL, (yyvsp[(5) - (6)].ll_node), LLNULL, (yyvsp[(1) - (6)].ll_node)); ;}
    break;

  case 504:
#line 4183 "gram1.y"
    { PTR_LLND p, r;
             PTR_SYMB s1, s2 = SMNULL, s3, arg_list;
	     PTR_HASH hash_entry;

	   /*  if (just_look_up_sym("=") != HSNULL) {
	        p = intrinsic_op_node("=", EQUAL, $2, $4);
   	        $$ = get_bfnd(fi, OVERLOADED_ASSIGN_STAT, SMNULL, p, $2, $4);
             }	      
             else */ if ((yyvsp[(2) - (4)].ll_node)->variant == FUNC_CALL) {
                if(parstate==INEXEC){
                  	  err("Declaration among executables", 30);
                 /*   $$=BFNULL;*/
 	         (yyval.bf_node) = get_bfnd(fi,STMTFN_STAT, SMNULL, (yyvsp[(2) - (4)].ll_node), LLNULL, LLNULL);
                } 
                else {	         
  	         (yyvsp[(2) - (4)].ll_node)->variant = STMTFN_DECL;
		 /* $2->entry.Template.ll_ptr2 = $4; */
                 if( (yyvsp[(2) - (4)].ll_node)->entry.Template.ll_ptr1) {
		   r = (yyvsp[(2) - (4)].ll_node)->entry.Template.ll_ptr1->entry.Template.ll_ptr1;
                   if(r->variant != VAR_REF && r->variant != ARRAY_REF){
                     err("A dummy argument of a statement function must be a scalar identifier", 333);
                     s1 = SMNULL;
                   }
                   else                       
		     s1 = r ->entry.Template.symbol;
                 } else
                   s1 = SMNULL;
		 if (s1)
	            s1->scope = cur_scope();
 	         (yyval.bf_node) = get_bfnd(fi,STMTFN_STAT, SMNULL, (yyvsp[(2) - (4)].ll_node), LLNULL, LLNULL);
	         add_scope_level((yyval.bf_node), NO);
                 arg_list = SMNULL;
		 if (s1) 
                 {
	            /*arg_list = SMNULL;*/
                    p = (yyvsp[(2) - (4)].ll_node)->entry.Template.ll_ptr1;
                    while (p != LLNULL)
                    {
		    /*   if (p->entry.Template.ll_ptr1->variant != VAR_REF) {
			  errstr("cftn.gram:1: illegal statement function %s.", $2->entry.Template.symbol->ident);
			  break;
		       } 
                    */
                       r = p->entry.Template.ll_ptr1;
                       if(r->variant != VAR_REF && r->variant != ARRAY_REF){
                         err("A dummy argument of a statement function must be a scalar identifier", 333);
                         break;
                       }
	               hash_entry = look_up_sym(r->entry.Template.symbol->parent->ident);
	               s3 = make_scalar(hash_entry, s1->type, IO);
                       replace_symbol_in_expr(s3,(yyvsp[(4) - (4)].ll_node));
	               if (arg_list == SMNULL) 
                          s2 = arg_list = s3;
             	       else 
                       {
                          s2->id_list = s3;
                          s2 = s3;
                       }
                       p = p->entry.Template.ll_ptr2;
                    }
                 }
  		    (yyvsp[(2) - (4)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (4)].ll_node);
		    install_param_list((yyvsp[(2) - (4)].ll_node)->entry.Template.symbol,
				       arg_list, LLNULL, FUNCTION_NAME);
	            delete_beyond_scope_level((yyval.bf_node));
		 
		/* else
		    errstr("cftn.gram: Illegal statement function declaration %s.", $2->entry.Template.symbol->ident); */
               }
	     }
	     else {
		(yyval.bf_node) = get_bfnd(fi,ASSIGN_STAT,SMNULL, (yyvsp[(2) - (4)].ll_node), (yyvsp[(4) - (4)].ll_node), LLNULL);
                 parstate = INEXEC;
             }
	  ;}
    break;

  case 505:
#line 4259 "gram1.y"
    { /*PTR_SYMB s;*/
	
	      /*s = make_scalar($2, TYNULL, LOCAL);*/
  	      (yyval.bf_node) = get_bfnd(fi, POINTER_ASSIGN_STAT, SMNULL, (yyvsp[(3) - (5)].ll_node), (yyvsp[(5) - (5)].ll_node), LLNULL);
	    ;}
    break;

  case 506:
#line 4271 "gram1.y"
    { PTR_SYMB p;

	      p = make_scalar((yyvsp[(5) - (5)].hash_entry), TYNULL, LOCAL);
	      p->variant = LABEL_VAR;
  	      (yyval.bf_node) = get_bfnd(fi,ASSLAB_STAT, p, (yyvsp[(3) - (5)].ll_node),LLNULL,LLNULL);
            ;}
    break;

  case 507:
#line 4278 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,CONT_STAT,SMNULL,LLNULL,LLNULL,LLNULL); ;}
    break;

  case 509:
#line 4281 "gram1.y"
    { inioctl = NO; ;}
    break;

  case 510:
#line 4283 "gram1.y"
    { PTR_LLND	p;

	      p = make_llnd(fi,EXPR_LIST, (yyvsp[(10) - (10)].ll_node), LLNULL, SMNULL);
	      p = make_llnd(fi,EXPR_LIST, (yyvsp[(8) - (10)].ll_node), p, SMNULL);
	      (yyval.bf_node)= get_bfnd(fi,ARITHIF_NODE, SMNULL, (yyvsp[(4) - (10)].ll_node),
			    make_llnd(fi,EXPR_LIST, (yyvsp[(6) - (10)].ll_node), p, SMNULL), LLNULL);
	      thiswasbranch = YES;
            ;}
    break;

  case 511:
#line 4292 "gram1.y"
    {
	      (yyval.bf_node) = subroutine_call((yyvsp[(1) - (1)].symbol), LLNULL, LLNULL, PLAIN);
/*	      match_parameters($1, LLNULL);
	      $$= get_bfnd(fi,PROC_STAT, $1, LLNULL, LLNULL, LLNULL);
*/	      endioctl(); 
            ;}
    break;

  case 512:
#line 4299 "gram1.y"
    {
	      (yyval.bf_node) = subroutine_call((yyvsp[(1) - (3)].symbol), LLNULL, LLNULL, PLAIN);
/*	      match_parameters($1, LLNULL);
	      $$= get_bfnd(fi,PROC_STAT,$1,LLNULL,LLNULL,LLNULL);
*/	      endioctl(); 
	    ;}
    break;

  case 513:
#line 4306 "gram1.y"
    {
	      (yyval.bf_node) = subroutine_call((yyvsp[(1) - (4)].symbol), (yyvsp[(3) - (4)].ll_node), LLNULL, PLAIN);
/*	      match_parameters($1, $3);
	      $$= get_bfnd(fi,PROC_STAT,$1,$3,LLNULL,LLNULL);
*/	      endioctl(); 
	    ;}
    break;

  case 514:
#line 4314 "gram1.y"
    {
	      (yyval.bf_node) = get_bfnd(fi,RETURN_STAT,SMNULL,(yyvsp[(3) - (3)].ll_node),LLNULL,LLNULL);
	      thiswasbranch = YES;
	    ;}
    break;

  case 515:
#line 4319 "gram1.y"
    {
	      (yyval.bf_node) = get_bfnd(fi,(yyvsp[(1) - (3)].token),SMNULL,(yyvsp[(3) - (3)].ll_node),LLNULL,LLNULL);
	      thiswasbranch = ((yyvsp[(1) - (3)].token) == STOP_STAT);
	    ;}
    break;

  case 516:
#line 4324 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, CYCLE_STMT, (yyvsp[(3) - (3)].symbol), LLNULL, LLNULL, LLNULL); ;}
    break;

  case 517:
#line 4327 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, EXIT_STMT, (yyvsp[(3) - (3)].symbol), LLNULL, LLNULL, LLNULL); ;}
    break;

  case 518:
#line 4330 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, ALLOCATE_STMT,  SMNULL, (yyvsp[(5) - (6)].ll_node), stat_alloc, LLNULL); ;}
    break;

  case 519:
#line 4333 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, DEALLOCATE_STMT, SMNULL, (yyvsp[(5) - (6)].ll_node), stat_alloc , LLNULL); ;}
    break;

  case 520:
#line 4336 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, NULLIFY_STMT, SMNULL, (yyvsp[(4) - (5)].ll_node), LLNULL, LLNULL); ;}
    break;

  case 521:
#line 4339 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi, WHERE_NODE, SMNULL, (yyvsp[(4) - (8)].ll_node), (yyvsp[(6) - (8)].ll_node), (yyvsp[(8) - (8)].ll_node)); ;}
    break;

  case 522:
#line 4357 "gram1.y"
    {(yyval.ll_node) = LLNULL;;}
    break;

  case 523:
#line 4361 "gram1.y"
    {
	      (yyval.bf_node)=get_bfnd(fi,GOTO_NODE,SMNULL,LLNULL,LLNULL,(PTR_LLND)(yyvsp[(3) - (3)].ll_node));
	      thiswasbranch = YES;
	    ;}
    break;

  case 524:
#line 4366 "gram1.y"
    { PTR_SYMB p;

	      if((yyvsp[(3) - (3)].hash_entry)->id_attr)
		p = (yyvsp[(3) - (3)].hash_entry)->id_attr;
	      else {
	        p = make_scalar((yyvsp[(3) - (3)].hash_entry), TYNULL, LOCAL);
		p->variant = LABEL_VAR;
	      }

	      if(p->variant == LABEL_VAR) {
		  (yyval.bf_node) = get_bfnd(fi,ASSGOTO_NODE,p,LLNULL,LLNULL,LLNULL);
		  thiswasbranch = YES;
	      }
	      else {
		  err("Must go to assigned variable", 48);
		  (yyval.bf_node) = BFNULL;
	      }
	    ;}
    break;

  case 525:
#line 4385 "gram1.y"
    { PTR_SYMB p;

	      if((yyvsp[(3) - (7)].hash_entry)->id_attr)
		p = (yyvsp[(3) - (7)].hash_entry)->id_attr;
	      else {
	        p = make_scalar((yyvsp[(3) - (7)].hash_entry), TYNULL, LOCAL);
		p->variant = LABEL_VAR;
	      }

	      if (p->variant == LABEL_VAR) {
		 (yyval.bf_node) = get_bfnd(fi,ASSGOTO_NODE,p,(yyvsp[(6) - (7)].ll_node),LLNULL,LLNULL);
		 thiswasbranch = YES;
	      }
	      else {
		err("Must go to assigned variable",48);
		(yyval.bf_node) = BFNULL;
	      }
	    ;}
    break;

  case 526:
#line 4404 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,COMGOTO_NODE, SMNULL, (yyvsp[(4) - (7)].ll_node), (yyvsp[(7) - (7)].ll_node), LLNULL); ;}
    break;

  case 529:
#line 4412 "gram1.y"
    { (yyval.symbol) = make_procedure((yyvsp[(3) - (4)].hash_entry), YES); ;}
    break;

  case 530:
#line 4416 "gram1.y"
    { 
              (yyval.ll_node) = set_ll_list((yyvsp[(2) - (2)].ll_node), LLNULL, EXPR_LIST);
              endioctl();
            ;}
    break;

  case 531:
#line 4421 "gram1.y"
    { 
               (yyval.ll_node) = set_ll_list((yyvsp[(1) - (4)].ll_node), (yyvsp[(4) - (4)].ll_node), EXPR_LIST);
               endioctl();
            ;}
    break;

  case 532:
#line 4428 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 533:
#line 4430 "gram1.y"
    { (yyval.ll_node)  = make_llnd(fi, KEYWORD_ARG, (yyvsp[(1) - (2)].ll_node), (yyvsp[(2) - (2)].ll_node), SMNULL); ;}
    break;

  case 534:
#line 4432 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,LABEL_ARG,(yyvsp[(2) - (2)].ll_node),LLNULL,SMNULL); ;}
    break;

  case 535:
#line 4435 "gram1.y"
    { (yyval.token) = PAUSE_NODE; ;}
    break;

  case 536:
#line 4436 "gram1.y"
    { (yyval.token) = STOP_STAT; ;}
    break;

  case 537:
#line 4447 "gram1.y"
    { if(parstate == OUTSIDE)
		{ PTR_BFND p;

		  p = get_bfnd(fi,PROG_HEDR, make_program(look_up_sym("_MAIN")), LLNULL, LLNULL, LLNULL);
		  set_blobs(p, global_bfnd, NEW_GROUP1);
		  add_scope_level(p, NO);
		  position = IN_PROC; 
		}
		if(parstate < INDATA) enddcl();
		parstate = INEXEC;
		yystno = 0;
	      ;}
    break;

  case 538:
#line 4462 "gram1.y"
    { intonly = YES; ;}
    break;

  case 539:
#line 4466 "gram1.y"
    { intonly = NO; ;}
    break;

  case 540:
#line 4474 "gram1.y"
    { (yyvsp[(1) - (2)].bf_node)->entry.Template.ll_ptr2 = (yyvsp[(2) - (2)].ll_node);
		  (yyval.bf_node) = (yyvsp[(1) - (2)].bf_node); ;}
    break;

  case 541:
#line 4477 "gram1.y"
    { PTR_LLND p, q = LLNULL;

		  q = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  q->entry.string_val = (char *)"unit";
		  q->type = global_string;
		  p = make_llnd(fi, SPEC_PAIR, q, (yyvsp[(2) - (2)].ll_node), SMNULL);
		  (yyvsp[(1) - (2)].bf_node)->entry.Template.ll_ptr2 = p;
		  endioctl();
		  (yyval.bf_node) = (yyvsp[(1) - (2)].bf_node); ;}
    break;

  case 542:
#line 4487 "gram1.y"
    { PTR_LLND p, q, r;

		  p = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  p->entry.string_val = (char *)"*";
		  p->type = global_string;
		  q = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  q->entry.string_val = (char *)"unit";
		  q->type = global_string;
		  r = make_llnd(fi, SPEC_PAIR, p, q, SMNULL);
		  (yyvsp[(1) - (2)].bf_node)->entry.Template.ll_ptr2 = r;
		  endioctl();
		  (yyval.bf_node) = (yyvsp[(1) - (2)].bf_node); ;}
    break;

  case 543:
#line 4500 "gram1.y"
    { PTR_LLND p, q, r;

		  p = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  p->entry.string_val = (char *)"**";
		  p->type = global_string;
		  q = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  q->entry.string_val = (char *)"unit";
		  q->type = global_string;
		  r = make_llnd(fi, SPEC_PAIR, p, q, SMNULL);
		  (yyvsp[(1) - (2)].bf_node)->entry.Template.ll_ptr2 = r;
		  endioctl();
		  (yyval.bf_node) = (yyvsp[(1) - (2)].bf_node); ;}
    break;

  case 544:
#line 4513 "gram1.y"
    { (yyvsp[(1) - (2)].bf_node)->entry.Template.ll_ptr2 = (yyvsp[(2) - (2)].ll_node);
		  (yyval.bf_node) = (yyvsp[(1) - (2)].bf_node); ;}
    break;

  case 545:
#line 4516 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(1) - (1)].bf_node); ;}
    break;

  case 546:
#line 4518 "gram1.y"
    { (yyvsp[(1) - (2)].bf_node)->entry.Template.ll_ptr2 = (yyvsp[(2) - (2)].ll_node);
		  (yyval.bf_node) = (yyvsp[(1) - (2)].bf_node); ;}
    break;

  case 547:
#line 4521 "gram1.y"
    { (yyvsp[(1) - (2)].bf_node)->entry.Template.ll_ptr2 = (yyvsp[(2) - (2)].ll_node);
		  (yyval.bf_node) = (yyvsp[(1) - (2)].bf_node); ;}
    break;

  case 548:
#line 4524 "gram1.y"
    { (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr2 = (yyvsp[(2) - (3)].ll_node);
		  (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1 = (yyvsp[(3) - (3)].ll_node);
		  (yyval.bf_node) = (yyvsp[(1) - (3)].bf_node); ;}
    break;

  case 549:
#line 4528 "gram1.y"
    { (yyvsp[(1) - (4)].bf_node)->entry.Template.ll_ptr2 = (yyvsp[(2) - (4)].ll_node);
		  (yyvsp[(1) - (4)].bf_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (4)].ll_node);
		  (yyval.bf_node) = (yyvsp[(1) - (4)].bf_node); ;}
    break;

  case 550:
#line 4537 "gram1.y"
    { (yyvsp[(1) - (2)].bf_node)->entry.Template.ll_ptr2 = (yyvsp[(2) - (2)].ll_node);
		  (yyval.bf_node) = (yyvsp[(1) - (2)].bf_node); ;}
    break;

  case 551:
#line 4540 "gram1.y"
    { (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr2 = (yyvsp[(2) - (3)].ll_node);
		  (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1 = (yyvsp[(3) - (3)].ll_node);
		  (yyval.bf_node) = (yyvsp[(1) - (3)].bf_node); ;}
    break;

  case 552:
#line 4544 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(1) - (1)].bf_node); ;}
    break;

  case 553:
#line 4546 "gram1.y"
    { (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1 = (yyvsp[(3) - (3)].ll_node);
		  (yyval.bf_node) = (yyvsp[(1) - (3)].bf_node); ;}
    break;

  case 554:
#line 4552 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(1) - (3)].bf_node); ;}
    break;

  case 555:
#line 4556 "gram1.y"
    {(yyval.bf_node) = get_bfnd(fi, BACKSPACE_STAT, SMNULL, LLNULL, LLNULL, LLNULL);;}
    break;

  case 556:
#line 4558 "gram1.y"
    {(yyval.bf_node) = get_bfnd(fi, REWIND_STAT, SMNULL, LLNULL, LLNULL, LLNULL);;}
    break;

  case 557:
#line 4560 "gram1.y"
    {(yyval.bf_node) = get_bfnd(fi, ENDFILE_STAT, SMNULL, LLNULL, LLNULL, LLNULL);;}
    break;

  case 558:
#line 4567 "gram1.y"
    { (yyval.bf_node) = (yyvsp[(1) - (3)].bf_node); ;}
    break;

  case 559:
#line 4571 "gram1.y"
    {(yyval.bf_node) = get_bfnd(fi, OPEN_STAT, SMNULL, LLNULL, LLNULL, LLNULL);;}
    break;

  case 560:
#line 4573 "gram1.y"
    {(yyval.bf_node) = get_bfnd(fi, CLOSE_STAT, SMNULL, LLNULL, LLNULL, LLNULL);;}
    break;

  case 561:
#line 4577 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi, INQUIRE_STAT, SMNULL, LLNULL, (yyvsp[(4) - (4)].ll_node), LLNULL);;}
    break;

  case 562:
#line 4579 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi, INQUIRE_STAT, SMNULL, (yyvsp[(5) - (5)].ll_node), (yyvsp[(4) - (5)].ll_node), LLNULL);;}
    break;

  case 563:
#line 4583 "gram1.y"
    { PTR_LLND p;
		  PTR_LLND q = LLNULL;

		  if ((yyvsp[(1) - (1)].ll_node)->variant == INT_VAL)
 	          {
		        PTR_LABEL r;

			r = make_label_node(fi, (long) (yyvsp[(1) - (1)].ll_node)->entry.ival);
			r->scope = cur_scope();
			p = make_llnd_label(fi, LABEL_REF, r);
		  }
		  else p = (yyvsp[(1) - (1)].ll_node); 
		  q = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  q->entry.string_val = (char *)"fmt";
		  q->type = global_string;
		  (yyval.ll_node) = make_llnd(fi, SPEC_PAIR, q, p, SMNULL);
		  endioctl();
		;}
    break;

  case 564:
#line 4602 "gram1.y"
    { PTR_LLND p;
		  PTR_LLND q;

		  p = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  p->entry.string_val = (char *)"*";
		  p->type = global_string;
		  q = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  q->entry.string_val = (char *)"fmt";
		  q->type = global_string;
		  (yyval.ll_node) = make_llnd(fi, SPEC_PAIR, q, p, SMNULL);
		  endioctl();
		;}
    break;

  case 565:
#line 4618 "gram1.y"
    { PTR_LLND p;

		  p = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  p->entry.string_val = (char *)"unit";
		  p->type = global_string;
		  (yyval.ll_node) = make_llnd(fi, SPEC_PAIR, p, (yyvsp[(2) - (3)].ll_node), SMNULL);
		  endioctl();
		;}
    break;

  case 566:
#line 4629 "gram1.y"
    { 
		  (yyval.ll_node) = (yyvsp[(2) - (3)].ll_node);
		  endioctl();
		 ;}
    break;

  case 567:
#line 4636 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); endioctl();;}
    break;

  case 568:
#line 4638 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (4)].ll_node), (yyvsp[(4) - (4)].ll_node), EXPR_LIST); endioctl();;}
    break;

  case 569:
#line 4642 "gram1.y"
    { PTR_LLND p;
		  PTR_LLND q;
 
		  nioctl++;
		  if ((nioctl == 2) && ((yyvsp[(1) - (1)].ll_node)->variant == INT_VAL))
 	          {
		        PTR_LABEL r;

			r = make_label_node(fi, (long) (yyvsp[(1) - (1)].ll_node)->entry.ival);
			r->scope = cur_scope();
			p = make_llnd_label(fi, LABEL_REF, r);
		  }
		  else p = (yyvsp[(1) - (1)].ll_node); 
		  q = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  if (nioctl == 1)
		        q->entry.string_val = (char *)"unit"; 
		  else {
                     if(((yyvsp[(1) - (1)].ll_node)->variant == VAR_REF) && (yyvsp[(1) - (1)].ll_node)->entry.Template.symbol->variant == NAMELIST_NAME)
                       q->entry.string_val = (char *)"nml";
                     else
                       q->entry.string_val = (char *)"fmt";
                  }
		  q->type = global_string;
		  (yyval.ll_node) = make_llnd(fi, SPEC_PAIR, q, p, SMNULL);
		;}
    break;

  case 570:
#line 4668 "gram1.y"
    { PTR_LLND p;
		  PTR_LLND q;

		  nioctl++;
		  p = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  p->entry.string_val = (char *)"*";
		  p->type = global_string;
		  q = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  if (nioctl == 1)
		        q->entry.string_val = (char *)"unit"; 
		  else  q->entry.string_val = (char *)"fmt";
		  q->type = global_string;
		  (yyval.ll_node) = make_llnd(fi, SPEC_PAIR, q, p, SMNULL);
		;}
    break;

  case 571:
#line 4683 "gram1.y"
    { PTR_LLND p;
		  PTR_LLND q;

		  nioctl++;
		  p = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  p->entry.string_val = (char *)"**";
		  p->type = global_string;
		  q = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  if (nioctl == 1)
		        q->entry.string_val = (char *)"unit"; 
		  else  q->entry.string_val = (char *)"fmt";
		  q->type = global_string;
		  (yyval.ll_node) = make_llnd(fi, SPEC_PAIR, q, p, SMNULL);
		;}
    break;

  case 572:
#line 4698 "gram1.y"
    { 
		  PTR_LLND p;
		  char *q;

		  q = (yyvsp[(1) - (2)].ll_node)->entry.string_val;
  		  if ((strcmp(q, "end") == 0) || (strcmp(q, "err") == 0) || (strcmp(q, "eor") == 0) || ((strcmp(q,"fmt") == 0) && ((yyvsp[(2) - (2)].ll_node)->variant == INT_VAL)))
 	          {
		        PTR_LABEL r;

			r = make_label_node(fi, (long) (yyvsp[(2) - (2)].ll_node)->entry.ival);
			r->scope = cur_scope();
			p = make_llnd_label(fi, LABEL_REF, r);
		  }
		  else p = (yyvsp[(2) - (2)].ll_node);

		  (yyval.ll_node) = make_llnd(fi, SPEC_PAIR, (yyvsp[(1) - (2)].ll_node), p, SMNULL); ;}
    break;

  case 573:
#line 4715 "gram1.y"
    { PTR_LLND p;
                  
		  p = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  p->entry.string_val = (char *)"*";
		  p->type = global_string;
		  (yyval.ll_node) = make_llnd(fi, SPEC_PAIR, (yyvsp[(1) - (2)].ll_node), p, SMNULL);
		;}
    break;

  case 574:
#line 4723 "gram1.y"
    { PTR_LLND p;
		  p = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  p->entry.string_val = (char *)"*";
		  p->type = global_string;
		  (yyval.ll_node) = make_llnd(fi, SPEC_PAIR, (yyvsp[(1) - (2)].ll_node), p, SMNULL);
		;}
    break;

  case 575:
#line 4732 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
		  (yyval.ll_node)->entry.string_val = copys(yytext);
		  (yyval.ll_node)->type = global_string;
	        ;}
    break;

  case 576:
#line 4740 "gram1.y"
    {(yyval.bf_node) = get_bfnd(fi, READ_STAT, SMNULL, LLNULL, LLNULL, LLNULL);;}
    break;

  case 577:
#line 4745 "gram1.y"
    {(yyval.bf_node) = get_bfnd(fi, WRITE_STAT, SMNULL, LLNULL, LLNULL, LLNULL);;}
    break;

  case 578:
#line 4750 "gram1.y"
    {
	    PTR_LLND p, q, l;

	    if ((yyvsp[(3) - (4)].ll_node)->variant == INT_VAL)
		{
		        PTR_LABEL r;

			r = make_label_node(fi, (long) (yyvsp[(3) - (4)].ll_node)->entry.ival);
			r->scope = cur_scope();
			p = make_llnd_label(fi, LABEL_REF, r);
		}
	    else p = (yyvsp[(3) - (4)].ll_node);
	    
            q = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
	    q->entry.string_val = (char *)"fmt";
            q->type = global_string;
            l = make_llnd(fi, SPEC_PAIR, q, p, SMNULL);

            (yyval.bf_node) = get_bfnd(fi, PRINT_STAT, SMNULL, LLNULL, l, LLNULL);
	    endioctl();
	   ;}
    break;

  case 579:
#line 4772 "gram1.y"
    { PTR_LLND p, q, r;
		
	     p = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
	     p->entry.string_val = (char *)"*";
	     p->type = global_string;
	     q = make_llnd(fi, KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
	     q->entry.string_val = (char *)"fmt";
             q->type = global_string;
             r = make_llnd(fi, SPEC_PAIR, q, p, SMNULL);
	     (yyval.bf_node) = get_bfnd(fi, PRINT_STAT, SMNULL, LLNULL, r, LLNULL);
	     endioctl();
           ;}
    break;

  case 580:
#line 4788 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST);;}
    break;

  case 581:
#line 4790 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST);;}
    break;

  case 582:
#line 4794 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 583:
#line 4796 "gram1.y"
    {
		  (yyvsp[(4) - (5)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(2) - (5)].ll_node);
		  (yyval.ll_node) = (yyvsp[(4) - (5)].ll_node);
		;}
    break;

  case 584:
#line 4803 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST);  (yyval.ll_node)->type = (yyvsp[(1) - (1)].ll_node)->type;;}
    break;

  case 585:
#line 4805 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 586:
#line 4807 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 587:
#line 4811 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); (yyval.ll_node)->type = (yyvsp[(1) - (3)].ll_node)->type;;}
    break;

  case 588:
#line 4813 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); (yyval.ll_node)->type = (yyvsp[(1) - (3)].ll_node)->type;;}
    break;

  case 589:
#line 4815 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); (yyval.ll_node)->type = (yyvsp[(1) - (3)].ll_node)->type;;}
    break;

  case 590:
#line 4817 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); (yyval.ll_node)->type = (yyvsp[(1) - (3)].ll_node)->type;;}
    break;

  case 591:
#line 4819 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); (yyval.ll_node)->type = (yyvsp[(1) - (3)].ll_node)->type;;}
    break;

  case 592:
#line 4821 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); (yyval.ll_node)->type = (yyvsp[(1) - (3)].ll_node)->type;;}
    break;

  case 593:
#line 4825 "gram1.y"
    { (yyval.ll_node) =  set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST);
	          (yyval.ll_node)->type = global_complex; ;}
    break;

  case 594:
#line 4828 "gram1.y"
    { (yyval.ll_node) =  set_ll_list((yyvsp[(2) - (3)].ll_node), LLNULL, EXPR_LIST);
                  (yyval.ll_node)->type = (yyvsp[(2) - (3)].ll_node)->type; ;}
    break;

  case 595:
#line 4831 "gram1.y"
    {
		  (yyvsp[(4) - (5)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(2) - (5)].ll_node);
		  (yyval.ll_node) =  set_ll_list((yyvsp[(4) - (5)].ll_node), LLNULL, EXPR_LIST);
                  (yyval.ll_node)->type = (yyvsp[(2) - (5)].ll_node)->type; 
		;}
    break;

  case 596:
#line 4837 "gram1.y"
    {
		  (yyvsp[(4) - (5)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(2) - (5)].ll_node);
		  (yyval.ll_node) =  set_ll_list((yyvsp[(4) - (5)].ll_node), LLNULL, EXPR_LIST);
                  (yyval.ll_node)->type = (yyvsp[(2) - (5)].ll_node)->type; 
		;}
    break;

  case 597:
#line 4843 "gram1.y"
    {
		  (yyvsp[(4) - (5)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(2) - (5)].ll_node);
		  (yyval.ll_node) =  set_ll_list((yyvsp[(4) - (5)].ll_node), LLNULL, EXPR_LIST);
                  (yyval.ll_node)->type = (yyvsp[(2) - (5)].ll_node)->type; 
		;}
    break;

  case 598:
#line 4851 "gram1.y"
    { inioctl = YES; ;}
    break;

  case 599:
#line 4855 "gram1.y"
    { startioctl();;}
    break;

  case 600:
#line 4863 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 601:
#line 4865 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(2) - (3)].ll_node); ;}
    break;

  case 602:
#line 4869 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 603:
#line 4871 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 604:
#line 4873 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,(yyvsp[(2) - (3)].token), (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), SMNULL);
	      set_expr_type((yyval.ll_node));
	    ;}
    break;

  case 605:
#line 4878 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,MULT_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), SMNULL);
	      set_expr_type((yyval.ll_node));
	    ;}
    break;

  case 606:
#line 4883 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,DIV_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), SMNULL);
	      set_expr_type((yyval.ll_node));
	    ;}
    break;

  case 607:
#line 4888 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,EXP_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), SMNULL);
	      set_expr_type((yyval.ll_node));
	    ;}
    break;

  case 608:
#line 4893 "gram1.y"
    {
	      if((yyvsp[(1) - (2)].token) == SUBT_OP)
		{
		  (yyval.ll_node) = make_llnd(fi,SUBT_OP, (yyvsp[(2) - (2)].ll_node), LLNULL, SMNULL);
		  set_expr_type((yyval.ll_node));
		}
	      else	(yyval.ll_node) = (yyvsp[(2) - (2)].ll_node);
	    ;}
    break;

  case 609:
#line 4902 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,CONCAT_OP, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), SMNULL);
	      set_expr_type((yyval.ll_node));
	    ;}
    break;

  case 610:
#line 4907 "gram1.y"
    { (yyval.ll_node) = LLNULL; ;}
    break;

  case 611:
#line 4912 "gram1.y"
    { comments = cur_comment = CMNULL; ;}
    break;

  case 612:
#line 4914 "gram1.y"
    { PTR_CMNT p;
	    p = make_comment(fi,*commentbuf, HALF);
	    if (cur_comment)
               cur_comment->next = p;
            else {
	       if ((pred_bfnd->control_parent->variant == LOGIF_NODE) ||(pred_bfnd->control_parent->variant == FORALL_STAT))

	           pred_bfnd->control_parent->entry.Template.cmnt_ptr = p;

	       else last_bfnd->entry.Template.cmnt_ptr = p;
            }
	    comments = cur_comment = CMNULL;
          ;}
    break;

  case 676:
#line 4997 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,HPF_TEMPLATE_STAT, SMNULL, (yyvsp[(3) - (3)].ll_node), LLNULL, LLNULL); ;}
    break;

  case 677:
#line 4999 "gram1.y"
    { PTR_SYMB s;
                if((yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr2)
                {
                  s = (yyvsp[(3) - (3)].ll_node)->entry.Template.ll_ptr1->entry.Template.symbol;
                  s->attr = s->attr | COMMON_BIT;
                }
	        add_to_lowLevelList((yyvsp[(3) - (3)].ll_node), (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
	      ;}
    break;

  case 678:
#line 5010 "gram1.y"
    {PTR_SYMB s;
	      PTR_LLND q;
	    /* 27.06.18
	      if(! explicit_shape)   
                err("Explicit shape specification is required", 50);
	    */  
	      s = make_array((yyvsp[(1) - (2)].hash_entry), TYNULL, (yyvsp[(2) - (2)].ll_node), ndim, LOCAL);
              if(s->attr & TEMPLATE_BIT)
                errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
              if((s->attr & PROCESSORS_BIT) || (s->attr & TASK_BIT) || (s->attr & DVM_POINTER_BIT))
                errstr( "Inconsistent declaration of identifier  %s ", s->ident, 16);
              else
	        s->attr = s->attr | TEMPLATE_BIT;
              if((yyvsp[(2) - (2)].ll_node)) s->attr = s->attr | DIMENSION_BIT;  
	      q = make_llnd(fi,ARRAY_REF, (yyvsp[(2) - (2)].ll_node), LLNULL, s);
	      s->type->entry.ar_decl.ranges = (yyvsp[(2) - (2)].ll_node);
	      (yyval.ll_node) = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	     ;}
    break;

  case 679:
#line 5031 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_DYNAMIC_DIR, SMNULL, (yyvsp[(3) - (3)].ll_node), LLNULL, LLNULL);;}
    break;

  case 680:
#line 5035 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 681:
#line 5037 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 682:
#line 5041 "gram1.y"
    {  PTR_SYMB s;
	      s = make_array((yyvsp[(1) - (1)].hash_entry), TYNULL, LLNULL, 0, LOCAL);
              if(s->attr &  DYNAMIC_BIT)
                errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
              if((s->attr & PROCESSORS_BIT) || (s->attr & TASK_BIT) || (s->attr & HEAP_BIT)) 
                errstr("Inconsistent declaration of identifier  %s", s->ident, 16); 
              else
                s->attr = s->attr | DYNAMIC_BIT;        
	      (yyval.ll_node) = make_llnd(fi,ARRAY_REF, LLNULL, LLNULL, s);
	   ;}
    break;

  case 683:
#line 5054 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_INHERIT_DIR, SMNULL, (yyvsp[(3) - (3)].ll_node), LLNULL, LLNULL);;}
    break;

  case 684:
#line 5058 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 685:
#line 5060 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 686:
#line 5064 "gram1.y"
    {  PTR_SYMB s;
	      s = make_array((yyvsp[(1) - (1)].hash_entry), TYNULL, LLNULL, 0, LOCAL);
              if((s->attr & PROCESSORS_BIT) ||(s->attr & TASK_BIT)  || (s->attr & TEMPLATE_BIT) || (s->attr & ALIGN_BIT) || (s->attr & DISTRIBUTE_BIT)) 
                errstr("Inconsistent declaration of identifier  %s", s->ident, 16); 
              else
                s->attr = s->attr | INHERIT_BIT;        
	      (yyval.ll_node) = make_llnd(fi,ARRAY_REF, LLNULL, LLNULL, s);
	   ;}
    break;

  case 687:
#line 5075 "gram1.y"
    { PTR_LLND q;
             q = set_ll_list((yyvsp[(3) - (4)].ll_node),LLNULL,EXPR_LIST);
              /* (void)fprintf(stderr,"hpf.gram: shadow\n");*/ 
	     (yyval.bf_node) = get_bfnd(fi,DVM_SHADOW_DIR,SMNULL,q,(yyvsp[(4) - (4)].ll_node),LLNULL);
            ;}
    break;

  case 688:
#line 5086 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(2) - (3)].ll_node);;}
    break;

  case 689:
#line 5090 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 690:
#line 5092 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 691:
#line 5096 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 692:
#line 5098 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), SMNULL);;}
    break;

  case 693:
#line 5100 "gram1.y"
    {
            if(parstate!=INEXEC) 
               err("Illegal shadow width specification", 56);  
            (yyval.ll_node) = make_llnd(fi,SHADOW_NAMES_OP, (yyvsp[(3) - (4)].ll_node), LLNULL, SMNULL);
          ;}
    break;

  case 694:
#line 5115 "gram1.y"
    {  PTR_SYMB s;
	      s = make_array((yyvsp[(1) - (1)].hash_entry), TYNULL, LLNULL, 0, LOCAL);
              if(s->attr & SHADOW_BIT)
                errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
              if((s->attr & PROCESSORS_BIT) ||(s->attr & TASK_BIT)  || (s->attr & TEMPLATE_BIT) || (s->attr & HEAP_BIT)) 
                      errstr( "Inconsistent declaration of identifier %s", s->ident, 16); 
              else
        	      s->attr = s->attr | SHADOW_BIT;  
	      (yyval.ll_node) = make_llnd(fi,ARRAY_REF, LLNULL, LLNULL, s);
	   ;}
    break;

  case 695:
#line 5127 "gram1.y"
    { PTR_SYMB s;
	      PTR_LLND q, r;
	      if(! explicit_shape) {
              err("Explicit shape specification is required", 50);
		/* $$ = BFNULL;*/
	      }
	      s = make_array((yyvsp[(3) - (4)].hash_entry), TYNULL, (yyvsp[(4) - (4)].ll_node), ndim, LOCAL);
              if(s->attr &  PROCESSORS_BIT)
                errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
              if((s->attr & ALIGN_BIT) ||(s->attr & DISTRIBUTE_BIT) ||(s->attr & TEMPLATE_BIT) || (s->attr & DYNAMIC_BIT) ||(s->attr & SHADOW_BIT) || (s->attr & TASK_BIT) || (s->attr & DVM_POINTER_BIT) || (s->attr & INHERIT_BIT))
                errstr("Inconsistent declaration of identifier %s", s->ident, 16);
              else
	        s->attr = s->attr | PROCESSORS_BIT;
              if((yyvsp[(4) - (4)].ll_node)) s->attr = s->attr | DIMENSION_BIT;
	      q = make_llnd(fi,ARRAY_REF, (yyvsp[(4) - (4)].ll_node), LLNULL, s);
	      s->type->entry.ar_decl.ranges = (yyvsp[(4) - (4)].ll_node);
	      r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	      (yyval.bf_node) = get_bfnd(fi,HPF_PROCESSORS_STAT, SMNULL, r, LLNULL, LLNULL);
	    ;}
    break;

  case 696:
#line 5147 "gram1.y"
    { PTR_SYMB s;
	      PTR_LLND q, r;
	      if(! explicit_shape) {
              err("Explicit shape specification is required", 50);
		/* $$ = BFNULL;*/
	      }
	      s = make_array((yyvsp[(3) - (4)].hash_entry), TYNULL, (yyvsp[(4) - (4)].ll_node), ndim, LOCAL);
              if(s->attr &  PROCESSORS_BIT)
                errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
              if((s->attr & ALIGN_BIT) ||(s->attr & DISTRIBUTE_BIT) ||(s->attr & TEMPLATE_BIT) || (s->attr & DYNAMIC_BIT) ||(s->attr & SHADOW_BIT) || (s->attr & TASK_BIT) || (s->attr & DVM_POINTER_BIT) || (s->attr & INHERIT_BIT))
                errstr("Inconsistent declaration of identifier %s", s->ident, 16);
              else
	        s->attr = s->attr | PROCESSORS_BIT;
              if((yyvsp[(4) - (4)].ll_node)) s->attr = s->attr | DIMENSION_BIT;
	      q = make_llnd(fi,ARRAY_REF, (yyvsp[(4) - (4)].ll_node), LLNULL, s);
	      s->type->entry.ar_decl.ranges = (yyvsp[(4) - (4)].ll_node);
	      r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	      (yyval.bf_node) = get_bfnd(fi,HPF_PROCESSORS_STAT, SMNULL, r, LLNULL, LLNULL);
	    ;}
    break;

  case 697:
#line 5167 "gram1.y"
    {  PTR_SYMB s;
	      PTR_LLND q, r;
	      if(! explicit_shape) {
		err("Explicit shape specification is required", 50);
		/*$$ = BFNULL;*/
	      }
	      s = make_array((yyvsp[(3) - (4)].hash_entry), TYNULL, (yyvsp[(4) - (4)].ll_node), ndim, LOCAL);
              if(s->attr &  PROCESSORS_BIT)
                errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
              if((s->attr & ALIGN_BIT) ||(s->attr & DISTRIBUTE_BIT) ||(s->attr & TEMPLATE_BIT) || (s->attr & DYNAMIC_BIT) ||(s->attr & SHADOW_BIT) || (s->attr & TASK_BIT) || (s->attr &  DVM_POINTER_BIT) || (s->attr & INHERIT_BIT) )
                errstr("Inconsistent declaration of identifier  %s", s->ident, 16);
              else
	        s->attr = s->attr | PROCESSORS_BIT;
              if((yyvsp[(4) - (4)].ll_node)) s->attr = s->attr | DIMENSION_BIT;
	      q = make_llnd(fi,ARRAY_REF, (yyvsp[(4) - (4)].ll_node), LLNULL, s);
	      s->type->entry.ar_decl.ranges = (yyvsp[(4) - (4)].ll_node);
	      r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	      add_to_lowLevelList(r, (yyvsp[(1) - (4)].bf_node)->entry.Template.ll_ptr1);
	;}
    break;

  case 698:
#line 5189 "gram1.y"
    {  PTR_LLND q,r;
                   q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, (yyvsp[(3) - (3)].symbol));
                   r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	           (yyval.bf_node) = get_bfnd(fi,DVM_INDIRECT_GROUP_DIR, SMNULL, r, LLNULL, LLNULL);
                ;}
    break;

  case 699:
#line 5195 "gram1.y"
    {  PTR_LLND q,r;
                   q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, (yyvsp[(3) - (3)].symbol));
                   r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
                   add_to_lowLevelList(r, (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
	           ;
                ;}
    break;

  case 700:
#line 5204 "gram1.y"
    {(yyval.symbol) = make_local_entity((yyvsp[(1) - (1)].hash_entry), REF_GROUP_NAME,global_default,LOCAL);
          if((yyval.symbol)->attr &  INDIRECT_BIT)
                errstr( "Multiple declaration of identifier  %s ", (yyval.symbol)->ident, 73);
           (yyval.symbol)->attr = (yyval.symbol)->attr | INDIRECT_BIT;
          ;}
    break;

  case 701:
#line 5212 "gram1.y"
    {  PTR_LLND q,r;
                   q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, (yyvsp[(3) - (3)].symbol));
                   r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	           (yyval.bf_node) = get_bfnd(fi,DVM_REMOTE_GROUP_DIR, SMNULL, r, LLNULL, LLNULL);
                ;}
    break;

  case 702:
#line 5218 "gram1.y"
    {  PTR_LLND q,r;
                   q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, (yyvsp[(3) - (3)].symbol));
                   r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
                   add_to_lowLevelList(r, (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
                ;}
    break;

  case 703:
#line 5226 "gram1.y"
    {(yyval.symbol) = make_local_entity((yyvsp[(1) - (1)].hash_entry), REF_GROUP_NAME,global_default,LOCAL);
           if((yyval.symbol)->attr &  INDIRECT_BIT)
                errstr( "Inconsistent declaration of identifier  %s ", (yyval.symbol)->ident, 16);
          ;}
    break;

  case 704:
#line 5233 "gram1.y"
    {  PTR_LLND q,r;
                   q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, (yyvsp[(3) - (3)].symbol));
                   r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	           (yyval.bf_node) = get_bfnd(fi,DVM_REDUCTION_GROUP_DIR, SMNULL, r, LLNULL, LLNULL);
                ;}
    break;

  case 705:
#line 5239 "gram1.y"
    {  PTR_LLND q,r;
                   q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, (yyvsp[(3) - (3)].symbol));
                   r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
                   add_to_lowLevelList(r, (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
	           ;
                ;}
    break;

  case 706:
#line 5248 "gram1.y"
    {(yyval.symbol) = make_local_entity((yyvsp[(1) - (1)].hash_entry), REDUCTION_GROUP_NAME,global_default,LOCAL);;}
    break;

  case 707:
#line 5252 "gram1.y"
    {  PTR_LLND q,r;
                   q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, (yyvsp[(3) - (3)].symbol));
                   r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	           (yyval.bf_node) = get_bfnd(fi,DVM_CONSISTENT_GROUP_DIR, SMNULL, r, LLNULL, LLNULL);
                ;}
    break;

  case 708:
#line 5258 "gram1.y"
    {  PTR_LLND q,r;
                   q = make_llnd(fi,VAR_REF, LLNULL, LLNULL, (yyvsp[(3) - (3)].symbol));
                   r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
                   add_to_lowLevelList(r, (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);	           
                ;}
    break;

  case 709:
#line 5266 "gram1.y"
    {(yyval.symbol) = make_local_entity((yyvsp[(1) - (1)].hash_entry), CONSISTENT_GROUP_NAME,global_default,LOCAL);;}
    break;

  case 710:
#line 5280 "gram1.y"
    { PTR_SYMB s;
            if(parstate == INEXEC){
              if (!(s = (yyvsp[(2) - (3)].hash_entry)->id_attr))
              {
	         s = make_array((yyvsp[(2) - (3)].hash_entry), TYNULL, LLNULL, 0, LOCAL);
	     	 s->decl = SOFT;
	      } 
            } else
              s = make_array((yyvsp[(2) - (3)].hash_entry), TYNULL, LLNULL, 0, LOCAL);

              (yyval.ll_node) = make_llnd(fi,ARRAY_REF, (yyvsp[(3) - (3)].ll_node), LLNULL, s);
            ;}
    break;

  case 711:
#line 5293 "gram1.y"
    { (yyval.ll_node) = LLNULL; opt_kwd_ = NO;;}
    break;

  case 712:
#line 5299 "gram1.y"
    { PTR_LLND q;
             if(!(yyvsp[(4) - (5)].ll_node))
               err("Distribution format list is omitted", 51);
            /* if($6)
               err("NEW_VALUE specification in DISTRIBUTE directive");*/
             q = set_ll_list((yyvsp[(3) - (5)].ll_node),LLNULL,EXPR_LIST);
	     (yyval.bf_node) = get_bfnd(fi,DVM_DISTRIBUTE_DIR,SMNULL,q,(yyvsp[(4) - (5)].ll_node),(yyvsp[(5) - (5)].ll_node));
            ;}
    break;

  case 713:
#line 5315 "gram1.y"
    { PTR_LLND q;
                /*  if(!$4)
                  {err("Distribution format is omitted", 51); errcnt--;}
                 */
              q = set_ll_list((yyvsp[(3) - (6)].ll_node),LLNULL,EXPR_LIST);
                 /* r = LLNULL;
                   if($6){
                     r = set_ll_list($6,LLNULL,EXPR_LIST);
                     if($7) r = set_ll_list(r,$7,EXPR_LIST);
                   } else
                     if($7) r = set_ll_list(r,$7,EXPR_LIST);
                 */
	      (yyval.bf_node) = get_bfnd(fi,DVM_REDISTRIBUTE_DIR,SMNULL,q,(yyvsp[(4) - (6)].ll_node),(yyvsp[(6) - (6)].ll_node));;}
    break;

  case 714:
#line 5330 "gram1.y"
    {
                 /* r = LLNULL;
                    if($5){
                      r = set_ll_list($5,LLNULL,EXPR_LIST);
                      if($6) r = set_ll_list(r,$6,EXPR_LIST);
                    } else
                      if($6) r = set_ll_list(r,$6,EXPR_LIST);
                  */
	      (yyval.bf_node) = get_bfnd(fi,DVM_REDISTRIBUTE_DIR,SMNULL,(yyvsp[(8) - (8)].ll_node) ,(yyvsp[(3) - (8)].ll_node),(yyvsp[(5) - (8)].ll_node) );
             ;}
    break;

  case 715:
#line 5358 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 716:
#line 5360 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 717:
#line 5364 "gram1.y"
    {(yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 718:
#line 5366 "gram1.y"
    {(yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 719:
#line 5370 "gram1.y"
    {  PTR_SYMB s;
 
          if(parstate == INEXEC){
            if (!(s = (yyvsp[(1) - (1)].hash_entry)->id_attr))
              {
	         s = make_array((yyvsp[(1) - (1)].hash_entry), TYNULL, LLNULL, 0, LOCAL);
	     	 s->decl = SOFT;
	      } 
            if(s->attr & PROCESSORS_BIT)
              errstr( "Illegal use of PROCESSORS name %s ", s->ident, 53);
            if(s->attr & TASK_BIT)
              errstr( "Illegal use of task array name %s ", s->ident, 71);

          } else {
            s = make_array((yyvsp[(1) - (1)].hash_entry), TYNULL, LLNULL, 0, LOCAL);
            if(s->attr & DISTRIBUTE_BIT)
              errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
            else if( (s->attr & PROCESSORS_BIT) || (s->attr & TASK_BIT) || (s->attr & INHERIT_BIT))
              errstr("Inconsistent declaration of identifier  %s",s->ident, 16);
            else
              s->attr = s->attr | DISTRIBUTE_BIT;
          } 
         if(s->attr & ALIGN_BIT)
               errstr("A distributee may not have the ALIGN attribute:%s",s->ident, 54);
          (yyval.ll_node) = make_llnd(fi,ARRAY_REF, LLNULL, LLNULL, s);               	  
	;}
    break;

  case 720:
#line 5399 "gram1.y"
    {  PTR_SYMB s;
          s = make_array((yyvsp[(1) - (4)].hash_entry), TYNULL, LLNULL, 0, LOCAL);
        
          if(parstate != INEXEC) 
               errstr( "Illegal distributee:%s", s->ident, 312);
          else {
            if(s->attr & PROCESSORS_BIT)
               errstr( "Illegal use of PROCESSORS name %s ", s->ident, 53);  
            if(s->attr & TASK_BIT)
               errstr( "Illegal use of task array name %s ", s->ident, 71);        
            if(s->attr & ALIGN_BIT)
               errstr("A distributee may not have the ALIGN attribute:%s",s->ident, 54);
            if(!(s->attr & DVM_POINTER_BIT))
               errstr("Illegal distributee:%s", s->ident, 312);
          /*s->attr = s->attr | DISTRIBUTE_BIT;*/
	  (yyval.ll_node) = make_llnd(fi,ARRAY_REF, (yyvsp[(3) - (4)].ll_node), LLNULL, s); 
          }
        
	;}
    break;

  case 721:
#line 5422 "gram1.y"
    {  PTR_SYMB s;
          if((s=(yyvsp[(1) - (1)].hash_entry)->id_attr) == SMNULL)
            s = make_array((yyvsp[(1) - (1)].hash_entry), TYNULL, LLNULL, 0, LOCAL);
          if((parstate == INEXEC) && !(s->attr & PROCESSORS_BIT))
               errstr( "'%s' is not processor array ", s->ident, 67);
	  (yyval.symbol) = s;
	;}
    break;

  case 722:
#line 5442 "gram1.y"
    { (yyval.ll_node) = LLNULL;  ;}
    break;

  case 723:
#line 5444 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (2)].ll_node);;}
    break;

  case 724:
#line 5448 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(2) - (3)].ll_node);;}
    break;

  case 725:
#line 5469 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(2) - (2)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 726:
#line 5471 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (4)].ll_node),(yyvsp[(4) - (4)].ll_node),EXPR_LIST); ;}
    break;

  case 727:
#line 5474 "gram1.y"
    { opt_kwd_ = YES; ;}
    break;

  case 728:
#line 5483 "gram1.y"
    {  
               (yyval.ll_node) = make_llnd(fi,BLOCK_OP, LLNULL, LLNULL, SMNULL);
        ;}
    break;

  case 729:
#line 5487 "gram1.y"
    {  err("Distribution format BLOCK(n) is not permitted in FDVM", 55);
          (yyval.ll_node) = make_llnd(fi,BLOCK_OP, (yyvsp[(4) - (5)].ll_node), LLNULL, SMNULL);
          endioctl();
        ;}
    break;

  case 730:
#line 5492 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,BLOCK_OP, LLNULL, LLNULL, (yyvsp[(3) - (4)].symbol)); ;}
    break;

  case 731:
#line 5494 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,BLOCK_OP,  (yyvsp[(5) - (6)].ll_node),  LLNULL,  (yyvsp[(3) - (6)].symbol)); ;}
    break;

  case 732:
#line 5496 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,BLOCK_OP,  LLNULL, (yyvsp[(3) - (4)].ll_node),  SMNULL); ;}
    break;

  case 733:
#line 5498 "gram1.y"
    { 
          (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
          (yyval.ll_node)->entry.string_val = (char *) "*";
          (yyval.ll_node)->type = global_string;
        ;}
    break;

  case 734:
#line 5504 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,INDIRECT_OP, LLNULL, LLNULL, (yyvsp[(3) - (4)].symbol)); ;}
    break;

  case 735:
#line 5506 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,INDIRECT_OP, (yyvsp[(3) - (4)].ll_node), LLNULL, SMNULL); ;}
    break;

  case 736:
#line 5510 "gram1.y"
    {  PTR_SYMB s;
	      s = make_array((yyvsp[(1) - (1)].hash_entry), TYNULL, LLNULL, 0, LOCAL);
              if((s->attr & PROCESSORS_BIT) ||(s->attr & TASK_BIT)  || (s->attr & TEMPLATE_BIT)) 
                errstr("Inconsistent declaration of identifier  %s", s->ident, 16); 
       
	      (yyval.symbol) = s;
	   ;}
    break;

  case 737:
#line 5520 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DERIVED_OP, (yyvsp[(2) - (6)].ll_node), (yyvsp[(6) - (6)].ll_node), SMNULL); ;}
    break;

  case 738:
#line 5524 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 739:
#line 5526 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 740:
#line 5531 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 741:
#line 5533 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), SMNULL);;}
    break;

  case 742:
#line 5537 "gram1.y"
    { 
              (yyval.ll_node) = make_llnd(fi,ARRAY_REF, LLNULL, LLNULL, (yyvsp[(1) - (1)].symbol));
	    ;}
    break;

  case 743:
#line 5541 "gram1.y"
    { 
              (yyval.ll_node) = make_llnd(fi,ARRAY_REF, (yyvsp[(3) - (4)].ll_node), LLNULL, (yyvsp[(1) - (4)].symbol));
	    ;}
    break;

  case 744:
#line 5547 "gram1.y"
    { 
              if (!((yyval.symbol) = (yyvsp[(1) - (1)].hash_entry)->id_attr))
              {
	         (yyval.symbol) = make_array((yyvsp[(1) - (1)].hash_entry), TYNULL, LLNULL,0,LOCAL);
	         (yyval.symbol)->decl = SOFT;
	      } 
            ;}
    break;

  case 745:
#line 5557 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 746:
#line 5559 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 747:
#line 5563 "gram1.y"
    {  (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 748:
#line 5565 "gram1.y"
    {  (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 749:
#line 5567 "gram1.y"
    {
                      (yyvsp[(2) - (3)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(3) - (3)].ll_node); 
                      (yyval.ll_node) = (yyvsp[(2) - (3)].ll_node);   
                   ;}
    break;

  case 750:
#line 5574 "gram1.y"
    { PTR_SYMB s;
            s = make_scalar((yyvsp[(1) - (1)].hash_entry),TYNULL,LOCAL);
	    (yyval.ll_node) = make_llnd(fi,DUMMY_REF, LLNULL, LLNULL, s);
            /*$$->type = global_int;*/
          ;}
    break;

  case 751:
#line 5591 "gram1.y"
    {  (yyval.ll_node) = LLNULL; ;}
    break;

  case 752:
#line 5593 "gram1.y"
    {  (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 753:
#line 5597 "gram1.y"
    {  (yyval.ll_node) = set_ll_list((yyvsp[(2) - (2)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 754:
#line 5599 "gram1.y"
    {  (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 755:
#line 5603 "gram1.y"
    {  if((yyvsp[(1) - (1)].ll_node)->type->variant != T_STRING)
                 errstr( "Illegal type of shadow_name", 627);
               (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); 
            ;}
    break;

  case 756:
#line 5610 "gram1.y"
    { char *q;
          nioctl = 1;
          q = (yyvsp[(1) - (2)].ll_node)->entry.string_val;
          if((!strcmp(q,"shadow")) && ((yyvsp[(2) - (2)].ll_node)->variant == INT_VAL))                          (yyval.ll_node) = make_llnd(fi,SPEC_PAIR, (yyvsp[(1) - (2)].ll_node), (yyvsp[(2) - (2)].ll_node), SMNULL);
          else
          {  err("Illegal shadow width specification", 56);
             (yyval.ll_node) = LLNULL;
          }
        ;}
    break;

  case 757:
#line 5620 "gram1.y"
    { char *ql, *qh;
          PTR_LLND p1, p2;
          nioctl = 2;
          ql = (yyvsp[(1) - (5)].ll_node)->entry.string_val;
          qh = (yyvsp[(4) - (5)].ll_node)->entry.string_val;
          if((!strcmp(ql,"low_shadow")) && ((yyvsp[(2) - (5)].ll_node)->variant == INT_VAL) && (!strcmp(qh,"high_shadow")) && ((yyvsp[(5) - (5)].ll_node)->variant == INT_VAL)) 
              {
                 p1 = make_llnd(fi,SPEC_PAIR, (yyvsp[(1) - (5)].ll_node), (yyvsp[(2) - (5)].ll_node), SMNULL);
                 p2 = make_llnd(fi,SPEC_PAIR, (yyvsp[(4) - (5)].ll_node), (yyvsp[(5) - (5)].ll_node), SMNULL);
                 (yyval.ll_node) = make_llnd(fi,CONS, p1, p2, SMNULL);
              } 
          else
          {  err("Illegal shadow width specification", 56);
             (yyval.ll_node) = LLNULL;
          }
        ;}
    break;

  case 758:
#line 5649 "gram1.y"
    { PTR_LLND q;
              q = set_ll_list((yyvsp[(3) - (4)].ll_node),LLNULL,EXPR_LIST);
              (yyval.bf_node) = (yyvsp[(4) - (4)].bf_node);
              (yyval.bf_node)->entry.Template.ll_ptr1 = q;
            ;}
    break;

  case 759:
#line 5664 "gram1.y"
    { PTR_LLND q;
              q = set_ll_list((yyvsp[(3) - (4)].ll_node),LLNULL,EXPR_LIST);
              (yyval.bf_node) = (yyvsp[(4) - (4)].bf_node);
              (yyval.bf_node)->variant = DVM_REALIGN_DIR; 
              (yyval.bf_node)->entry.Template.ll_ptr1 = q;
            ;}
    break;

  case 760:
#line 5671 "gram1.y"
    {
              (yyval.bf_node) = (yyvsp[(3) - (6)].bf_node);
              (yyval.bf_node)->variant = DVM_REALIGN_DIR; 
              (yyval.bf_node)->entry.Template.ll_ptr1 = (yyvsp[(6) - (6)].ll_node);
            ;}
    break;

  case 761:
#line 5689 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 762:
#line 5691 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 763:
#line 5695 "gram1.y"
    {  PTR_SYMB s;
          s = make_array((yyvsp[(1) - (1)].hash_entry), TYNULL, LLNULL, 0, LOCAL);
          if((s->attr & ALIGN_BIT)) 
                errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
          if((s->attr & PROCESSORS_BIT) || (s->attr & TASK_BIT) || (s->attr & INHERIT_BIT)) 
                errstr( "Inconsistent declaration of identifier  %s", s->ident, 16); 
          else  if(s->attr & DISTRIBUTE_BIT)
               errstr( "An alignee may not have the DISTRIBUTE attribute:'%s'", s->ident,57);             else
                s->attr = s->attr | ALIGN_BIT;     
	  (yyval.ll_node) = make_llnd(fi,ARRAY_REF, LLNULL, LLNULL, s);
	;}
    break;

  case 764:
#line 5709 "gram1.y"
    {PTR_SYMB s;
        s = (yyvsp[(1) - (1)].ll_node)->entry.Template.symbol;
        if(s->attr & PROCESSORS_BIT)
               errstr( "Illegal use of PROCESSORS name %s ", s->ident, 53);
        else  if(s->attr & TASK_BIT)
              errstr( "Illegal use of task array name %s ", s->ident, 71);
        else if( !(s->attr & DIMENSION_BIT) && !(s->attr & DVM_POINTER_BIT))
            errstr("The alignee %s isn't an array", s->ident, 58);
        else {
            /*  if(!(s->attr & DYNAMIC_BIT))
                 errstr("'%s' hasn't the DYNAMIC attribute", s->ident, 59);
             */
              if(!(s->attr & ALIGN_BIT) && !(s->attr & INHERIT_BIT))
                 errstr("'%s' hasn't the ALIGN attribute", s->ident, 60);
              if(s->attr & DISTRIBUTE_BIT)
                 errstr("An alignee may not have the DISTRIBUTE attribute: %s", s->ident, 57);

/*               if(s->entry.var_decl.local == IO)
 *                 errstr("An alignee may not be the dummy argument");
*/
          }
	  (yyval.ll_node) = make_llnd(fi,ARRAY_REF, LLNULL, LLNULL, s);
	;}
    break;

  case 765:
#line 5735 "gram1.y"
    { /* PTR_LLND r;
              if($7) {
                r = set_ll_list($6,LLNULL,EXPR_LIST);
                r = set_ll_list(r,$7,EXPR_LIST);
              }
              else
                r = $6;
              */
            (yyval.bf_node) = get_bfnd(fi,DVM_ALIGN_DIR,SMNULL,LLNULL,(yyvsp[(2) - (6)].ll_node),(yyvsp[(6) - (6)].ll_node));
           ;}
    break;

  case 766:
#line 5748 "gram1.y"
    {
           (yyval.ll_node) = make_llnd(fi,ARRAY_REF, (yyvsp[(3) - (4)].ll_node), LLNULL, (yyvsp[(1) - (4)].symbol));        
          ;}
    break;

  case 767:
#line 5764 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 768:
#line 5766 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 769:
#line 5769 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 770:
#line 5771 "gram1.y"
    {
                  (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
                  (yyval.ll_node)->entry.string_val = (char *) "*";
                  (yyval.ll_node)->type = global_string;
                 ;}
    break;

  case 771:
#line 5777 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 772:
#line 5781 "gram1.y"
    { 
         /* if(parstate == INEXEC){ *for REALIGN directive*
              if (!($$ = $1->id_attr))
              {
	         $$ = make_array($1, TYNULL, LLNULL,0,LOCAL);
	     	 $$->decl = SOFT;
	      } 
          } else
             $$ = make_array($1, TYNULL, LLNULL, 0, LOCAL);
          */
          if (!((yyval.symbol) = (yyvsp[(1) - (1)].hash_entry)->id_attr))
          {
	       (yyval.symbol) = make_array((yyvsp[(1) - (1)].hash_entry), TYNULL, LLNULL,0,LOCAL);
	       (yyval.symbol)->decl = SOFT;
	  } 
          (yyval.symbol)->attr = (yyval.symbol)->attr | ALIGN_BASE_BIT;
          if((yyval.symbol)->attr & PROCESSORS_BIT)
               errstr( "Illegal use of PROCESSORS name %s ", (yyval.symbol)->ident, 53);
          else  if((yyval.symbol)->attr & TASK_BIT)
               errstr( "Illegal use of task array name %s ", (yyval.symbol)->ident, 71);
          else
          if((parstate == INEXEC) /* for  REALIGN directive */
             &&   !((yyval.symbol)->attr & DIMENSION_BIT) && !((yyval.symbol)->attr & DVM_POINTER_BIT))
            errstr("The align-target %s isn't declared as array", (yyval.symbol)->ident, 61); 
         ;}
    break;

  case 773:
#line 5809 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 774:
#line 5811 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 775:
#line 5815 "gram1.y"
    { PTR_SYMB s;
            s = make_scalar((yyvsp[(1) - (1)].hash_entry),TYNULL,LOCAL);
            if(s->type->variant != T_INT || s->attr & PARAMETER_BIT)             
              errstr("The align-dummy %s isn't a scalar integer variable", s->ident, 62); 
	   (yyval.ll_node) = make_llnd(fi,VAR_REF, LLNULL, LLNULL, s);
           (yyval.ll_node)->type = global_int;
         ;}
    break;

  case 776:
#line 5823 "gram1.y"
    {  
          (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
          (yyval.ll_node)->entry.string_val = (char *) "*";
          (yyval.ll_node)->type = global_string;
        ;}
    break;

  case 777:
#line 5829 "gram1.y"
    {   (yyval.ll_node) = make_llnd(fi,DDOT, LLNULL, LLNULL, SMNULL); ;}
    break;

  case 778:
#line 5832 "gram1.y"
    { PTR_SYMB s;
	             PTR_LLND q, r, p;
                     int numdim;
                     if(type_options & PROCESSORS_BIT) {    /* 27.06.18 || (type_options & TEMPLATE_BIT)){ */
                       if(! explicit_shape) {
                         err("Explicit shape specification is required", 50);
		         /*$$ = BFNULL;*/
	               }
                     } 

                    /*  else {
                       if($6)
                         err("Shape specification is not permitted", 263);
                     } */

                     if(type_options & DIMENSION_BIT)
                       { p = attr_dims; numdim = attr_ndim;}
                     else
                       { p = LLNULL; numdim = 0; }
                     if((yyvsp[(6) - (6)].ll_node))          /*dimension information after the object name*/
                     { p = (yyvsp[(6) - (6)].ll_node); numdim = ndim;} /*overrides the DIMENSION attribute */
	             s = make_array((yyvsp[(5) - (6)].hash_entry), TYNULL, p, numdim, LOCAL);

                     if((type_options & COMMON_BIT) && !(type_options & TEMPLATE_BIT))
                     {
                        err("Illegal combination of attributes", 63);
                        type_options = type_options & (~COMMON_BIT);
                     }
                     if((type_options & PROCESSORS_BIT) &&((type_options & ALIGN_BIT) ||(type_options & DISTRIBUTE_BIT) ||(type_options & TEMPLATE_BIT) || (type_options & DYNAMIC_BIT) ||(type_options & SHADOW_BIT) ))
                        err("Illegal combination of attributes", 63);
                     else  if((type_options & PROCESSORS_BIT) && ((s->attr & ALIGN_BIT) ||(s->attr & DISTRIBUTE_BIT) ||(s->attr & TEMPLATE_BIT) || (s->attr & DYNAMIC_BIT) ||(s->attr & SHADOW_BIT)) )
                     {  errstr("Inconsistent declaration of  %s", s->ident, 16);
                        type_options = type_options & (~PROCESSORS_BIT);
                     }
                     else if ((s->attr & PROCESSORS_BIT) && ((type_options & ALIGN_BIT) ||(type_options & DISTRIBUTE_BIT) ||(type_options & TEMPLATE_BIT) || (type_options & DYNAMIC_BIT) ||(type_options & SHADOW_BIT))) 
                        errstr("Inconsistent declaration of  %s", s->ident, 16);
                     else if ((s->attr & INHERIT_BIT) && ((type_options & ALIGN_BIT) ||(type_options & DISTRIBUTE_BIT)))
                        errstr("Inconsistent declaration of  %s", s->ident, 16);
                     if(( s->attr & DISTRIBUTE_BIT) &&  (type_options & DISTRIBUTE_BIT))
                           errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
                     if(( s->attr & ALIGN_BIT) &&  (type_options & ALIGN_BIT))
                           errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
                     if(( s->attr & SHADOW_BIT) &&  (type_options & SHADOW_BIT))
                           errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
                     if(( s->attr & TEMPLATE_BIT) &&  (type_options & TEMPLATE_BIT))
                           errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
                     if(( s->attr & PROCESSORS_BIT) &&  (type_options & PROCESSORS_BIT))
                           errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
	             s->attr = s->attr | type_options;
                     if((yyvsp[(6) - (6)].ll_node)) s->attr = s->attr | DIMENSION_BIT;
                     if((s->attr & DISTRIBUTE_BIT) && (s->attr & ALIGN_BIT))
                       errstr("%s has the DISTRIBUTE and ALIGN attribute",s->ident, 64);
	             q = make_llnd(fi,ARRAY_REF, (yyvsp[(6) - (6)].ll_node), LLNULL, s);
	             if(p) s->type->entry.ar_decl.ranges = p;
	             r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	             (yyval.bf_node) = get_bfnd(fi,DVM_VAR_DECL, SMNULL, r, LLNULL,(yyvsp[(1) - (6)].ll_node));
	            ;}
    break;

  case 779:
#line 5890 "gram1.y"
    { PTR_SYMB s;
	             PTR_LLND q, r, p;
                     int numdim;
                    if(type_options & PROCESSORS_BIT) { /*23.10.18  || (type_options & TEMPLATE_BIT)){ */
                       if(! explicit_shape) {
                         err("Explicit shape specification is required", 50);
		         /*$$ = BFNULL;*/
	               }
                     } 
                    /* else {
                       if($4)
                         err("Shape specification is not permitted", 263);
                     } */
                     if(type_options & DIMENSION_BIT)
                       { p = attr_dims; numdim = attr_ndim;}
                     else
                       { p = LLNULL; numdim = 0; }
                     if((yyvsp[(4) - (4)].ll_node))                   /*dimension information after the object name*/
                     { p = (yyvsp[(4) - (4)].ll_node); numdim = ndim;}/*overrides the DIMENSION attribute */
	             s = make_array((yyvsp[(3) - (4)].hash_entry), TYNULL, p, numdim, LOCAL);

                     if((type_options & COMMON_BIT) && !(type_options & TEMPLATE_BIT))
                     {
                        err("Illegal combination of attributes", 63);
                        type_options = type_options & (~COMMON_BIT);
                     }
                     if((type_options & PROCESSORS_BIT) &&((type_options & ALIGN_BIT) ||(type_options & DISTRIBUTE_BIT) ||(type_options & TEMPLATE_BIT) || (type_options & DYNAMIC_BIT) ||(type_options & SHADOW_BIT) ))
                       err("Illegal combination of attributes", 63);
                     else  if((type_options & PROCESSORS_BIT) && ((s->attr & ALIGN_BIT) ||(s->attr & DISTRIBUTE_BIT) ||(s->attr & TEMPLATE_BIT) || (s->attr & DYNAMIC_BIT) ||(s->attr & SHADOW_BIT)) )
                     {  errstr("Inconsistent declaration of identifier %s", s->ident, 16);
                        type_options = type_options & (~PROCESSORS_BIT);
                     }
                     else if ((s->attr & PROCESSORS_BIT) && ((type_options & ALIGN_BIT) ||(type_options & DISTRIBUTE_BIT) ||(type_options & TEMPLATE_BIT) || (type_options & DYNAMIC_BIT) ||(type_options & SHADOW_BIT))) 
                          errstr("Inconsistent declaration of identifier  %s", s->ident,16);
                     else if ((s->attr & INHERIT_BIT) && ((type_options & ALIGN_BIT) ||(type_options & DISTRIBUTE_BIT)))
                          errstr("Inconsistent declaration of identifier %s", s->ident, 16);
                     if(( s->attr & DISTRIBUTE_BIT) &&  (type_options & DISTRIBUTE_BIT))
                          errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
                     if(( s->attr & ALIGN_BIT) &&  (type_options & ALIGN_BIT))
                          errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
                     if(( s->attr & SHADOW_BIT) &&  (type_options & SHADOW_BIT))
                          errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
                     if(( s->attr & TEMPLATE_BIT) &&  (type_options & TEMPLATE_BIT))
                          errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
                     if(( s->attr & PROCESSORS_BIT) &&  (type_options & PROCESSORS_BIT))
                          errstr( "Multiple declaration of identifier  %s ", s->ident, 73);   
	             s->attr = s->attr | type_options;
                     if((yyvsp[(4) - (4)].ll_node)) s->attr = s->attr | DIMENSION_BIT;
                     if((s->attr & DISTRIBUTE_BIT) && (s->attr & ALIGN_BIT))
                           errstr("%s has the DISTRIBUTE and ALIGN attribute",s->ident, 64);
	             q = make_llnd(fi,ARRAY_REF, (yyvsp[(4) - (4)].ll_node), LLNULL, s);
	             if(p) s->type->entry.ar_decl.ranges = p;
	             r = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);
	             add_to_lowLevelList(r, (yyvsp[(1) - (4)].bf_node)->entry.Template.ll_ptr1);
	            ;}
    break;

  case 780:
#line 5954 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); type_options = type_opt; ;}
    break;

  case 781:
#line 5956 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (4)].ll_node),(yyvsp[(4) - (4)].ll_node),EXPR_LIST); type_options = type_options | type_opt;;}
    break;

  case 782:
#line 5959 "gram1.y"
    { type_opt = TEMPLATE_BIT;
               (yyval.ll_node) = make_llnd(fi,TEMPLATE_OP,LLNULL,LLNULL,SMNULL);
               ;}
    break;

  case 783:
#line 5963 "gram1.y"
    { type_opt = PROCESSORS_BIT;
                (yyval.ll_node) = make_llnd(fi,PROCESSORS_OP,LLNULL,LLNULL,SMNULL);
               ;}
    break;

  case 784:
#line 5967 "gram1.y"
    { type_opt = PROCESSORS_BIT;
                (yyval.ll_node) = make_llnd(fi,PROCESSORS_OP,LLNULL,LLNULL,SMNULL);
               ;}
    break;

  case 785:
#line 5971 "gram1.y"
    { type_opt = DYNAMIC_BIT;
                (yyval.ll_node) = make_llnd(fi,DYNAMIC_OP,LLNULL,LLNULL,SMNULL);
               ;}
    break;

  case 786:
#line 5988 "gram1.y"
    {
                if(! explicit_shape) {
                  err("Explicit shape specification is required", 50);
                }
                if(! (yyvsp[(3) - (4)].ll_node)) {
                  err("No shape specification", 65);
	        }
                type_opt = DIMENSION_BIT;
                attr_ndim = ndim; attr_dims = (yyvsp[(3) - (4)].ll_node);
                (yyval.ll_node) = make_llnd(fi,DIMENSION_OP,(yyvsp[(3) - (4)].ll_node),LLNULL,SMNULL);
	       ;}
    break;

  case 787:
#line 6000 "gram1.y"
    { type_opt = SHADOW_BIT;
                  (yyval.ll_node) = make_llnd(fi,SHADOW_OP,(yyvsp[(2) - (2)].ll_node),LLNULL,SMNULL);
                 ;}
    break;

  case 788:
#line 6004 "gram1.y"
    { type_opt = ALIGN_BIT;
                  (yyval.ll_node) = make_llnd(fi,ALIGN_OP,(yyvsp[(3) - (7)].ll_node),(yyvsp[(7) - (7)].ll_node),SMNULL);
                 ;}
    break;

  case 789:
#line 6008 "gram1.y"
    { type_opt = ALIGN_BIT;
                  (yyval.ll_node) = make_llnd(fi,ALIGN_OP,LLNULL,SMNULL,SMNULL);
                ;}
    break;

  case 790:
#line 6018 "gram1.y"
    { 
                 type_opt = DISTRIBUTE_BIT;
                 (yyval.ll_node) = make_llnd(fi,DISTRIBUTE_OP,(yyvsp[(2) - (4)].ll_node),(yyvsp[(4) - (4)].ll_node),SMNULL);
                ;}
    break;

  case 791:
#line 6023 "gram1.y"
    { 
                 type_opt = DISTRIBUTE_BIT;
                 (yyval.ll_node) = make_llnd(fi,DISTRIBUTE_OP,LLNULL,LLNULL,SMNULL);
                ;}
    break;

  case 792:
#line 6028 "gram1.y"
    {
                 type_opt = COMMON_BIT;
                 (yyval.ll_node) = make_llnd(fi,COMMON_OP, LLNULL, LLNULL, SMNULL);
                ;}
    break;

  case 793:
#line 6035 "gram1.y"
    { 
	      PTR_LLND  l;
	      l = make_llnd(fi, TYPE_OP, LLNULL, LLNULL, SMNULL);
	      l->type = (yyvsp[(1) - (11)].data_type);
	      (yyval.bf_node) = get_bfnd(fi,DVM_POINTER_DIR, SMNULL, (yyvsp[(11) - (11)].ll_node),(yyvsp[(7) - (11)].ll_node), l);
	    ;}
    break;

  case 794:
#line 6043 "gram1.y"
    {ndim = 0;;}
    break;

  case 795:
#line 6044 "gram1.y"
    { PTR_LLND  q;
             if(ndim == maxdim)
		err("Too many dimensions", 43);
	      else if(ndim < maxdim)
		q = make_llnd(fi,DDOT,LLNULL,LLNULL,SMNULL);
	      ++ndim;
              (yyval.ll_node) = set_ll_list(q, LLNULL, EXPR_LIST);
	       /*$$ = make_llnd(fi,EXPR_LIST, q, LLNULL, SMNULL);*/
	       /*$$->type = global_default;*/
	    ;}
    break;

  case 796:
#line 6055 "gram1.y"
    { PTR_LLND  q;
             if(ndim == maxdim)
		err("Too many dimensions", 43);
	      else if(ndim < maxdim)
		q = make_llnd(fi,DDOT,LLNULL,LLNULL,SMNULL);
	      ++ndim;
              (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), q, EXPR_LIST);
            ;}
    break;

  case 797:
#line 6066 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 798:
#line 6068 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 799:
#line 6072 "gram1.y"
    {PTR_SYMB s;
           /* s = make_scalar($1,TYNULL,LOCAL);*/
            s = make_array((yyvsp[(1) - (1)].hash_entry),TYNULL,LLNULL,0,LOCAL);
            s->attr = s->attr | DVM_POINTER_BIT;
            if((s->attr & PROCESSORS_BIT) || (s->attr & TASK_BIT) || (s->attr & INHERIT_BIT))
               errstr( "Inconsistent declaration of identifier %s", s->ident, 16);     
            (yyval.ll_node) = make_llnd(fi,VAR_REF,LLNULL,LLNULL,s);
            ;}
    break;

  case 800:
#line 6083 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_HEAP_DIR, SMNULL, (yyvsp[(3) - (3)].ll_node), LLNULL, LLNULL);;}
    break;

  case 801:
#line 6087 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 802:
#line 6089 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 803:
#line 6093 "gram1.y"
    {  PTR_SYMB s;
	      s = make_array((yyvsp[(1) - (1)].hash_entry), TYNULL, LLNULL, 0, LOCAL);
              s->attr = s->attr | HEAP_BIT;
              if((s->attr & PROCESSORS_BIT) ||(s->attr & TASK_BIT)  || (s->attr & TEMPLATE_BIT) || (s->attr & ALIGN_BIT) || (s->attr & DISTRIBUTE_BIT) || (s->attr & INHERIT_BIT) || (s->attr & DYNAMIC_BIT) || (s->attr & SHADOW_BIT) || (s->attr & DVM_POINTER_BIT)) 
                errstr("Inconsistent declaration of identifier  %s", s->ident, 16); 
      
	      (yyval.ll_node) = make_llnd(fi,ARRAY_REF, LLNULL, LLNULL, s);
	   ;}
    break;

  case 804:
#line 6104 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_CONSISTENT_DIR, SMNULL, (yyvsp[(3) - (3)].ll_node), LLNULL, LLNULL);;}
    break;

  case 805:
#line 6108 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 806:
#line 6110 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 807:
#line 6114 "gram1.y"
    {  PTR_SYMB s;
	      s = make_array((yyvsp[(1) - (1)].hash_entry), TYNULL, LLNULL, 0, LOCAL);
              s->attr = s->attr | CONSISTENT_BIT;
              if((s->attr & PROCESSORS_BIT) ||(s->attr & TASK_BIT)  || (s->attr & TEMPLATE_BIT) || (s->attr & ALIGN_BIT) || (s->attr & DISTRIBUTE_BIT) || (s->attr & INHERIT_BIT) || (s->attr & DYNAMIC_BIT) || (s->attr & SHADOW_BIT) || (s->attr & DVM_POINTER_BIT)) 
                errstr("Inconsistent declaration of identifier  %s", s->ident, 16); 
      
	      (yyval.ll_node) = make_llnd(fi,ARRAY_REF, LLNULL, LLNULL, s);
	   ;}
    break;

  case 808:
#line 6126 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_ASYNCID_DIR, SMNULL, (yyvsp[(3) - (3)].ll_node), LLNULL, LLNULL);;}
    break;

  case 809:
#line 6128 "gram1.y"
    { PTR_LLND p;
              p = make_llnd(fi,COMM_LIST, LLNULL, LLNULL, SMNULL);              
              (yyval.bf_node) = get_bfnd(fi,DVM_ASYNCID_DIR, SMNULL, (yyvsp[(8) - (8)].ll_node), p, LLNULL);
            ;}
    break;

  case 810:
#line 6135 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 811:
#line 6137 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 812:
#line 6141 "gram1.y"
    {  PTR_SYMB s;
              if((yyvsp[(2) - (2)].ll_node)){
                  s = make_array((yyvsp[(1) - (2)].hash_entry), global_default, (yyvsp[(2) - (2)].ll_node), ndim, LOCAL);
		  s->variant = ASYNC_ID;
                  s->attr = s->attr | DIMENSION_BIT;
                  s->type->entry.ar_decl.ranges = (yyvsp[(2) - (2)].ll_node);
                  (yyval.ll_node) = make_llnd(fi,ARRAY_REF, (yyvsp[(2) - (2)].ll_node), LLNULL, s);
              } else {
              s = make_local_entity((yyvsp[(1) - (2)].hash_entry), ASYNC_ID, global_default, LOCAL);
	      (yyval.ll_node) = make_llnd(fi,VAR_REF, LLNULL, LLNULL, s);
              }
	   ;}
    break;

  case 813:
#line 6157 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_NEW_VALUE_DIR,SMNULL, LLNULL, LLNULL,LLNULL);;}
    break;

  case 814:
#line 6167 "gram1.y"
    {  if((yyvsp[(6) - (7)].ll_node) &&  (yyvsp[(6) - (7)].ll_node)->entry.Template.symbol->attr & TASK_BIT)
                        (yyval.bf_node) = get_bfnd(fi,DVM_PARALLEL_TASK_DIR,SMNULL,(yyvsp[(6) - (7)].ll_node),(yyvsp[(7) - (7)].ll_node),(yyvsp[(4) - (7)].ll_node));
                    else
                        (yyval.bf_node) = get_bfnd(fi,DVM_PARALLEL_ON_DIR,SMNULL,(yyvsp[(6) - (7)].ll_node),(yyvsp[(7) - (7)].ll_node),(yyvsp[(4) - (7)].ll_node));
                 ;}
    break;

  case 815:
#line 6176 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 816:
#line 6178 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 818:
#line 6185 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(3) - (3)].ll_node);;}
    break;

  case 819:
#line 6188 "gram1.y"
    { (yyval.ll_node) = LLNULL; opt_kwd_ = NO;;}
    break;

  case 820:
#line 6193 "gram1.y"
    {
          if((yyvsp[(1) - (4)].ll_node)->type->variant != T_ARRAY) 
           errstr("'%s' isn't array", (yyvsp[(1) - (4)].ll_node)->entry.Template.symbol->ident, 66);
                 (yyvsp[(1) - (4)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(3) - (4)].ll_node);
                 (yyval.ll_node) = (yyvsp[(1) - (4)].ll_node);
                 (yyval.ll_node)->type = (yyvsp[(1) - (4)].ll_node)->type->entry.ar_decl.base_type;
         ;}
    break;

  case 821:
#line 6203 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 822:
#line 6205 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 823:
#line 6209 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 824:
#line 6211 "gram1.y"
    {
             (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
             (yyval.ll_node)->entry.string_val = (char *) "*";
             (yyval.ll_node)->type = global_string;
            ;}
    break;

  case 825:
#line 6219 "gram1.y"
    {  (yyval.ll_node) = LLNULL;;}
    break;

  case 826:
#line 6221 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 827:
#line 6225 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 828:
#line 6227 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (2)].ll_node),(yyvsp[(2) - (2)].ll_node),EXPR_LIST); ;}
    break;

  case 839:
#line 6244 "gram1.y"
    { if((yyvsp[(5) - (8)].symbol)->attr & INDIRECT_BIT)
                            errstr("'%s' is not remote group name", (yyvsp[(5) - (8)].symbol)->ident, 68);
                          (yyval.ll_node) = make_llnd(fi,REMOTE_ACCESS_OP,(yyvsp[(7) - (8)].ll_node),LLNULL,(yyvsp[(5) - (8)].symbol));
                        ;}
    break;

  case 840:
#line 6249 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,REMOTE_ACCESS_OP,(yyvsp[(5) - (6)].ll_node),LLNULL,SMNULL);;}
    break;

  case 841:
#line 6253 "gram1.y"
    {
                          (yyval.ll_node) = make_llnd(fi,CONSISTENT_OP,(yyvsp[(7) - (8)].ll_node),LLNULL,(yyvsp[(5) - (8)].symbol));
                        ;}
    break;

  case 842:
#line 6257 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,CONSISTENT_OP,(yyvsp[(5) - (6)].ll_node),LLNULL,SMNULL);;}
    break;

  case 843:
#line 6261 "gram1.y"
    {  
            if(((yyval.symbol)=(yyvsp[(1) - (1)].hash_entry)->id_attr) == SMNULL){
                errstr("'%s' is not declared as group", (yyvsp[(1) - (1)].hash_entry)->ident, 74);
                (yyval.symbol) = make_local_entity((yyvsp[(1) - (1)].hash_entry),CONSISTENT_GROUP_NAME,global_default,LOCAL);
            } else {
                if((yyval.symbol)->variant != CONSISTENT_GROUP_NAME)
                   errstr("'%s' is not declared as group", (yyvsp[(1) - (1)].hash_entry)->ident, 74);
            }
          ;}
    break;

  case 844:
#line 6274 "gram1.y"
    {(yyval.ll_node) = make_llnd(fi,NEW_SPEC_OP,(yyvsp[(5) - (6)].ll_node),LLNULL,SMNULL);;}
    break;

  case 845:
#line 6278 "gram1.y"
    {(yyval.ll_node) = make_llnd(fi,NEW_SPEC_OP,(yyvsp[(5) - (6)].ll_node),LLNULL,SMNULL);;}
    break;

  case 846:
#line 6282 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_PRIVATE_OP,(yyvsp[(5) - (6)].ll_node),LLNULL,SMNULL);;}
    break;

  case 847:
#line 6286 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_CUDA_BLOCK_OP,(yyvsp[(5) - (6)].ll_node),LLNULL,SMNULL);;}
    break;

  case 848:
#line 6289 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST);;}
    break;

  case 849:
#line 6291 "gram1.y"
    {(yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST);;}
    break;

  case 850:
#line 6293 "gram1.y"
    {(yyval.ll_node) = set_ll_list((yyvsp[(1) - (5)].ll_node),(yyvsp[(3) - (5)].ll_node),EXPR_LIST); (yyval.ll_node) = set_ll_list((yyval.ll_node),(yyvsp[(5) - (5)].ll_node),EXPR_LIST);;}
    break;

  case 851:
#line 6297 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST);;}
    break;

  case 852:
#line 6299 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST);;}
    break;

  case 853:
#line 6303 "gram1.y"
    { if(!((yyvsp[(5) - (8)].symbol)->attr & INDIRECT_BIT))
                         errstr("'%s' is not indirect group name", (yyvsp[(5) - (8)].symbol)->ident, 313);
                      (yyval.ll_node) = make_llnd(fi,INDIRECT_ACCESS_OP,(yyvsp[(7) - (8)].ll_node),LLNULL,(yyvsp[(5) - (8)].symbol));
                    ;}
    break;

  case 854:
#line 6308 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,INDIRECT_ACCESS_OP,(yyvsp[(5) - (6)].ll_node),LLNULL,SMNULL);;}
    break;

  case 855:
#line 6312 "gram1.y"
    {(yyval.ll_node) = make_llnd(fi,STAGE_OP,(yyvsp[(5) - (6)].ll_node),LLNULL,SMNULL);;}
    break;

  case 856:
#line 6316 "gram1.y"
    {(yyval.ll_node) = make_llnd(fi,ACROSS_OP,(yyvsp[(4) - (4)].ll_node),LLNULL,SMNULL);;}
    break;

  case 857:
#line 6318 "gram1.y"
    {(yyval.ll_node) = make_llnd(fi,ACROSS_OP,(yyvsp[(4) - (5)].ll_node),(yyvsp[(5) - (5)].ll_node),SMNULL);;}
    break;

  case 858:
#line 6322 "gram1.y"
    {  if((yyvsp[(3) - (5)].ll_node))
                     (yyval.ll_node) = make_llnd(fi,DDOT,(yyvsp[(3) - (5)].ll_node),(yyvsp[(4) - (5)].ll_node),SMNULL);
                   else
                     (yyval.ll_node) = (yyvsp[(4) - (5)].ll_node);
                ;}
    break;

  case 859:
#line 6330 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "in";
              (yyval.ll_node)->type = global_string;
            ;}
    break;

  case 860:
#line 6336 "gram1.y"
    {
	      (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "out";
              (yyval.ll_node)->type = global_string;
            ;}
    break;

  case 861:
#line 6342 "gram1.y"
    {  (yyval.ll_node) = LLNULL; opt_kwd_ = NO;;}
    break;

  case 862:
#line 6346 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST);;}
    break;

  case 863:
#line 6348 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST);;}
    break;

  case 864:
#line 6352 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 865:
#line 6354 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (4)].ll_node);
                    (yyval.ll_node)-> entry.Template.ll_ptr1 = (yyvsp[(3) - (4)].ll_node);  
                  ;}
    break;

  case 866:
#line 6358 "gram1.y"
    { /*  PTR_LLND p;
                       p = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
                       p->entry.string_val = (char *) "corner";
                       p->type = global_string;
                   */
                   (yyvsp[(1) - (7)].ll_node)-> entry.Template.ll_ptr1 = (yyvsp[(3) - (7)].ll_node);  
                   (yyval.ll_node) = make_llnd(fi,ARRAY_OP,(yyvsp[(1) - (7)].ll_node),(yyvsp[(6) - (7)].ll_node),SMNULL);
                 ;}
    break;

  case 867:
#line 6370 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST);;}
    break;

  case 868:
#line 6372 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST);;}
    break;

  case 869:
#line 6376 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT, (yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), SMNULL);;}
    break;

  case 870:
#line 6380 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST);;}
    break;

  case 871:
#line 6382 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST);;}
    break;

  case 872:
#line 6386 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,(yyvsp[(1) - (5)].ll_node),make_llnd(fi,DDOT,(yyvsp[(3) - (5)].ll_node),(yyvsp[(5) - (5)].ll_node),SMNULL),SMNULL); ;}
    break;

  case 873:
#line 6388 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,(yyvsp[(1) - (3)].ll_node),make_llnd(fi,DDOT,(yyvsp[(3) - (3)].ll_node),LLNULL,SMNULL),SMNULL); ;}
    break;

  case 874:
#line 6390 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,(yyvsp[(1) - (3)].ll_node),make_llnd(fi,DDOT,LLNULL,(yyvsp[(3) - (3)].ll_node),SMNULL),SMNULL); ;}
    break;

  case 875:
#line 6392 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,(yyvsp[(1) - (1)].ll_node),LLNULL,SMNULL); ;}
    break;

  case 876:
#line 6394 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,LLNULL,make_llnd(fi,DDOT,(yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),SMNULL),SMNULL); ;}
    break;

  case 877:
#line 6396 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,LLNULL,make_llnd(fi,DDOT,(yyvsp[(1) - (1)].ll_node),LLNULL,SMNULL),SMNULL); ;}
    break;

  case 878:
#line 6398 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT,LLNULL,make_llnd(fi,DDOT,LLNULL,(yyvsp[(1) - (1)].ll_node),SMNULL),SMNULL); ;}
    break;

  case 879:
#line 6402 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(3) - (3)].ll_node);;}
    break;

  case 880:
#line 6406 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(3) - (3)].ll_node);;}
    break;

  case 881:
#line 6410 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(3) - (3)].ll_node);;}
    break;

  case 882:
#line 6414 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(2) - (3)].ll_node);;}
    break;

  case 883:
#line 6418 "gram1.y"
    {PTR_LLND q;
                /* q = set_ll_list($9,$6,EXPR_LIST); */
                 q = set_ll_list((yyvsp[(6) - (10)].ll_node),LLNULL,EXPR_LIST); /*podd 11.10.01*/
                 q = add_to_lowLevelList((yyvsp[(9) - (10)].ll_node),q);        /*podd 11.10.01*/
                 (yyval.ll_node) = make_llnd(fi,REDUCTION_OP,q,LLNULL,SMNULL);
                ;}
    break;

  case 884:
#line 6425 "gram1.y"
    {PTR_LLND q;
                 q = set_ll_list((yyvsp[(6) - (8)].ll_node),LLNULL,EXPR_LIST);
                 (yyval.ll_node) = make_llnd(fi,REDUCTION_OP,q,LLNULL,SMNULL);
                ;}
    break;

  case 885:
#line 6431 "gram1.y"
    {  (yyval.ll_node) = make_llnd(fi,REDUCTION_OP,(yyvsp[(9) - (10)].ll_node),LLNULL,(yyvsp[(6) - (10)].symbol)); ;}
    break;

  case 886:
#line 6435 "gram1.y"
    { opt_kwd_r = YES; ;}
    break;

  case 887:
#line 6438 "gram1.y"
    { opt_kwd_r = NO; ;}
    break;

  case 888:
#line 6442 "gram1.y"
    { 
                  if(((yyval.symbol)=(yyvsp[(1) - (1)].hash_entry)->id_attr) == SMNULL) {
                      errstr("'%s' is not declared as reduction group", (yyvsp[(1) - (1)].hash_entry)->ident, 69);
                      (yyval.symbol) = make_local_entity((yyvsp[(1) - (1)].hash_entry),REDUCTION_GROUP_NAME,global_default,LOCAL);
                  } else {
                    if((yyval.symbol)->variant != REDUCTION_GROUP_NAME)
                      errstr("'%s' is not declared as reduction group", (yyvsp[(1) - (1)].hash_entry)->ident, 69);
                  }
                ;}
    break;

  case 889:
#line 6455 "gram1.y"
    {(yyval.ll_node) = set_ll_list((yyvsp[(2) - (2)].ll_node),LLNULL,EXPR_LIST);;}
    break;

  case 890:
#line 6457 "gram1.y"
    {(yyval.ll_node) = set_ll_list((yyvsp[(1) - (4)].ll_node),(yyvsp[(4) - (4)].ll_node),EXPR_LIST);;}
    break;

  case 891:
#line 6461 "gram1.y"
    {(yyval.ll_node) = make_llnd(fi,ARRAY_OP,(yyvsp[(1) - (4)].ll_node),(yyvsp[(3) - (4)].ll_node),SMNULL);;}
    break;

  case 892:
#line 6463 "gram1.y"
    {(yyvsp[(3) - (6)].ll_node) = set_ll_list((yyvsp[(3) - (6)].ll_node),(yyvsp[(5) - (6)].ll_node),EXPR_LIST);
            (yyval.ll_node) = make_llnd(fi,ARRAY_OP,(yyvsp[(1) - (6)].ll_node),(yyvsp[(3) - (6)].ll_node),SMNULL);;}
    break;

  case 893:
#line 6468 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "sum";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 894:
#line 6474 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "product";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 895:
#line 6480 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "min";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 896:
#line 6486 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "max";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 897:
#line 6492 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "or";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 898:
#line 6498 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "and";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 899:
#line 6504 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "eqv";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 900:
#line 6510 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "neqv";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 901:
#line 6516 "gram1.y"
    { err("Illegal reduction operation name", 70);
               errcnt--;
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "unknown";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 902:
#line 6525 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "maxloc";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 903:
#line 6531 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "minloc";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 904:
#line 6548 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,SHADOW_RENEW_OP,(yyvsp[(5) - (6)].ll_node),LLNULL,SMNULL);;}
    break;

  case 905:
#line 6556 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,SHADOW_START_OP,LLNULL,LLNULL,(yyvsp[(4) - (4)].symbol));;}
    break;

  case 906:
#line 6564 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,SHADOW_WAIT_OP,LLNULL,LLNULL,(yyvsp[(4) - (4)].symbol));;}
    break;

  case 907:
#line 6566 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,SHADOW_COMP_OP,LLNULL,LLNULL,SMNULL);;}
    break;

  case 908:
#line 6568 "gram1.y"
    {  (yyvsp[(5) - (9)].ll_node)-> entry.Template.ll_ptr1 = (yyvsp[(7) - (9)].ll_node); (yyval.ll_node) = make_llnd(fi,SHADOW_COMP_OP,(yyvsp[(5) - (9)].ll_node),LLNULL,SMNULL);;}
    break;

  case 909:
#line 6572 "gram1.y"
    {(yyval.symbol) = make_local_entity((yyvsp[(1) - (1)].hash_entry), SHADOW_GROUP_NAME,global_default,LOCAL);;}
    break;

  case 910:
#line 6576 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST);;}
    break;

  case 911:
#line 6578 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST);;}
    break;

  case 912:
#line 6582 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 913:
#line 6584 "gram1.y"
    { PTR_LLND p;
          p = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
          p->entry.string_val = (char *) "corner";
          p->type = global_string;
          (yyval.ll_node) = make_llnd(fi,ARRAY_OP,(yyvsp[(1) - (5)].ll_node),p,SMNULL);
         ;}
    break;

  case 914:
#line 6592 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (5)].ll_node);
          (yyval.ll_node)-> entry.Template.ll_ptr1 = (yyvsp[(4) - (5)].ll_node);  
        ;}
    break;

  case 915:
#line 6596 "gram1.y"
    { PTR_LLND p;
          p = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
          p->entry.string_val = (char *) "corner";
          p->type = global_string;
          (yyvsp[(1) - (9)].ll_node)-> entry.Template.ll_ptr1 = (yyvsp[(4) - (9)].ll_node);  
          (yyval.ll_node) = make_llnd(fi,ARRAY_OP,(yyvsp[(1) - (9)].ll_node),p,SMNULL);
       ;}
    break;

  case 916:
#line 6607 "gram1.y"
    { optcorner = YES; ;}
    break;

  case 917:
#line 6611 "gram1.y"
    { PTR_SYMB s;
         s = (yyvsp[(1) - (1)].ll_node)->entry.Template.symbol;
         if(s->attr & PROCESSORS_BIT)
             errstr( "Illegal use of PROCESSORS name %s ", s->ident, 53);
         else if(s->attr & TASK_BIT)
             errstr( "Illegal use of task array name %s ", s->ident, 71);
         else
           if(s->type->variant != T_ARRAY) 
             errstr("'%s' isn't array", s->ident, 66);
           else 
              if((!(s->attr & DISTRIBUTE_BIT)) && (!(s->attr & ALIGN_BIT)))
               ; /*errstr("hpf.gram: %s is not distributed array", s->ident);*/
                
         (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);
        ;}
    break;

  case 918:
#line 6629 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 919:
#line 6631 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 920:
#line 6635 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_SHADOW_START_DIR,(yyvsp[(3) - (3)].symbol),LLNULL,LLNULL,LLNULL);;}
    break;

  case 921:
#line 6637 "gram1.y"
    {errstr("Missing DVM directive prefix", 49);;}
    break;

  case 922:
#line 6641 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_SHADOW_WAIT_DIR,(yyvsp[(3) - (3)].symbol),LLNULL,LLNULL,LLNULL);;}
    break;

  case 923:
#line 6643 "gram1.y"
    {errstr("Missing DVM directive prefix", 49);;}
    break;

  case 924:
#line 6647 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_SHADOW_GROUP_DIR,(yyvsp[(3) - (6)].symbol),(yyvsp[(5) - (6)].ll_node),LLNULL,LLNULL);;}
    break;

  case 925:
#line 6651 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_REDUCTION_START_DIR,(yyvsp[(3) - (3)].symbol),LLNULL,LLNULL,LLNULL);;}
    break;

  case 926:
#line 6655 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_REDUCTION_WAIT_DIR,(yyvsp[(3) - (3)].symbol),LLNULL,LLNULL,LLNULL);;}
    break;

  case 927:
#line 6664 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_CONSISTENT_START_DIR,(yyvsp[(3) - (3)].symbol),LLNULL,LLNULL,LLNULL);;}
    break;

  case 928:
#line 6668 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_CONSISTENT_WAIT_DIR,(yyvsp[(3) - (3)].symbol),LLNULL,LLNULL,LLNULL);;}
    break;

  case 929:
#line 6672 "gram1.y"
    { if(((yyvsp[(4) - (7)].symbol)->attr & INDIRECT_BIT))
                errstr("'%s' is not remote group name", (yyvsp[(4) - (7)].symbol)->ident, 68);
           (yyval.bf_node) = get_bfnd(fi,DVM_REMOTE_ACCESS_DIR,(yyvsp[(4) - (7)].symbol),(yyvsp[(6) - (7)].ll_node),LLNULL,LLNULL);
         ;}
    break;

  case 930:
#line 6677 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_REMOTE_ACCESS_DIR,SMNULL,(yyvsp[(4) - (5)].ll_node),LLNULL,LLNULL);;}
    break;

  case 931:
#line 6681 "gram1.y"
    {  
            if(((yyval.symbol)=(yyvsp[(1) - (1)].hash_entry)->id_attr) == SMNULL){
                errstr("'%s' is not declared as group", (yyvsp[(1) - (1)].hash_entry)->ident, 74);
                (yyval.symbol) = make_local_entity((yyvsp[(1) - (1)].hash_entry),REF_GROUP_NAME,global_default,LOCAL);
            } else {
              if((yyval.symbol)->variant != REF_GROUP_NAME)
                errstr("'%s' is not declared as group", (yyvsp[(1) - (1)].hash_entry)->ident, 74);
            }
          ;}
    break;

  case 932:
#line 6693 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 933:
#line 6695 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 934:
#line 6699 "gram1.y"
    {
              (yyval.ll_node) = (yyvsp[(1) - (4)].ll_node);
              (yyval.ll_node)->entry.Template.ll_ptr1 = (yyvsp[(3) - (4)].ll_node);
            ;}
    break;

  case 935:
#line 6704 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 936:
#line 6708 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 937:
#line 6710 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 938:
#line 6714 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 939:
#line 6716 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT, LLNULL, LLNULL, SMNULL);;}
    break;

  case 940:
#line 6720 "gram1.y"
    {  PTR_LLND q;
             q = make_llnd(fi,EXPR_LIST, (yyvsp[(3) - (3)].ll_node), LLNULL, SMNULL);
             (yyval.bf_node) = get_bfnd(fi,DVM_TASK_DIR,SMNULL,q,LLNULL,LLNULL);
          ;}
    break;

  case 941:
#line 6725 "gram1.y"
    {   PTR_LLND q;
              q = make_llnd(fi,EXPR_LIST, (yyvsp[(3) - (3)].ll_node), LLNULL, SMNULL);
	      add_to_lowLevelList(q, (yyvsp[(1) - (3)].bf_node)->entry.Template.ll_ptr1);
          ;}
    break;

  case 942:
#line 6732 "gram1.y"
    { 
             PTR_SYMB s;
	      s = make_array((yyvsp[(1) - (2)].hash_entry), global_int, (yyvsp[(2) - (2)].ll_node), ndim, LOCAL);
              if((yyvsp[(2) - (2)].ll_node)){
                  s->attr = s->attr | DIMENSION_BIT;
                  s->type->entry.ar_decl.ranges = (yyvsp[(2) - (2)].ll_node);
              }
              else
                  err("No dimensions in TASK directive", 75);
              if(ndim > 1)
                  errstr("Illegal rank of '%s'", s->ident, 76);
              if(s->attr & TASK_BIT)
                errstr( "Multiple declaration of identifier  %s ", s->ident, 73);
              if((s->attr & ALIGN_BIT) ||(s->attr & DISTRIBUTE_BIT) ||(s->attr & TEMPLATE_BIT) || (s->attr & DYNAMIC_BIT) ||(s->attr & SHADOW_BIT) || (s->attr & PROCESSORS_BIT)  || (s->attr & DVM_POINTER_BIT) || (s->attr & INHERIT_BIT))
                errstr("Inconsistent declaration of identifier  %s", s->ident, 16);
              else
	        s->attr = s->attr | TASK_BIT;
    
	      (yyval.ll_node) = make_llnd(fi,ARRAY_REF, (yyvsp[(2) - (2)].ll_node), LLNULL, s);	  
	    ;}
    break;

  case 943:
#line 6755 "gram1.y"
    {(yyval.bf_node) = get_bfnd(fi,DVM_TASK_REGION_DIR,(yyvsp[(3) - (3)].symbol),LLNULL,LLNULL,LLNULL);;}
    break;

  case 944:
#line 6757 "gram1.y"
    {(yyval.bf_node) = get_bfnd(fi,DVM_TASK_REGION_DIR,(yyvsp[(3) - (4)].symbol),(yyvsp[(4) - (4)].ll_node),LLNULL,LLNULL);;}
    break;

  case 945:
#line 6759 "gram1.y"
    {(yyval.bf_node) = get_bfnd(fi,DVM_TASK_REGION_DIR,(yyvsp[(3) - (4)].symbol),LLNULL,(yyvsp[(4) - (4)].ll_node),LLNULL);;}
    break;

  case 946:
#line 6761 "gram1.y"
    {(yyval.bf_node) = get_bfnd(fi,DVM_TASK_REGION_DIR,(yyvsp[(3) - (5)].symbol),(yyvsp[(4) - (5)].ll_node),(yyvsp[(5) - (5)].ll_node),LLNULL);;}
    break;

  case 947:
#line 6763 "gram1.y"
    {(yyval.bf_node) = get_bfnd(fi,DVM_TASK_REGION_DIR,(yyvsp[(3) - (5)].symbol),(yyvsp[(5) - (5)].ll_node),(yyvsp[(4) - (5)].ll_node),LLNULL);;}
    break;

  case 948:
#line 6767 "gram1.y"
    { PTR_SYMB s;
              if((s=(yyvsp[(1) - (1)].hash_entry)->id_attr) == SMNULL)
                s = make_array((yyvsp[(1) - (1)].hash_entry), TYNULL, LLNULL, 0, LOCAL);
              
              if(!(s->attr & TASK_BIT))
                 errstr("'%s' is not task array", s->ident, 77);
              (yyval.symbol) = s;
              ;}
    break;

  case 949:
#line 6778 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_END_TASK_REGION_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 950:
#line 6782 "gram1.y"
    {  PTR_SYMB s;
              PTR_LLND q;
             /*
              s = make_array($1, TYNULL, LLNULL, 0, LOCAL);                           
	      if((parstate == INEXEC) && !(s->attr & TASK_BIT))
                 errstr("'%s' is not task array", s->ident, 77);  
              q =  set_ll_list($3,LLNULL,EXPR_LIST);
	      $$ = make_llnd(fi,ARRAY_REF, q, LLNULL, s);
              */

              s = (yyvsp[(1) - (4)].symbol);
              q =  set_ll_list((yyvsp[(3) - (4)].ll_node),LLNULL,EXPR_LIST);
	      (yyval.ll_node) = make_llnd(fi,ARRAY_REF, q, LLNULL, s);
	   ;}
    break;

  case 951:
#line 6797 "gram1.y"
    {  PTR_LLND q; 
              q =  set_ll_list((yyvsp[(3) - (4)].ll_node),LLNULL,EXPR_LIST);
	      (yyval.ll_node) = make_llnd(fi,ARRAY_REF, q, LLNULL, (yyvsp[(1) - (4)].symbol));
	   ;}
    break;

  case 952:
#line 6804 "gram1.y"
    {              
         (yyval.bf_node) = get_bfnd(fi,DVM_ON_DIR,SMNULL,(yyvsp[(3) - (4)].ll_node),(yyvsp[(4) - (4)].ll_node),LLNULL);
    ;}
    break;

  case 953:
#line 6810 "gram1.y"
    {(yyval.ll_node) = LLNULL;;}
    break;

  case 954:
#line 6812 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 955:
#line 6816 "gram1.y"
    {(yyval.bf_node) = get_bfnd(fi,DVM_END_ON_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 956:
#line 6820 "gram1.y"
    { PTR_LLND q;
        /* if(!($6->attr & PROCESSORS_BIT))
           errstr("'%s' is not processor array", $6->ident, 67);
         */
        q = make_llnd(fi,ARRAY_REF, (yyvsp[(7) - (7)].ll_node), LLNULL, (yyvsp[(6) - (7)].symbol));
        (yyval.bf_node) = get_bfnd(fi,DVM_MAP_DIR,SMNULL,(yyvsp[(3) - (7)].ll_node),q,LLNULL);
      ;}
    break;

  case 957:
#line 6828 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_MAP_DIR,SMNULL,(yyvsp[(3) - (6)].ll_node),LLNULL,(yyvsp[(6) - (6)].ll_node)); ;}
    break;

  case 958:
#line 6832 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_PREFETCH_DIR,(yyvsp[(3) - (3)].symbol),LLNULL,LLNULL,LLNULL);;}
    break;

  case 959:
#line 6836 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_RESET_DIR,(yyvsp[(3) - (3)].symbol),LLNULL,LLNULL,LLNULL);;}
    break;

  case 960:
#line 6844 "gram1.y"
    { if(!((yyvsp[(4) - (7)].symbol)->attr & INDIRECT_BIT))
                         errstr("'%s' is not indirect group name", (yyvsp[(4) - (7)].symbol)->ident, 313);
                      (yyval.bf_node) = get_bfnd(fi,DVM_INDIRECT_ACCESS_DIR,(yyvsp[(4) - (7)].symbol),(yyvsp[(6) - (7)].ll_node),LLNULL,LLNULL);
                    ;}
    break;

  case 961:
#line 6849 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_INDIRECT_ACCESS_DIR,SMNULL,(yyvsp[(4) - (5)].ll_node),LLNULL,LLNULL);;}
    break;

  case 962:
#line 6863 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 963:
#line 6865 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 964:
#line 6869 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 965:
#line 6871 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (4)].ll_node); (yyval.ll_node)->entry.Template.ll_ptr1 = (yyvsp[(3) - (4)].ll_node);;}
    break;

  case 966:
#line 6880 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,HPF_INDEPENDENT_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 967:
#line 6882 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,HPF_INDEPENDENT_DIR,SMNULL, (yyvsp[(3) - (3)].ll_node), LLNULL, LLNULL);;}
    break;

  case 968:
#line 6884 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,HPF_INDEPENDENT_DIR,SMNULL, LLNULL, (yyvsp[(3) - (3)].ll_node), LLNULL);;}
    break;

  case 969:
#line 6886 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,HPF_INDEPENDENT_DIR,SMNULL, (yyvsp[(3) - (4)].ll_node), (yyvsp[(4) - (4)].ll_node),LLNULL);;}
    break;

  case 970:
#line 6922 "gram1.y"
    {(yyval.ll_node) = make_llnd(fi,REDUCTION_OP,(yyvsp[(5) - (6)].ll_node),LLNULL,SMNULL);;}
    break;

  case 971:
#line 6926 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_ASYNCHRONOUS_DIR,SMNULL,(yyvsp[(3) - (3)].ll_node),LLNULL,LLNULL);;}
    break;

  case 972:
#line 6930 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_ENDASYNCHRONOUS_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 973:
#line 6934 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_ASYNCWAIT_DIR,SMNULL,(yyvsp[(3) - (3)].ll_node),LLNULL,LLNULL);;}
    break;

  case 974:
#line 6938 "gram1.y"
    {  
            if(((yyval.symbol)=(yyvsp[(1) - (1)].hash_entry)->id_attr) == SMNULL) {
                errstr("'%s' is not declared as ASYNCID", (yyvsp[(1) - (1)].hash_entry)->ident, 115);
                (yyval.symbol) = make_local_entity((yyvsp[(1) - (1)].hash_entry),ASYNC_ID,global_default,LOCAL);
            } else {
              if((yyval.symbol)->variant != ASYNC_ID)
                errstr("'%s' is not declared as ASYNCID", (yyvsp[(1) - (1)].hash_entry)->ident, 115);
            }
     ;}
    break;

  case 975:
#line 6950 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,VAR_REF, LLNULL, LLNULL, (yyvsp[(1) - (1)].symbol));;}
    break;

  case 976:
#line 6952 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ARRAY_REF, (yyvsp[(3) - (4)].ll_node), LLNULL, (yyvsp[(1) - (4)].symbol));;}
    break;

  case 977:
#line 6956 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_F90_DIR,SMNULL,(yyvsp[(3) - (5)].ll_node),(yyvsp[(5) - (5)].ll_node),LLNULL);;}
    break;

  case 978:
#line 6959 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_DEBUG_DIR,SMNULL,(yyvsp[(3) - (3)].ll_node),LLNULL,LLNULL);;}
    break;

  case 979:
#line 6961 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_DEBUG_DIR,SMNULL,(yyvsp[(3) - (6)].ll_node),(yyvsp[(5) - (6)].ll_node),LLNULL);;}
    break;

  case 980:
#line 6965 "gram1.y"
    { 
              (yyval.ll_node) = set_ll_list((yyvsp[(2) - (2)].ll_node), LLNULL, EXPR_LIST);
              endioctl();
            ;}
    break;

  case 981:
#line 6970 "gram1.y"
    { 
              (yyval.ll_node) = set_ll_list((yyvsp[(1) - (4)].ll_node), (yyvsp[(4) - (4)].ll_node), EXPR_LIST);
              endioctl();
            ;}
    break;

  case 982:
#line 6977 "gram1.y"
    { (yyval.ll_node)  = make_llnd(fi, KEYWORD_ARG, (yyvsp[(1) - (2)].ll_node), (yyvsp[(2) - (2)].ll_node), SMNULL); ;}
    break;

  case 983:
#line 6980 "gram1.y"
    {
	         (yyval.ll_node) = make_llnd(fi,INT_VAL, LLNULL, LLNULL, SMNULL);
	         (yyval.ll_node)->entry.ival = atoi(yytext);
	         (yyval.ll_node)->type = global_int;
	        ;}
    break;

  case 984:
#line 6988 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_ENDDEBUG_DIR,SMNULL,(yyvsp[(3) - (3)].ll_node),LLNULL,LLNULL);;}
    break;

  case 985:
#line 6992 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_INTERVAL_DIR,SMNULL,(yyvsp[(3) - (3)].ll_node),LLNULL,LLNULL);;}
    break;

  case 986:
#line 6996 "gram1.y"
    { (yyval.ll_node) = LLNULL;;}
    break;

  case 987:
#line 6999 "gram1.y"
    { if((yyvsp[(1) - (1)].ll_node)->type->variant != T_INT)             
                    err("Illegal interval number", 78);
                  (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);
                 ;}
    break;

  case 988:
#line 7007 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_EXIT_INTERVAL_DIR,SMNULL,(yyvsp[(3) - (3)].ll_node),LLNULL,LLNULL);;}
    break;

  case 989:
#line 7011 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_ENDINTERVAL_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 990:
#line 7015 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_TRACEON_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 991:
#line 7019 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_TRACEOFF_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 992:
#line 7023 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_BARRIER_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 993:
#line 7027 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_CHECK_DIR,SMNULL,(yyvsp[(9) - (9)].ll_node),(yyvsp[(5) - (9)].ll_node),LLNULL); ;}
    break;

  case 994:
#line 7031 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_IO_MODE_DIR,SMNULL,(yyvsp[(4) - (5)].ll_node),LLNULL,LLNULL);;}
    break;

  case 995:
#line 7034 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 996:
#line 7036 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 997:
#line 7040 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_ASYNC_OP,LLNULL,LLNULL,SMNULL);;}
    break;

  case 998:
#line 7042 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_LOCAL_OP, LLNULL,LLNULL,SMNULL);;}
    break;

  case 999:
#line 7044 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,PARALLEL_OP, LLNULL,LLNULL,SMNULL);;}
    break;

  case 1000:
#line 7048 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_SHADOW_ADD_DIR,SMNULL,(yyvsp[(4) - (9)].ll_node),(yyvsp[(6) - (9)].ll_node),(yyvsp[(9) - (9)].ll_node)); ;}
    break;

  case 1001:
#line 7052 "gram1.y"
    {
                 if((yyvsp[(1) - (4)].ll_node)->type->variant != T_ARRAY) 
                    errstr("'%s' isn't array", (yyvsp[(1) - (4)].ll_node)->entry.Template.symbol->ident, 66);
                 if(!((yyvsp[(1) - (4)].ll_node)->entry.Template.symbol->attr & TEMPLATE_BIT))
                    errstr("'%s' isn't TEMPLATE", (yyvsp[(1) - (4)].ll_node)->entry.Template.symbol->ident, 628);
                 (yyvsp[(1) - (4)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(3) - (4)].ll_node);
                 (yyval.ll_node) = (yyvsp[(1) - (4)].ll_node);
                 /*$$->type = $1->type->entry.ar_decl.base_type;*/
               ;}
    break;

  case 1002:
#line 7064 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 1003:
#line 7066 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 1004:
#line 7070 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 1005:
#line 7072 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 1006:
#line 7076 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(2) - (2)].ll_node);;}
    break;

  case 1007:
#line 7078 "gram1.y"
    { (yyval.ll_node) = LLNULL; opt_kwd_ = NO;;}
    break;

  case 1008:
#line 7082 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_LOCALIZE_DIR,SMNULL,(yyvsp[(4) - (7)].ll_node),(yyvsp[(6) - (7)].ll_node),LLNULL); ;}
    break;

  case 1009:
#line 7086 "gram1.y"
    {
                 if((yyvsp[(1) - (1)].ll_node)->type->variant != T_ARRAY) 
                    errstr("'%s' isn't array", (yyvsp[(1) - (1)].ll_node)->entry.Template.symbol->ident, 66); 
                 (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);
                ;}
    break;

  case 1010:
#line 7092 "gram1.y"
    {
                 if((yyvsp[(1) - (4)].ll_node)->type->variant != T_ARRAY) 
                    errstr("'%s' isn't array", (yyvsp[(1) - (4)].ll_node)->entry.Template.symbol->ident, 66); 
                                 
                 (yyvsp[(1) - (4)].ll_node)->entry.Template.ll_ptr1 = (yyvsp[(3) - (4)].ll_node);
                 (yyval.ll_node) = (yyvsp[(1) - (4)].ll_node);
                 (yyval.ll_node)->type = (yyvsp[(1) - (4)].ll_node)->type->entry.ar_decl.base_type;
                ;}
    break;

  case 1011:
#line 7104 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 1012:
#line 7106 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 1013:
#line 7110 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 1014:
#line 7112 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,DDOT, LLNULL, LLNULL, SMNULL);;}
    break;

  case 1015:
#line 7116 "gram1.y"
    { 
            (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL, LLNULL, LLNULL, SMNULL);
            (yyval.ll_node)->entry.string_val = (char *) "*";
            (yyval.ll_node)->type = global_string;
          ;}
    break;

  case 1016:
#line 7124 "gram1.y"
    { 
                PTR_LLND q;
                if((yyvsp[(16) - (16)].ll_node))
                  q = make_llnd(fi,ARRAY_OP, (yyvsp[(14) - (16)].ll_node), (yyvsp[(16) - (16)].ll_node), SMNULL);
                else
                  q = (yyvsp[(14) - (16)].ll_node);                  
                (yyval.bf_node) = get_bfnd(fi,DVM_CP_CREATE_DIR,SMNULL,(yyvsp[(3) - (16)].ll_node),(yyvsp[(8) - (16)].ll_node),q); 
              ;}
    break;

  case 1017:
#line 7135 "gram1.y"
    { (yyval.ll_node) = LLNULL; ;}
    break;

  case 1018:
#line 7137 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi, PARALLEL_OP, LLNULL, LLNULL, SMNULL); ;}
    break;

  case 1019:
#line 7139 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_LOCAL_OP, LLNULL, LLNULL, SMNULL); ;}
    break;

  case 1020:
#line 7143 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_CP_LOAD_DIR,SMNULL,(yyvsp[(3) - (3)].ll_node),LLNULL,LLNULL); ;}
    break;

  case 1021:
#line 7147 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_CP_SAVE_DIR,SMNULL,(yyvsp[(3) - (3)].ll_node),LLNULL,LLNULL); ;}
    break;

  case 1022:
#line 7149 "gram1.y"
    {
                PTR_LLND q;
                q = make_llnd(fi,ACC_ASYNC_OP,LLNULL,LLNULL,SMNULL);
                (yyval.bf_node) = get_bfnd(fi,DVM_CP_SAVE_DIR,SMNULL,(yyvsp[(3) - (6)].ll_node),q,LLNULL);
              ;}
    break;

  case 1023:
#line 7157 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_CP_WAIT_DIR,SMNULL,(yyvsp[(3) - (9)].ll_node),(yyvsp[(8) - (9)].ll_node),LLNULL); ;}
    break;

  case 1024:
#line 7161 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_TEMPLATE_CREATE_DIR,SMNULL,(yyvsp[(4) - (5)].ll_node),LLNULL,LLNULL); ;}
    break;

  case 1025:
#line 7164 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 1026:
#line 7166 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); ;}
    break;

  case 1027:
#line 7170 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,DVM_TEMPLATE_DELETE_DIR,SMNULL,(yyvsp[(4) - (5)].ll_node),LLNULL,LLNULL); ;}
    break;

  case 1028:
#line 7174 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 1029:
#line 7176 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); ;}
    break;

  case 1057:
#line 7209 "gram1.y"
    {
          (yyval.bf_node) = get_bfnd(fi,OMP_ONETHREAD_DIR,SMNULL,LLNULL,LLNULL,LLNULL);
	;}
    break;

  case 1058:
#line 7215 "gram1.y"
    {
  	   (yyval.bf_node) = make_endparallel();
	;}
    break;

  case 1059:
#line 7221 "gram1.y"
    {
  	   (yyval.bf_node) = make_parallel();
           (yyval.bf_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (4)].ll_node);
	   opt_kwd_ = NO;
	;}
    break;

  case 1060:
#line 7227 "gram1.y"
    {
  	   (yyval.bf_node) = make_parallel();
	   opt_kwd_ = NO;
	;}
    break;

  case 1061:
#line 7233 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(3) - (4)].ll_node),LLNULL,EXPR_LIST);
	;}
    break;

  case 1062:
#line 7237 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(1) - (5)].ll_node),(yyvsp[(4) - (5)].ll_node),EXPR_LIST);	
	;}
    break;

  case 1072:
#line 7254 "gram1.y"
    {
		(yyval.ll_node) = (yyvsp[(4) - (5)].ll_node);
        ;}
    break;

  case 1073:
#line 7259 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,OMP_PRIVATE,(yyvsp[(2) - (2)].ll_node),LLNULL,SMNULL);
	;}
    break;

  case 1074:
#line 7264 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,OMP_FIRSTPRIVATE,(yyvsp[(2) - (2)].ll_node),LLNULL,SMNULL);
	;}
    break;

  case 1075:
#line 7270 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,OMP_LASTPRIVATE,(yyvsp[(2) - (2)].ll_node),LLNULL,SMNULL);
	;}
    break;

  case 1076:
#line 7276 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,OMP_COPYIN,(yyvsp[(2) - (2)].ll_node),LLNULL,SMNULL);
	;}
    break;

  case 1077:
#line 7282 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,OMP_SHARED,(yyvsp[(2) - (2)].ll_node),LLNULL,SMNULL);
	;}
    break;

  case 1078:
#line 7287 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,OMP_DEFAULT,(yyvsp[(4) - (5)].ll_node),LLNULL,SMNULL);
	;}
    break;

  case 1079:
#line 7293 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
		(yyval.ll_node)->entry.string_val = (char *) "private";
		(yyval.ll_node)->type = global_string;
	;}
    break;

  case 1080:
#line 7299 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
		(yyval.ll_node)->entry.string_val = (char *) "shared";
		(yyval.ll_node)->type = global_string;
	;}
    break;

  case 1081:
#line 7305 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
		(yyval.ll_node)->entry.string_val = (char *) "none";
		(yyval.ll_node)->type = global_string;
	;}
    break;

  case 1082:
#line 7312 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,OMP_IF,(yyvsp[(3) - (4)].ll_node),LLNULL,SMNULL);
	;}
    break;

  case 1083:
#line 7318 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,OMP_NUM_THREADS,(yyvsp[(3) - (4)].ll_node),LLNULL,SMNULL);
	;}
    break;

  case 1084:
#line 7324 "gram1.y"
    {
		PTR_LLND q;
		q = set_ll_list((yyvsp[(3) - (4)].ll_node),LLNULL,EXPR_LIST);
		(yyval.ll_node) = make_llnd(fi,OMP_REDUCTION,q,LLNULL,SMNULL);
	;}
    break;

  case 1085:
#line 7331 "gram1.y"
    {(yyval.ll_node) = make_llnd(fi,DDOT,(yyvsp[(2) - (4)].ll_node),(yyvsp[(4) - (4)].ll_node),SMNULL);;}
    break;

  case 1087:
#line 7337 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "+";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1088:
#line 7343 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "-";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1089:
#line 7350 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "*";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1090:
#line 7356 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "/";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1091:
#line 7362 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "min";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1092:
#line 7368 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "max";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1093:
#line 7374 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) ".or.";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1094:
#line 7380 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) ".and.";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1095:
#line 7386 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) ".eqv.";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1096:
#line 7392 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) ".neqv.";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1097:
#line 7398 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "iand";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1098:
#line 7404 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "ieor";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1099:
#line 7410 "gram1.y"
    {
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "ior";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1100:
#line 7416 "gram1.y"
    { err("Illegal reduction operation name", 70);
               errcnt--;
              (yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
              (yyval.ll_node)->entry.string_val = (char *) "unknown";
              (yyval.ll_node)->type = global_string;
             ;}
    break;

  case 1101:
#line 7426 "gram1.y"
    {
  	   (yyval.bf_node) = make_sections((yyvsp[(4) - (4)].ll_node));
	   opt_kwd_ = NO;
	;}
    break;

  case 1102:
#line 7431 "gram1.y"
    {
  	   (yyval.bf_node) = make_sections(LLNULL);
	   opt_kwd_ = NO;
	;}
    break;

  case 1103:
#line 7437 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(3) - (4)].ll_node),LLNULL,EXPR_LIST);
	;}
    break;

  case 1104:
#line 7441 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(1) - (5)].ll_node),(yyvsp[(4) - (5)].ll_node),EXPR_LIST);
	;}
    break;

  case 1109:
#line 7453 "gram1.y"
    {
		PTR_LLND q;
   	        (yyval.bf_node) = make_endsections();
		q = set_ll_list((yyvsp[(4) - (4)].ll_node),LLNULL,EXPR_LIST);
                (yyval.bf_node)->entry.Template.ll_ptr1 = q;
                opt_kwd_ = NO;
	;}
    break;

  case 1110:
#line 7461 "gram1.y"
    {
   	        (yyval.bf_node) = make_endsections();
	        opt_kwd_ = NO; 
	;}
    break;

  case 1111:
#line 7467 "gram1.y"
    {
           (yyval.bf_node) = make_ompsection();
	;}
    break;

  case 1112:
#line 7473 "gram1.y"
    {
           (yyval.bf_node) = get_bfnd(fi,OMP_DO_DIR,SMNULL,(yyvsp[(4) - (4)].ll_node),LLNULL,LLNULL);
	   opt_kwd_ = NO;
	;}
    break;

  case 1113:
#line 7478 "gram1.y"
    {
           (yyval.bf_node) = get_bfnd(fi,OMP_DO_DIR,SMNULL,LLNULL,LLNULL,LLNULL);
	   opt_kwd_ = NO;
	;}
    break;

  case 1114:
#line 7484 "gram1.y"
    {
		PTR_LLND q;
		q = set_ll_list((yyvsp[(4) - (4)].ll_node),LLNULL,EXPR_LIST);
	        (yyval.bf_node) = get_bfnd(fi,OMP_END_DO_DIR,SMNULL,q,LLNULL,LLNULL);
      	        opt_kwd_ = NO;
	;}
    break;

  case 1115:
#line 7491 "gram1.y"
    {
           (yyval.bf_node) = get_bfnd(fi,OMP_END_DO_DIR,SMNULL,LLNULL,LLNULL,LLNULL);
	   opt_kwd_ = NO;
	;}
    break;

  case 1116:
#line 7497 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(3) - (4)].ll_node),LLNULL,EXPR_LIST);
	;}
    break;

  case 1117:
#line 7501 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(1) - (5)].ll_node),(yyvsp[(4) - (5)].ll_node),EXPR_LIST);
	;}
    break;

  case 1124:
#line 7515 "gram1.y"
    {
		/*$$ = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
		$$->entry.string_val = (char *) "ORDERED";
		$$->type = global_string;*/
                (yyval.ll_node) = make_llnd(fi,OMP_ORDERED,LLNULL,LLNULL,SMNULL);
	;}
    break;

  case 1125:
#line 7524 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,OMP_SCHEDULE,(yyvsp[(4) - (7)].ll_node),(yyvsp[(6) - (7)].ll_node),SMNULL);
	;}
    break;

  case 1126:
#line 7528 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,OMP_SCHEDULE,(yyvsp[(4) - (5)].ll_node),LLNULL,SMNULL);
	;}
    break;

  case 1127:
#line 7534 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
		(yyval.ll_node)->entry.string_val = (char *) "STATIC";
		(yyval.ll_node)->type = global_string;
		
	;}
    break;

  case 1128:
#line 7541 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
		(yyval.ll_node)->entry.string_val = (char *) "DYNAMIC";
		(yyval.ll_node)->type = global_string;
		
	;}
    break;

  case 1129:
#line 7548 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
		(yyval.ll_node)->entry.string_val = (char *) "GUIDED";
		(yyval.ll_node)->type = global_string;
		
	;}
    break;

  case 1130:
#line 7555 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,KEYWORD_VAL,LLNULL,LLNULL,SMNULL);
		(yyval.ll_node)->entry.string_val = (char *) "RUNTIME";
		(yyval.ll_node)->type = global_string;
		
	;}
    break;

  case 1131:
#line 7564 "gram1.y"
    {
  	   (yyval.bf_node) = make_single();
           (yyval.bf_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (4)].ll_node);
	   opt_kwd_ = NO;
	;}
    break;

  case 1132:
#line 7570 "gram1.y"
    {
  	   (yyval.bf_node) = make_single();
	   opt_kwd_ = NO;
	;}
    break;

  case 1133:
#line 7576 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(3) - (4)].ll_node),LLNULL,EXPR_LIST);
	;}
    break;

  case 1134:
#line 7580 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(1) - (5)].ll_node),(yyvsp[(4) - (5)].ll_node),EXPR_LIST);
	;}
    break;

  case 1137:
#line 7590 "gram1.y"
    {
  	   (yyval.bf_node) = make_endsingle();
           (yyval.bf_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (4)].ll_node);
	   opt_kwd_ = NO;
	;}
    break;

  case 1138:
#line 7596 "gram1.y"
    {
  	   (yyval.bf_node) = make_endsingle();
	   opt_kwd_ = NO;
	;}
    break;

  case 1139:
#line 7602 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(3) - (4)].ll_node),LLNULL,EXPR_LIST);
	;}
    break;

  case 1140:
#line 7606 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(1) - (5)].ll_node),(yyvsp[(4) - (5)].ll_node),EXPR_LIST);
	;}
    break;

  case 1143:
#line 7617 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,OMP_COPYPRIVATE,(yyvsp[(2) - (2)].ll_node),LLNULL,SMNULL);
	;}
    break;

  case 1144:
#line 7623 "gram1.y"
    {
		(yyval.ll_node) = make_llnd(fi,OMP_NOWAIT,LLNULL,LLNULL,SMNULL);
	;}
    break;

  case 1145:
#line 7629 "gram1.y"
    {
           (yyval.bf_node) = make_workshare();
	;}
    break;

  case 1146:
#line 7634 "gram1.y"
    {
		PTR_LLND q;
   	        (yyval.bf_node) = make_endworkshare();
		q = set_ll_list((yyvsp[(4) - (4)].ll_node),LLNULL,EXPR_LIST);
                (yyval.bf_node)->entry.Template.ll_ptr1 = q;
  	        opt_kwd_ = NO;
	;}
    break;

  case 1147:
#line 7642 "gram1.y"
    {
   	        (yyval.bf_node) = make_endworkshare();
                opt_kwd_ = NO;
	;}
    break;

  case 1148:
#line 7648 "gram1.y"
    {
           (yyval.bf_node) = get_bfnd(fi,OMP_PARALLEL_DO_DIR,SMNULL,(yyvsp[(4) - (4)].ll_node),LLNULL,LLNULL);
	   opt_kwd_ = NO;
	;}
    break;

  case 1149:
#line 7653 "gram1.y"
    {
           (yyval.bf_node) = get_bfnd(fi,OMP_PARALLEL_DO_DIR,SMNULL,LLNULL,LLNULL,LLNULL);
	   opt_kwd_ = NO;
	;}
    break;

  case 1150:
#line 7660 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(3) - (4)].ll_node),LLNULL,EXPR_LIST);
	;}
    break;

  case 1151:
#line 7664 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(1) - (5)].ll_node),(yyvsp[(4) - (5)].ll_node),EXPR_LIST);
	;}
    break;

  case 1163:
#line 7684 "gram1.y"
    {
           (yyval.bf_node) = get_bfnd(fi,OMP_END_PARALLEL_DO_DIR,SMNULL,LLNULL,LLNULL,LLNULL);
	;}
    break;

  case 1164:
#line 7689 "gram1.y"
    {
           (yyval.bf_node) = make_parallelsections((yyvsp[(4) - (4)].ll_node));
	   opt_kwd_ = NO;
	;}
    break;

  case 1165:
#line 7694 "gram1.y"
    {
           (yyval.bf_node) = make_parallelsections(LLNULL);
	   opt_kwd_ = NO;
	;}
    break;

  case 1166:
#line 7701 "gram1.y"
    {
           (yyval.bf_node) = make_endparallelsections();
	;}
    break;

  case 1167:
#line 7706 "gram1.y"
    {
           (yyval.bf_node) = make_parallelworkshare();
           (yyval.bf_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (4)].ll_node);
	   opt_kwd_ = NO;
	;}
    break;

  case 1168:
#line 7712 "gram1.y"
    {
           (yyval.bf_node) = make_parallelworkshare();
	   opt_kwd_ = NO;
	;}
    break;

  case 1169:
#line 7718 "gram1.y"
    {
           (yyval.bf_node) = make_endparallelworkshare();
	;}
    break;

  case 1170:
#line 7723 "gram1.y"
    { 
	   (yyval.bf_node) = get_bfnd(fi,OMP_THREADPRIVATE_DIR, SMNULL, (yyvsp[(3) - (3)].ll_node), LLNULL, LLNULL);
	;}
    break;

  case 1171:
#line 7728 "gram1.y"
    {
  	   (yyval.bf_node) = make_master();
	;}
    break;

  case 1172:
#line 7733 "gram1.y"
    {
  	   (yyval.bf_node) = make_endmaster();
	;}
    break;

  case 1173:
#line 7737 "gram1.y"
    {
  	   (yyval.bf_node) = make_ordered();
	;}
    break;

  case 1174:
#line 7742 "gram1.y"
    {
  	   (yyval.bf_node) = make_endordered();
	;}
    break;

  case 1175:
#line 7747 "gram1.y"
    {
           (yyval.bf_node) = get_bfnd(fi,OMP_BARRIER_DIR,SMNULL,LLNULL,LLNULL,LLNULL);
	;}
    break;

  case 1176:
#line 7751 "gram1.y"
    {
           (yyval.bf_node) = get_bfnd(fi,OMP_ATOMIC_DIR,SMNULL,LLNULL,LLNULL,LLNULL);
	;}
    break;

  case 1177:
#line 7756 "gram1.y"
    {
           (yyval.bf_node) = get_bfnd(fi,OMP_FLUSH_DIR,SMNULL,(yyvsp[(3) - (3)].ll_node),LLNULL,LLNULL);
	;}
    break;

  case 1178:
#line 7760 "gram1.y"
    {
           (yyval.bf_node) = get_bfnd(fi,OMP_FLUSH_DIR,SMNULL,LLNULL,LLNULL,LLNULL);
	;}
    break;

  case 1179:
#line 7766 "gram1.y"
    {
  	   (yyval.bf_node) = make_critical();
           (yyval.bf_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (5)].ll_node);
	;}
    break;

  case 1180:
#line 7771 "gram1.y"
    {
  	   (yyval.bf_node) = make_critical();
	;}
    break;

  case 1181:
#line 7777 "gram1.y"
    {
  	   (yyval.bf_node) = make_endcritical();
           (yyval.bf_node)->entry.Template.ll_ptr1 = (yyvsp[(4) - (5)].ll_node);
	;}
    break;

  case 1182:
#line 7782 "gram1.y"
    {
  	   (yyval.bf_node) = make_endcritical();
	;}
    break;

  case 1183:
#line 7788 "gram1.y"
    { 
		PTR_SYMB s;
		PTR_LLND l;
		s = make_common((yyvsp[(2) - (5)].hash_entry)); 
		l = make_llnd(fi,VAR_REF, LLNULL, LLNULL, s);
		(yyval.ll_node) = make_llnd(fi,OMP_THREADPRIVATE, l, LLNULL, SMNULL);
	;}
    break;

  case 1184:
#line 7798 "gram1.y"
    {
		(yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST);
	;}
    break;

  case 1185:
#line 7802 "gram1.y"
    {	
		(yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST);
	;}
    break;

  case 1186:
#line 7806 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST);
	;}
    break;

  case 1187:
#line 7810 "gram1.y"
    { 
		(yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST);
	;}
    break;

  case 1188:
#line 7815 "gram1.y"
    {
		operator_slash = 1;
	;}
    break;

  case 1189:
#line 7818 "gram1.y"
    {
		operator_slash = 0;
	;}
    break;

  case 1196:
#line 7831 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi,ACC_REGION_DIR,SMNULL,(yyvsp[(3) - (3)].ll_node),LLNULL,LLNULL);;}
    break;

  case 1197:
#line 7835 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi,ACC_CHECKSECTION_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 1198:
#line 7839 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi,ACC_GET_ACTUAL_DIR,SMNULL,(yyvsp[(4) - (5)].ll_node),LLNULL,LLNULL);;}
    break;

  case 1199:
#line 7841 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi,ACC_GET_ACTUAL_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 1200:
#line 7843 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi,ACC_GET_ACTUAL_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 1201:
#line 7847 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi,ACC_ACTUAL_DIR,SMNULL,(yyvsp[(4) - (5)].ll_node),LLNULL,LLNULL);;}
    break;

  case 1202:
#line 7849 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi,ACC_ACTUAL_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 1203:
#line 7851 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi,ACC_ACTUAL_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 1204:
#line 7855 "gram1.y"
    { (yyval.ll_node) = LLNULL;;}
    break;

  case 1205:
#line 7857 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node); ;}
    break;

  case 1206:
#line 7861 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 1207:
#line 7863 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 1208:
#line 7867 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(2) - (2)].ll_node);;}
    break;

  case 1209:
#line 7870 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(2) - (2)].ll_node);;}
    break;

  case 1210:
#line 7873 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(2) - (2)].ll_node);;}
    break;

  case 1211:
#line 7878 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_INOUT_OP,(yyvsp[(3) - (4)].ll_node),LLNULL,SMNULL);;}
    break;

  case 1212:
#line 7880 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_IN_OP,(yyvsp[(3) - (4)].ll_node),LLNULL,SMNULL);;}
    break;

  case 1213:
#line 7882 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_OUT_OP,(yyvsp[(3) - (4)].ll_node),LLNULL,SMNULL);;}
    break;

  case 1214:
#line 7884 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_LOCAL_OP,(yyvsp[(3) - (4)].ll_node),LLNULL,SMNULL);;}
    break;

  case 1215:
#line 7886 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_INLOCAL_OP,(yyvsp[(3) - (4)].ll_node),LLNULL,SMNULL);;}
    break;

  case 1216:
#line 7890 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_TARGETS_OP,(yyvsp[(3) - (4)].ll_node),LLNULL,SMNULL);;}
    break;

  case 1217:
#line 7894 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_ASYNC_OP,LLNULL,LLNULL,SMNULL);;}
    break;

  case 1218:
#line 7899 "gram1.y"
    { (yyval.ll_node) = (yyvsp[(1) - (1)].ll_node);;}
    break;

  case 1219:
#line 7903 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 1220:
#line 7905 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 1221:
#line 7909 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_HOST_OP, LLNULL,LLNULL,SMNULL);;}
    break;

  case 1222:
#line 7911 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_CUDA_OP, LLNULL,LLNULL,SMNULL);;}
    break;

  case 1223:
#line 7915 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi,ACC_END_REGION_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 1224:
#line 7919 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi,ACC_END_CHECKSECTION_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 1230:
#line 7930 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi,SPF_ANALYSIS_DIR,SMNULL,(yyvsp[(3) - (4)].ll_node),LLNULL,LLNULL);;}
    break;

  case 1231:
#line 7934 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi,SPF_PARALLEL_DIR,SMNULL,(yyvsp[(3) - (4)].ll_node),LLNULL,LLNULL);;}
    break;

  case 1232:
#line 7938 "gram1.y"
    {  (yyval.bf_node) = get_bfnd(fi,SPF_TRANSFORM_DIR,SMNULL,(yyvsp[(3) - (4)].ll_node),LLNULL,LLNULL);;}
    break;

  case 1233:
#line 7942 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,SPF_PARALLEL_REG_DIR,(yyvsp[(2) - (2)].symbol),LLNULL,LLNULL,LLNULL);;}
    break;

  case 1234:
#line 7946 "gram1.y"
    { (yyval.bf_node) = get_bfnd(fi,SPF_END_PARALLEL_REG_DIR,SMNULL,LLNULL,LLNULL,LLNULL);;}
    break;

  case 1235:
#line 7950 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 1236:
#line 7952 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 1239:
#line 7960 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,REDUCTION_OP,(yyvsp[(4) - (5)].ll_node),LLNULL,SMNULL); ;}
    break;

  case 1240:
#line 7964 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACC_PRIVATE_OP,(yyvsp[(4) - (5)].ll_node),LLNULL,SMNULL);;}
    break;

  case 1241:
#line 7968 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 1242:
#line 7970 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 1246:
#line 7980 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,SHADOW_OP,(yyvsp[(4) - (5)].ll_node),LLNULL,SMNULL);;}
    break;

  case 1247:
#line 7984 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,ACROSS_OP,(yyvsp[(4) - (5)].ll_node),LLNULL,SMNULL);;}
    break;

  case 1248:
#line 7988 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,REMOTE_ACCESS_OP,(yyvsp[(4) - (5)].ll_node),LLNULL,SMNULL);;}
    break;

  case 1249:
#line 7992 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node),LLNULL,EXPR_LIST); ;}
    break;

  case 1250:
#line 7994 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node),(yyvsp[(3) - (3)].ll_node),EXPR_LIST); ;}
    break;

  case 1251:
#line 7998 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,SPF_NOINLINE_OP,LLNULL,LLNULL,SMNULL);;}
    break;

  case 1252:
#line 8000 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,SPF_FISSION_OP,(yyvsp[(4) - (5)].ll_node),LLNULL,SMNULL);;}
    break;

  case 1253:
#line 8002 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,SPF_PRIVATES_EXPANSION_OP,LLNULL,LLNULL,SMNULL);;}
    break;

  case 1254:
#line 8004 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,SPF_PRIVATES_EXPANSION_OP,(yyvsp[(4) - (5)].ll_node),LLNULL,SMNULL);;}
    break;

  case 1255:
#line 8007 "gram1.y"
    { (yyval.ll_node) = make_llnd(fi,SPF_SHRINK_OP,(yyvsp[(4) - (5)].ll_node),LLNULL,SMNULL);;}
    break;

  case 1256:
#line 8011 "gram1.y"
    { (yyval.symbol) = make_parallel_region((yyvsp[(1) - (1)].hash_entry));;}
    break;

  case 1257:
#line 8015 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (1)].ll_node), LLNULL, EXPR_LIST); ;}
    break;

  case 1258:
#line 8017 "gram1.y"
    { (yyval.ll_node) = set_ll_list((yyvsp[(1) - (3)].ll_node), (yyvsp[(3) - (3)].ll_node), EXPR_LIST); ;}
    break;


/* Line 1267 of yacc.c.  */
#line 13773 "gram1.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



