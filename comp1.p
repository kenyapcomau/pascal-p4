                                                                                PASCP    2
(*$C+,T-,D-,L-*)                                                                BOOT     2
 (**********************************************                                P        2
  *                                            *                                P        3
  *                                            *                                P        4
  *         PORTABLE PASCAL COMPILER           *                                P        5
  *         ************************           *                                P        6
  *                                            *                                P        7
  *                PASCAL P4                   *                                P        8
  *                                            *                                P        9
  *                                            *                                P       10
  *     AUTHORS:                               *                                P       11
  *              URS AMMANN                    *                                P       12
  *              KESAV NORI                    *                                P       13
  *              CHRISTIAN JACOBI              *                                P       14
  *                                            *                                P       15
  *     ADDRESS:                               *                                P       16
  *                                            *                                P       17
  *          INSTITUT FUER INFORMATIK          *                                P       18
  *          EIDG. TECHNISCHE HOCHSCHULE       *                                P       19
  *          CH-8096 ZUERICH                   *                                P       20
  *                                            *                                P       21
  *                                            *                                P       22
  *     LAST CHANGES COMPLETED IN MAY 76       *                                P       23
  *                                            *                                P       24
  *                                            *                                P       25
  **********************************************)                               P       26
                                                                                PASCP   44
                                                                                PASCP   45
PROGRAM PASCALCOMPILER(INPUT,OUTPUT,PRR);                                       PASCP   46
                                                                                PASCP   47
                                                                                PASCP   48
                                                                                PASCP   49
CONST DISPLIMIT = 20; MAXLEVEL = 10;                                            X        1
      INTSIZE = 1;                                                              INSTAL   1
      INTAL = 1;                                                                INSTAL   2
      REALSIZE = 2;                                                             INSTAL   3
      REALAL = 1;                                                               INSTAL   4
      CHARSIZE = 1;                                                             INSTAL   5
      CHARAL = 1;                                                               INSTAL   6
      CHARMAX = 1;                                                              INSTAL   7
      BOOLSIZE = 1;                                                             INSTAL   8
      BOOLAL = 1;                                                               INSTAL   9
      PTRSIZE = 2;                                                              INSTAL  10
      ADRAL = 1;                                                                INSTAL  11
      SETSIZE = 4;                                                              INSTAL  12
      SETAL = 1;                                                                INSTAL  13
      STACKELSIZE = 1;                                                          INSTAL  14
      STACKAL = 1;                                                              INSTAL  15
      STRGLGTH = 24;                                                            INSTAL  16
      SETHIGH = 63; SETLOW = 0;                                                 INSTAL  17
      ORDMAXCHAR = 127; ORDMINCHAR = 0;                                         INSTAL  18
      LCAFTERMARKSTACK = 10;                                                    INSTAL  19
      MAXINT = 32767;                                                           INSTAL  20
      FILEAL = CHARAL;                                                          P       42
   (* STACKELSIZE = MINIMUM SIZE FOR 1 STACKELEMENT                             P       44
                  = K*STACKAL                                                   P       45
      STACKAL     = SCM(ALL OTHER AL-CONSTANTS)                                 P       46
      CHARMAX     = SCM(CHARSIZE,CHARAL)                                        P       47
                    SCM = SMALLEST COMMON MULTIPLE                              P       48
      LCAFTERMARKSTACK >= 4*PTRSIZE+MAX(X-SIZE)                                 P       49
                       = K1*STACKELSIZE          *)                             P       50
      MAXSTACK = 1;                                                             P       51
      PARMAL = STACKAL;                                                         P       54
      PARMSIZE = STACKELSIZE;                                                   P       55
      RECAL = STACKAL;                                                          P       56
      FILEBUFFER = 4;                                                           PASCP   56
      MAXADDR = MAXINT;                                                         X        2
                                                                                PASCP   57
                                                                                PASCP   58
                                                                                PASCP   59
TYPE                                                        (*DESCRIBING:*)     PASCP   60
                                                            (*************)     PASCP   61
                                                                                PASCP   62
                                                                                PASCP   63
                                                            (*BASIC SYMBOLS*)   PASCP   64
                                                            (***************)   PASCP   65
                                                                                PASCP   66
     SYMBOL = (IDENT,INTCONST,REALCONST,STRINGCONST,NOTSY,MULOP,ADDOP,RELOP,    PASCP   67
               LPARENT,RPARENT,LBRACK,RBRACK,COMMA,SEMICOLON,PERIOD,ARROW,      PASCP   68
               COLON,BECOMES,LABELSY,CONSTSY,TYPESY,VARSY,FUNCSY,PROGSY,        PASCP   69
               PROCSY,SETSY,PACKEDSY,ARRAYSY,RECORDSY,FILESY,FORWARDSY,         PASCP   70
               BEGINSY,IFSY,CASESY,REPEATSY,WHILESY,FORSY,WITHSY,               PASCP   71
               GOTOSY,ENDSY,ELSESY,UNTILSY,OFSY,DOSY,TOSY,DOWNTOSY,             PASCP   72
               THENSY,OTHERSY);                                                 PASCP   73
     OPERATOR = (MUL,RDIV,ANDOP,IDIV,IMOD,PLUS,MINUS,OROP,LTOP,LEOP,GEOP,GTOP,  PASCP   74
                 NEOP,EQOP,INOP,NOOP);                                          PASCP   75
     SETOFSYS = SET OF SYMBOL;                                                  PASCP   76
     CHTP = (LETTER,NUMBER,SPECIAL,ILLEGAL);                                    P       57
                                                                                PASCP   77
                                                            (*CONSTANTS*)       PASCP   78
                                                            (***********)       PASCP   79
                                                                                PASCP   80
     CSTCLASS = (REEL,PSET,STRG);                                               PASCP   81
     CSP = ' CONSTANT;                                                          PASCP   82
     CONSTANT = RECORD CASE CCLASS: CSTCLASS OF                                 PASCP   83
                         REEL: (RVAL: PACKED ARRAY [1..STRGLGTH] OF CHAR);      PASCP   84
                         PSET: (PVAL: SET OF SETLOW..SETHIGH);                  PASCP   85
                         STRG: (SLGTH: 0..STRGLGTH;                             PASCP   86
                                SVAL: PACKED ARRAY [1..STRGLGTH] OF CHAR)       PASCP   87
                       END;                                                     PASCP   88
                                                                                PASCP   89
     VALU = RECORD CASE INTVAL: BOOLEAN OF  (*INTVAL NEVER SET NORE TESTED*)    PASCP   90
                     TRUE:  (IVAL: INTEGER);                                    PASCP   91
                     FALSE: (VALP: CSP)                                         PASCP   92
                   END;                                                         PASCP   93
                                                                                PASCP   94
                                                           (*DATA STRUCTURES*)  PASCP   95
                                                           (*****************)  PASCP   96
     LEVRANGE = 0..MAXLEVEL; ADDRRANGE = 0..MAXADDR;                            PASCP   97
     STRUCTFORM = (SCALAR,SUBRANGE,POINTER,POWER,ARRAYS,RECORDS,FILES,          PASCP   98
                   TAGFLD,VARIANT);                                             PASCP   99
     DECLKIND = (STANDARD,DECLARED);                                            PASCP  100
     STP = ' STRUCTURE; CTP = ' IDENTIFIER;                                     PASCP  101
                                                                                PASCP  102
     STRUCTURE = PACKED RECORD                                                  PASCP  103
                   MARKED: BOOLEAN;   (*FOR TEST PHASE ONLY*)                   PASCP  104
                   SIZE: ADDRRANGE;                                             PASCP  105
                   CASE FORM: STRUCTFORM OF                                     PASCP  106
                     SCALAR:   (CASE SCALKIND: DECLKIND OF                      PASCP  107
                                  DECLARED: (FCONST: CTP));                     PASCP  108
                     SUBRANGE: (RANGETYPE: STP; MIN,MAX: VALU);                 PASCP  109
                     POINTER:  (ELTYPE: STP);                                   PASCP  110
                     POWER:    (ELSET: STP);                                    PASCP  111
                     ARRAYS:   (AELTYPE,INXTYPE: STP);                          PASCP  112
                     RECORDS:  (FSTFLD: CTP; RECVAR: STP);                      PASCP  113
                     FILES:    (FILTYPE: STP);                                  PASCP  114
                     TAGFLD:   (TAGFIELDP: CTP; FSTVAR: STP);                   PASCP  115
                     VARIANT:  (NXTVAR,SUBVAR: STP; VARVAL: VALU)               PASCP  116
                   END;                                                         PASCP  117
                                                                                PASCP  118
                                                            (*NAMES*)           PASCP  119
                                                            (*******)           PASCP  120
                                                                                PASCP  121
     IDCLASS = (TYPES,KONST,VARS,FIELD,PROC,FUNC);                              PASCP  122
     SETOFIDS = SET OF IDCLASS;                                                 PASCP  123
     IDKIND = (ACTUAL,FORMAL);                                                  PASCP  124
     ALPHA = PACKED ARRAY [1..8] OF CHAR;                                       PASCP  125
                                                                                PASCP  126
     IDENTIFIER = PACKED RECORD                                                 PASCP  127
                   NAME: ALPHA; LLINK, RLINK: CTP;                              PASCP  128
                   IDTYPE: STP; NEXT: CTP;                                      PASCP  129
                   CASE KLASS: IDCLASS OF                                       PASCP  130
                     KONST: (VALUES: VALU);                                     PASCP  131
                     VARS:  (VKIND: IDKIND; VLEV: LEVRANGE; VADDR: ADDRRANGE);  PASCP  132
                     FIELD: (FLDADDR: ADDRRANGE);                               PASCP  133
                     PROC,                                                      PASCP  134
                     FUNC:  (CASE PFDECKIND: DECLKIND OF                        PASCP  135
                              STANDARD: (KEY: 1..15);                           PASCP  136
                              DECLARED: (PFLEV: LEVRANGE; PFNAME: INTEGER;      PASCP  137
                                          CASE PFKIND: IDKIND OF                PASCP  138
                                           ACTUAL: (FORWDECL, EXTERN:           PASCP  139
                                                    BOOLEAN)))                  PASCP  140
                   END;                                                         PASCP  141
                                                                                PASCP  142
                                                                                PASCP  143
     DISPRANGE = 0..DISPLIMIT;                                                  PASCP  144
     WHERE = (BLCK,CREC,VREC,REC);                                              PASCP  145
                                                                                PASCP  146
                                                            (*EXPRESSIONS*)     PASCP  147
                                                            (*************)     PASCP  148
     ATTRKIND = (CST,VARBL,EXPR);                                               PASCP  149
     VACCESS = (DRCT,INDRCT,INXD);                                              PASCP  150
                                                                                PASCP  151
     ATTR = RECORD TYPTR: STP;                                                  PASCP  152
              CASE KIND: ATTRKIND OF                                            PASCP  153
                CST:   (CVAL: VALU);                                            PASCP  154
                VARBL: (CASE ACCESS: VACCESS OF                                 PASCP  155
                          DRCT: (VLEVEL: LEVRANGE; DPLMT: ADDRRANGE);           PASCP  156
                          INDRCT: (IDPLMT: ADDRRANGE))                          PASCP  157
              END;                                                              PASCP  158
                                                                                PASCP  159
     TESTP = ' TESTPOINTER;                                                     PASCP  160
     TESTPOINTER = PACKED RECORD                                                PASCP  161
                     ELT1,ELT2 : STP;                                           PASCP  162
                     LASTTESTP : TESTP                                          PASCP  163
                   END;                                                         PASCP  164
                                                                                PASCP  165
                                                                 (*LABELS*)     PASCP  166
                                                                 (********)     PASCP  167
     LBP = ' LABL;                                                              PASCP  168
     LABL = RECORD NEXTLAB: LBP; DEFINED: BOOLEAN;                              PASCP  169
                   LABVAL, LABNAME: INTEGER                                     PASCP  170
            END;                                                                PASCP  171
                                                                                PASCP  172
     EXTFILEP = 'FILEREC;                                                       PASCP  173
     FILEREC = RECORD FILENAME:ALPHA; NEXTFILE:EXTFILEP END;                    PASCP  174
                                                                                PASCP  175
(*-------------------------------------------------------------------------*)   PASCP  176
                                                                                PASCP  177
                                                                                PASCP  178
VAR                                                                             PASCP  179
                                    (*RETURNED BY SOURCE PROGRAM SCANNER        PASCP  181
                                     INSYMBOL:                                  PASCP  182
                                     **********)                                PASCP  183
                                                                                PASCP  184
    SY: SYMBOL;                     (*LAST SYMBOL*)                             PASCP  185
    OP: OPERATOR;                   (*CLASSIFICATION OF LAST SYMBOL*)           PASCP  186
    VAL: VALU;                      (*VALUE OF LAST CONSTANT*)                  PASCP  187
    LGTH: INTEGER;                  (*LENGTH OF LAST STRING CONSTANT*)          PASCP  188
    ID: ALPHA;                      (*LAST IDENTIFIER (POSSIBLY TRUNCATED)*)    PASCP  189
    KK: 1..8;                       (*NR OF CHARS IN LAST IDENTIFIER*)          PASCP  190
    CH: CHAR;                       (*LAST CHARACTER*)                          PASCP  191
    EOL: BOOLEAN;                   (*END OF LINE FLAG*)                        PASCP  192
                                                                                PASCP  193
                                                                                PASCP  194
                                    (*COUNTERS:*)                               PASCP  195
                                    (***********)                               PASCP  196
                                                                                PASCP  197
    CHCNT: INTEGER;                 (*CHARACTER COUNTER*)                       P       58
    LC,IC: ADDRRANGE;               (*DATA LOCATION AND INSTRUCTION COUNTER*)   PASCP  199
    LINECOUNT: INTEGER;                                                         PASCP  200
                                                                                PASCP  201
                                                                                PASCP  202
                                    (*SWITCHES:*)                               PASCP  203
                                    (***********)                               PASCP  204
                                                                                PASCP  205
    DP,                             (*DECLARATION PART*)                        PASCP  206
    PRTERR,                     (*TO ALLOW FORWARD REFERENCES IN POINTER TYPE   PASCP  207
                                  DECLARATION BY SUPPRESSING ERROR MESSAGE*)    PASCP  208
    LIST,PRCODE,PRTABLES: BOOLEAN;  (*OUTPUT OPTIONS FOR                        PASCP  209
                                        -- SOURCE PROGRAM LISTING               PASCP  210
                                        -- PRINTING SYMBOLIC CODE               PASCP  211
                                        -- DISPLAYING IDENT AND STRUCT TABLES   PASCP  212
                                        --> PROCEDURE OPTION*)                  PASCP  213
    DEBUG: BOOLEAN;                                                             P       59
                                                                                PASCP  214
                                                                                PASCP  215
                                    (*POINTERS:*)                               PASCP  216
                                    (***********)                               PASCP  217
    PARMPTR,                                                                    P       60
    INTPTR,REALPTR,CHARPTR,                                                     PASCP  218
    BOOLPTR,NILPTR,TEXTPTR: STP;    (*POINTERS TO ENTRIES OF STANDARD IDS*)     PASCP  219
    UTYPPTR,UCSTPTR,UVARPTR,                                                    PASCP  220
    UFLDPTR,UPRCPTR,UFCTPTR,        (*POINTERS TO ENTRIES FOR UNDECLARED IDS*)  PASCP  221
    FWPTR: CTP;                     (*HEAD OF CHAIN OF FORW DECL TYPE IDS*)     PASCP  222
    FEXTFILEP: EXTFILEP;            (*HEAD OF CHAIN OF EXTERNAL FILES*)         PASCP  223
    GLOBTESTP: TESTP;                (*LAST TESTPOINTER*)                       PASCP  224
                                                                                PASCP  225
                                                                                PASCP  226
                                    (*BOOKKEEPING OF DECLARATION LEVELS:*)      PASCP  227
                                    (************************************)      PASCP  228
                                                                                PASCP  229
    LEVEL: LEVRANGE;                (*CURRENT STATIC LEVEL*)                    PASCP  230
    DISX,                           (*LEVEL OF LAST ID SEARCHED BY SEARCHID*)   PASCP  231
    TOP: DISPRANGE;                 (*TOP OF DISPLAY*)                          PASCP  232
                                                                                PASCP  233
    DISPLAY:                        (*WHERE:   MEANS:*)                         PASCP  234
      ARRAY [DISPRANGE] OF                                                      PASCP  235
        PACKED RECORD               (*=BLCK:   ID IS VARIABLE ID*)              PASCP  236
          FNAME: CTP; FLABEL: LBP;  (*=CREC:   ID IS FIELD ID IN RECORD WITH*)  PASCP  237
          CASE OCCUR: WHERE OF      (*         CONSTANT ADDRESS*)               PASCP  238
            CREC: (CLEV: LEVRANGE;  (*=VREC:   ID IS FIELD ID IN RECORD WITH*)  PASCP  239
                  CDSPL: ADDRRANGE);(*         VARIABLE ADDRESS*)               PASCP  240
            VREC: (VDSPL: ADDRRANGE)                                            PASCP  241
          END;                      (* --> PROCEDURE WITHSTATEMENT*)            PASCP  242
                                                                                PASCP  243
                                                                                PASCP  244
                                    (*ERROR MESSAGES:*)                         PASCP  245
                                    (*****************)                         PASCP  246
                                                                                PASCP  247
    ERRINX: 0..10;                  (*NR OF ERRORS IN CURRENT SOURCE LINE*)     PASCP  248
    ERRLIST:                                                                    PASCP  249
      ARRAY [1..10] OF                                                          PASCP  250
        PACKED RECORD POS: INTEGER;                                             P       61
                      NMR: 1..400                                               PASCP  252
               END;                                                             PASCP  253
                                                                                PASCP  254
                                                                                PASCP  255
                                                                                PASCP  256
                                                                                PASCP  257
                                    (*EXPRESSION COMPILATION:*)                 PASCP  258
                                    (*************************)                 PASCP  259
                                                                                PASCP  260
    GATTR: ATTR;                    (*DESCRIBES THE EXPR CURRENTLY COMPILED*)   PASCP  261
                                                                                PASCP  262
                                                                                PASCP  263
                                    (*STRUCTURED CONSTANTS:*)                   PASCP  264
                                    (***********************)                   PASCP  265
                                                                                PASCP  266
    CONSTBEGSYS,SIMPTYPEBEGSYS,TYPEBEGSYS,BLOCKBEGSYS,SELECTSYS,FACBEGSYS,      PASCP  267
    STATBEGSYS,TYPEDELS: SETOFSYS;                                              PASCP  268
    CHARTP : ARRAY[CHAR] OF CHTP;                                               P       62
    RW:  ARRAY [1..35(*NR. OF RES. WORDS*)] OF ALPHA;                           PASCP  269
    FRW: ARRAY [1..9] OF 1..36(*NR. OF RES. WORDS + 1*);                        PASCP  270
    RSY: ARRAY [1..35(*NR. OF RES. WORDS*)] OF SYMBOL;                          PASCP  271
   SSY: ARRAY [CHAR] OF SYMBOL;                                                 J        1
    ROP: ARRAY [1..35(*NR. OF RES. WORDS*)] OF OPERATOR;                        PASCP  273
   SOP: ARRAY [CHAR] OF OPERATOR;                                               J        2
    NA:  ARRAY [1..35] OF ALPHA;                                                PASCP  275
    MN:  ARRAY[0..60] OF PACKED ARRAY[1..4] OF CHAR;                            P       63
    SNA: ARRAY [1..23] OF PACKED ARRAY [1..4] OF CHAR;                          PASCP  277
    CDX: ARRAY[0..60] OF -4..+4;                                                P       64
    PDX: ARRAY[1..23] OF -7..+7;                                                P       65
    ORDINT: ARRAY[CHAR] OF INTEGER;                                             CH       1
                                                                                CH       2
    INTLABEL,MXINT10,DIGMAX: INTEGER;                                           PASCP  279
                                                                                PASCP  280
(*-------------------------------------------------------------------------*)   PASCP  281
                                                                                PASCP  282
                                                                                PASCP  283
PROCEDURE ENDOFLINE;                                                            PASCP  284
    VAR LASTPOS,FREEPOS,CURRPOS,CURRNMR,F,K: INTEGER;                           PASCP  285
  BEGIN                                                                         PASCP  286
    IF ERRINX > 0 THEN   (*OUTPUT ERROR MESSAGES*)                              PASCP  287
      BEGIN WRITE(OUTPUT,# ****  #:15);                                         PASCP  288
        LASTPOS := 0; FREEPOS := 1;                                             PASCP  289
        FOR K := 1 TO ERRINX DO                                                 PASCP  290
          BEGIN                                                                 PASCP  291
            WITH ERRLIST[K] DO                                                  PASCP  292
              BEGIN CURRPOS := POS; CURRNMR := NMR END;                         PASCP  293
            IF CURRPOS = LASTPOS THEN WRITE(OUTPUT,#,#)                         PASCP  294
            ELSE                                                                PASCP  295
              BEGIN                                                             PASCP  296
                WHILE FREEPOS < CURRPOS DO                                      PASCP  297
                  BEGIN WRITE(OUTPUT,# #); FREEPOS := FREEPOS + 1 END;          PASCP  298
                WRITE(OUTPUT,#'#);                                              PASCP  299
                LASTPOS := CURRPOS                                              PASCP  300
              END;                                                              PASCP  301
            IF CURRNMR < 10 THEN F := 1                                         PASCP  302
            ELSE IF CURRNMR < 100 THEN F := 2                                   PASCP  303
              ELSE F := 3;                                                      PASCP  304
            WRITE(OUTPUT,CURRNMR:F);                                            PASCP  305
            FREEPOS := FREEPOS + F + 1                                          PASCP  306
          END;                                                                  PASCP  307
        WRITELN(OUTPUT); ERRINX := 0                                            PASCP  308
      END;                                                                      PASCP  309
    IF LIST AND (NOT EOF(INPUT)) THEN                                           P       66
      BEGIN LINECOUNT := LINECOUNT + 1; WRITE(OUTPUT,LINECOUNT:6,#  #:2);       PASCP  311
        IF DP THEN WRITE(OUTPUT,LC:7) ELSE WRITE(OUTPUT,IC:7);                  PASCP  312
        WRITE(OUTPUT,# #)                                                       PASCP  313
      END;                                                                      PASCP  314
    CHCNT := 0                                                                  PASCP  315
  END  (*ENDOFLINE*) ;                                                          PASCP  316
                                                                                PASCP  317
  PROCEDURE ERROR(FERRNR: INTEGER);                                             PASCP  318
  BEGIN                                                                         PASCP  319
    IF ERRINX >= 9 THEN                                                         PASCP  320
      BEGIN ERRLIST[10].NMR := 255; ERRINX := 10 END                            PASCP  321
    ELSE                                                                        PASCP  322
      BEGIN ERRINX := ERRINX + 1;                                               PASCP  323
        ERRLIST[ERRINX].NMR := FERRNR                                           PASCP  324
      END;                                                                      PASCP  325
    ERRLIST[ERRINX].POS := CHCNT                                                PASCP  326
  END (*ERROR*) ;                                                               PASCP  327
                                                                                PASCP  328
  PROCEDURE INSYMBOL;                                                           PASCP  329
    (*READ NEXT BASIC SYMBOL OF SOURCE PROGRAM AND RETURN ITS                   PASCP  330
    DESCRIPTION IN THE GLOBAL VARIABLES SY, OP, ID, VAL AND LGTH*)              PASCP  331
    LABEL 1,2,3;                                                                PASCP  332
    VAR I,K: INTEGER;                                                           PASCP  333
        DIGIT: PACKED ARRAY [1..STRGLGTH] OF CHAR;                              PASCP  334
        STRING: PACKED ARRAY [1..STRGLGTH] OF CHAR;                             PASCP  335
        LVP: CSP;TEST: BOOLEAN;                                                 PASCP  336
                                                                                PASCP  337
    PROCEDURE NEXTCH;                                                           PASCP  338
    BEGIN IF EOL THEN                                                           PASCP  339
      BEGIN IF LIST THEN WRITELN(OUTPUT); ENDOFLINE                             PASCP  340
      END;                                                                      PASCP  341
      IF NOT EOF(INPUT) THEN                                                    PASCP  342
       BEGIN EOL := EOLN(INPUT); READ(INPUT,CH);                                PASCP  343
        IF LIST THEN WRITE(OUTPUT,CH);                                          PASCP  344
        CHCNT := CHCNT + 1                                                      PASCP  345
       END                                                                      PASCP  346
      ELSE                                                                      P       67
        BEGIN WRITELN(OUTPUT,#   *** EOF #,#ENCOUNTERED#);                      P       68
          TEST := FALSE                                                         P       69
        END                                                                     P       70
    END;                                                                        PASCP  348
                                                                                PASCP  349
    PROCEDURE OPTIONS;                                                          PASCP  350
    BEGIN                                                                       PASCP  351
      REPEAT NEXTCH;                                                            PASCP  352
        IF CH <> #*# THEN                                                       PASCP  353
          BEGIN                                                                 PASCP  354
            IF CH = #T# THEN                                                    PASCP  355
              BEGIN NEXTCH; PRTABLES := CH = #+# END                            PASCP  356
            ELSE                                                                PASCP  357
              IF CH = #L# THEN                                                  PASCP  358
                BEGIN NEXTCH; LIST := CH = #+#;                                 PASCP  359
                  IF NOT LIST THEN WRITELN(OUTPUT)                              PASCP  360
                END                                                             PASCP  361
              ELSE                                                              PASCP  362
             IF CH = #D# THEN                                                   P       71
               BEGIN NEXTCH; DEBUG := CH = #+# END                              P       72
             ELSE                                                               P       73
                IF CH = #C# THEN                                                PASCP  363
                  BEGIN NEXTCH; PRCODE := CH = #+# END;                         PASCP  364
            NEXTCH                                                              PASCP  365
          END                                                                   PASCP  366
      UNTIL CH <> #,#                                                           PASCP  367
    END (*OPTIONS*) ;                                                           PASCP  368
                                                                                PASCP  369
  BEGIN (*INSYMBOL*)                                                            PASCP  370
  1:                                                                            PASCP  371
    REPEAT WHILE (CH = # #) AND NOT EOL DO NEXTCH;                              PASCP  372
      TEST := EOL;                                                              PASCP  373
      IF TEST THEN NEXTCH                                                       PASCP  374
    UNTIL NOT TEST;                                                             PASCP  375
    IF CHARTP[CH] = ILLEGAL THEN                                                P       74
      BEGIN SY := OTHERSY; OP := NOOP;                                          P       75
        ERROR(399); NEXTCH                                                      P       76
      END                                                                       P       77
    ELSE                                                                        P       78
    CASE CH OF                                                                  PASCP  376
      #A#,#B#,#C#,#D#,#E#,#F#,#G#,#H#,#I#,                                      PASCP  377
      #J#,#K#,#L#,#M#,#N#,#O#,#P#,#Q#,#R#,                                      PASCP  378
      #S#,#T#,#U#,#V#,#W#,#X#,#Y#,#Z#:                                          PASCP  379
        BEGIN K := 0;                                                           PASCP  380
          REPEAT                                                                PASCP  381
            IF K < 8 THEN                                                       PASCP  382
             BEGIN K := K + 1; ID[K] := CH END ;                                PASCP  383
            NEXTCH                                                              PASCP  384
           UNTIL CHARTP[CH] IN [SPECIAL,ILLEGAL];                               P       79
          IF K >= KK THEN KK := K                                               PASCP  386
          ELSE                                                                  PASCP  387
            REPEAT ID[KK] := # #; KK := KK - 1                                  PASCP  388
            UNTIL KK = K;                                                       PASCP  389
          FOR I := FRW[K] TO FRW[K+1] - 1 DO                                    PASCP  390
            IF RW[I] = ID THEN                                                  PASCP  391
              BEGIN SY := RSY[I]; OP := ROP[I]; GOTO 2 END;                     PASCP  392
            SY := IDENT; OP := NOOP;                                            PASCP  393
  2:    END;                                                                    PASCP  394
      #0#,#1#,#2#,#3#,#4#,#5#,#6#,#7#,#8#,#9#:                                  PASCP  395
        BEGIN OP := NOOP; I := 0;                                               PASCP  396
          REPEAT I := I+1; IF I<= DIGMAX THEN DIGIT[I] := CH; NEXTCH            PASCP  397
          UNTIL CHARTP[CH] <> NUMBER;                                           P       80
          IF (CH = #.#) OR (CH = #E#) THEN                                      PASCP  399
            BEGIN                                                               PASCP  400
                  K := I;                                                       PASCP  401
                  IF CH = #.# THEN                                              PASCP  402
                    BEGIN K := K+1; IF K <= DIGMAX THEN DIGIT[K] := CH;         PASCP  403
                      NEXTCH; IF CH = #.# THEN BEGIN CH := #:#; GOTO 3 END;     PASCP  404
                      IF CHARTP[CH] <> NUMBER THEN ERROR(201)                   P       81
                      ELSE                                                      PASCP  407
                        REPEAT K := K + 1;                                      PASCP  408
                          IF K <= DIGMAX THEN DIGIT[K] := CH; NEXTCH            PASCP  409
                        UNTIL CHARTP[CH] <>  NUMBER                             P       82
                    END;                                                        PASCP  411
                  IF CH = #E# THEN                                              PASCP  412
                    BEGIN K := K+1; IF K <= DIGMAX THEN DIGIT[K] := CH;         PASCP  413
                      NEXTCH;                                                   PASCP  414
                      IF (CH = #+#) OR (CH =#-#) THEN                           PASCP  415
                        BEGIN K := K+1; IF K <= DIGMAX THEN DIGIT[K] := CH;     PASCP  416
                          NEXTCH                                                PASCP  417
                        END;                                                    PASCP  418
                      IF CHARTP[CH] <> NUMBER THEN ERROR(201)                   P       83
                      ELSE                                                      PASCP  421
                        REPEAT K := K+1;                                        PASCP  422
                          IF K <= DIGMAX THEN DIGIT[K] := CH; NEXTCH            PASCP  423
                        UNTIL CHARTP[CH] <> NUMBER                              P       84
                     END;                                                       PASCP  425
                   NEW(LVP,REEL); SY:= REALCONST; LVP'.CCLASS := REEL;          PASCP  426
                   WITH LVP' DO                                                 PASCP  427
                     BEGIN FOR I := 1 TO STRGLGTH DO RVAL[I] := # #;            PASCP  428
                       IF K <= DIGMAX THEN                                      PASCP  429
                         FOR I := 2 TO K + 1 DO RVAL[I] := DIGIT[I-1]           PASCP  430
                       ELSE BEGIN ERROR(203); RVAL[2] := #0#;                   PASCP  431
                              RVAL[3] := #.#; RVAL[4] := #0#                    PASCP  432
                            END                                                 PASCP  433
                     END;                                                       PASCP  434
                   VAL.VALP := LVP                                              PASCP  435
            END                                                                 PASCP  436
          ELSE                                                                  PASCP  437
  3:        BEGIN                                                               PASCP  438
              IF I > DIGMAX THEN BEGIN ERROR(203); VAL.IVAL := 0 END            PASCP  439
              ELSE                                                              PASCP  440
                WITH VAL DO                                                     PASCP  441
                  BEGIN IVAL := 0;                                              PASCP  442
                    FOR K := 1 TO I DO                                          PASCP  443
                      BEGIN                                                     PASCP  444
                        IF IVAL <= MXINT10 THEN                                 PASCP  445
                          IVAL := IVAL*10+ORDINT[DIGIT[K]]                      CH       3
                        ELSE BEGIN ERROR(203); IVAL := 0 END                    PASCP  447
                      END;                                                      PASCP  448
                    SY := INTCONST                                              PASCP  449
                 END                                                            PASCP  450
            END                                                                 PASCP  451
        END;                                                                    PASCP  452
      ####:                                                                     PASCP  453
        BEGIN LGTH := 0; SY := STRINGCONST;  OP := NOOP;                        PASCP  454
          REPEAT                                                                PASCP  455
            REPEAT NEXTCH; LGTH := LGTH + 1;                                    PASCP  456
                   IF LGTH <= STRGLGTH THEN STRING[LGTH] := CH                  PASCP  457
            UNTIL (EOL) OR (CH = ####);                                         PASCP  458
            IF EOL THEN ERROR(202) ELSE NEXTCH                                  PASCP  459
          UNTIL CH <> ####;                                                     PASCP  460
          LGTH := LGTH - 1;   (*NOW LGTH = NR OF CHARS IN STRING*)              PASCP  461
          IF LGTH = 0 THEN ERROR(205)                                           KEN      1
          ELSE                                                                  KEN      2
          IF LGTH = 1 THEN VAL.IVAL := ORD(STRING[1])                           PASCP  462
          ELSE                                                                  PASCP  463
            BEGIN NEW(LVP,STRG); LVP'.CCLASS:=STRG;                             PASCP  464
              IF LGTH > STRGLGTH THEN                                           PASCP  465
                BEGIN ERROR(399); LGTH := STRGLGTH END;                         PASCP  466
              WITH LVP' DO                                                      PASCP  467
                BEGIN SLGTH := LGTH;                                            PASCP  468
                  FOR I := 1 TO LGTH DO SVAL[I] := STRING[I]                    PASCP  469
                END;                                                            PASCP  470
              VAL.VALP := LVP                                                   PASCP  471
            END                                                                 PASCP  472
        END;                                                                    PASCP  473
      #:#:                                                                      PASCP  474
        BEGIN OP := NOOP; NEXTCH;                                               PASCP  475
          IF CH = #=# THEN                                                      PASCP  476
            BEGIN SY := BECOMES; NEXTCH END                                     PASCP  477
          ELSE SY := COLON                                                      PASCP  478
        END;                                                                    PASCP  479
      #.#:                                                                      PASCP  480
        BEGIN OP := NOOP; NEXTCH;                                               PASCP  481
          IF CH = #.# THEN                                                      PASCP  482
            BEGIN SY := COLON; NEXTCH END                                       PASCP  483
          ELSE SY := PERIOD                                                     PASCP  484
        END;                                                                    PASCP  485
      #<#:                                                                      PASCP  486
        BEGIN NEXTCH; SY := RELOP;                                              PASCP  487
          IF CH = #=# THEN                                                      PASCP  488
            BEGIN OP := LEOP; NEXTCH END                                        PASCP  489
          ELSE                                                                  PASCP  490
            IF CH = #># THEN                                                    PASCP  491
              BEGIN OP := NEOP; NEXTCH END                                      PASCP  492
            ELSE OP := LTOP                                                     PASCP  493
        END;                                                                    PASCP  494
      #>#:                                                                      PASCP  495
        BEGIN NEXTCH; SY := RELOP;                                              PASCP  496
          IF CH = #=# THEN                                                      PASCP  497
            BEGIN OP := GEOP; NEXTCH END                                        PASCP  498
          ELSE OP := GTOP                                                       PASCP  499
        END;                                                                    PASCP  500
      #(#:                                                                      PASCP  501
       BEGIN NEXTCH;                                                            PASCP  502
         IF CH = #*# THEN                                                       PASCP  503
           BEGIN NEXTCH;                                                        PASCP  504
             IF CH = #$# THEN OPTIONS;                                          PASCP  505
             REPEAT                                                             PASCP  506
               WHILE (CH <> #*#) AND NOT EOF(INPUT) DO NEXTCH;                  PASCP  507
               NEXTCH                                                           PASCP  508
             UNTIL (CH = #)#) OR EOF(INPUT);                                    PASCP  509
             NEXTCH; GOTO 1                                                     PASCP  510
           END;                                                                 PASCP  511
         SY := LPARENT; OP := NOOP                                              PASCP  512
       END;                                                                     PASCP  513
      #*#,#+#,#-#,                                                              PASCP  514
      #=#,#/#,#)#,                                                              PASCP  515
      #[#,#]#,#,#,#;#,#'#,#$#:                                                  PASCP  516
        BEGIN SY := SSY[CH]; OP := SOP[CH];                                     PASCP  517
          NEXTCH                                                                PASCP  518
        END;                                                                    PASCP  519
      # #: SY := OTHERSY                                                        P       85
    END (*CASE*)                                                                PASCP  523
  END (*INSYMBOL*) ;                                                            PASCP  524
                                                                                PASCP  525
  PROCEDURE ENTERID(FCP: CTP);                                                  PASCP  526
    (*ENTER ID POINTED AT BY FCP INTO THE NAME-TABLE,                           PASCP  527
     WHICH ON EACH DECLARATION LEVEL IS ORGANISED AS                            PASCP  528
     AN UNBALANCED BINARY TREE*)                                                PASCP  529
    VAR NAM: ALPHA; LCP, LCP1: CTP; LLEFT: BOOLEAN;                             PASCP  530
  BEGIN NAM := FCP'.NAME;                                                       PASCP  531
    LCP := DISPLAY[TOP].FNAME;                                                  PASCP  532
    IF LCP = NIL THEN                                                           PASCP  533
      DISPLAY[TOP].FNAME := FCP                                                 PASCP  534
    ELSE                                                                        PASCP  535
      BEGIN                                                                     PASCP  536
        REPEAT LCP1 := LCP;                                                     PASCP  537
          IF LCP'.NAME = NAM THEN   (*NAME CONFLICT, FOLLOW RIGHT LINK*)        PASCP  538
            BEGIN ERROR(101); LCP := LCP'.RLINK; LLEFT := FALSE END             PASCP  539
          ELSE                                                                  PASCP  540
            IF LCP'.NAME < NAM THEN                                             PASCP  541
              BEGIN LCP := LCP'.RLINK; LLEFT := FALSE END                       PASCP  542
            ELSE BEGIN LCP := LCP'.LLINK; LLEFT := TRUE END                     PASCP  543
        UNTIL LCP = NIL;                                                        PASCP  544
        IF LLEFT THEN LCP1'.LLINK := FCP ELSE LCP1'.RLINK := FCP                PASCP  545
      END;                                                                      PASCP  546
    FCP'.LLINK := NIL; FCP'.RLINK := NIL                                        PASCP  547
  END (*ENTERID*) ;                                                             PASCP  548
                                                                                PASCP  549
  PROCEDURE SEARCHSECTION(FCP: CTP; VAR FCP1: CTP);                             PASCP  550
    (*TO FIND RECORD FIELDS AND FORWARD DECLARED PROCEDURE ID#S                 PASCP  551
     --> PROCEDURE PROCEDUREDECLARATION                                         PASCP  552
     --> PROCEDURE SELECTOR*)                                                   PASCP  553
     LABEL 1;                                                                   PASCP  554
  BEGIN                                                                         PASCP  555
    WHILE FCP <> NIL DO                                                         PASCP  556
      IF FCP'.NAME = ID THEN GOTO 1                                             PASCP  557
      ELSE IF FCP'.NAME < ID THEN FCP := FCP'.RLINK                             PASCP  558
        ELSE FCP := FCP'.LLINK;                                                 PASCP  559
1:  FCP1 := FCP                                                                 PASCP  560
  END (*SEARCHSECTION*) ;                                                       PASCP  561
                                                                                PASCP  562
  PROCEDURE SEARCHID(FIDCLS: SETOFIDS; VAR FCP: CTP);                           PASCP  563
    LABEL 1;                                                                    PASCP  564
    VAR LCP: CTP;                                                               PASCP  565
  BEGIN                                                                         PASCP  566
    FOR DISX := TOP DOWNTO 0 DO                                                 PASCP  567
      BEGIN LCP := DISPLAY[DISX].FNAME;                                         PASCP  568
        WHILE LCP <> NIL DO                                                     PASCP  569
          IF LCP'.NAME = ID THEN                                                PASCP  570
            IF LCP'.KLASS IN FIDCLS THEN GOTO 1                                 PASCP  571
            ELSE                                                                PASCP  572
              BEGIN IF PRTERR THEN ERROR(103);                                  PASCP  573
                LCP := LCP'.RLINK                                               PASCP  574
              END                                                               PASCP  575
          ELSE                                                                  PASCP  576
            IF LCP'.NAME < ID THEN                                              PASCP  577
              LCP := LCP'.RLINK                                                 PASCP  578
            ELSE LCP := LCP'.LLINK                                              PASCP  579
      END;                                                                      PASCP  580
    (*SEARCH NOT SUCCSESSFUL; SUPPRESS ERROR MESSAGE IN CASE                    PASCP  581
     OF FORWARD REFERENCED TYPE ID IN POINTER TYPE DEFINITION                   PASCP  582
     --> PROCEDURE SIMPLETYPE*)                                                 PASCP  583
    IF PRTERR THEN                                                              PASCP  584
      BEGIN ERROR(104);                                                         PASCP  585
        (*TO AVOID RETURNING NIL, REFERENCE AN ENTRY                            PASCP  586
         FOR AN UNDECLARED ID OF APPROPRIATE CLASS                              PASCP  587
         --> PROCEDURE ENTERUNDECL*)                                            PASCP  588
        IF TYPES IN FIDCLS THEN LCP := UTYPPTR                                  PASCP  589
        ELSE                                                                    PASCP  590
          IF VARS IN FIDCLS THEN LCP := UVARPTR                                 PASCP  591
          ELSE                                                                  PASCP  592
            IF FIELD IN FIDCLS THEN LCP := UFLDPTR                              PASCP  593
            ELSE                                                                PASCP  594
              IF KONST IN FIDCLS THEN LCP := UCSTPTR                            PASCP  595
              ELSE                                                              PASCP  596
                IF PROC IN FIDCLS THEN LCP := UPRCPTR                           PASCP  597
                ELSE LCP := UFCTPTR;                                            PASCP  598
      END;                                                                      PASCP  599
1:  FCP := LCP                                                                  PASCP  600
  END (*SEARCHID*) ;                                                            PASCP  601
                                                                                PASCP  602
  PROCEDURE GETBOUNDS(FSP: STP; VAR FMIN,FMAX: INTEGER);                        PASCP  603
    (*GET INTERNAL BOUNDS OF SUBRANGE OR SCALAR TYPE*)                          PASCP  604
   (*ASSUME FSP<>INTPTR AND FSP<>REALPTR*)                                      P       86
  BEGIN                                                                         PASCP  607
    FMIN := 0; FMAX := 0;                                                       P       87
    IF FSP <> NIL THEN                                                          P       88
    WITH FSP' DO                                                                PASCP  608
      IF FORM = SUBRANGE THEN                                                   PASCP  609
        BEGIN FMIN := MIN.IVAL; FMAX := MAX.IVAL END                            PASCP  610
      ELSE                                                                      PASCP  611
          IF FSP = CHARPTR THEN                                                 P       89
            BEGIN FMIN := ORDMINCHAR; FMAX := ORDMAXCHAR                        P       90
            END                                                                 P       91
          ELSE                                                                  PASCP  614
            IF FCONST <> NIL THEN                                               P       92
              FMAX := FCONST'.VALUES.IVAL                                       P       93
  END (*GETBOUNDS*) ;                                                           PASCP  619
                                                                                P       94
  FUNCTION ALIGNQUOT(FSP: STP): INTEGER;                                        P       95
  BEGIN                                                                         P       96
    ALIGNQUOT := 1;                                                             P       97
    IF FSP <> NIL THEN                                                          P       98
      WITH FSP' DO                                                              P       99
        CASE FORM OF                                                            P      100
          SCALAR:   IF FSP=INTPTR THEN ALIGNQUOT := INTAL                       P      101
                    ELSE IF FSP=BOOLPTR THEN ALIGNQUOT := BOOLAL                P      102
                    ELSE IF SCALKIND=DECLARED THEN ALIGNQUOT := INTAL           P      103
                    ELSE IF FSP=CHARPTR THEN ALIGNQUOT := CHARAL                P      104
                    ELSE IF FSP=REALPTR THEN ALIGNQUOT := REALAL                P      105
                    ELSE (*PARMPTR*) ALIGNQUOT := PARMAL;                       P      106
          SUBRANGE: ALIGNQUOT := ALIGNQUOT(RANGETYPE);                          P      107
          POINTER:  ALIGNQUOT := ADRAL;                                         P      108
          POWER:    ALIGNQUOT := SETAL;                                         P      109
          FILES:    ALIGNQUOT := FILEAL;                                        P      110
          ARRAYS:   ALIGNQUOT := ALIGNQUOT(AELTYPE);                            P      111
          RECORDS:  ALIGNQUOT := RECAL;                                         P      112
          VARIANT,TAGFLD: ERROR(501)                                            P      113
        END                                                                     P      114
  END (*ALIGNQUOT*);                                                            P      115
                                                                                P      116
  PROCEDURE ALIGN(FSP: STP; VAR FLC: ADDRRANGE);                                P      117
    VAR K,L: INTEGER;                                                           P      118
  BEGIN                                                                         P      119
    K := ALIGNQUOT(FSP);                                                        P      120
    L := FLC-1;                                                                 P      121
    FLC := L + K -(K + L) MOD K                                                 X7       1
  END (*ALIGN*);                                                                P      123
                                                                                PASCP  620
  PROCEDURE PRINTTABLES(FB: BOOLEAN);                                           PASCP  621
    (*PRINT DATA STRUCTURE AND NAME TABLE*)                                     PASCP  622
    VAR I, LIM: DISPRANGE;                                                      PASCP  623
                                                                                PASCP  624
    PROCEDURE MARKER;                                                           PASCP  625
      (*MARK DATA STRUCTURE ENTRIES TO AVOID MULTIPLE PRINTOUT*)                PASCP  626
      VAR I: INTEGER;                                                           PASCP  627
                                                                                PASCP  628
      PROCEDURE MARKCTP(FP: CTP); FORWARD;                                      PASCP  629
                                                                                PASCP  630
      PROCEDURE MARKSTP(FP: STP);                                               PASCP  631
        (*MARK DATA STRUCTURES, PREVENT CYCLES*)                                PASCP  632
      BEGIN                                                                     PASCP  633
        IF FP <> NIL THEN                                                       PASCP  634
          WITH FP' DO                                                           PASCP  635
            BEGIN MARKED := TRUE;                                               PASCP  636
              CASE FORM OF                                                      PASCP  637
              SCALAR:   ;                                                       PASCP  638
              SUBRANGE: MARKSTP(RANGETYPE);                                     PASCP  639
              POINTER:  (*DON#T MARK ELTYPE: CYCLE POSSIBLE; WILL BE MARKED     PASCP  640
                        ANYWAY, IF FP = TRUE*) ;                                PASCP  641
              POWER:    MARKSTP(ELSET) ;                                        PASCP  642
              ARRAYS:   BEGIN MARKSTP(AELTYPE); MARKSTP(INXTYPE) END;           PASCP  643
              RECORDS:  BEGIN MARKCTP(FSTFLD); MARKSTP(RECVAR) END;             PASCP  644
              FILES:    MARKSTP(FILTYPE);                                       PASCP  645
              TAGFLD:   MARKSTP(FSTVAR);                                        PASCP  646
              VARIANT:  BEGIN MARKSTP(NXTVAR); MARKSTP(SUBVAR) END              PASCP  647
              END (*CASE*)                                                      PASCP  648
            END (*WITH*)                                                        PASCP  649
      END (*MARKSTP*);                                                          PASCP  650
                                                                                PASCP  651
      PROCEDURE MARKCTP;                                                        PASCP  652
      BEGIN                                                                     PASCP  653
        IF FP <> NIL THEN                                                       PASCP  654
          WITH FP' DO                                                           PASCP  655
            BEGIN MARKCTP(LLINK); MARKCTP(RLINK);                               PASCP  656
              MARKSTP(IDTYPE)                                                   PASCP  657
            END                                                                 PASCP  658
      END (*MARKCTP*);                                                          PASCP  659
                                                                                PASCP  660
    BEGIN (*MARK*)                                                              PASCP  661
      FOR I := TOP DOWNTO LIM DO                                                PASCP  662
        MARKCTP(DISPLAY[I].FNAME)                                               PASCP  663
    END (*MARK*);                                                               PASCP  664
                                                                                PASCP  665
    PROCEDURE FOLLOWCTP(FP: CTP); FORWARD;                                      PASCP  666
                                                                                PASCP  667
    PROCEDURE FOLLOWSTP(FP: STP);                                               PASCP  668
    BEGIN                                                                       PASCP  669
      IF FP <> NIL THEN                                                         PASCP  670
        WITH FP' DO                                                             PASCP  671
          IF MARKED THEN                                                        PASCP  672
            BEGIN MARKED := FALSE; WRITE(OUTPUT,# #:4,ORD(FP):6,SIZE:10);       PASCP  673
              CASE FORM OF                                                      PASCP  674
              SCALAR:   BEGIN WRITE(OUTPUT,#SCALAR#:10);                        PASCP  675
                          IF SCALKIND = STANDARD THEN                           PASCP  676
                           WRITE(OUTPUT,#STANDARD#:10)                          PASCP  677
                          ELSE WRITE(OUTPUT,#DECLARED#:10,# #:4,ORD(FCONST):6); PASCP  678
                          WRITELN(OUTPUT)                                       PASCP  679
                        END;                                                    PASCP  680
              SUBRANGE:BEGIN                                                    PASCP  681
                        WRITE(OUTPUT,#SUBRANGE#:10,# #:4,ORD(RANGETYPE):6);     PASCP  682
                            IF RANGETYPE <> REALPTR THEN                        PASCP  683
                              WRITE(OUTPUT,MIN.IVAL,MAX.IVAL)                   PASCP  684
                            ELSE                                                PASCP  685
                              IF (MIN.VALP <> NIL) AND (MAX.VALP <> NIL) THEN   PASCP  686
                                WRITE(OUTPUT,# #,MIN.VALP'.RVAL:9,              PASCP  687
                                      # #,MAX.VALP'.RVAL:9);                    PASCP  688
                            WRITELN(OUTPUT); FOLLOWSTP(RANGETYPE);              PASCP  689
                          END;                                                  PASCP  690
              POINTER:  WRITELN(OUTPUT,#POINTER#:10,# #:4,ORD(ELTYPE):6);       PASCP  691
              POWER:    BEGIN WRITELN(OUTPUT,#SET#:10,# #:4,ORD(ELSET):6);      PASCP  692
                            FOLLOWSTP(ELSET)                                    PASCP  693
                          END;                                                  PASCP  694
              ARRAYS:   BEGIN                                                   PASCP  695
                         WRITELN(OUTPUT,#ARRAY#:10,# #:4,ORD(AELTYPE):6,# #:4,  PASCP  696
                            ORD(INXTYPE):6);                                    PASCP  697
                            FOLLOWSTP(AELTYPE); FOLLOWSTP(INXTYPE)              PASCP  698
                          END;                                                  PASCP  699
              RECORDS:  BEGIN                                                   PASCP  700
                        WRITELN(OUTPUT,#RECORD#:10,# #:4,ORD(FSTFLD):6,# #:4,   PASCP  701
                            ORD(RECVAR):6); FOLLOWCTP(FSTFLD);                  PASCP  702
                            FOLLOWSTP(RECVAR)                                   PASCP  703
                          END;                                                  PASCP  704
              FILES:    BEGIN WRITE(OUTPUT,#FILE#:10,# #:4,ORD(FILTYPE):6);     PASCP  705
                            FOLLOWSTP(FILTYPE)                                  PASCP  706
                          END;                                                  PASCP  707
              TAGFLD:   BEGIN WRITELN(OUTPUT,#TAGFLD#:10,# #:4,ORD(TAGFIELDP):6,PASCP  708
                            # #:4,ORD(FSTVAR):6);                               PASCP  709
                            FOLLOWSTP(FSTVAR)                                   PASCP  710
                          END;                                                  PASCP  711
              VARIANT:  BEGIN WRITELN(OUTPUT,#VARIANT#:10,# #:4,ORD(NXTVAR):6,  PASCP  712
                            # #:4,ORD(SUBVAR):6,VARVAL.IVAL);                   PASCP  713
                            FOLLOWSTP(NXTVAR); FOLLOWSTP(SUBVAR)                PASCP  714
                          END                                                   PASCP  715
              END (*CASE*)                                                      PASCP  716
            END (*IF MARKED*)                                                   PASCP  717
    END (*FOLLOWSTP*);                                                          PASCP  718
                                                                                PASCP  719
    PROCEDURE FOLLOWCTP;                                                        PASCP  720
      VAR I: INTEGER;                                                           PASCP  721
    BEGIN                                                                       PASCP  722
      IF FP <> NIL THEN                                                         PASCP  723
        WITH FP' DO                                                             PASCP  724
          BEGIN WRITE(OUTPUT,# #:4,ORD(FP):6,# #,NAME:9,# #:4,ORD(LLINK):6,     PASCP  725
            # #:4,ORD(RLINK):6,# #:4,ORD(IDTYPE):6);                            PASCP  726
            CASE KLASS OF                                                       PASCP  727
              TYPES: WRITE(OUTPUT,#TYPE#:10);                                   PASCP  728
              KONST: BEGIN WRITE(OUTPUT,#CONSTANT#:10,# #:4,ORD(NEXT):6);       PASCP  729
                     IF IDTYPE <> NIL THEN                                      PASCP  730
                         IF IDTYPE = REALPTR THEN                               PASCP  731
                           BEGIN                                                PASCP  732
                             IF VALUES.VALP <> NIL THEN                         PASCP  733
                               WRITE(OUTPUT,# #,VALUES.VALP'.RVAL:9)            PASCP  734
                           END                                                  PASCP  735
                         ELSE                                                   PASCP  736
                           IF IDTYPE'.FORM = ARRAYS THEN  (*STRINGCONST*)       PASCP  737
                             BEGIN                                              PASCP  738
                               IF VALUES.VALP <> NIL THEN                       PASCP  739
                                 BEGIN WRITE(OUTPUT,# #);                       PASCP  740
                                   WITH VALUES.VALP' DO                         PASCP  741
                                     FOR I := 1 TO SLGTH DO                     PASCP  742
                                      WRITE(OUTPUT,SVAL[I])                     PASCP  743
                                 END                                            PASCP  744
                             END                                                PASCP  745
                           ELSE WRITE(OUTPUT,VALUES.IVAL)                       PASCP  746
                       END;                                                     PASCP  747
              VARS:  BEGIN WRITE(OUTPUT,#VARIABLE#:10);                         PASCP  748
                        IF VKIND = ACTUAL THEN WRITE(OUTPUT,#ACTUAL#:10)        PASCP  749
                        ELSE WRITE(OUTPUT,#FORMAL#:10);                         PASCP  750
                        WRITE(OUTPUT,# #:4,ORD(NEXT):6,VLEV,# #:4,VADDR:6 );    PASCP  751
                      END;                                                      PASCP  752
              FIELD: WRITE(OUTPUT,#FIELD#:10,# #:4,ORD(NEXT):6,# #:4,FLDADDR:6);PASCP  753
              PROC,                                                             PASCP  754
              FUNC:  BEGIN                                                      PASCP  755
                        IF KLASS = PROC THEN WRITE(OUTPUT,#PROCEDURE#:10)       PASCP  756
                        ELSE WRITE(OUTPUT,#FUNCTION#:10);                       PASCP  757
                        IF PFDECKIND = STANDARD THEN                            PASCP  758
                         WRITE(OUTPUT,#STANDARD#:10,                            PASCP  759
                          KEY:10)                                               PASCP  760
                        ELSE                                                    PASCP  761
                          BEGIN WRITE(OUTPUT,#DECLARED#:10,# #:4,ORD(NEXT):6);  PASCP  762
                            WRITE(OUTPUT,PFLEV,# #:4,PFNAME:6);                 PASCP  763
                            IF PFKIND = ACTUAL THEN                             PASCP  764
                              BEGIN WRITE(OUTPUT,#ACTUAL#:10);                  PASCP  765
                                IF FORWDECL THEN WRITE(OUTPUT,#FORWARD#:10)     PASCP  766
                                ELSE WRITE(OUTPUT,#NOTFORWARD#:10);             PASCP  767
                                IF EXTERN THEN WRITE(OUTPUT,#EXTERN#:10)        PASCP  768
                                ELSE WRITE(OUTPUT,#NOT EXTERN#:10);             PASCP  769
                              END                                               PASCP  770
                            ELSE WRITE(OUTPUT,#FORMAL#:10)                      PASCP  771
                          END                                                   PASCP  772
                     END                                                        PASCP  773
            END (*CASE*);                                                       PASCP  774
            WRITELN(OUTPUT); FOLLOWCTP(LLINK); FOLLOWCTP(RLINK);                PASCP  775
            FOLLOWSTP(IDTYPE)                                                   PASCP  776
          END (*WITH*)                                                          PASCP  777
    END (*FOLLOWCTP*);                                                          PASCP  778
                                                                                PASCP  779
  BEGIN (*PRINTTABLES*)                                                         PASCP  780
    WRITELN(OUTPUT); WRITELN(OUTPUT); WRITELN(OUTPUT);                          PASCP  781
    IF FB THEN LIM := 0                                                         PASCP  782
    ELSE BEGIN LIM := TOP; WRITE(OUTPUT,# LOCAL#) END;                          PASCP  783
    WRITELN(OUTPUT,# TABLES #); WRITELN(OUTPUT);                                PASCP  784
    MARKER;                                                                     PASCP  785
    FOR I := TOP DOWNTO LIM DO                                                  PASCP  786
      FOLLOWCTP(DISPLAY[I].FNAME);                                              PASCP  787
      WRITELN(OUTPUT);                                                          PASCP  788
      IF NOT EOL THEN WRITE(OUTPUT,# #:CHCNT+16)                                PASCP  789
  END (*PRINTTABLES*);                                                          PASCP  790
                                                                                PASCP  791
  PROCEDURE GENLABEL(VAR NXTLAB: INTEGER);                                      PASCP  792
  BEGIN INTLABEL := INTLABEL + 1;                                               PASCP  793
    NXTLAB := INTLABEL                                                          PASCP  794
  END (*GENLABEL*);                                                             PASCP  795
                                                                                PASCP  796
  PROCEDURE BLOCK(FSYS: SETOFSYS; FSY: SYMBOL; FPROCP: CTP);                    PASCP  797
    VAR LSY: SYMBOL; TEST: BOOLEAN;                                             PASCP  798
                                                                                PASCP  799
    PROCEDURE SKIP(FSYS: SETOFSYS);                                             PASCP  800
      (*SKIP INPUT STRING UNTIL RELEVANT SYMBOL FOUND*)                         PASCP  801
    BEGIN                                                                       P      124
      IF NOT EOF(INPUT) THEN                                                    P      125
        BEGIN WHILE NOT(SY IN FSYS) AND (NOT EOF(INPUT)) DO INSYMBOL;           P      126
          IF NOT (SY IN FSYS) THEN INSYMBOL                                     P      127
        END                                                                     P      128
    END (*SKIP*) ;                                                              PASCP  803
                                                                                PASCP  804
    PROCEDURE CONSTANT(FSYS: SETOFSYS; VAR FSP: STP; VAR FVALU: VALU);          PASCP  805
      VAR LSP: STP; LCP: CTP; SIGN: (NONE,POS,NEG);                             PASCP  806
          LVP: CSP; I: 2..STRGLGTH;                                             PASCP  807
    BEGIN LSP := NIL; FVALU.IVAL := 0;                                          PASCP  808
      IF NOT(SY IN CONSTBEGSYS) THEN                                            PASCP  809
        BEGIN ERROR(50); SKIP(FSYS+CONSTBEGSYS) END;                            PASCP  810
      IF SY IN CONSTBEGSYS THEN                                                 PASCP  811
        BEGIN                                                                   PASCP  812
          IF SY = STRINGCONSTSY THEN                                            PASCP  813
            BEGIN                                                               PASCP  814
              IF LGTH = 1 THEN LSP := CHARPTR                                   PASCP  815
              ELSE                                                              PASCP  816
                BEGIN                                                           PASCP  817
                  NEW(LSP,ARRAYS);                                              PASCP  818
                  WITH LSP' DO                                                  PASCP  819
                    BEGIN AELTYPE := CHARPTR; INXTYPE := NIL;                   PASCP  820
                       SIZE := LGTH*CHARSIZE; FORM := ARRAYS                    PASCP  821
                    END                                                         PASCP  822
                END;                                                            PASCP  823
              FVALU := VAL; INSYMBOL                                            PASCP  824
            END                                                                 PASCP  825
          ELSE                                                                  PASCP  826
            BEGIN                                                               PASCP  827
              SIGN := NONE;                                                     PASCP  828
              IF (SY = ADDOP) AND (OP IN [PLUS,MINUS]) THEN                     PASCP  829
                BEGIN IF OP = PLUS THEN SIGN := POS ELSE SIGN := NEG;           PASCP  830
                  INSYMBOL                                                      PASCP  831
                END;                                                            PASCP  832
              IF SY = IDENT THEN                                                PASCP  833
                BEGIN SEARCHID([KONST],LCP);                                    PASCP  834
                  WITH LCP' DO                                                  PASCP  835
                    BEGIN LSP := IDTYPE; FVALU := VALUES END;                   PASCP  836
                  IF SIGN <> NONE THEN                                          PASCP  837
                    IF LSP = INTPTR THEN                                        PASCP  838
                      BEGIN IF SIGN = NEG THEN FVALU.IVAL := -FVALU.IVAL END    PASCP  839
                    ELSE                                                        PASCP  840
                      IF LSP = REALPTR THEN                                     PASCP  841
                        BEGIN                                                   PASCP  842
                          IF SIGN = NEG THEN                                    PASCP  843
                            BEGIN NEW(LVP,REEL);                                PASCP  844
                              IF FVALU.VALP'.RVAL[1] = #-# THEN                 PASCP  845
                                LVP'.RVAL[1] := #+#                             PASCP  846
                              ELSE LVP'.RVAL[1] := #-#;                         PASCP  847
                              FOR I := 2 TO STRGLGTH DO                         PASCP  848
                                LVP'.RVAL[I] := FVALU.VALP'.RVAL[I];            PASCP  849
                              FVALU.VALP := LVP;                                PASCP  850
                            END                                                 PASCP  851
                          END                                                   PASCP  852
                        ELSE ERROR(105);                                        PASCP  853
                  INSYMBOL;                                                     PASCP  854
                END                                                             PASCP  855
              ELSE                                                              PASCP  856
                IF SY = INTCONST THEN                                           PASCP  857
                  BEGIN IF SIGN = NEG THEN VAL.IVAL := -VAL.IVAL;               PASCP  858
                    LSP := INTPTR; FVALU := VAL; INSYMBOL                       PASCP  859
                  END                                                           PASCP  860
                ELSE                                                            PASCP  861
                  IF SY = REALCONST THEN                                        PASCP  862
                    BEGIN IF SIGN = NEG THEN VAL.VALP'.RVAL[1] := #-#;          PASCP  863
                      LSP := REALPTR; FVALU := VAL; INSYMBOL                    PASCP  864
                    END                                                         PASCP  865
                  ELSE                                                          PASCP  866
                    BEGIN ERROR(106); SKIP(FSYS) END                            PASCP  867
            END;                                                                PASCP  868
          IF NOT (SY IN FSYS) THEN                                              PASCP  869
            BEGIN ERROR(6); SKIP(FSYS) END                                      PASCP  870
          END;                                                                  PASCP  871
      FSP := LSP                                                                PASCP  872
    END (*CONSTANT*) ;                                                          PASCP  873
                                                                                PASCP  874
    FUNCTION EQUALBOUNDS(FSP1,FSP2: STP): BOOLEAN;                              P      129
      VAR LMIN1,LMIN2,LMAX1,LMAX2: INTEGER;                                     P      130
    BEGIN                                                                       P      131
      IF (FSP1=NIL) OR (FSP2=NIL) THEN EQUALBOUNDS := TRUE                      P      132
      ELSE                                                                      P      133
        BEGIN                                                                   P      134
          GETBOUNDS(FSP1,LMIN1,LMAX1);                                          P      135
          GETBOUNDS(FSP2,LMIN2,LMAX2);                                          P      136
          EQUALBOUNDS := (LMIN1=LMIN2) AND (LMAX1=LMAX2)                        P      137
        END                                                                     P      138
    END (*EQUALBOUNDS*) ;                                                       P      139
                                                                                P      140
    FUNCTION COMPTYPES(FSP1,FSP2: STP) : BOOLEAN;                               PASCP  875
      (*DECIDE WHETHER STRUCTURES POINTED AT BY FSP1 AND FSP2 ARE COMPATIBLE*)  PASCP  876
      VAR NXT1,NXT2: CTP; COMP: BOOLEAN;                                        PASCP  877
        LTESTP1,LTESTP2 : TESTP;                                                PASCP  878
    BEGIN                                                                       PASCP  879
      IF FSP1 = FSP2 THEN COMPTYPES := TRUE                                     PASCP  880
      ELSE                                                                      PASCP  881
        IF (FSP1 <> NIL) AND (FSP2 <> NIL) THEN                                 PASCP  882
          IF FSP1'.FORM = FSP2'.FORM THEN                                       PASCP  883
            CASE FSP1'.FORM OF                                                  PASCP  884
              SCALAR:                                                           PASCP  885
                COMPTYPES := FALSE;                                             PASCP  886
                (* IDENTICAL SCALARS DECLARED ON DIFFERENT LEVELS ARE           PASCP  887
                 NOT RECOGNIZED TO BE COMPATIBLE*)                              PASCP  888
              SUBRANGE:                                                         PASCP  889
                COMPTYPES := COMPTYPES(FSP1'.RANGETYPE,FSP2'.RANGETYPE);        PASCP  890
              POINTER:                                                          PASCP  891
                  BEGIN                                                         PASCP  892
                    COMP := FALSE; LTESTP1 := GLOBTESTP;                        PASCP  893
                    LTESTP2 := GLOBTESTP;                                       PASCP  894
                    WHILE LTESTP1 <> NIL DO                                     PASCP  895
                      WITH LTESTP1' DO                                          PASCP  896
                        BEGIN                                                   PASCP  897
                          IF (ELT1 = FSP1'.ELTYPE) AND                          PASCP  898
                            (ELT2 = FSP2'.ELTYPE) THEN COMP := TRUE;            PASCP  899
                          LTESTP1 := LASTTESTP                                  PASCP  900
                        END;                                                    PASCP  901
                    IF NOT COMP THEN                                            PASCP  902
                      BEGIN NEW(LTESTP1);                                       PASCP  903
                        WITH LTESTP1' DO                                        PASCP  904
                          BEGIN ELT1 := FSP1'.ELTYPE;                           PASCP  905
                            ELT2 := FSP2'.ELTYPE;                               PASCP  906
                            LASTTESTP := GLOBTESTP                              PASCP  907
                          END;                                                  PASCP  908
                        GLOBTESTP := LTESTP1;                                   PASCP  909
                        COMP := COMPTYPES(FSP1'.ELTYPE,FSP2'.ELTYPE)            PASCP  910
                      END;                                                      PASCP  911
                    COMPTYPES := COMP; GLOBTESTP := LTESTP2                     PASCP  912
                  END;                                                          PASCP  913
              POWER:                                                            PASCP  914
                COMPTYPES := COMPTYPES(FSP1'.ELSET,FSP2'.ELSET);                PASCP  915
              ARRAYS:                                                           PASCP  916
                BEGIN                                                           P      141
                  COMP := COMPTYPES(FSP1'.AELTYPE,FSP2'.AELTYPE)                P      142
                      AND COMPTYPES(FSP1'.INXTYPE,FSP2'.INXTYPE);               P      143
                  COMPTYPES := COMP AND                                         P      144
                      EQUALBOUNDS(FSP1'.INXTYPE,FSP2'.INXTYPE)                  P      145
                      AND (FSP1'.SIZE = FSP2'.SIZE)                             KEN      3
               END;                                                             P      146
              RECORDS:                                                          PASCP  923
                BEGIN NXT1 := FSP1'.FSTFLD; NXT2 := FSP2'.FSTFLD; COMP:=TRUE;   PASCP  924
                  WHILE (NXT1 <> NIL) AND (NXT2 <> NIL) DO                      PASCP  925
                    BEGIN COMP:=COMP AND COMPTYPES(NXT1'.IDTYPE,NXT2'.IDTYPE);  PASCP  926
                      NXT1 := NXT1'.NEXT; NXT2 := NXT2'.NEXT                    PASCP  927
                    END;                                                        PASCP  928
                  COMPTYPES := COMP AND (NXT1 = NIL) AND (NXT2 = NIL)           PASCP  929
                              AND(FSP1'.RECVAR = NIL)AND(FSP2'.RECVAR = NIL)    PASCP  930
                END;                                                            PASCP  931
                (*IDENTICAL RECORDS ARE RECOGNIZED TO BE COMPATIBLE             PASCP  932
                 IFF NO VARIANTS OCCUR*)                                        PASCP  933
              FILES:                                                            PASCP  934
                COMPTYPES := COMPTYPES(FSP1'.FILTYPE,FSP2'.FILTYPE)             PASCP  935
            END (*CASE*)                                                        PASCP  936
          ELSE (*FSP1'.FORM <> FSP2'.FORM*)                                     PASCP  937
            IF FSP1'.FORM = SUBRANGE THEN                                       PASCP  938
              COMPTYPES := COMPTYPES(FSP1'.RANGETYPE,FSP2)                      PASCP  939
            ELSE                                                                PASCP  940
              IF FSP2'.FORM = SUBRANGE THEN                                     PASCP  941
                COMPTYPES := COMPTYPES(FSP1,FSP2'.RANGETYPE)                    PASCP  942
              ELSE COMPTYPES := FALSE                                           PASCP  943
        ELSE COMPTYPES := TRUE                                                  PASCP  944
    END (*COMPTYPES*) ;                                                         PASCP  945
                                                                                PASCP  946
    FUNCTION STRING(FSP: STP) : BOOLEAN;                                        PASCP  947
    BEGIN STRING := FALSE;                                                      PASCP  948
      IF FSP <> NIL THEN                                                        PASCP  949
        IF FSP'.FORM = ARRAYS THEN                                              PASCP  950
          IF COMPTYPES(FSP'.AELTYPE,CHARPTR) THEN STRING := TRUE                PASCP  951
    END (*STRING*) ;                                                            PASCP  952
                                                                                PASCP  953
    PROCEDURE TYP(FSYS: SETOFSYS; VAR FSP: STP; VAR FSIZE: ADDRRANGE);          PASCP  954
      VAR LSP,LSP1,LSP2: STP; OLDTOP: DISPRANGE; LCP: CTP;                      PASCP  955
          LSIZE,DISPL: ADDRRANGE; LMIN,LMAX: INTEGER;                           PASCP  956
                                                                                PASCP  957
      PROCEDURE SIMPLETYPE(FSYS:SETOFSYS; VAR FSP:STP; VAR FSIZE:ADDRRANGE);    PASCP  958
        VAR LSP,LSP1: STP; LCP,LCP1: CTP; TTOP: DISPRANGE;                      PASCP  959
            LCNT: INTEGER; LVALU: VALU;                                         PASCP  960
      BEGIN FSIZE := 1;                                                         PASCP  961
        IF NOT (SY IN SIMPTYPEBEGSYS) THEN                                      PASCP  962
          BEGIN ERROR(1); SKIP(FSYS + SIMPTYPEBEGSYS) END;                      PASCP  963
        IF SY IN SIMPTYPEBEGSYS THEN                                            PASCP  964
          BEGIN                                                                 PASCP  965
            IF SY = LPARENT THEN                                                PASCP  966
              BEGIN TTOP := TOP;   (*DECL. CONSTS LOCAL TO INNERMOST BLOCK*)    PASCP  967
                WHILE DISPLAY[TOP].OCCUR <> BLCK DO TOP := TOP - 1;             PASCP  968
                NEW(LSP,SCALAR,DECLARED);                                       PASCP  969
                WITH LSP' DO                                                    PASCP  970
                  BEGIN SIZE := INTSIZE; FORM := SCALAR;                        PASCP  971
                    SCALKIND := DECLARED                                        PASCP  972
                  END;                                                          PASCP  973
                LCP1 := NIL; LCNT := 0;                                         PASCP  974
                REPEAT INSYMBOL;                                                PASCP  975
                  IF SY = IDENT THEN                                            PASCP  976
                    BEGIN NEW(LCP,KONST);                                       PASCP  977
                      WITH LCP' DO                                              PASCP  978
                        BEGIN NAME := ID; IDTYPE := LSP; NEXT := LCP1;          PASCP  979
                          VALUES.IVAL := LCNT; KLASS := KONST                   PASCP  980
                        END;                                                    PASCP  981
                      ENTERID(LCP);                                             PASCP  982
                      LCNT := LCNT + 1;                                         PASCP  983
                      LCP1 := LCP; INSYMBOL                                     PASCP  984
                    END                                                         PASCP  985
                  ELSE ERROR(2);                                                PASCP  986
                  IF NOT (SY IN FSYS + [COMMA,RPARENT]) THEN                    PASCP  987
                    BEGIN ERROR(6); SKIP(FSYS + [COMMA,RPARENT]) END            PASCP  988
                UNTIL SY <> COMMA;                                              PASCP  989
                LSP'.FCONST := LCP1; TOP := TTOP;                               PASCP  990
                IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)                     PASCP  991
              END                                                               PASCP  992
            ELSE                                                                PASCP  993
              BEGIN                                                             PASCP  994
                IF SY = IDENT THEN                                              PASCP  995
                  BEGIN SEARCHID([TYPES,KONST],LCP);                            PASCP  996
                    INSYMBOL;                                                   PASCP  997
                    IF LCP'.KLASS = KONST THEN                                  PASCP  998
                      BEGIN NEW(LSP,SUBRANGE);                                  PASCP  999
                        WITH LSP', LCP' DO                                      PASCP 1000
                          BEGIN RANGETYPE := IDTYPE; FORM := SUBRANGE;          PASCP 1001
                            IF STRING(RANGETYPE) THEN                           PASCP 1002
                              BEGIN ERROR(148); RANGETYPE := NIL END;           PASCP 1003
                            MIN := VALUES; SIZE := INTSIZE                      PASCP 1004
                          END;                                                  PASCP 1005
                        IF SY = COLON THEN INSYMBOL ELSE ERROR(5);              PASCP 1006
                        CONSTANT(FSYS,LSP1,LVALU);                              PASCP 1007
                        LSP'.MAX := LVALU;                                      PASCP 1008
                        IF LSP'.RANGETYPE <> LSP1 THEN ERROR(107)               PASCP 1009
                      END                                                       PASCP 1010
                    ELSE                                                        PASCP 1011
                      BEGIN LSP := LCP'.IDTYPE;                                 PASCP 1012
                        IF LSP <> NIL THEN FSIZE := LSP'.SIZE                   PASCP 1013
                      END                                                       PASCP 1014
                  END (*SY = IDENT*)                                            PASCP 1015
                ELSE                                                            PASCP 1016
                  BEGIN NEW(LSP,SUBRANGE); LSP'.FORM := SUBRANGE;               PASCP 1017
                    CONSTANT(FSYS + [COLON],LSP1,LVALU);                        PASCP 1018
                    IF STRING(LSP1) THEN                                        PASCP 1019
                      BEGIN ERROR(148); LSP1 := NIL END;                        PASCP 1020
                    WITH LSP' DO                                                PASCP 1021
                      BEGIN RANGETYPE:=LSP1; MIN:=LVALU; SIZE:=INTSIZE END;     PASCP 1022
                    IF SY = COLON THEN INSYMBOL ELSE ERROR(5);                  PASCP 1023
                    CONSTANT(FSYS,LSP1,LVALU);                                  PASCP 1024
                    LSP'.MAX := LVALU;                                          PASCP 1025
                    IF LSP'.RANGETYPE <> LSP1 THEN ERROR(107)                   PASCP 1026
                  END;                                                          PASCP 1027
                IF LSP <> NIL THEN                                              PASCP 1028
                  WITH LSP' DO                                                  PASCP 1029
                    IF FORM = SUBRANGE THEN                                     PASCP 1030
                      IF RANGETYPE <> NIL THEN                                  PASCP 1031
                        IF RANGETYPE = REALPTR THEN ERROR(399)                  PASCP 1032
                        ELSE                                                    PASCP 1033
                          IF MIN.IVAL > MAX.IVAL THEN ERROR(102)                PASCP 1034
              END;                                                              PASCP 1035
            FSP := LSP;                                                         PASCP 1036
            IF NOT (SY IN FSYS) THEN                                            PASCP 1037
              BEGIN ERROR(6); SKIP(FSYS) END                                    PASCP 1038
          END                                                                   PASCP 1039
            ELSE FSP := NIL                                                     PASCP 1040
      END (*SIMPLETYPE*) ;                                                      PASCP 1041
                                                                                PASCP 1042
      PROCEDURE FIELDLIST(FSYS: SETOFSYS; VAR FRECVAR: STP);                    PASCP 1043
        VAR LCP,LCP1,NXT,NXT1: CTP; LSP,LSP1,LSP2,LSP3,LSP4: STP;               PASCP 1044
            MINSIZE,MAXSIZE,LSIZE: ADDRRANGE; LVALU: VALU;                      PASCP 1045
      BEGIN NXT1 := NIL; LSP := NIL;                                            PASCP 1046
        IF NOT (SY IN (FSYS+[IDENT,CASESY])) THEN                               X2       1
          BEGIN ERROR(19); SKIP(FSYS + [IDENT,CASESY]) END;                     PASCP 1048
        WHILE SY = IDENT DO                                                     PASCP 1049
          BEGIN NXT := NXT1;                                                    PASCP 1050
            REPEAT                                                              PASCP 1051
              IF SY = IDENT THEN                                                PASCP 1052
                BEGIN NEW(LCP,FIELD);                                           PASCP 1053
                  WITH LCP' DO                                                  PASCP 1054
                    BEGIN NAME := ID; IDTYPE := NIL; NEXT := NXT;               PASCP 1055
                      KLASS := FIELD                                            PASCP 1056
                    END;                                                        PASCP 1057
                  NXT := LCP;                                                   PASCP 1058
                  ENTERID(LCP);                                                 PASCP 1059
                  INSYMBOL                                                      PASCP 1060
                END                                                             PASCP 1061
              ELSE ERROR(2);                                                    PASCP 1062
              IF NOT (SY IN [COMMA,COLON]) THEN                                 PASCP 1063
                BEGIN ERROR(6); SKIP(FSYS + [COMMA,COLON,SEMICOLON,CASESY])     PASCP 1064
                END;                                                            PASCP 1065
            TEST := SY <> COMMA;                                                PASCP 1066
              IF NOT TEST  THEN INSYMBOL                                        PASCP 1067
            UNTIL TEST;                                                         PASCP 1068
            IF SY = COLON THEN INSYMBOL ELSE ERROR(5);                          PASCP 1069
            TYP(FSYS + [CASESY,SEMICOLON],LSP,LSIZE);                           PASCP 1070
            WHILE NXT <> NXT1 DO                                                PASCP 1071
              WITH NXT' DO                                                      PASCP 1072
                BEGIN ALIGN(LSP,DISPL);                                         X3       1
                  IDTYPE := LSP; FLDADDR := DISPL;                              X3       2
                  NXT := NEXT; DISPL := DISPL + LSIZE                           PASCP 1074
                END;                                                            PASCP 1075
            NXT1 := LCP;                                                        PASCP 1076
            WHILE SY = SEMICOLON DO                                             PASCP 1077
              BEGIN INSYMBOL;                                                   PASCP 1078
                IF NOT (SY IN [IDENT,CASESY,SEMICOLON]) THEN                    PASCP 1079
                  BEGIN ERROR(19); SKIP(FSYS + [IDENT,CASESY]) END              PASCP 1080
              END                                                               PASCP 1081
          END (*WHILE*);                                                        PASCP 1082
        NXT := NIL;                                                             PASCP 1083
        WHILE NXT1 <> NIL DO                                                    PASCP 1084
          WITH NXT1' DO                                                         PASCP 1085
            BEGIN LCP := NEXT; NEXT := NXT; NXT := NXT1; NXT1 := LCP END;       PASCP 1086
        IF SY = CASESY THEN                                                     PASCP 1087
          BEGIN NEW(LSP,TAGFLD);                                                PASCP 1088
            WITH LSP' DO                                                        PASCP 1089
              BEGIN TAGFIELDP := NIL; FSTVAR := NIL; FORM:=TAGFLD END;          PASCP 1090
            FRECVAR := LSP;                                                     PASCP 1091
            INSYMBOL;                                                           PASCP 1092
            IF SY = IDENT THEN                                                  PASCP 1093
              BEGIN NEW(LCP,FIELD);                                             PASCP 1094
                WITH LCP' DO                                                    PASCP 1095
                  BEGIN NAME := ID; IDTYPE := NIL; KLASS:=FIELD;                PASCP 1096
                    NEXT := NIL; FLDADDR := DISPL                               PASCP 1097
                  END;                                                          PASCP 1098
                ENTERID(LCP);                                                   PASCP 1099
                INSYMBOL;                                                       PASCP 1100
                IF SY = COLON THEN INSYMBOL ELSE ERROR(5);                      PASCP 1101
                IF SY = IDENT THEN                                              PASCP 1102
                  BEGIN SEARCHID([TYPES],LCP1);                                 PASCP 1103
                    LSP1 := LCP1'.IDTYPE;                                       PASCP 1104
                    IF LSP1 <> NIL THEN                                         PASCP 1105
                      BEGIN ALIGN(LSP1,DISPL);                                  X4       1
                        LCP'.FLDADDR := DISPL;                                  P      148
                        DISPL := DISPL+LSP1'.SIZE;                              P      149
                        IF (LSP1'.FORM <= SUBRANGE) OR STRING(LSP1) THEN        PASCP 1107
                          BEGIN IF COMPTYPES(REALPTR,LSP1) THEN ERROR(109)      PASCP 1108
                            ELSE IF STRING(LSP1) THEN ERROR(399);               PASCP 1109
                            LCP'.IDTYPE := LSP1; LSP'.TAGFIELDP := LCP;         PASCP 1110
                          END                                                   PASCP 1111
                        ELSE ERROR(110);                                        PASCP 1112
                    END;                                                        PASCP 1113
                    INSYMBOL;                                                   PASCP 1114
                  END                                                           PASCP 1115
                ELSE BEGIN ERROR(2); SKIP(FSYS + [OFSY,LPARENT]) END            PASCP 1116
              END                                                               PASCP 1117
            ELSE BEGIN ERROR(2); SKIP(FSYS + [OFSY,LPARENT]) END;               PASCP 1118
            LSP'.SIZE := DISPL;                                                 PASCP 1119
            IF SY = OFSY THEN INSYMBOL ELSE ERROR(8);                           PASCP 1120
            LSP1 := NIL; MINSIZE := DISPL; MAXSIZE := DISPL;                    PASCP 1121
            REPEAT LSP2 := NIL;                                                 PASCP 1122
              IF NOT (SY IN FSYS + [SEMICOLON]) THEN                            P      150
              BEGIN                                                             P      151
              REPEAT CONSTANT(FSYS + [COMMA,COLON,LPARENT],LSP3,LVALU);         PASCP 1123
                IF LSP'.TAGFIELDP <> NIL THEN                                   PASCP 1124
                 IF NOT COMPTYPES(LSP'.TAGFIELDP'.IDTYPE,LSP3)THEN ERROR(111);  PASCP 1125
                NEW(LSP3,VARIANT);                                              PASCP 1126
                WITH LSP3' DO                                                   PASCP 1127
                  BEGIN NXTVAR := LSP1; SUBVAR := LSP2; VARVAL := LVALU;        PASCP 1128
                    FORM := VARIANT                                             PASCP 1129
                  END;                                                          PASCP 1130
                LSP4 := LSP1;                                                   P      152
                WHILE LSP4 <> NIL DO                                            P      153
                  WITH LSP4' DO                                                 P      154
                    BEGIN                                                       P      155
                      IF VARVAL.IVAL = LVALU.IVAL THEN ERROR(178);              P      156
                      LSP4 := NXTVAR                                            P      157
                    END;                                                        P      158
                LSP1 := LSP3; LSP2 := LSP3;                                     PASCP 1131
                TEST := SY <> COMMA;                                            PASCP 1132
                IF NOT TEST THEN INSYMBOL                                       PASCP 1133
              UNTIL TEST;                                                       PASCP 1134
              IF SY = COLON THEN INSYMBOL ELSE ERROR(5);                        PASCP 1135
              IF SY = LPARENT THEN INSYMBOL ELSE ERROR(9);                      PASCP 1136
              FIELDLIST(FSYS + [RPARENT,SEMICOLON],LSP2);                       PASCP 1137
              IF DISPL > MAXSIZE THEN MAXSIZE := DISPL;                         PASCP 1138
              WHILE LSP3 <> NIL DO                                              PASCP 1139
                BEGIN LSP4 := LSP3'.SUBVAR; LSP3'.SUBVAR := LSP2;               PASCP 1140
                  LSP3'.SIZE := DISPL;                                          PASCP 1141
                  LSP3 := LSP4                                                  PASCP 1142
                END;                                                            PASCP 1143
              IF SY = RPARENT THEN                                              PASCP 1144
                BEGIN INSYMBOL;                                                 PASCP 1145
                  IF NOT (SY IN FSYS + [SEMICOLON]) THEN                        PASCP 1146
                    BEGIN ERROR(6); SKIP(FSYS + [SEMICOLON]) END                PASCP 1147
                END                                                             PASCP 1148
              ELSE ERROR(4);                                                    PASCP 1149
              END;                                                              P      159
              TEST := SY <> SEMICOLON;                                          PASCP 1150
              IF NOT TEST THEN                                                  PASCP 1151
                BEGIN DISPL := MINSIZE;                                         PASCP 1152
                      INSYMBOL                                                  PASCP 1153
                END                                                             PASCP 1154
            UNTIL TEST;                                                         PASCP 1155
            DISPL := MAXSIZE;                                                   PASCP 1156
            LSP'.FSTVAR := LSP1;                                                PASCP 1157
          END                                                                   PASCP 1158
        ELSE FRECVAR := NIL                                                     PASCP 1159
      END (*FIELDLIST*) ;                                                       PASCP 1160
                                                                                PASCP 1161
    BEGIN (*TYP*)                                                               PASCP 1162
      IF NOT (SY IN TYPEBEGSYS) THEN                                            PASCP 1163
         BEGIN ERROR(10); SKIP(FSYS + TYPEBEGSYS) END;                          PASCP 1164
      IF SY IN TYPEBEGSYS THEN                                                  PASCP 1165
        BEGIN                                                                   PASCP 1166
          IF SY IN SIMPTYPEBEGSYS THEN SIMPLETYPE(FSYS,FSP,FSIZE)               PASCP 1167
          ELSE                                                                  PASCP 1168
    (*'*)     IF SY = ARROW THEN                                                PASCP 1169
              BEGIN NEW(LSP,POINTER); FSP := LSP;                               PASCP 1170
                WITH LSP' DO                                                    PASCP 1171
                  BEGIN ELTYPE := NIL; SIZE := PTRSIZE; FORM:=POINTER END;      PASCP 1172
                INSYMBOL;                                                       PASCP 1173
                IF SY = IDENT THEN                                              PASCP 1174
                  BEGIN PRTERR := FALSE; (*NO ERROR IF SEARCH NOT SUCCESSFUL*)  PASCP 1175
                    SEARCHID([TYPES],LCP); PRTERR := TRUE;                      PASCP 1176
                    IF LCP = NIL THEN   (*FORWARD REFERENCED TYPE ID*)          PASCP 1177
                      BEGIN NEW(LCP,TYPES);                                     PASCP 1178
                        WITH LCP' DO                                            PASCP 1179
                          BEGIN NAME := ID; IDTYPE := LSP;                      PASCP 1180
                            NEXT := FWPTR; KLASS := TYPES                       PASCP 1181
                          END;                                                  PASCP 1182
                        FWPTR := LCP                                            PASCP 1183
                      END                                                       PASCP 1184
                    ELSE                                                        PASCP 1185
                      BEGIN                                                     PASCP 1186
                        IF LCP'.IDTYPE <> NIL THEN                              PASCP 1187
                          IF LCP'.IDTYPE'.FORM = FILES THEN ERROR(108)          PASCP 1188
                          ELSE LSP'.ELTYPE := LCP'.IDTYPE                       PASCP 1189
                      END;                                                      PASCP 1190
                    INSYMBOL;                                                   PASCP 1191
                  END                                                           PASCP 1192
                ELSE ERROR(2);                                                  PASCP 1193
              END                                                               PASCP 1194
            ELSE                                                                PASCP 1195
              BEGIN                                                             PASCP 1196
                IF SY = PACKEDSY THEN                                           PASCP 1197
                  BEGIN INSYMBOL;                                               PASCP 1198
                    IF NOT (SY IN TYPEDELS) THEN                                PASCP 1199
                      BEGIN                                                     PASCP 1200
                        ERROR(10); SKIP(FSYS + TYPEDELS)                        PASCP 1201
                      END                                                       PASCP 1202
                  END;                                                          PASCP 1203
    (*ARRAY*)     IF SY = ARRAYSY THEN                                          PASCP 1204
                  BEGIN INSYMBOL;                                               PASCP 1205
                    IF SY = LBRACK THEN INSYMBOL ELSE ERROR(11);                PASCP 1206
                    LSP1 := NIL;                                                PASCP 1207
                    REPEAT NEW(LSP,ARRAYS);                                     PASCP 1208
                      WITH LSP' DO                                              PASCP 1209
                        BEGIN AELTYPE := LSP1; INXTYPE := NIL; FORM:=ARRAYS END;PASCP 1210
                      LSP1 := LSP;                                              PASCP 1211
                      SIMPLETYPE(FSYS + [COMMA,RBRACK,OFSY],LSP2,LSIZE);        PASCP 1212
                      LSP1'.SIZE := LSIZE;                                      PASCP 1213
                      IF LSP2 <> NIL THEN                                       PASCP 1214
                        IF LSP2'.FORM <= SUBRANGE THEN                          PASCP 1215
                          BEGIN                                                 PASCP 1216
                            IF LSP2 = REALPTR THEN                              PASCP 1217
                              BEGIN ERROR(109); LSP2 := NIL END                 PASCP 1218
                            ELSE                                                PASCP 1219
                              IF LSP2 = INTPTR THEN                             PASCP 1220
                                BEGIN ERROR(149); LSP2 := NIL END;              PASCP 1221
                            LSP'.INXTYPE := LSP2                                PASCP 1222
                          END                                                   PASCP 1223
                        ELSE BEGIN ERROR(113); LSP2 := NIL END;                 PASCP 1224
                      TEST := SY <> COMMA;                                      PASCP 1225
                      IF NOT TEST THEN INSYMBOL                                 PASCP 1226
                    UNTIL TEST;                                                 PASCP 1227
                    IF SY = RBRACK THEN INSYMBOL ELSE ERROR(12);                PASCP 1228
                    IF SY = OFSY THEN INSYMBOL ELSE ERROR(8);                   PASCP 1229
                    TYP(FSYS,LSP,LSIZE);                                        PASCP 1230
                    REPEAT                                                      PASCP 1231
                      WITH LSP1' DO                                             PASCP 1232
                        BEGIN LSP2 := AELTYPE; AELTYPE := LSP;                  PASCP 1233
                          IF INXTYPE <> NIL THEN                                PASCP 1234
                            BEGIN GETBOUNDS(INXTYPE,LMIN,LMAX);                 PASCP 1235
                              ALIGN(LSP,LSIZE);                                 P      160
                              LSIZE := LSIZE*(LMAX - LMIN + 1);                 PASCP 1236
                              SIZE := LSIZE                                     PASCP 1237
                            END                                                 PASCP 1238
                        END;                                                    PASCP 1239
                      LSP := LSP1; LSP1 := LSP2                                 PASCP 1240
                    UNTIL LSP1 = NIL                                            PASCP 1241
                  END                                                           PASCP 1242
                ELSE                                                            PASCP 1243
    (*RECORD*)      IF SY = RECORDSY THEN                                       PASCP 1244
                    BEGIN INSYMBOL;                                             PASCP 1245
                      OLDTOP := TOP;                                            PASCP 1246
                      IF TOP < DISPLIMIT THEN                                   PASCP 1247
                        BEGIN TOP := TOP + 1;                                   PASCP 1248
                          WITH DISPLAY[TOP] DO                                  PASCP 1249
                            BEGIN FNAME := NIL;                                 PASCP 1250
                              FLABEL := NIL;                                    PASCP 1251
                                  OCCUR := REC                                  PASCP 1252
                            END                                                 PASCP 1253
                        END                                                     PASCP 1254
                      ELSE ERROR(250);                                          PASCP 1255
                      DISPL := 0;                                               PASCP 1256
                      FIELDLIST(FSYS-[SEMICOLON]+[ENDSY],LSP1);                 PASCP 1257
                      NEW(LSP,RECORDS);                                         PASCP 1258
                      WITH LSP' DO                                              PASCP 1259
                        BEGIN FSTFLD := DISPLAY[TOP].FNAME;                     PASCP 1260
                          RECVAR := LSP1; SIZE := DISPL; FORM := RECORDS        PASCP 1261
                        END;                                                    PASCP 1262
                      TOP := OLDTOP;                                            PASCP 1263
                      IF SY = ENDSY THEN INSYMBOL ELSE ERROR(13)                PASCP 1264
                    END                                                         PASCP 1265
                  ELSE                                                          PASCP 1266
    (*SET*)           IF SY = SETSY THEN                                        PASCP 1267
                      BEGIN INSYMBOL;                                           PASCP 1268
                        IF SY = OFSY THEN INSYMBOL ELSE ERROR(8);               PASCP 1269
                        SIMPLETYPE(FSYS,LSP1,LSIZE);                            PASCP 1270
                        IF LSP1 <> NIL THEN                                     PASCP 1271
                          IF LSP1'.FORM > SUBRANGE THEN                         PASCP 1272
                            BEGIN ERROR(115); LSP1 := NIL END                   PASCP 1273
                          ELSE                                                  PASCP 1274
                            IF LSP1 = REALPTR THEN                              KEN      4
                              BEGIN ERROR(114); LSP1 := NIL END                 KEN      5
                            ELSE IF LSP1 = INTPTR THEN                          KEN      6
                              BEGIN ERROR(169); LSP1 := NIL END                 KEN      7
                            ELSE                                                KEN      8
                              BEGIN GETBOUNDS(LSP1,LMIN,LMAX);                  KEN      9
                                IF (LMIN < SETLOW) OR (LMAX > SETHIGH) THEN     KEN     10
                                  ERROR(169);                                   KEN     11
                              END;                                              KEN     12
                        NEW(LSP,POWER);                                         PASCP 1276
                        WITH LSP' DO                                            PASCP 1277
                          BEGIN ELSET:=LSP1; SIZE:=SETSIZE; FORM:=POWER END;    PASCP 1278
                      END                                                       PASCP 1279
                    ELSE                                                        PASCP 1280
    (*FILE*)            IF SY = FILESY THEN                                     PASCP 1281
                          BEGIN INSYMBOL;                                       P      161
                            ERROR(399); SKIP(FSYS); LSP := NIL                  P      162
                          END;                                                  P      163
                FSP := LSP                                                      PASCP 1283
              END;                                                              PASCP 1284
          IF NOT (SY IN FSYS) THEN                                              PASCP 1285
            BEGIN ERROR(6); SKIP(FSYS) END                                      PASCP 1286
        END                                                                     PASCP 1287
      ELSE FSP := NIL;                                                          PASCP 1288
      IF FSP = NIL THEN FSIZE := 1 ELSE FSIZE := FSP'.SIZE                      PASCP 1289
    END (*TYP*) ;                                                               PASCP 1290
                                                                                PASCP 1291
    PROCEDURE LABELDECLARATION;                                                 PASCP 1292
      VAR LLP: LBP; REDEF: BOOLEAN; LBNAME: INTEGER;                            PASCP 1293
    BEGIN                                                                       PASCP 1294
      REPEAT                                                                    PASCP 1295
        IF SY = INTCONST THEN                                                   PASCP 1296
          WITH DISPLAY[TOP] DO                                                  PASCP 1297
            BEGIN LLP := FLABEL; REDEF := FALSE;                                PASCP 1298
              WHILE (LLP <> NIL) AND NOT REDEF DO                               PASCP 1299
                IF LLP'.LABVAL <> VAL.IVAL THEN                                 PASCP 1300
                  LLP := LLP'.NEXTLAB                                           PASCP 1301
                ELSE BEGIN REDEF := TRUE; ERROR(166) END;                       PASCP 1302
              IF NOT REDEF THEN                                                 PASCP 1303
                BEGIN NEW(LLP);                                                 PASCP 1304
                  WITH LLP' DO                                                  PASCP 1305
                    BEGIN LABVAL := VAL.IVAL; GENLABEL(LBNAME);                 PASCP 1306
                      DEFINED := FALSE; NEXTLAB := FLABEL; LABNAME := LBNAME    PASCP 1307
                    END;                                                        PASCP 1308
                  FLABEL := LLP                                                 PASCP 1309
                END;                                                            PASCP 1310
              INSYMBOL                                                          PASCP 1311
            END                                                                 PASCP 1312
        ELSE ERROR(15);                                                         PASCP 1313
        IF NOT ( SY IN FSYS + [COMMA, SEMICOLON] ) THEN                         PASCP 1314
          BEGIN ERROR(6); SKIP(FSYS+[COMMA,SEMICOLON]) END;                     PASCP 1315
        TEST := SY <> COMMA;                                                    PASCP 1316
        IF NOT TEST THEN INSYMBOL                                               PASCP 1317
      UNTIL TEST;                                                               PASCP 1318
      IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14)                            PASCP 1319
    END (* LABELDECLARATION *) ;                                                PASCP 1320
                                                                                PASCP 1321
    PROCEDURE CONSTDECLARATION;                                                 PASCP 1322
      VAR LCP: CTP; LSP: STP; LVALU: VALU;                                      PASCP 1323
    BEGIN                                                                       PASCP 1324
      IF SY <> IDENT THEN                                                       PASCP 1325
        BEGIN ERROR(2); SKIP(FSYS + [IDENT]) END;                               PASCP 1326
      WHILE SY = IDENT DO                                                       PASCP 1327
        BEGIN NEW(LCP,KONST);                                                   PASCP 1328
          WITH LCP' DO                                                          PASCP 1329
            BEGIN NAME := ID; IDTYPE := NIL; NEXT := NIL; KLASS:=KONST END;     PASCP 1330
          INSYMBOL;                                                             PASCP 1331
          IF (SY = RELOP) AND (OP = EQOP) THEN INSYMBOL ELSE ERROR(16);         PASCP 1332
          CONSTANT(FSYS + [SEMICOLON],LSP,LVALU);                               PASCP 1333
          ENTERID(LCP);                                                         PASCP 1334
          LCP'.IDTYPE := LSP; LCP'.VALUES := LVALU;                             PASCP 1335
          IF SY = SEMICOLON THEN                                                PASCP 1336
            BEGIN INSYMBOL;                                                     PASCP 1337
              IF NOT (SY IN FSYS + [IDENT]) THEN                                PASCP 1338
                BEGIN ERROR(6); SKIP(FSYS + [IDENT]) END                        PASCP 1339
            END                                                                 PASCP 1340
          ELSE ERROR(14)                                                        PASCP 1341
        END                                                                     PASCP 1342
    END (*CONSTDECLARATION*) ;                                                  PASCP 1343
                                                                                PASCP 1344
    PROCEDURE TYPEDECLARATION;                                                  PASCP 1345
      VAR LCP,LCP1,LCP2: CTP; LSP: STP; LSIZE: ADDRRANGE;                       PASCP 1346
    BEGIN                                                                       PASCP 1347
      IF SY <> IDENT THEN                                                       PASCP 1348
        BEGIN ERROR(2); SKIP(FSYS + [IDENT]) END;                               PASCP 1349
      WHILE SY = IDENT DO                                                       PASCP 1350
        BEGIN NEW(LCP,TYPES);                                                   PASCP 1351
          WITH LCP' DO                                                          PASCP 1352
            BEGIN NAME := ID; IDTYPE := NIL; KLASS := TYPES END;                PASCP 1353
          INSYMBOL;                                                             PASCP 1354
          IF (SY = RELOP) AND (OP = EQOP) THEN INSYMBOL ELSE ERROR(16);         PASCP 1355
          TYP(FSYS + [SEMICOLON],LSP,LSIZE);                                    PASCP 1356
          ENTERID(LCP);                                                         PASCP 1357
          LCP'.IDTYPE := LSP;                                                   PASCP 1358
          (*HAS ANY FORWARD REFERENCE BEEN SATISFIED:*)                         PASCP 1359
          LCP1 := FWPTR;                                                        PASCP 1360
          WHILE LCP1 <> NIL DO                                                  PASCP 1361
            BEGIN                                                               PASCP 1362
              IF LCP1'.NAME = LCP'.NAME THEN                                    PASCP 1363
                BEGIN LCP1'.IDTYPE'.ELTYPE := LCP'.IDTYPE;                      PASCP 1364
                  IF LCP1 <> FWPTR THEN                                         PASCP 1365
                    LCP2'.NEXT := LCP1'.NEXT                                    PASCP 1366
                  ELSE FWPTR := LCP1'.NEXT;                                     PASCP 1367
                END                                                             KEN     14
              ELSE LCP2 := LCP1;                                                KEN     15
              LCP1 := LCP1'.NEXT;                                               KEN     16
              LCP2 := LCP1; LCP1 := LCP1'.NEXT                                  PASCP 1369
            END;                                                                PASCP 1370
          IF SY = SEMICOLON THEN                                                PASCP 1371
            BEGIN INSYMBOL;                                                     PASCP 1372
              IF NOT (SY IN FSYS + [IDENT]) THEN                                PASCP 1373
                BEGIN ERROR(6); SKIP(FSYS + [IDENT]) END                        PASCP 1374
            END                                                                 PASCP 1375
          ELSE ERROR(14)                                                        PASCP 1376
        END;                                                                    PASCP 1377
      IF FWPTR <> NIL THEN                                                      PASCP 1378
        BEGIN ERROR(117); WRITELN(OUTPUT);                                      PASCP 1379
          REPEAT WRITELN(OUTPUT,# TYPE-ID #,FWPTR'.NAME);                       PASCP 1380
            FWPTR := FWPTR'.NEXT                                                PASCP 1381
          UNTIL FWPTR = NIL;                                                    PASCP 1382
          IF NOT EOL THEN WRITE(OUTPUT,# #: CHCNT+16)                           PASCP 1383
        END                                                                     PASCP 1384
    END (*TYPEDECLARATION*) ;                                                   PASCP 1385
                                                                                PASCP 1386
    PROCEDURE VARDECLARATION;                                                   PASCP 1387
      VAR LCP,NXT: CTP; LSP: STP; LSIZE: ADDRRANGE;                             PASCP 1388
    BEGIN NXT := NIL;                                                           PASCP 1389
      REPEAT                                                                    PASCP 1390
        REPEAT                                                                  PASCP 1391
          IF SY = IDENT THEN                                                    PASCP 1392
            BEGIN NEW(LCP,VARS);                                                PASCP 1393
              WITH LCP' DO                                                      PASCP 1394
               BEGIN NAME := ID; NEXT := NXT; KLASS := VARS;                    PASCP 1395
                  IDTYPE := NIL; VKIND := ACTUAL; VLEV := LEVEL                 PASCP 1396
                END;                                                            PASCP 1397
              ENTERID(LCP);                                                     PASCP 1398
              NXT := LCP;                                                       PASCP 1399
              INSYMBOL;                                                         PASCP 1400
            END                                                                 PASCP 1401
          ELSE ERROR(2);                                                        PASCP 1402
          IF NOT (SY IN FSYS + [COMMA,COLON] + TYPEDELS) THEN                   PASCP 1403
            BEGIN ERROR(6); SKIP(FSYS+[COMMA,COLON,SEMICOLON]+TYPEDELS) END;    PASCP 1404
          TEST := SY <> COMMA;                                                  PASCP 1405
          IF NOT TEST THEN INSYMBOL                                             PASCP 1406
        UNTIL TEST;                                                             PASCP 1407
        IF SY = COLON THEN INSYMBOL ELSE ERROR(5);                              PASCP 1408
        TYP(FSYS + [SEMICOLON] + TYPEDELS,LSP,LSIZE);                           PASCP 1409
        WHILE NXT <> NIL DO                                                     PASCP 1410
          WITH  NXT' DO                                                         PASCP 1411
            BEGIN ALIGN(LSP,LC);                                                P      164
              IDTYPE := LSP; VADDR := LC;                                       P      165
              LC := LC + LSIZE; NXT := NEXT                                     PASCP 1413
            END;                                                                PASCP 1414
        IF SY = SEMICOLON THEN                                                  PASCP 1415
          BEGIN INSYMBOL;                                                       PASCP 1416
            IF NOT (SY IN FSYS + [IDENT]) THEN                                  PASCP 1417
              BEGIN ERROR(6); SKIP(FSYS + [IDENT]) END                          PASCP 1418
          END                                                                   PASCP 1419
        ELSE ERROR(14)                                                          PASCP 1420
      UNTIL (SY <> IDENT) AND NOT (SY IN TYPEDELS);                             PASCP 1421
      IF FWPTR <> NIL THEN                                                      PASCP 1422
        BEGIN ERROR(117); WRITELN(OUTPUT);                                      PASCP 1423
          REPEAT WRITELN(OUTPUT,# TYPE-ID #,FWPTR'.NAME);                       PASCP 1424
            FWPTR := FWPTR'.NEXT                                                PASCP 1425
          UNTIL FWPTR = NIL;                                                    PASCP 1426
          IF NOT EOL THEN WRITE(OUTPUT,# #: CHCNT+16)                           PASCP 1427
        END                                                                     PASCP 1428
    END (*VARDECLARATION*) ;                                                    PASCP 1429
                                                                                PASCP 1430
    PROCEDURE PROCDECLARATION(FSY: SYMBOL);                                     PASCP 1431
      VAR OLDLEV: 0..MAXLEVEL; LSY: SYMBOL; LCP,LCP1: CTP; LSP: STP;            PASCP 1432
          FORW: BOOLEAN; OLDTOP: DISPRANGE; PARCNT: INTEGER;                    PASCP 1433
          LLC,LCM: ADDRRANGE; LBNAME: INTEGER; MARKP: 'INTEGER;                 PASCP 1434
                                                                                PASCP 1435
      PROCEDURE PARAMETERLIST(FSY: SETOFSYS; VAR FPAR: CTP);                    PASCP 1436
        VAR LCP,LCP1,LCP2,LCP3: CTP; LSP: STP; LKIND: IDKIND;                   PASCP 1437
          LLC,ADDRLSIZE: ADDRRANGE; COUNT,LSIZE: INTEGER;                       P      166
      BEGIN LCP1 := NIL;                                                        PASCP 1439
        IF NOT (SY IN FSY + [LPARENT]) THEN                                     PASCP 1440
          BEGIN ERROR(7); SKIP(FSYS + FSY + [LPARENT]) END;                     PASCP 1441
        IF SY = LPARENT THEN                                                    PASCP 1442
          BEGIN IF FORW THEN ERROR(119);                                        PASCP 1443
            INSYMBOL;                                                           PASCP 1444
            IF NOT (SY IN [IDENT,VARSY,PROCSY,FUNCSY]) THEN                     PASCP 1445
              BEGIN ERROR(7); SKIP(FSYS + [IDENT,RPARENT]) END;                 PASCP 1446
            WHILE SY IN [IDENT,VARSY,PROCSY,FUNCSY] DO                          PASCP 1447
              BEGIN                                                             PASCP 1448
                IF SY = PROCSY THEN                                             PASCP 1449
                  BEGIN ERROR(399);                                             PASCP 1450
                    REPEAT INSYMBOL;                                            PASCP 1451
                      IF SY = IDENT THEN                                        PASCP 1452
                      BEGIN NEW(LCP,PROC,DECLARED,FORMAL);                      PASCP 1453
                          WITH LCP' DO                                          PASCP 1454
                            BEGIN NAME := ID; IDTYPE := NIL; NEXT := LCP1;      PASCP 1455
                              PFLEV := LEVEL (*BEWARE OF PARAMETER PROCEDURES*);PASCP 1456
                              KLASS:=PROC;PFDECKIND:=DECLARED;PFKIND:=FORMAL    PASCP 1457
                            END;                                                PASCP 1458
                          ENTERID(LCP);                                         PASCP 1459
                          LCP1 := LCP;                                          P      167
                          ALIGN(PARMPTR,LC);                                    P      168
                          (*LC := LC + SOME SIZE *)                             P      169
                          INSYMBOL                                              PASCP 1461
                        END                                                     PASCP 1462
                      ELSE ERROR(2);                                            PASCP 1463
                      IF NOT (SY IN FSYS + [COMMA,SEMICOLON,RPARENT]) THEN      PASCP 1464
                        BEGIN ERROR(7);SKIP(FSYS+[COMMA,SEMICOLON,RPARENT])END  PASCP 1465
                    UNTIL SY <> COMMA                                           PASCP 1466
                  END                                                           PASCP 1467
                ELSE                                                            PASCP 1468
                  BEGIN                                                         PASCP 1469
                    IF SY = FUNCSY THEN                                         PASCP 1470
                      BEGIN ERROR(399); LCP2 := NIL;                            PASCP 1471
                        REPEAT INSYMBOL;                                        PASCP 1472
                          IF SY = IDENT THEN                                    PASCP 1473
                            BEGIN NEW(LCP,FUNC,DECLARED,FORMAL);                PASCP 1474
                              WITH LCP' DO                                      PASCP 1475
                                BEGIN NAME := ID; IDTYPE := NIL; NEXT := LCP2;  PASCP 1476
                                  PFLEV := LEVEL (*BEWARE PARAM FUNCS*);        PASCP 1477
                                  KLASS:=FUNC;PFDECKIND:=DECLARED;              PASCP 1478
                                  PFKIND:=FORMAL                                PASCP 1479
                                END;                                            PASCP 1480
                              ENTERID(LCP);                                     PASCP 1481
                             LCP2 := LCP;                                       P      170
                             ALIGN(PARMPTR,LC);                                 P      171
                             (*LC := LC + SOME SIZE*)                           P      172
                              INSYMBOL;                                         PASCP 1483
                            END;                                                PASCP 1484
                          IF NOT (SY IN [COMMA,COLON] + FSYS) THEN              PASCP 1485
                           BEGIN ERROR(7);SKIP(FSYS+[COMMA,SEMICOLON,RPARENT])  PASCP 1486
                            END                                                 PASCP 1487
                        UNTIL SY <> COMMA;                                      PASCP 1488
                        IF SY = COLON THEN                                      PASCP 1489
                          BEGIN INSYMBOL;                                       PASCP 1490
                            IF SY = IDENT THEN                                  PASCP 1491
                              BEGIN SEARCHID([TYPES],LCP);                      PASCP 1492
                                LSP := LCP'.IDTYPE;                             PASCP 1493
                                IF LSP <> NIL THEN                              PASCP 1494
                                 IF NOT(LSP'.FORM IN[SCALAR,SUBRANGE,POINTER])  PASCP 1495
                                    THEN BEGIN ERROR(120); LSP := NIL END;      PASCP 1496
                                LCP3 := LCP2;                                   PASCP 1497
                                WHILE LCP2 <> NIL DO                            PASCP 1498
                                  BEGIN LCP2'.IDTYPE := LSP; LCP := LCP2;       PASCP 1499
                                    LCP2 := LCP2'.NEXT                          PASCP 1500
                                  END;                                          PASCP 1501
                                LCP'.NEXT := LCP1; LCP1 := LCP3;                PASCP 1502
                                INSYMBOL                                        PASCP 1503
                              END                                               PASCP 1504
                            ELSE ERROR(2);                                      PASCP 1505
                            IF NOT (SY IN FSYS + [SEMICOLON,RPARENT]) THEN      PASCP 1506
                              BEGIN ERROR(7);SKIP(FSYS+[SEMICOLON,RPARENT])END  PASCP 1507
                          END                                                   PASCP 1508
                        ELSE ERROR(5)                                           PASCP 1509
                      END                                                       PASCP 1510
                    ELSE                                                        PASCP 1511
                      BEGIN                                                     PASCP 1512
                        IF SY = VARSY THEN                                      PASCP 1513
                          BEGIN LKIND := FORMAL; INSYMBOL END                   PASCP 1514
                        ELSE LKIND := ACTUAL;                                   PASCP 1515
                        LCP2 := NIL;                                            PASCP 1516
                        COUNT := 0;                                             PASCP 1517
                        REPEAT                                                  PASCP 1518
                          IF SY = IDENT THEN                                    PASCP 1519
                            BEGIN NEW(LCP,VARS);                                PASCP 1520
                              WITH LCP' DO                                      PASCP 1521
                                BEGIN NAME:=ID; IDTYPE:=NIL; KLASS:=VARS;       PASCP 1522
                                  VKIND := LKIND; NEXT := LCP2; VLEV := LEVEL;  PASCP 1523
                                END;                                            PASCP 1524
                              ENTERID(LCP);                                     PASCP 1525
                              LCP2 := LCP; COUNT := COUNT+1;                    PASCP 1526
                              INSYMBOL;                                         PASCP 1527
                            END;                                                PASCP 1528
                          IF NOT (SY IN [COMMA,COLON] + FSYS) THEN              PASCP 1529
                           BEGIN ERROR(7);SKIP(FSYS+[COMMA,SEMICOLON,RPARENT])  PASCP 1530
                            END;                                                PASCP 1531
                          TEST := SY <> COMMA;                                  PASCP 1532
                          IF NOT TEST THEN INSYMBOL                             PASCP 1533
                        UNTIL TEST;                                             PASCP 1534
                        IF SY = COLON THEN                                      PASCP 1535
                          BEGIN INSYMBOL;                                       PASCP 1536
                            IF SY = IDENT THEN                                  PASCP 1537
                              BEGIN SEARCHID([TYPES],LCP);                      PASCP 1538
                                LSP := LCP'.IDTYPE;                             PASCP 1539
                                LSIZE := PTRSIZE;                               P      173
                                IF LSP <> NIL THEN                              PASCP 1540
                                  IF LKIND=ACTUAL THEN                          P      174
                                    IF LSP'.FORM<=POWER THEN LSIZE := LSP'.SIZE P      175
                                    ELSE IF LSP'.FORM=FILES THEN ERROR(121);    P      176
                                ADDRLSIZE:=LSIZE; ALIGN(PARMPTR,ADDRLSIZE);     P      177
                                LCP3 := LCP2;                                   PASCP 1543
                                ALIGN(PARMPTR,LC);                              P      178
                                LC := LC+COUNT*LSIZE;                           P      179
                                LLC := LC;                                      PASCP 1548
                                WHILE LCP2 <> NIL DO                            PASCP 1549
                                  BEGIN LCP := LCP2;                            PASCP 1550
                                    WITH LCP2' DO                               PASCP 1551
                                      BEGIN IDTYPE := LSP;                      P      180
                                        LLC := LLC-LSIZE;                       P      181
                                        VADDR := LLC;                           PASCP 1553
                                      END;                                      PASCP 1554
                                    LCP2 := LCP2'.NEXT                          PASCP 1555
                                  END;                                          PASCP 1556
                                LCP'.NEXT := LCP1; LCP1 := LCP3;                PASCP 1557
                                INSYMBOL                                        PASCP 1558
                              END                                               PASCP 1559
                            ELSE ERROR(2);                                      PASCP 1560
                            IF NOT (SY IN FSYS + [SEMICOLON,RPARENT]) THEN      PASCP 1561
                              BEGIN ERROR(7);SKIP(FSYS+[SEMICOLON,RPARENT])END  PASCP 1562
                          END                                                   PASCP 1563
                        ELSE ERROR(5);                                          PASCP 1564
                      END;                                                      PASCP 1565
                  END;                                                          PASCP 1566
                IF SY = SEMICOLON THEN                                          PASCP 1567
                  BEGIN INSYMBOL;                                               PASCP 1568
                    IF NOT (SY IN FSYS + [IDENT,VARSY,PROCSY,FUNCSY]) THEN      PASCP 1569
                      BEGIN ERROR(7); SKIP(FSYS + [IDENT,RPARENT]) END          PASCP 1570
                  END                                                           PASCP 1571
              END (*WHILE*) ;                                                   PASCP 1572
            IF SY = RPARENT THEN                                                PASCP 1573
              BEGIN INSYMBOL;                                                   PASCP 1574
                IF NOT (SY IN FSY + FSYS) THEN                                  PASCP 1575
                  BEGIN ERROR(6); SKIP(FSY + FSYS) END                          PASCP 1576
              END                                                               PASCP 1577
            ELSE ERROR(4);                                                      PASCP 1578
            LCP3 := NIL;                                                        PASCP 1579
            (*REVERSE POINTERS AND RESERVE LOCAL CELLS FOR COPIES OF MULTIPLE   PASCP 1580
             VALUES*)                                                           PASCP 1581
            WHILE LCP1 <> NIL DO                                                PASCP 1582
              WITH LCP1' DO                                                     PASCP 1583
                BEGIN LCP2 := NEXT; NEXT := LCP3;                               PASCP 1584
                  IF KLASS = VARS THEN                                          PASCP 1585
                    IF IDTYPE <> NIL THEN                                       PASCP 1586
                      IF (VKIND=ACTUAL)AND(IDTYPE'.FORM>POWER) THEN             P      182
                        BEGIN ALIGN(IDTYPE,LC);                                 P      183
                          VADDR := LC;                                          P      184
                          LC := LC+IDTYPE'.SIZE;                                P      185
                        END;                                                    PASCP 1589
                  LCP3 := LCP1; LCP1 := LCP2                                    PASCP 1590
                END;                                                            PASCP 1591
            FPAR := LCP3                                                        PASCP 1592
          END                                                                   PASCP 1593
            ELSE FPAR := NIL                                                    PASCP 1594
    END (*PARAMETERLIST*) ;                                                     PASCP 1595
                                                                                PASCP 1596
    BEGIN (*PROCDECLARATION*)                                                   PASCP 1597
      LLC := LC; LC := LCAFTERMARKSTACK; FORW := FALSE;                         P      186
      IF SY = IDENT THEN                                                        PASCP 1599
        BEGIN SEARCHSECTION(DISPLAY[TOP].FNAME,LCP); (*DECIDE WHETHER FORW.*)   PASCP 1600
          IF LCP <> NIL THEN                                                    PASCP 1601
          BEGIN                                                                 PASCP 1602
            IF LCP'.KLASS = PROC THEN                                           PASCP 1603
              FORW := LCP'.FORWDECL AND(FSY = PROCSY)AND(LCP'.PFKIND = ACTUAL)  PASCP 1604
            ELSE                                                                PASCP 1605
              IF LCP'.KLASS = FUNC THEN                                         PASCP 1606
                FORW:=LCP'.FORWDECL AND(FSY=FUNCSY)AND(LCP'.PFKIND=ACTUAL)      PASCP 1607
              ELSE FORW := FALSE;                                               PASCP 1608
            IF NOT FORW THEN ERROR(160)                                         PASCP 1609
          END;                                                                  P      187
          IF NOT FORW THEN                                                      PASCP 1612
            BEGIN                                                               PASCP 1613
              IF FSY = PROCSY THEN NEW(LCP,PROC,DECLARED,ACTUAL)                PASCP 1614
              ELSE NEW(LCP,FUNC,DECLARED,ACTUAL);                               PASCP 1615
              WITH LCP' DO                                                      PASCP 1616
                BEGIN NAME := ID; IDTYPE := NIL;                                PASCP 1617
                  EXTERN := FALSE; PFLEV := LEVEL; GENLABEL(LBNAME);            PASCP 1618
                  PFDECKIND := DECLARED; PFKIND := ACTUAL; PFNAME := LBNAME;    PASCP 1619
                  IF FSY = PROCSY THEN KLASS := PROC                            PASCP 1620
                  ELSE KLASS := FUNC                                            PASCP 1621
                END;                                                            PASCP 1622
              ENTERID(LCP)                                                      PASCP 1623
            END                                                                 PASCP 1624
          ELSE                                                                  PASCP 1625
            BEGIN LCP1 := LCP'.NEXT;                                            PASCP 1626
              WHILE LCP1 <> NIL DO                                              PASCP 1627
                BEGIN                                                           PASCP 1628
                  WITH LCP1' DO                                                 PASCP 1629
                    IF KLASS = VARS THEN                                        PASCP 1630
                      IF IDTYPE <> NIL THEN                                     PASCP 1631
                        BEGIN LCM := VADDR + IDTYPE'.SIZE;                      PASCP 1632
                          IF LCM > LC THEN LC := LCM                            PASCP 1633
                        END;                                                    PASCP 1634
                  LCP1 := LCP1'.NEXT                                            PASCP 1635
                END                                                             PASCP 1636
              END;                                                              PASCP 1637
          INSYMBOL                                                              PASCP 1638
        END                                                                     PASCP 1639
      ELSE                                                                      P      188
        BEGIN ERROR(2); LCP := UFCTPTR END;                                     P      189
      OLDLEV := LEVEL; OLDTOP := TOP;                                           PASCP 1641
      IF LEVEL < MAXLEVEL THEN LEVEL := LEVEL + 1 ELSE ERROR(251);              PASCP 1642
      IF TOP < DISPLIMIT THEN                                                   PASCP 1643
        BEGIN TOP := TOP + 1;                                                   PASCP 1644
          WITH DISPLAY[TOP] DO                                                  PASCP 1645
            BEGIN                                                               PASCP 1646
              IF FORW THEN FNAME := LCP'.NEXT                                   PASCP 1647
              ELSE FNAME := NIL;                                                PASCP 1648
              FLABEL := NIL;                                                    PASCP 1649
              OCCUR := BLCK                                                     PASCP 1650
            END                                                                 PASCP 1651
        END                                                                     PASCP 1652
      ELSE ERROR(250);                                                          PASCP 1653
      IF FSY = PROCSY THEN                                                      PASCP 1654
        BEGIN PARAMETERLIST([SEMICOLON],LCP1);                                  PASCP 1655
          IF NOT FORW THEN LCP'.NEXT := LCP1                                    PASCP 1656
        END                                                                     PASCP 1657
      ELSE                                                                      PASCP 1658
        BEGIN PARAMETERLIST([SEMICOLON,COLON],LCP1);                            PASCP 1659
          IF NOT FORW THEN LCP'.NEXT := LCP1;                                   PASCP 1660
          IF SY = COLON THEN                                                    PASCP 1661
            BEGIN INSYMBOL;                                                     PASCP 1662
              IF SY = IDENT THEN                                                PASCP 1663
                BEGIN IF FORW THEN ERROR(122);                                  PASCP 1664
                  SEARCHID([TYPES],LCP1);                                       PASCP 1665
                  LSP := LCP1'.IDTYPE;                                          PASCP 1666
                  LCP'.IDTYPE := LSP;                                           PASCP 1667
                  IF LSP <> NIL THEN                                            PASCP 1668
                    IF NOT (LSP'.FORM IN [SCALAR,SUBRANGE,POINTER]) THEN        PASCP 1669
                      BEGIN ERROR(120); LCP'.IDTYPE := NIL END;                 PASCP 1670
                  INSYMBOL                                                      PASCP 1671
                END                                                             PASCP 1672
              ELSE BEGIN ERROR(2); SKIP(FSYS + [SEMICOLON]) END                 PASCP 1673
            END                                                                 PASCP 1674
          ELSE                                                                  PASCP 1675
            IF NOT FORW THEN ERROR(123)                                         PASCP 1676
        END;                                                                    PASCP 1677
      IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);                           PASCP 1678
      IF SY = FORWARDSY THEN                                                    PASCP 1679
        BEGIN                                                                   PASCP 1680
          IF FORW THEN ERROR(161)                                               PASCP 1681
          ELSE LCP'.FORWDECL := TRUE;                                           PASCP 1682
          INSYMBOL;                                                             PASCP 1683
          IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);                       PASCP 1684
          IF NOT (SY IN FSYS) THEN                                              PASCP 1685
            BEGIN ERROR(6); SKIP(FSYS) END                                      PASCP 1686
        END                                                                     PASCP 1687
      ELSE                                                                      PASCP 1688
        BEGIN LCP'.FORWDECL := FALSE; MARK(MARKP);                              BOOT     3
          REPEAT BLOCK(FSYS,SEMICOLON,LCP);                                     PASCP 1690
            IF SY = SEMICOLON THEN                                              PASCP 1691
              BEGIN IF PRTABLES THEN PRINTTABLES(FALSE); INSYMBOL;              PASCP 1692
                IF NOT (SY IN [BEGINSY,PROCSY,FUNCSY]) THEN                     PASCP 1693
                  BEGIN ERROR(6); SKIP(FSYS) END                                PASCP 1694
              END                                                               PASCP 1695
            ELSE ERROR(14)                                                      PASCP 1696
          UNTIL (SY IN [BEGINSY,PROCSY,FUNCSY]) OR EOF(INPUT);                  P      190
          RELEASE(MARKP); (* RETURN LOCAL ENTRIES ON RUNTIME HEAP *)            PASCP 1698
        END;                                                                    PASCP 1699
      LEVEL := OLDLEV; TOP := OLDTOP; LC := LLC;                                PASCP 1700
    END (*PROCDECLARATION*) ;                                                   PASCP 1701
                                                                                PASCP 1702
    PROCEDURE BODY(FSYS: SETOFSYS);                                             PASCP 1703
      CONST CSTOCCMAX=65; CIXMAX=1000;                                          J        3
      TYPE OPRANGE = 0..63;                                                     PASCP 1705
      VAR                                                                       PASCP 1706
          LLCP:CTP; SAVEID:ALPHA;                                               PASCP 1707
          CSTPTR: ARRAY [1..CSTOCCMAX] OF CSP;                                  PASCP 1708
          CSTPTRIX: 0..CSTOCCMAX;                                               PASCP 1709
          (*ALLOWS REFERENCING OF NONINTEGER CONSTANTS BY AN INDEX              PASCP 1710
           (INSTEAD OF A POINTER), WHICH CAN BE STORED IN THE P2-FIELD          PASCP 1711
           OF THE INSTRUCTION RECORD UNTIL WRITEOUT.                            PASCP 1712
           --> PROCEDURE LOAD, PROCEDURE WRITEOUT*)                             PASCP 1713
          I, ENTNAME, SEGSIZE: INTEGER;                                         PASCP 1714
          STACKTOP, TOPNEW, TOPMAX: INTEGER;                                    P      191
          LCMAX,LLC1: ADDRRANGE; LCP: CTP;                                      PASCP 1715
          LLP: LBP;                                                             PASCP 1716
                                                                                PASCP 1717
                                                                                PASCP 1718
      PROCEDURE MES(I: INTEGER);                                                P      192
      BEGIN TOPNEW := TOPNEW + CDX[I]*MAXSTACK;                                 P      193
        IF TOPNEW > TOPMAX THEN TOPMAX := TOPNEW                                P      194
      END;                                                                      P      195
      PROCEDURE PUTIC;                                                          PASCP 1719
      BEGIN IF IC MOD 10 = 0 THEN WRITELN(PRR,#I#,IC:5) END;                    PASCP 1720
                                                                                PASCP 1721
                                                                                PASCP 1722
      PROCEDURE GEN0(FOP: OPRANGE);                                             PASCP 1723
      BEGIN                                                                     PASCP 1724
        IF PRCODE THEN BEGIN PUTIC; WRITELN(PRR,MN[FOP]:4) END;                 PASCP 1725
        IC := IC + 1; MES(FOP)                                                  P      196
      END (*GEN0*) ;                                                            PASCP 1727
                                                                                PASCP 1728
      PROCEDURE GEN1(FOP: OPRANGE; FP2: INTEGER);                               PASCP 1729
        VAR K: INTEGER;                                                         PASCP 1730
      BEGIN                                                                     PASCP 1731
        IF PRCODE THEN                                                          PASCP 1732
          BEGIN PUTIC; WRITE(PRR,MN[FOP]:4);                                    PASCP 1733
            IF FOP = 30 THEN                                                    P      197
              BEGIN WRITELN(PRR,SNA[FP2]:12);                                   P      198
                TOPNEW := TOPNEW + PDX[FP2]*MAXSTACK;                           P      199
                IF TOPNEW > TOPMAX THEN TOPMAX := TOPNEW                        P      200
              END                                                               P      201
            ELSE                                                                P      202
              BEGIN                                                             P      203
                IF FOP = 38 THEN                                                P      204
                   BEGIN WRITE(PRR,####);                                       PASCP 1736
                     WITH CSTPTR[FP2]' DO                                       PASCP 1737
                     BEGIN                                                      P      205
                       FOR K := 1 TO SLGTH DO WRITE(PRR,SVAL[K]:1);             PASCP 1738
                      FOR K := SLGTH+1 TO STRGLGTH DO WRITE(PRR,# #);           P      206
                     END;                                                       P      207
                     WRITELN(PRR,####)                                          PASCP 1739
                   END                                                          PASCP 1740
                 ELSE IF FOP = 42 THEN WRITELN(PRR,CHR(FP2))                    PASCP 1741
                      ELSE WRITELN(PRR,FP2:12);                                 P      208
                MES(FOP)                                                        P      209
              END                                                               P      210
          END;                                                                  PASCP 1743
        IC := IC + 1                                                            PASCP 1744
      END (*GEN1*) ;                                                            PASCP 1745
                                                                                PASCP 1746
      PROCEDURE GEN2(FOP: OPRANGE; FP1,FP2: INTEGER);                           PASCP 1747
        VAR K : INTEGER;                                                        PASCP 1748
      BEGIN                                                                     PASCP 1749
        IF PRCODE THEN                                                          PASCP 1750
          BEGIN PUTIC; WRITE(PRR,MN[FOP]:4);                                    PASCP 1751
            CASE FOP OF                                                         PASCP 1752
              45,50,54,56:                                                      PASCP 1753
                WRITELN(PRR,# #,FP1:3,FP2:8);                                   PASCP 1754
              47,48,49,52,53,55:                                                PASCP 1755
                BEGIN WRITE(PRR,CHR(FP1));                                      PASCP 1756
                  IF CHR(FP1) = #M# THEN WRITE(PRR,FP2:11);                     PASCP 1757
                  WRITELN(PRR)                                                  PASCP 1758
                END;                                                            PASCP 1759
              51:                                                               PASCP 1760
                CASE FP1 OF                                                     PASCP 1761
                  1: WRITELN(PRR,#I #,FP2);                                     PASCP 1762
                  2: BEGIN WRITE(PRR,#R #);                                     PASCP 1763
                       WITH CSTPTR[FP2]' DO                                     PASCP 1764
                         FOR K := 1 TO STRGLGTH DO WRITE(PRR,RVAL[K]);          PASCP 1765
                       WRITELN(PRR)                                             PASCP 1766
                     END;                                                       PASCP 1767
                  3: WRITELN(PRR,#B #,FP2);                                     PASCP 1768
                  4: WRITELN(PRR,#N#);                                          PASCP 1769
                  6: WRITELN(PRR,#C ###:3,CHR(FP2),####);                       P      211
                  5: BEGIN WRITE(PRR,#(#);                                      PASCP 1770
                       WITH CSTPTR[FP2]' DO                                     PASCP 1771
                         FOR K := SETLOW TO SETHIGH DO                          PASCP 1772
                           IF K IN PVAL THEN WRITE(PRR,K:3);                    PASCP 1773
                       WRITELN(PRR,#)#)                                         PASCP 1774
                     END                                                        PASCP 1775
                END                                                             PASCP 1776
            END;                                                                PASCP 1777
          END;                                                                  PASCP 1778
        IC := IC + 1; MES(FOP)                                                  P      212
      END (*GEN2*) ;                                                            PASCP 1780
                                                                                PASCP 1781
      PROCEDURE GENTYPINDICATOR(FSP: STP);                                      P      213
      BEGIN                                                                     P      214
        IF FSP<>NIL THEN                                                        P      215
          WITH FSP' DO                                                          P      216
            CASE FORM OF                                                        P      217
             SCALAR: IF FSP=INTPTR THEN WRITE(PRR,#I#)                          P      218
                     ELSE                                                       P      219
                       IF FSP=BOOLPTR THEN WRITE(PRR,#B#)                       P      220
                       ELSE                                                     P      221
                         IF FSP=CHARPTR THEN WRITE(PRR,#C#)                     P      222
                         ELSE                                                   P      223
                           IF SCALKIND = DECLARED THEN WRITE(PRR,#I#)           P      224
                           ELSE WRITE(PRR,#R#);                                 P      225
             SUBRANGE: GENTYPINDICATOR(RANGETYPE);                              P      226
             POINTER:  WRITE(PRR,#A#);                                          P      227
             POWER:    WRITE(PRR,#S#);                                          P      228
             RECORDS,ARRAYS: WRITE(PRR,#M#);                                    P      229
             FILES,TAGFLD,VARIANT: ERROR(500)                                   P      230
            END                                                                 P      231
      END (*TYPINDICATOR*);                                                     P      232
                                                                                P      233
      PROCEDURE GEN0T(FOP: OPRANGE; FSP: STP);                                  P      234
      BEGIN                                                                     P      235
        IF PRCODE THEN                                                          P      236
          BEGIN PUTIC;                                                          P      237
            WRITE(PRR,MN[FOP]:4);                                               P      238
            GENTYPINDICATOR(FSP);                                               P      239
            WRITELN(PRR);                                                       P      240
          END;                                                                  P      241
        IC := IC + 1; MES(FOP)                                                  P      242
      END (*GEN0T*);                                                            P      243
                                                                                P      244
      PROCEDURE GEN1T(FOP: OPRANGE; FP2: INTEGER; FSP: STP);                    P      245
      BEGIN                                                                     P      246
        IF PRCODE THEN                                                          P      247
          BEGIN PUTIC;                                                          P      248
            WRITE(PRR,MN[FOP]:4);                                               P      249
            GENTYPINDICATOR(FSP);                                               P      250
            WRITELN(PRR,FP2:11)                                                 P      251
          END;                                                                  P      252
        IC := IC + 1; MES(FOP)                                                  P      253
      END (*GEN1T*);                                                            P      254
                                                                                P      255
      PROCEDURE GEN2T(FOP: OPRANGE; FP1,FP2: INTEGER; FSP: STP);                P      256
      BEGIN                                                                     P      257
        IF PRCODE THEN                                                          P      258
          BEGIN PUTIC;                                                          P      259
            WRITE(PRR,MN[FOP]: 4);                                              P      260
            GENTYPINDICATOR(FSP);                                               P      261
            WRITELN(PRR,FP1:3+ORD(ABS(FP1)>99)*5,FP2:8);                        P      262
          END;                                                                  P      263
        IC := IC + 1; MES(FOP)                                                  P      264
      END (*GEN2T*);                                                            P      265
                                                                                P      266
      PROCEDURE LOAD;                                                           PASCP 1782
      BEGIN                                                                     PASCP 1783
        WITH GATTR DO                                                           PASCP 1784
          IF TYPTR <> NIL THEN                                                  PASCP 1785
            BEGIN                                                               PASCP 1786
              CASE KIND OF                                                      PASCP 1787
                CST:   IF (TYPTR'.FORM = SCALAR) AND (TYPTR <> REALPTR) THEN    PASCP 1788
                         IF TYPTR = BOOLPTR THEN GEN2(51(*LDC*),3,CVAL.IVAL)    PASCP 1789
                         ELSE                                                   P      267
                           IF TYPTR=CHARPTR THEN                                P      268
                             GEN2(51(*LDC*),6,CVAL.IVAL)                        P      269
                           ELSE GEN2(51(*LDC*),1,CVAL.IVAL)                     P      270
                       ELSE                                                     PASCP 1791
                         IF TYPTR = NILPTR THEN GEN2(51(*LDC*),4,0)             PASCP 1792
                         ELSE                                                   PASCP 1793
                           IF CSTPTRIX >= CSTOCCMAX THEN ERROR(254)             PASCP 1794
                           ELSE                                                 PASCP 1795
                             BEGIN CSTPTRIX := CSTPTRIX + 1;                    PASCP 1796
                               CSTPTR[CSTPTRIX] := CVAL.VALP;                   PASCP 1797
                               IF TYPTR = REALPTR THEN                          PASCP 1798
                                 GEN2(51(*LDC*),2,CSTPTRIX)                     PASCP 1799
                               ELSE                                             PASCP 1800
                                  GEN2(51(*LDC*),5,CSTPTRIX)                    PASCP 1801
                             END;                                               PASCP 1802
                VARBL: CASE ACCESS OF                                           PASCP 1803
                         DRCT:   IF VLEVEL<=1 THEN                              P      271
                                   GEN1T(39(*LDO*),DPLMT,TYPTR)                 P      272
                                 ELSE GEN2T(54(*LOD*),LEVEL-VLEVEL,DPLMT,TYPTR);P      273
                         INDRCT: GEN1T(35(*IND*),IDPLMT,TYPTR);                 P      274
                         INXD:   ERROR(400)                                     PASCP 1807
                       END;                                                     PASCP 1808
                EXPR:                                                           PASCP 1809
              END;                                                              PASCP 1810
              KIND := EXPR                                                      PASCP 1811
            END                                                                 PASCP 1812
      END (*LOAD*) ;                                                            PASCP 1813
                                                                                PASCP 1814
      PROCEDURE STORE(VAR FATTR: ATTR);                                         PASCP 1815
      BEGIN                                                                     PASCP 1816
        WITH FATTR DO                                                           PASCP 1817
          IF TYPTR <> NIL THEN                                                  PASCP 1818
            CASE ACCESS OF                                                      PASCP 1819
              DRCT:   IF VLEVEL <= 1 THEN GEN1T(43(*SRO*),DPLMT,TYPTR)          P      275
                      ELSE GEN2T(56(*STR*),LEVEL-VLEVEL,DPLMT,TYPTR);           P      276
              INDRCT: IF IDPLMT <> 0 THEN ERROR(400)                            PASCP 1822
                      ELSE GEN0T(26(*STO*),TYPTR);                              P      277
              INXD:   ERROR(400)                                                PASCP 1824
            END                                                                 PASCP 1825
      END (*STORE*) ;                                                           PASCP 1826
                                                                                PASCP 1827
      PROCEDURE LOADADDRESS;                                                    PASCP 1828
      BEGIN                                                                     PASCP 1829
        WITH GATTR DO                                                           PASCP 1830
          IF TYPTR <> NIL THEN                                                  PASCP 1831
            BEGIN                                                               PASCP 1832
              CASE KIND OF                                                      PASCP 1833
                CST:   IF STRING(TYPTR) THEN                                    PASCP 1834
                         IF CSTPTRIX >= CSTOCCMAX THEN ERROR(254)               PASCP 1835
                         ELSE                                                   PASCP 1836
                           BEGIN CSTPTRIX := CSTPTRIX + 1;                      PASCP 1837
                             CSTPTR[CSTPTRIX] := CVAL.VALP;                     PASCP 1838
                             GEN1(38(*LCA*),CSTPTRIX)                           PASCP 1839
                           END                                                  PASCP 1840
                       ELSE ERROR(400);                                         PASCP 1841
                VARBL: CASE ACCESS OF                                           PASCP 1842
                         DRCT:   IF VLEVEL <= 1 THEN GEN1(37(*LAO*),DPLMT)      PASCP 1843
                                 ELSE GEN2(50(*LDA*),LEVEL-VLEVEL,DPLMT);       PASCP 1844
                         INDRCT: IF IDPLMT <> 0 THEN                            P      278
                                   GEN1T(34(*INC*),IDPLMT,NILPTR);              P      279
                         INXD:   ERROR(400)                                     PASCP 1846
                       END;                                                     PASCP 1847
                EXPR:  ERROR(400)                                               PASCP 1848
              END;                                                              PASCP 1849
              KIND := VARBL; ACCESS := INDRCT; IDPLMT := 0                      PASCP 1850
            END                                                                 PASCP 1851
      END (*LOADADDRESS*) ;                                                     PASCP 1852
                                                                                PASCP 1853
                                                                                PASCP 1854
      PROCEDURE GENFJP(FADDR: INTEGER);                                         PASCP 1855
      BEGIN LOAD;                                                               PASCP 1856
        IF GATTR.TYPTR <> NIL THEN                                              PASCP 1857
          IF GATTR.TYPTR <> BOOLPTR THEN ERROR(144);                            PASCP 1858
        IF PRCODE THEN BEGIN PUTIC; WRITELN(PRR,MN[33]:4,# L#:8,FADDR:4) END;   PASCP 1859
        IC := IC + 1; MES(33)                                                   P      280
      END (*GENFJP*) ;                                                          PASCP 1861
                                                                                PASCP 1862
      PROCEDURE GENUJPXJP(FOP: OPRANGE; FP2: INTEGER);                          P      281
     BEGIN                                                                      PASCP 1864
       IF PRCODE THEN                                                           PASCP 1865
          BEGIN PUTIC; WRITELN(PRR, MN[FOP]:4, # L#:8,FP2:4) END;               PASCP 1866
        IC := IC + 1; MES(FOP)                                                  P      282
      END (*GENUJPENT*);                                                        PASCP 1868
                                                                                PASCP 1869
                                                                                PASCP 1870
      PROCEDURE GENCUPENT(FOP: OPRANGE; FP1,FP2: INTEGER);                      P      283
      BEGIN                                                                     P      284
        IF PRCODE THEN                                                          P      285
          BEGIN PUTIC;                                                          P      286
            WRITELN(PRR,MN[FOP]:4,FP1:4,#L#:4,FP2:4)                            P      287
          END;                                                                  P      288
        IC := IC + 1; MES(FOP)                                                  P      289
      END;                                                                      P      290
                                                                                PASCP 1877
                                                                                P      291
      PROCEDURE CHECKBNDS(FSP: STP);                                            P      292
        VAR LMIN,LMAX: INTEGER;                                                 P      293
      BEGIN                                                                     P      294
        IF FSP <> NIL THEN                                                      P      295
          IF FSP <> INTPTR THEN                                                 P      296
            IF FSP <> REALPTR THEN                                              P      297
              IF FSP'.FORM <= SUBRANGE THEN                                     P      298
                BEGIN                                                           P      299
                  GETBOUNDS(FSP,LMIN,LMAX);                                     P      300
                  GEN2T(45(*CHK*),LMIN,LMAX,FSP)                                P      301
                END                                                             P      302
      END (*CHECKBNDS*);                                                        P      303
                                                                                P      304
                                                                                PASCP 1878
      PROCEDURE PUTLABEL(LABNAME: INTEGER);                                     PASCP 1879
      BEGIN IF PRCODE THEN WRITELN(PRR, #L#, LABNAME:4)                         PASCP 1880
      END (*PUTLABEL*);                                                         PASCP 1881
                                                                                PASCP 1882
      PROCEDURE STATEMENT(FSYS: SETOFSYS);                                      PASCP 1883
        LABEL 1;                                                                PASCP 1884
        VAR LCP: CTP; LLP: LBP;                                                 PASCP 1885
                                                                                PASCP 1886
        PROCEDURE EXPRESSION(FSYS: SETOFSYS); FORWARD;                          PASCP 1887
                                                                                PASCP 1888
        PROCEDURE SELECTOR(FSYS: SETOFSYS; FCP: CTP);                           PASCP 1889
        VAR LATTR: ATTR; LCP: CTP; LSIZE,LMIN,LMAX: INTEGER;                    P      305
                   ADDRLSIZE: ADDRRANGE;                                        FIX      1
        BEGIN                                                                   PASCP 1891
          WITH FCP', GATTR DO                                                   PASCP 1892
            BEGIN TYPTR := IDTYPE; KIND := VARBL;                               PASCP 1893
              CASE KLASS OF                                                     PASCP 1894
                VARS:                                                           PASCP 1895
                  IF VKIND = ACTUAL THEN                                        PASCP 1896
                    BEGIN ACCESS := DRCT; VLEVEL := VLEV;                       PASCP 1897
                      DPLMT := VADDR                                            PASCP 1898
                    END                                                         PASCP 1899
                  ELSE                                                          PASCP 1900
                    BEGIN GEN2T(54(*LOD*),LEVEL-VLEV,VADDR,NILPTR);             P      306
                      ACCESS := INDRCT; IDPLMT := 0                             PASCP 1902
                    END;                                                        PASCP 1903
                FIELD:                                                          PASCP 1904
                  WITH DISPLAY[DISX] DO                                         PASCP 1905
                    IF OCCUR = CREC THEN                                        PASCP 1906
                      BEGIN ACCESS := DRCT; VLEVEL := CLEV;                     PASCP 1907
                        DPLMT := CDSPL + FLDADDR                                PASCP 1908
                      END                                                       PASCP 1909
                    ELSE                                                        PASCP 1910
                      BEGIN                                                     PASCP 1911
                        IF LEVEL = 1 THEN GEN1T(39(*LDO*),VDSPL,NILPTR)         P      307
                        ELSE GEN2T(54(*LOD*),0,VDSPL,NILPTR);                   P      308
                        ACCESS := INDRCT; IDPLMT := FLDADDR                     PASCP 1914
                      END;                                                      PASCP 1915
                FUNC:                                                           PASCP 1916
                  IF PFDECKIND = STANDARD THEN                                  P      309
                    BEGIN ERROR(150); TYPTR := NIL END                          P      310
                  ELSE                                                          P      311
                    BEGIN                                                       P      312
                      IF PFKIND = FORMAL THEN ERROR(151)                        P      313
                      ELSE                                                      P      314
                        IF (PFLEV+1<>LEVEL)OR(FPROCP<>FCP) THEN ERROR(177);     P      315
                        BEGIN ACCESS := DRCT; VLEVEL := PFLEV + 1;              PASCP 1923
                          DPLMT := 0   (*IMPL. RELAT. ADDR. OF FCT. RESULT*)    PASCP 1924
                        END                                                     PASCP 1925
                    END                                                         P      316
              END (*CASE*)                                                      PASCP 1926
            END (*WITH*);                                                       PASCP 1927
          IF NOT (SY IN SELECTSYS + FSYS) THEN                                  PASCP 1928
            BEGIN ERROR(59); SKIP(SELECTSYS + FSYS) END;                        PASCP 1929
          WHILE SY IN SELECTSYS DO                                              PASCP 1930
            BEGIN                                                               PASCP 1931
        (*[*)   IF SY = LBRACK THEN                                             PASCP 1932
                BEGIN                                                           PASCP 1933
                  REPEAT LATTR := GATTR;                                        PASCP 1934
                    WITH LATTR DO                                               PASCP 1935
                      IF TYPTR <> NIL THEN                                      PASCP 1936
                        IF TYPTR'.FORM <> ARRAYS THEN                           PASCP 1937
                          BEGIN ERROR(138); TYPTR := NIL END;                   PASCP 1938
                    LOADADDRESS;                                                PASCP 1939
                    INSYMBOL; EXPRESSION(FSYS + [COMMA,RBRACK]);                PASCP 1940
                    LOAD;                                                       PASCP 1941
                    IF GATTR.TYPTR <> NIL THEN                                  PASCP 1942
                      IF GATTR.TYPTR'.FORM<>SCALAR THEN ERROR(113)              P      317
                      ELSE IF NOT COMPTYPES(GATTR.TYPTR,INTPTR) THEN            P      318
                             GEN0T(58(*ORD*),GATTR.TYPTR);                      P      319
                    IF LATTR.TYPTR <> NIL THEN                                  PASCP 1944
                      WITH LATTR.TYPTR' DO                                      PASCP 1945
                        BEGIN                                                   PASCP 1946
                          IF COMPTYPES(INXTYPE,GATTR.TYPTR) THEN                PASCP 1947
                            BEGIN                                               PASCP 1948
                              IF INXTYPE <> NIL THEN                            PASCP 1949
                                BEGIN GETBOUNDS(INXTYPE,LMIN,LMAX);             PASCP 1950
                                  IF DEBUG THEN                                 P      320
                                  GEN2T(45(*CHK*),LMIN,LMAX,INTPTR);            J        4
                                IF LMIN>0 THEN GEN1T(31(*DEC*),LMIN,INTPTR)     J        5
                                  ELSE IF LMIN<0 THEN                           P      323
                                  GEN1T(34(*INC*),-LMIN,INTPTR);                J        6
                                  (*OR SIMPLY GEN1(31,LMIN)*)                   PASCP 1953
                                END                                             PASCP 1954
                            END                                                 PASCP 1955
                          ELSE ERROR(139);                                      PASCP 1956
                          WITH GATTR DO                                         PASCP 1957
                            BEGIN TYPTR := AELTYPE; KIND := VARBL;              PASCP 1958
                              ACCESS := INDRCT; IDPLMT := 0                     PASCP 1959
                            END;                                                PASCP 1960
                          IF GATTR.TYPTR <> NIL THEN                            PASCP 1961
                            BEGIN                                               P      325
                              LSIZE := GATTR.TYPTR'.SIZE;                       P      326
                              ADDRLSIZE:=LSIZE;ALIGN(GATTR.TYPTR,ADDRLSIZE);    P      327
                              GEN1(36(*IXA*),LSIZE)                             P      328
                            END                                                 P      329
                        END                                                     PASCP 1963
                  UNTIL SY <> COMMA;                                            PASCP 1964
                  IF SY = RBRACK THEN INSYMBOL ELSE ERROR(12)                   PASCP 1965
                END (*IF SY = LBRACK*)                                          PASCP 1966
              ELSE                                                              PASCP 1967
        (*.*)     IF SY = PERIOD THEN                                           PASCP 1968
                  BEGIN                                                         PASCP 1969
                    WITH GATTR DO                                               PASCP 1970
                      BEGIN                                                     PASCP 1971
                        IF TYPTR <> NIL THEN                                    PASCP 1972
                          IF TYPTR'.FORM <> RECORDS THEN                        PASCP 1973
                            BEGIN ERROR(140); TYPTR := NIL END;                 PASCP 1974
                        INSYMBOL;                                               PASCP 1975
                        IF SY = IDENT THEN                                      PASCP 1976
                          BEGIN                                                 PASCP 1977
                            IF TYPTR <> NIL THEN                                PASCP 1978
                              BEGIN SEARCHSECTION(TYPTR'.FSTFLD,LCP);           PASCP 1979
                                IF LCP = NIL THEN                               PASCP 1980
                                  BEGIN ERROR(152); TYPTR := NIL END            PASCP 1981
                                ELSE                                            PASCP 1982
                                  WITH LCP' DO                                  PASCP 1983
                                    BEGIN TYPTR := IDTYPE;                      PASCP 1984
                                      CASE ACCESS OF                            PASCP 1985
                                        DRCT:   DPLMT := DPLMT + FLDADDR;       PASCP 1986
                                        INDRCT: IDPLMT := IDPLMT + FLDADDR;     PASCP 1987
                                        INXD:   ERROR(400)                      PASCP 1988
                                      END                                       PASCP 1989
                                    END                                         PASCP 1990
                              END;                                              PASCP 1991
                            INSYMBOL                                            PASCP 1992
                          END (*SY = IDENT*)                                    PASCP 1993
                        ELSE ERROR(2)                                           PASCP 1994
                      END (*WITH GATTR*)                                        PASCP 1995
                  END (*IF SY = PERIOD*)                                        PASCP 1996
                ELSE                                                            PASCP 1997
        (*'*)       BEGIN                                                       PASCP 1998
                    IF GATTR.TYPTR <> NIL THEN                                  PASCP 1999
                      WITH GATTR,TYPTR' DO                                      PASCP 2000
                        IF FORM = POINTER THEN                                  PASCP 2001
                          BEGIN LOAD; TYPTR := ELTYPE;                          P      330
                            IF DEBUG THEN GEN2T(45(*CHK*),1,MAXADDR,NILPTR);    P      331
                            WITH GATTR DO                                       PASCP 2003
                              BEGIN KIND := VARBL; ACCESS := INDRCT;            PASCP 2004
                                IDPLMT := 0                                     PASCP 2005
                              END                                               PASCP 2006
                          END                                                   PASCP 2007
                        ELSE                                                    PASCP 2008
                          IF FORM = FILES THEN TYPTR := FILTYPE                 PASCP 2009
                          ELSE ERROR(141);                                      PASCP 2010
                    INSYMBOL                                                    PASCP 2011
                  END;                                                          PASCP 2012
              IF NOT (SY IN FSYS + SELECTSYS) THEN                              PASCP 2013
                BEGIN ERROR(6); SKIP(FSYS + SELECTSYS) END                      PASCP 2014
            END (*WHILE*)                                                       PASCP 2015
        END (*SELECTOR*) ;                                                      PASCP 2016
                                                                                PASCP 2017
        PROCEDURE CALL(FSYS: SETOFSYS; FCP: CTP);                               PASCP 2018
          VAR LKEY: 1..15;                                                      PASCP 2019
                                                                                PASCP 2020
          PROCEDURE VARIABLE(FSYS: SETOFSYS);                                   PASCP 2021
            VAR LCP: CTP;                                                       PASCP 2022
          BEGIN                                                                 PASCP 2023
            IF SY = IDENT THEN                                                  PASCP 2024
              BEGIN SEARCHID([VARS,FIELD],LCP); INSYMBOL END                    PASCP 2025
            ELSE BEGIN ERROR(2); LCP := UVARPTR END;                            PASCP 2026
            SELECTOR(FSYS,LCP)                                                  PASCP 2027
          END (*VARIABLE*) ;                                                    PASCP 2028
                                                                                PASCP 2029
          PROCEDURE GETPUTRESETREWRITE;                                         PASCP 2030
          BEGIN VARIABLE(FSYS + [RPARENT]); LOADADDRESS;                        PASCP 2031
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2032
              IF GATTR.TYPTR'.FORM <> FILES THEN ERROR(116);                    PASCP 2033
            IF LKEY <= 2 THEN GEN1(30(*CSP*),LKEY(*GET,PUT*))                   PASCP 2034
            ELSE ERROR(399)                                                     PASCP 2035
          END (*GETPUTRESETREWRITE*) ;                                          PASCP 2036
                                                                                PASCP 2037
          PROCEDURE READ;                                                       PASCP 2038
            VAR LCP:CTP; LLEV:LEVRANGE; LADDR:ADDRRANGE;                        PASCP 2039
                LSP : STP;                                                      P      332
          BEGIN                                                                 P      333
            LLEV := 1; LADDR := LCAFTERMARKSTACK;                               P      334
            IF SY = LPARENT THEN                                                P      335
            BEGIN INSYMBOL;                                                     P      336
            VARIABLE(FSYS + [COMMA,RPARENT]);                                   P      337
            LSP := GATTR.TYPTR; TEST := FALSE;                                  P      338
            IF LSP <> NIL THEN                                                  P      339
              IF LSP'.FORM = FILES THEN                                         P      340
                WITH GATTR, LSP' DO                                             P      341
                  BEGIN                                                         P      342
                    IF FILTYPE = CHARPTR THEN                                   P      343
                      BEGIN LLEV := VLEVEL; LADDR := DPLMT END                  P      344
                    ELSE ERROR(399);                                            P      345
                    IF SY = RPARENT THEN                                        P      346
                      BEGIN IF LKEY = 5 THEN ERROR(116);                        P      347
                        TEST := TRUE                                            P      348
                      END                                                       P      349
                    ELSE                                                        P      350
                      IF SY <> COMMA THEN                                       P      351
                        BEGIN ERROR(116); SKIP(FSYS + [COMMA,RPARENT]) END;     P      352
                    IF SY = COMMA THEN                                          P      353
                      BEGIN INSYMBOL; VARIABLE(FSYS + [COMMA,RPARENT])          P      354
                      END                                                       P      355
                    ELSE TEST := TRUE                                           P      356
                  END;                                                          P      357
           IF NOT TEST THEN                                                     P      358
            REPEAT LOADADDRESS;                                                 P      359
              GEN2(50(*LDA*),LEVEL-LLEV,LADDR);                                 PASCP 2063
              IF GATTR.TYPTR <> NIL THEN                                        PASCP 2064
                IF GATTR.TYPTR'.FORM <= SUBRANGE THEN                           PASCP 2065
                  IF COMPTYPES(INTPTR,GATTR.TYPTR) THEN                         PASCP 2066
                    GEN1(30(*CSP*),3(*RDI*))                                    PASCP 2067
                  ELSE                                                          PASCP 2068
                    IF COMPTYPES(REALPTR,GATTR.TYPTR) THEN                      PASCP 2069
                      GEN1(30(*CSP*),4(*RDR*))                                  PASCP 2070
                    ELSE                                                        PASCP 2071
                      IF COMPTYPES(CHARPTR,GATTR.TYPTR) THEN                    PASCP 2072
                        GEN1(30(*CSP*),5(*RDC*))                                PASCP 2073
                      ELSE ERROR(399)                                           PASCP 2074
                ELSE ERROR(116);                                                PASCP 2075
              TEST := SY <> COMMA;                                              PASCP 2076
              IF NOT TEST THEN                                                  P      360
                BEGIN INSYMBOL; VARIABLE(FSYS + [COMMA,RPARENT])                P      361
                END                                                             P      362
            UNTIL TEST;                                                         P      363
            IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)                         P      364
            END                                                                 P      365
              ELSE IF LKEY = 5 THEN ERROR(116);                                 P      366
           IF LKEY = 11 THEN                                                    PASCP 2080
             BEGIN GEN2(50(*LDA*),LEVEL - LLEV, LADDR);                         PASCP 2081
               GEN1(30(*CSP*),21(*RLN*))                                        PASCP 2082
             END                                                                PASCP 2083
          END (*READ*) ;                                                        PASCP 2084
                                                                                PASCP 2085
          PROCEDURE WRITE;                                                      PASCP 2086
            VAR LSP: STP; DEFAULT : BOOLEAN; LLKEY: 1..15;                      PASCP 2087
                LCP:CTP; LLEV:LEVRANGE; LADDR,LEN:ADDRRANGE;                    PASCP 2088
          BEGIN LLKEY := LKEY;                                                  PASCP 2089
            LLEV := 1; LADDR := LCAFTERMARKSTACK + CHARMAX;                     P      367
            IF SY = LPARENT THEN                                                P      368
            BEGIN INSYMBOL;                                                     P      369
            EXPRESSION(FSYS + [COMMA,COLON,RPARENT]);                           P      370
            LSP := GATTR.TYPTR; TEST := FALSE;                                  P      371
            IF LSP <> NIL THEN                                                  P      372
              IF LSP'.FORM = FILES THEN                                         P      373
                WITH GATTR, LSP' DO                                             P      374
                  BEGIN                                                         P      375
                    IF FILTYPE = CHARPTR THEN                                   P      376
                      BEGIN LLEV := VLEVEL; LADDR := DPLMT END                  P      377
                    ELSE ERROR(399);                                            P      378
                    IF SY = RPARENT THEN                                        P      379
                      BEGIN IF LLKEY = 6  THEN ERROR(116);                      P      380
                        TEST := TRUE                                            P      381
                      END                                                       P      382
                    ELSE                                                        P      383
                      IF SY <> COMMA THEN                                       P      384
                        BEGIN ERROR(116); SKIP(FSYS+[COMMA,RPARENT]) END;       P      385
                    IF SY = COMMA THEN                                          P      386
                      BEGIN INSYMBOL; EXPRESSION(FSYS+[COMMA,COLON,RPARENT])    P      387
                      END                                                       P      388
                    ELSE TEST := TRUE                                           P      389
                  END;                                                          P      390
           IF NOT TEST THEN                                                     P      391
            REPEAT                                                              P      392
              LSP := GATTR.TYPTR;                                               PASCP 2111
              IF LSP <> NIL THEN                                                PASCP 2112
                IF LSP'.FORM <= SUBRANGE THEN LOAD ELSE LOADADDRESS;            PASCP 2113
              IF SY = COLON THEN                                                PASCP 2114
                BEGIN INSYMBOL; EXPRESSION(FSYS + [COMMA,COLON,RPARENT]);       PASCP 2115
                  IF GATTR.TYPTR <> NIL THEN                                    PASCP 2116
                    IF GATTR.TYPTR <> INTPTR THEN ERROR(116);                   PASCP 2117
                  LOAD; DEFAULT := FALSE                                        PASCP 2118
                END                                                             PASCP 2119
              ELSE DEFAULT := TRUE;                                             PASCP 2120
              IF SY = COLON THEN                                                PASCP 2121
                BEGIN INSYMBOL; EXPRESSION(FSYS + [COMMA,RPARENT]);             PASCP 2122
                  IF GATTR.TYPTR <> NIL THEN                                    PASCP 2123
                    IF GATTR.TYPTR <> INTPTR THEN ERROR(116);                   PASCP 2124
                  IF LSP <> REALPTR THEN ERROR(124);                            PASCP 2125
                  LOAD; ERROR(399);                                             PASCP 2126
                END                                                             PASCP 2127
              ELSE                                                              PASCP 2128
                IF LSP = INTPTR THEN                                            PASCP 2129
                  BEGIN IF DEFAULT THEN GEN2(51(*LDC*),1,10);                   PASCP 2130
                    GEN2(50(*LDA*),LEVEL-LLEV,LADDR);                           PASCP 2131
                    GEN1(30(*CSP*),6(*WRI*))                                    PASCP 2132
                  END                                                           PASCP 2133
                ELSE                                                            PASCP 2134
                  IF LSP = REALPTR THEN                                         PASCP 2135
                    BEGIN IF DEFAULT THEN GEN2(51(*LDC*),1,20);                 PASCP 2136
                      GEN2(50(*LDA*),LEVEL-LLEV,LADDR);                         PASCP 2137
                      GEN1(30(*CSP*),8(*WRR*))                                  PASCP 2138
                    END                                                         PASCP 2139
                  ELSE                                                          PASCP 2140
                    IF LSP = CHARPTR THEN                                       PASCP 2141
                      BEGIN IF DEFAULT THEN GEN2(51(*LDC*),1,1);                PASCP 2142
                        GEN2(50(*LDA*),LEVEL-LLEV,LADDR);                       PASCP 2143
                        GEN1(30(*CSP*),9(*WRC*))                                PASCP 2144
                      END                                                       PASCP 2145
                    ELSE                                                        PASCP 2146
                      IF LSP <> NIL THEN                                        PASCP 2147
                        BEGIN                                                   PASCP 2148
                          IF LSP'.FORM = SCALAR THEN ERROR(399)                 PASCP 2149
                          ELSE                                                  PASCP 2150
                            IF STRING(LSP) THEN                                 PASCP 2151
                              BEGIN LEN := LSP'.SIZE DIV CHARMAX;               P      393
                                IF DEFAULT THEN                                 PASCP 2153
                                      GEN2(51(*LDC*),1,LEN);                    PASCP 2154
                                GEN2(51(*LDC*),1,LEN);                          PASCP 2155
                                GEN2(50(*LDA*),LEVEL-LLEV,LADDR);               PASCP 2156
                                GEN1(30(*CSP*),10(*WRS*))                       PASCP 2157
                              END                                               PASCP 2158
                            ELSE ERROR(116)                                     PASCP 2159
                        END;                                                    PASCP 2160
              TEST := SY <> COMMA;                                              PASCP 2161
              IF NOT TEST THEN                                                  P      394
                BEGIN INSYMBOL; EXPRESSION(FSYS + [COMMA,COLON,RPARENT])        P      395
                END                                                             P      396
            UNTIL TEST;                                                         PASCP 2163
            IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)                         P      397
            END                                                                 P      398
              ELSE IF LKEY = 6 THEN ERROR(116);                                 P      399
            IF LLKEY = 12 THEN (*WRITELN*)                                      PASCP 2165
              BEGIN GEN2(50(*LDA*),LEVEL-LLEV,LADDR);                           PASCP 2166
                GEN1(30(*CSP*),22(*WLN*))                                       PASCP 2167
              END                                                               PASCP 2168
          END (*WRITE*) ;                                                       PASCP 2169
                                                                                PASCP 2170
          PROCEDURE PACK;                                                       PASCP 2171
            VAR LSP,LSP1: STP;                                                  PASCP 2172
          BEGIN ERROR(399); VARIABLE(FSYS + [COMMA,RPARENT]);                   PASCP 2173
            LSP := NIL; LSP1 := NIL;                                            PASCP 2174
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2175
              WITH GATTR.TYPTR' DO                                              PASCP 2176
                IF FORM = ARRAYS THEN                                           PASCP 2177
                  BEGIN LSP := INXTYPE; LSP1 := AELTYPE END                     PASCP 2178
                ELSE ERROR(116);                                                PASCP 2179
            IF SY = COMMA THEN INSYMBOL ELSE ERROR(20);                         PASCP 2180
            EXPRESSION(FSYS + [COMMA,RPARENT]);                                 PASCP 2181
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2182
              IF GATTR.TYPTR'.FORM <> SCALAR THEN ERROR(116)                    PASCP 2183
              ELSE                                                              PASCP 2184
                IF NOT COMPTYPES(LSP,GATTR.TYPTR) THEN ERROR(116);              PASCP 2185
            IF SY = COMMA THEN INSYMBOL ELSE ERROR(20);                         PASCP 2186
            VARIABLE(FSYS + [RPARENT]);                                         PASCP 2187
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2188
              WITH GATTR.TYPTR' DO                                              PASCP 2189
                IF FORM = ARRAYS THEN                                           PASCP 2190
                  BEGIN                                                         PASCP 2191
                    IF NOT COMPTYPES(AELTYPE,LSP1)                              PASCP 2192
                      OR NOT COMPTYPES(INXTYPE,LSP) THEN                        PASCP 2193
                      ERROR(116)                                                PASCP 2194
                  END                                                           PASCP 2195
                ELSE ERROR(116)                                                 PASCP 2196
          END (*PACK*) ;                                                        PASCP 2197
                                                                                PASCP 2198
          PROCEDURE UNPACK;                                                     PASCP 2199
            VAR LSP,LSP1: STP;                                                  PASCP 2200
          BEGIN ERROR(399); VARIABLE(FSYS + [COMMA,RPARENT]);                   PASCP 2201
            LSP := NIL; LSP1 := NIL;                                            PASCP 2202
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2203
              WITH GATTR.TYPTR' DO                                              PASCP 2204
                IF FORM = ARRAYS THEN                                           PASCP 2205
                  BEGIN LSP := INXTYPE; LSP1 := AELTYPE END                     PASCP 2206
                ELSE ERROR(116);                                                PASCP 2207
            IF SY = COMMA THEN INSYMBOL ELSE ERROR(20);                         PASCP 2208
            VARIABLE(FSYS + [COMMA,RPARENT]);                                   PASCP 2209
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2210
              WITH GATTR.TYPTR' DO                                              PASCP 2211
                IF FORM = ARRAYS THEN                                           PASCP 2212
                  BEGIN                                                         PASCP 2213
                    IF NOT COMPTYPES(AELTYPE,LSP1)                              PASCP 2214
                      OR NOT COMPTYPES(INXTYPE,LSP) THEN                        PASCP 2215
                      ERROR(116)                                                PASCP 2216
                  END                                                           PASCP 2217
                ELSE ERROR(116);                                                PASCP 2218
            IF SY = COMMA THEN INSYMBOL ELSE ERROR(20);                         PASCP 2219
            EXPRESSION(FSYS + [RPARENT]);                                       PASCP 2220
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2221
              IF GATTR.TYPTR'.FORM <> SCALAR THEN ERROR(116)                    PASCP 2222
              ELSE                                                              PASCP 2223
                IF NOT COMPTYPES(LSP,GATTR.TYPTR) THEN ERROR(116);              PASCP 2224
          END (*UNPACK*) ;                                                      PASCP 2225
                                                                                PASCP 2226
          PROCEDURE NEW;                                                        PASCP 2227
            LABEL 1;                                                            PASCP 2228
            VAR LSP,LSP1: STP; VARTS,LMIN,LMAX: INTEGER;                        PASCP 2229
                LSIZE,LSZ: ADDRRANGE; LVAL: VALU;                               PASCP 2230
          BEGIN VARIABLE(FSYS + [COMMA,RPARENT]); LOADADDRESS;                  PASCP 2231
            LSP := NIL; VARTS := 0; LSIZE := 0;                                 PASCP 2232
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2233
              WITH GATTR.TYPTR' DO                                              PASCP 2234
                IF FORM = POINTER THEN                                          PASCP 2235
                  BEGIN                                                         PASCP 2236
                    IF ELTYPE <> NIL THEN                                       PASCP 2237
                      BEGIN LSIZE := ELTYPE'.SIZE;                              PASCP 2238
                        IF ELTYPE'.FORM = RECORDS THEN LSP := ELTYPE'.RECVAR    PASCP 2239
                      END                                                       PASCP 2240
                  END                                                           PASCP 2241
                ELSE ERROR(116);                                                PASCP 2242
            WHILE SY = COMMA DO                                                 PASCP 2243
              BEGIN INSYMBOL;CONSTANT(FSYS + [COMMA,RPARENT],LSP1,LVAL);        PASCP 2244
                VARTS := VARTS + 1;                                             PASCP 2245
                (*CHECK TO INSERT HERE: IS CONSTANT IN TAGFIELDTYPE RANGE*)     PASCP 2246
                IF LSP = NIL THEN ERROR(158)                                    PASCP 2247
                ELSE                                                            PASCP 2248
                  IF LSP'.FORM <> TAGFLD THEN ERROR(162)                        PASCP 2249
                  ELSE                                                          PASCP 2250
                    IF LSP'.TAGFIELDP <> NIL THEN                               PASCP 2251
                      IF STRING(LSP1) OR (LSP1 = REALPTR) THEN ERROR(159)       PASCP 2252
                      ELSE                                                      PASCP 2253
                        IF COMPTYPES(LSP'.TAGFIELDP'.IDTYPE,LSP1) THEN          PASCP 2254
                          BEGIN                                                 PASCP 2255
                            LSP1 := LSP'.FSTVAR;                                PASCP 2256
                            WHILE LSP1 <> NIL DO                                PASCP 2257
                              WITH LSP1' DO                                     PASCP 2258
                                IF VARVAL.IVAL = LVAL.IVAL THEN                 PASCP 2259
                                  BEGIN LSIZE := SIZE; LSP := SUBVAR;           PASCP 2260
                                    GOTO 1                                      PASCP 2261
                                  END                                           PASCP 2262
                                ELSE LSP1 := NXTVAR;                            PASCP 2263
                            LSIZE := LSP'.SIZE; LSP := NIL;                     PASCP 2264
                          END                                                   PASCP 2265
                        ELSE ERROR(116);                                        PASCP 2266
          1:  END (*WHILE*) ;                                                   PASCP 2267
            GEN2(51(*LDC*),1,LSIZE);                                            PASCP 2268
            GEN1(30(*CSP*),12(*NEW*));                                          PASCP 2269
          END (*NEW*) ;                                                         PASCP 2270
                                                                                PASCP 2271
          PROCEDURE MARK;                                                       PASCP 2272
          BEGIN VARIABLE(FSYS+[RPARENT]);                                       PASCP 2273
             IF GATTR.TYPTR <> NIL THEN                                         PASCP 2274
               IF GATTR.TYPTR'.FORM = POINTER THEN                              PASCP 2275
                 BEGIN LOADADDRESS; GEN1(30(*CSP*),23(*SAV*)) END               PASCP 2276
               ELSE ERROR(116)                                                  PASCP 2277
          END(*MARK*);                                                          PASCP 2278
                                                                                PASCP 2279
          PROCEDURE RELEASE;                                                    PASCP 2280
          BEGIN VARIABLE(FSYS+[RPARENT]);                                       PASCP 2281
                IF GATTR.TYPTR <> NIL THEN                                      PASCP 2282
                   IF GATTR.TYPTR'.FORM = POINTER THEN                          PASCP 2283
                      BEGIN LOAD; GEN1(30(*CSP*),13(*RST*)) END                 PASCP 2284
                   ELSE ERROR(116)                                              PASCP 2285
          END (*RELEASE*);                                                      PASCP 2286
                                                                                PASCP 2287
                                                                                PASCP 2288
                                                                                PASCP 2289
          PROCEDURE ABS;                                                        PASCP 2290
          BEGIN                                                                 PASCP 2291
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2292
              IF GATTR.TYPTR = INTPTR THEN GEN0(0(*ABI*))                       PASCP 2293
              ELSE                                                              PASCP 2294
                IF GATTR.TYPTR = REALPTR THEN GEN0(1(*ABR*))                    PASCP 2295
                ELSE BEGIN ERROR(125); GATTR.TYPTR := INTPTR END                PASCP 2296
          END (*ABS*) ;                                                         PASCP 2297
                                                                                PASCP 2298
          PROCEDURE SQR;                                                        PASCP 2299
          BEGIN                                                                 PASCP 2300
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2301
              IF GATTR.TYPTR = INTPTR THEN GEN0(24(*SQI*))                      PASCP 2302
              ELSE                                                              PASCP 2303
                IF GATTR.TYPTR = REALPTR THEN GEN0(25(*SQR*))                   PASCP 2304
                ELSE BEGIN ERROR(125); GATTR.TYPTR := INTPTR END                PASCP 2305
          END (*SQR*) ;                                                         PASCP 2306
                                                                                PASCP 2307
          PROCEDURE TRUNC;                                                      PASCP 2308
          BEGIN                                                                 PASCP 2309
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2310
              IF GATTR.TYPTR <> REALPTR THEN ERROR(125);                        PASCP 2311
            GEN0(27(*TRC*));                                                    PASCP 2312
            GATTR.TYPTR := INTPTR                                               PASCP 2313
          END (*TRUNC*) ;                                                       PASCP 2314
                                                                                PASCP 2315
          PROCEDURE ODD;                                                        PASCP 2316
          BEGIN                                                                 PASCP 2317
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2318
              IF GATTR.TYPTR <> INTPTR THEN ERROR(125);                         PASCP 2319
            GEN0(20(*ODD*));                                                    PASCP 2320
            GATTR.TYPTR := BOOLPTR                                              PASCP 2321
          END (*ODD*) ;                                                         PASCP 2322
                                                                                PASCP 2323
          PROCEDURE ORD;                                                        PASCP 2324
          BEGIN                                                                 PASCP 2325
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2326
              IF GATTR.TYPTR'.FORM >= POWER THEN ERROR(125);                    PASCP 2327
            GEN0T(58(*ORD*),GATTR.TYPTR);                                       P      400
            GATTR.TYPTR := INTPTR                                               PASCP 2328
          END (*ORD*) ;                                                         PASCP 2329
                                                                                PASCP 2330
          PROCEDURE CHR;                                                        PASCP 2331
          BEGIN                                                                 PASCP 2332
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2333
              IF GATTR.TYPTR <> INTPTR THEN ERROR(125);                         PASCP 2334
            GEN0(59(*CHR*));                                                    P      401
            GATTR.TYPTR := CHARPTR                                              PASCP 2335
          END (*CHR*) ;                                                         PASCP 2336
                                                                                PASCP 2337
                                                                                PASCP 2338
                                                                                PASCP 2339
          PROCEDURE PREDSUCC;                                                   PASCP 2340
          BEGIN                                                                 P      402
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2342
              IF GATTR.TYPTR'.FORM <> SCALAR THEN ERROR(125);                   PASCP 2343
            IF LKEY = 7 THEN GEN1T(31(*DEC*),1,GATTR.TYPTR)                     X1       1
            ELSE GEN1T(34(*INC*),1,GATTR.TYPTR)                                 X1       2
          END (*PREDSUCC*) ;                                                    PASCP 2344
                                                                                PASCP 2345
          PROCEDURE EOF;                                                        PASCP 2346
          BEGIN                                                                 P      405
            IF SY = LPARENT THEN                                                P      406
              BEGIN INSYMBOL; VARIABLE(FSYS + [RPARENT]);                       P      407
                IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)                     P      408
              END                                                               P      409
            ELSE                                                                P      410
              WITH GATTR DO                                                     P      411
                BEGIN TYPTR := TEXTPTR; KIND := VARBL; ACCESS := DRCT;          P      412
                  VLEVEL := 1; DPLMT := LCAFTERMARKSTACK                        P      413
                END;                                                            P      414
            LOADADDRESS;                                                        P      415
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 2348
              IF GATTR.TYPTR'.FORM <> FILES THEN ERROR(125);                    PASCP 2349
            IF LKEY = 9 THEN GEN0(8(*EOF*)) ELSE GEN1(30(*CSP*),14(*ELN*));     PASCP 2350
              GATTR.TYPTR := BOOLPTR                                            PASCP 2351
          END (*EOF*) ;                                                         PASCP 2352
                                                                                PASCP 2353
          PROCEDURE CALLNONSTANDARD;                                            PASCP 2354
            VAR NXT,LCP: CTP; LSP: STP; LKIND: IDKIND; LB: BOOLEAN;             PASCP 2355
                LOCPAR, LLC: ADDRRANGE;                                         PASCP 2356
          BEGIN LOCPAR := 0;                                                    PASCP 2357
            WITH FCP' DO                                                        PASCP 2358
              BEGIN NXT := NEXT; LKIND := PFKIND;                               PASCP 2359
                IF NOT EXTERN THEN GEN1(41(*MST*),LEVEL-PFLEV)                  PASCP 2360
              END;                                                              PASCP 2361
            IF SY = LPARENT THEN                                                PASCP 2362
              BEGIN LLC := LC;                                                  PASCP 2363
                REPEAT LB := FALSE; (*DECIDE WHETHER PROC/FUNC MUST BE PASSED*) PASCP 2364
                  IF LKIND = ACTUAL THEN                                        PASCP 2365
                    BEGIN                                                       PASCP 2366
                      IF NXT = NIL THEN ERROR(126)                              PASCP 2367
                      ELSE LB := NXT'.KLASS IN [PROC,FUNC]                      PASCP 2368
                    END ELSE ERROR(399);                                        PASCP 2369
                  (*FOR FORMAL PROC/FUNC LB IS FALSE AND EXPRESSION             PASCP 2370
                   WILL BE CALLED, WHICH WILL ALLWAYS INTERPRET A PROC/FUNC ID  PASCP 2371
                  AT ITS BEGINNING AS A CALL RATHER THAN A PARAMETER PASSING.   PASCP 2372
                  IN THIS IMPLEMENTATION, PARAMETER PROCEDURES/FUNCTIONS        PASCP 2373
                  ARE THEREFORE NOT ALLOWED TO HAVE PROCEDURE/FUNCTION          PASCP 2374
                  PARAMETERS*)                                                  PASCP 2375
                  INSYMBOL;                                                     PASCP 2376
                  IF LB THEN   (*PASS FUNCTION OR PROCEDURE*)                   PASCP 2377
                    BEGIN ERROR(399);                                           PASCP 2378
                      IF SY <> IDENT THEN                                       PASCP 2379
                        BEGIN ERROR(2); SKIP(FSYS + [COMMA,RPARENT]) END        PASCP 2380
                      ELSE                                                      PASCP 2381
                        BEGIN                                                   PASCP 2382
                          IF NXT'.KLASS = PROC THEN SEARCHID([PROC],LCP)        PASCP 2383
                          ELSE                                                  PASCP 2384
                            BEGIN SEARCHID([FUNC],LCP);                         PASCP 2385
                              IF NOT COMPTYPES(LCP'.IDTYPE,NXT'.IDTYPE) THEN    PASCP 2386
                                ERROR(128)                                      PASCP 2387
                            END;                                                PASCP 2388
                          INSYMBOL;                                             PASCP 2389
                          IF NOT (SY IN FSYS + [COMMA,RPARENT]) THEN            PASCP 2390
                            BEGIN ERROR(6); SKIP(FSYS + [COMMA,RPARENT]) END    PASCP 2391
                        END                                                     PASCP 2392
                    END (*IF LB*)                                               PASCP 2393
                  ELSE                                                          PASCP 2394
                    BEGIN EXPRESSION(FSYS + [COMMA,RPARENT]);                   PASCP 2395
                      IF GATTR.TYPTR <> NIL THEN                                PASCP 2396
                        IF LKIND = ACTUAL THEN                                  PASCP 2397
                          BEGIN                                                 PASCP 2398
                            IF NXT <> NIL THEN                                  PASCP 2399
                              BEGIN LSP := NXT'.IDTYPE;                         PASCP 2400
                                IF LSP <> NIL THEN                              PASCP 2401
                                  BEGIN                                         PASCP 2402
                                    IF (NXT'.VKIND = ACTUAL) THEN               PASCP 2403
                                      IF LSP'.FORM <= POWER THEN                P      416
                                      BEGIN LOAD;                               PASCP 2405
                                        IF DEBUG THEN CHECKBNDS(LSP);           P      417
                                        IF COMPTYPES(REALPTR,LSP)               PASCP 2406
                                           AND (GATTR.TYPTR = INTPTR) THEN      PASCP 2407
                                          BEGIN GEN0(10(*FLT*));                PASCP 2408
                                            GATTR.TYPTR := REALPTR              PASCP 2409
                                          END;                                  PASCP 2410
                                        LOCPAR := LOCPAR+LSP'.SIZE;             P      418
                                        ALIGN(PARMPTR,LOCPAR);                  P      419
                                      END                                       PASCP 2412
                                      ELSE                                      PASCP 2413
                                      BEGIN                                     PASCP 2414
                                        LOADADDRESS;                            P      420
                                        LOCPAR := LOCPAR+PTRSIZE;               P      421
                                        ALIGN(PARMPTR,LOCPAR)                   P      422
                                      END                                       PASCP 2440
                                    ELSE                                        PASCP 2441
                                      IF GATTR.KIND = VARBL THEN                PASCP 2442
                                       BEGIN LOADADDRESS;                       P      423
                                         LOCPAR := LOCPAR+PTRSIZE;              X5       1
                                         ALIGN(PARMPTR,LOCPAR);                 X5       2
                                        END                                     PASCP 2444
                                      ELSE ERROR(154);                          PASCP 2445
                                    IF NOT COMPTYPES(LSP,GATTR.TYPTR) THEN      PASCP 2446
                                      ERROR(142)                                PASCP 2447
                                  END                                           PASCP 2448
                            END                                                 PASCP 2449
                      END                                                       PASCP 2450
                     ELSE (*LKIND = FORMAL*)                                    PASCP 2451
                      BEGIN (*PASS FORMAL PARAM*)                               PASCP 2452
                     END                                                        PASCP 2453
                 END;                                                           PASCP 2454
                 IF (LKIND = ACTUAL) AND (NXT <> NIL) THEN NXT := NXT'.NEXT     PASCP 2455
                UNTIL SY <> COMMA;                                              PASCP 2456
                LC := LLC;                                                      PASCP 2457
              IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)                       PASCP 2458
            END (*IF LPARENT*);                                                 PASCP 2459
            IF LKIND = ACTUAL THEN                                              PASCP 2460
              BEGIN IF NXT <> NIL THEN ERROR(126);                              PASCP 2461
                WITH FCP' DO                                                    PASCP 2462
                  BEGIN                                                         PASCP 2463
                    IF EXTERN THEN GEN1(30(*CSP*),PFNAME)                       PASCP 2464
                    ELSE GENCUPENT(46(*CUP*),LOCPAR,PFNAME);                    P      425
                  END                                                           PASCP 2466
              END;                                                              PASCP 2467
            GATTR.TYPTR := FCP'.IDTYPE                                          PASCP 2468
          END (*CALLNONSTANDARD*) ;                                             PASCP 2469
                                                                                PASCP 2470
        BEGIN (*CALL*)                                                          PASCP 2471
          IF FCP'.PFDECKIND = STANDARD THEN                                     PASCP 2472
            BEGIN LKEY := FCP'.KEY;                                             P      426
              IF FCP'.KLASS = PROC THEN                                         PASCP 2475
               BEGIN                                                            P      427
                IF NOT(LKEY IN [5,6,11,12]) THEN                                P      428
                  IF SY = LPARENT THEN INSYMBOL ELSE ERROR(9);                  P      429
                CASE LKEY OF                                                    PASCP 2476
                  1,2,                                                          PASCP 2477
                  3,4:  GETPUTRESETREWRITE;                                     PASCP 2478
                  5,11:    READ;                                                PASCP 2479
                  6,12:    WRITE;                                               PASCP 2480
                  7:    PACK;                                                   PASCP 2481
                  8:    UNPACK;                                                 PASCP 2482
                  9:    NEW;                                                    PASCP 2483
                  10:   RELEASE;                                                PASCP 2484
                  13:   MARK                                                    PASCP 2485
                END;                                                            P      430
                IF NOT(LKEY IN [5,6,11,12]) THEN                                P      431
                  IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)                   P      432
               END                                                              P      433
              ELSE                                                              PASCP 2487
                BEGIN                                                           P      434
                  IF LKEY <= 8 THEN                                             P      435
                    BEGIN                                                       P      436
                      IF SY = LPARENT THEN INSYMBOL ELSE ERROR(9);              P      437
                      EXPRESSION(FSYS+[RPARENT]); LOAD                          P      438
                    END;                                                        P      439
                  CASE LKEY OF                                                  PASCP 2490
                    1:    ABS;                                                  PASCP 2491
                    2:    SQR;                                                  PASCP 2492
                    3:    TRUNC;                                                PASCP 2493
                    4:    ODD;                                                  PASCP 2494
                    5:    ORD;                                                  PASCP 2495
                    6:    CHR;                                                  PASCP 2496
                    7,8:  PREDSUCC;                                             PASCP 2497
                    9,10:    EOF                                                PASCP 2498
                  END;                                                          P      440
                  IF LKEY <= 8 THEN                                             P      441
                    IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)                 P      442
                END;                                                            PASCP 2500
            END (*STANDARD PROCEDURES AND FUNCTIONS*)                           PASCP 2502
          ELSE CALLNONSTANDARD                                                  PASCP 2503
        END (*CALL*) ;                                                          PASCP 2504
                                                                                PASCP 2505
        PROCEDURE EXPRESSION;                                                   PASCP 2506
          VAR LATTR: ATTR; LOP: OPERATOR; TYPIND: CHAR; LSIZE: ADDRRANGE;       PASCP 2507
                                                                                PASCP 2508
          PROCEDURE SIMPLEEXPRESSION(FSYS: SETOFSYS);                           PASCP 2509
            VAR LATTR: ATTR; LOP: OPERATOR; SIGNED: BOOLEAN;                    PASCP 2510
                                                                                PASCP 2511
            PROCEDURE TERM(FSYS: SETOFSYS);                                     PASCP 2512
              VAR LATTR: ATTR; LOP: OPERATOR;                                   PASCP 2513
                                                                                PASCP 2514
              PROCEDURE FACTOR(FSYS: SETOFSYS);                                 PASCP 2515
                VAR LCP: CTP; LVP: CSP; VARPART: BOOLEAN;                       PASCP 2516
                    CSTPART: SET OF 0..58; LSP: STP;                            PASCP 2517
              BEGIN                                                             PASCP 2518
                IF NOT (SY IN FACBEGSYS) THEN                                   PASCP 2519
                  BEGIN ERROR(58); SKIP(FSYS + FACBEGSYS);                      PASCP 2520
                    GATTR.TYPTR := NIL                                          PASCP 2521
                  END;                                                          PASCP 2522
                WHILE SY IN FACBEGSYS DO                                        PASCP 2523
                  BEGIN                                                         PASCP 2524
                    CASE SY OF                                                  PASCP 2525
              (*ID*)    IDENT:                                                  PASCP 2526
                        BEGIN SEARCHID([KONST,VARS,FIELD,FUNC],LCP);            PASCP 2527
                          INSYMBOL;                                             PASCP 2528
                          IF LCP'.KLASS = FUNC THEN                             PASCP 2529
                            BEGIN CALL(FSYS,LCP);                               P      443
                              WITH GATTR DO                                     P      444
                                BEGIN KIND := EXPR;                             P      445
                                  IF TYPTR <> NIL THEN                          P      446
                                    IF TYPTR'.FORM=SUBRANGE THEN                P      447
                                      TYPTR := TYPTR'.RANGETYPE                 P      448
                                END                                             P      449
                            END                                                 P      450
                          ELSE                                                  PASCP 2531
                            IF LCP'.KLASS = KONST THEN                          PASCP 2532
                              WITH GATTR, LCP' DO                               PASCP 2533
                                BEGIN TYPTR := IDTYPE; KIND := CST;             PASCP 2534
                                  CVAL := VALUES                                PASCP 2535
                                END                                             PASCP 2536
                            ELSE                                                PASCP 2537
                              BEGIN SELECTOR(FSYS,LCP);                         PASCP 2538
                                IF GATTR.TYPTR<>NIL THEN(*ELIM.SUBR.TYPES TO*)  PASCP 2539
                                  WITH GATTR,TYPTR' DO(*SIMPLIFY LATER TESTS*)  PASCP 2540
                                    IF FORM = SUBRANGE THEN                     PASCP 2541
                                      TYPTR := RANGETYPE                        PASCP 2542
                              END                                               PASCP 2543
                        END;                                                    PASCP 2544
              (*CST*)   INTCONST:                                               PASCP 2545
                        BEGIN                                                   PASCP 2546
                          WITH GATTR DO                                         PASCP 2547
                            BEGIN TYPTR := INTPTR; KIND := CST;                 PASCP 2548
                              CVAL := VAL                                       PASCP 2549
                            END;                                                PASCP 2550
                          INSYMBOL                                              PASCP 2551
                        END;                                                    PASCP 2552
                      REALCONST:                                                PASCP 2553
                        BEGIN                                                   PASCP 2554
                          WITH GATTR DO                                         PASCP 2555
                            BEGIN TYPTR := REALPTR; KIND := CST;                PASCP 2556
                              CVAL := VAL                                       PASCP 2557
                            END;                                                PASCP 2558
                          INSYMBOL                                              PASCP 2559
                        END;                                                    PASCP 2560
                      STRINGCONST:                                              PASCP 2561
                        BEGIN                                                   PASCP 2562
                          WITH GATTR DO                                         PASCP 2563
                            BEGIN                                               PASCP 2564
                              IF LGTH = 1 THEN TYPTR := CHARPTR                 PASCP 2565
                              ELSE                                              PASCP 2566
                                BEGIN NEW(LSP,ARRAYS);                          PASCP 2567
                                  WITH LSP' DO                                  PASCP 2568
                                    BEGIN AELTYPE := CHARPTR; FORM:=ARRAYS;     PASCP 2569
                                      INXTYPE := NIL; SIZE := LGTH*CHARSIZE     PASCP 2570
                                    END;                                        PASCP 2571
                                  TYPTR := LSP                                  PASCP 2572
                                END;                                            PASCP 2573
                              KIND := CST; CVAL := VAL                          PASCP 2574
                            END;                                                PASCP 2575
                          INSYMBOL                                              PASCP 2576
                        END;                                                    PASCP 2577
              (*(*)     LPARENT:                                                PASCP 2578
                        BEGIN INSYMBOL; EXPRESSION(FSYS + [RPARENT]);           PASCP 2579
                          IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)           PASCP 2580
                        END;                                                    PASCP 2581
              (*NOT*)   NOTSY:                                                  PASCP 2582
                        BEGIN INSYMBOL; FACTOR(FSYS);                           PASCP 2583
                          LOAD; GEN0(19(*NOT*));                                PASCP 2584
                          IF GATTR.TYPTR <> NIL THEN                            PASCP 2585
                            IF GATTR.TYPTR <> BOOLPTR THEN                      PASCP 2586
                              BEGIN ERROR(135); GATTR.TYPTR := NIL END;         PASCP 2587
                        END;                                                    PASCP 2588
              (*[*)     LBRACK:                                                 PASCP 2589
                        BEGIN INSYMBOL; CSTPART := [ ]; VARPART := FALSE;       PASCP 2590
                          NEW(LSP,POWER);                                       PASCP 2591
                          WITH LSP' DO                                          PASCP 2592
                            BEGIN ELSET:=NIL;SIZE:=SETSIZE;FORM:=POWER END;     PASCP 2593
                          IF SY = RBRACK THEN                                   PASCP 2594
                            BEGIN                                               PASCP 2595
                              WITH GATTR DO                                     PASCP 2596
                                BEGIN TYPTR := LSP; KIND := CST END;            PASCP 2597
                              INSYMBOL                                          PASCP 2598
                            END                                                 PASCP 2599
                          ELSE                                                  PASCP 2600
                            BEGIN                                               PASCP 2601
                              REPEAT EXPRESSION(FSYS + [COMMA,RBRACK]);         PASCP 2602
                                IF GATTR.TYPTR <> NIL THEN                      PASCP 2603
                                  IF GATTR.TYPTR'.FORM <> SCALAR THEN           PASCP 2604
                                    BEGIN ERROR(136); GATTR.TYPTR := NIL END    PASCP 2605
                                  ELSE                                          PASCP 2606
                                    IF COMPTYPES(LSP'.ELSET,GATTR.TYPTR) THEN   PASCP 2607
                                      BEGIN                                     PASCP 2608
                                        IF GATTR.KIND = CST THEN                PASCP 2609
                                          IF (GATTR.CVAL.IVAL < SETLOW) OR      P      451
                                            (GATTR.CVAL.IVAL > SETHIGH) THEN    P      452
                                            ERROR(304)                          P      453
                                          ELSE                                  P      454
                                            CSTPART := CSTPART+[GATTR.CVAL.IVAL]P      455
                                        ELSE                                    PASCP 2611
                                          BEGIN LOAD;                           P      456
                                            IF NOT COMPTYPES(GATTR.TYPTR,INTPTR)P      457
                                            THEN GEN0T(58(*ORD*),GATTR.TYPTR);  P      458
                                            GEN0(23(*SGS*));                    P      459
                                            IF VARPART THEN GEN0(28(*UNI*))     PASCP 2613
                                            ELSE VARPART := TRUE                PASCP 2614
                                          END;                                  PASCP 2615
                                        LSP'.ELSET := GATTR.TYPTR;              PASCP 2616
                                        GATTR.TYPTR := LSP                      PASCP 2617
                                      END                                       PASCP 2618
                                    ELSE ERROR(137);                            PASCP 2619
                                TEST := SY <> COMMA;                            PASCP 2620
                                IF NOT TEST THEN INSYMBOL                       PASCP 2621
                              UNTIL TEST;                                       PASCP 2622
                              IF SY = RBRACK THEN INSYMBOL ELSE ERROR(12)       PASCP 2623
                            END;                                                PASCP 2624
                          IF VARPART THEN                                       PASCP 2625
                            BEGIN                                               PASCP 2626
                              IF CSTPART <> [ ] THEN                            PASCP 2627
                                BEGIN NEW(LVP,PSET); LVP'.PVAL := CSTPART;      PASCP 2628
                                  LVP'.CCLASS := PSET;                          PASCP 2629
                                  IF CSTPTRIX = CSTOCCMAX THEN ERROR(254)       PASCP 2630
                                  ELSE                                          PASCP 2631
                                    BEGIN CSTPTRIX := CSTPTRIX + 1;             PASCP 2632
                                      CSTPTR[CSTPTRIX] := LVP;                  PASCP 2633
                                      GEN2(51(*LDC*),5,CSTPTRIX);               PASCP 2634
                                      GEN0(28(*UNI*)); GATTR.KIND := EXPR       PASCP 2635
                                    END                                         PASCP 2636
                                END                                             PASCP 2637
                            END                                                 PASCP 2638
                          ELSE                                                  PASCP 2639
                            BEGIN NEW(LVP,PSET); LVP'.PVAL := CSTPART;          PASCP 2640
                              LVP'.CCLASS := PSET;                              PASCP 2641
                              GATTR.CVAL.VALP := LVP                            PASCP 2642
                            END                                                 PASCP 2643
                        END                                                     PASCP 2644
                    END (*CASE*) ;                                              PASCP 2645
                    IF NOT (SY IN FSYS) THEN                                    PASCP 2646
                      BEGIN ERROR(6); SKIP(FSYS + FACBEGSYS) END                PASCP 2647
                  END (*WHILE*)                                                 PASCP 2648
              END (*FACTOR*) ;                                                  PASCP 2649
                                                                                PASCP 2650
            BEGIN (*TERM*)                                                      PASCP 2651
              FACTOR(FSYS + [MULOP]);                                           PASCP 2652
              WHILE SY = MULOP DO                                               PASCP 2653
                      BEGIN LOAD; LATTR := GATTR; LOP := OP;                    PASCP 2654
                  INSYMBOL; FACTOR(FSYS + [MULOP]); LOAD;                       PASCP 2655
                  IF (LATTR.TYPTR <> NIL) AND (GATTR.TYPTR <> NIL) THEN         PASCP 2656
                    CASE LOP OF                                                 PASCP 2657
            (***)       MUL:  IF (LATTR.TYPTR=INTPTR)AND(GATTR.TYPTR=INTPTR)    PASCP 2658
                              THEN GEN0(15(*MPI*))                              PASCP 2659
                            ELSE                                                PASCP 2660
                              BEGIN                                             PASCP 2661
                                IF LATTR.TYPTR = INTPTR THEN                    PASCP 2662
                                  BEGIN GEN0(9(*FLO*));                         PASCP 2663
                                    LATTR.TYPTR := REALPTR                      PASCP 2664
                                  END                                           PASCP 2665
                                ELSE                                            PASCP 2666
                                  IF GATTR.TYPTR = INTPTR THEN                  PASCP 2667
                                    BEGIN GEN0(10(*FLT*));                      PASCP 2668
                                      GATTR.TYPTR := REALPTR                    PASCP 2669
                                    END;                                        PASCP 2670
                                IF (LATTR.TYPTR = REALPTR)                      PASCP 2671
                                  AND(GATTR.TYPTR=REALPTR)THEN GEN0(16(*MPR*))  PASCP 2672
                                ELSE                                            PASCP 2673
                                  IF(LATTR.TYPTR'.FORM=POWER)                   PASCP 2674
                                    AND COMPTYPES(LATTR.TYPTR,GATTR.TYPTR)THEN  PASCP 2675
                                    GEN0(12(*INT*))                             PASCP 2676
                                  ELSE BEGIN ERROR(134);GATTR.TYPTR:=NIL END    PASCP 2677
                              END;                                              PASCP 2678
            (*/*)       RDIV: BEGIN                                             PASCP 2679
                              IF GATTR.TYPTR = INTPTR THEN                      PASCP 2684
                                  BEGIN GEN0(10(*FLT*));                        PASCP 2685
                                  GATTR.TYPTR := REALPTR                        PASCP 2686
                                END;                                            PASCP 2687
                              IF LATTR.TYPTR = INTPTR THEN                      J        7
                                BEGIN GEN0(9(*FLO*));                           J        8
                                  LATTR.TYPTR := REALPTR                        J        9
                                END;                                            J       10
                              IF (LATTR.TYPTR = REALPTR)                        PASCP 2688
                                AND (GATTR.TYPTR=REALPTR)THEN GEN0(7(*DVR*))    PASCP 2689
                              ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END     PASCP 2690
                            END;                                                PASCP 2691
            (*DIV*)     IDIV: IF (LATTR.TYPTR = INTPTR)                         PASCP 2692
                              AND (GATTR.TYPTR = INTPTR) THEN GEN0(6(*DVI*))    PASCP 2693
                            ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END;      PASCP 2694
            (*MOD*)     IMOD: IF (LATTR.TYPTR = INTPTR)                         PASCP 2695
                              AND (GATTR.TYPTR = INTPTR) THEN GEN0(14(*MOD*))   PASCP 2696
                            ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END;      PASCP 2697
            (*AND*)     ANDOP:IF (LATTR.TYPTR = BOOLPTR)                        PASCP 2698
                              AND (GATTR.TYPTR = BOOLPTR) THEN GEN0(4(*AND*))   PASCP 2699
                            ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END       PASCP 2700
                    END (*CASE*)                                                PASCP 2701
                  ELSE GATTR.TYPTR := NIL                                       PASCP 2702
                END (*WHILE*)                                                   PASCP 2703
            END (*TERM*) ;                                                      PASCP 2704
                                                                                PASCP 2705
          BEGIN (*SIMPLEEXPRESSION*)                                            PASCP 2706
            SIGNED := FALSE;                                                    PASCP 2707
            IF (SY = ADDOP) AND (OP IN [PLUS,MINUS]) THEN                       PASCP 2708
              BEGIN SIGNED := OP = MINUS; INSYMBOL END;                         PASCP 2709
            TERM(FSYS + [ADDOP]);                                               PASCP 2710
            IF SIGNED THEN                                                      PASCP 2711
              BEGIN LOAD;                                                       PASCP 2712
                IF GATTR.TYPTR = INTPTR THEN GEN0(17(*NGI*))                    PASCP 2713
                ELSE                                                            PASCP 2714
                  IF GATTR.TYPTR = REALPTR THEN GEN0(18(*NGR*))                 PASCP 2715
                  ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END                 PASCP 2716
              END;                                                              PASCP 2717
            WHILE SY = ADDOP DO                                                 PASCP 2718
              BEGIN LOAD; LATTR := GATTR; LOP := OP;                            PASCP 2719
                INSYMBOL; TERM(FSYS + [ADDOP]); LOAD;                           PASCP 2720
                IF (LATTR.TYPTR <> NIL) AND (GATTR.TYPTR <> NIL) THEN           PASCP 2721
          CASE LOP OF                                                           PASCP 2722
         (*+*)       PLUS:                                                      PASCP 7223
                   IF (LATTR.TYPTR = INTPTR)AND(GATTR.TYPTR = INTPTR) THEN      PASCP 2724
                        GEN0(2(*ADI*))                                          PASCP 2725
                      ELSE                                                      PASCP 2726
                        BEGIN                                                   PASCP 2727
                          IF LATTR.TYPTR = INTPTR THEN                          PASCP 2728
                            BEGIN GEN0(9(*FLO*));                               PASCP 2729
                              LATTR.TYPTR := REALPTR                            PASCP 2730
                            END                                                 PASCP 2731
                          ELSE                                                  PASCP 2732
                       IF GATTR.TYPTR = INTPTR THEN                             PASCP 2733
                              BEGIN GEN0(10(*LT*));                             PASCP 2734
                                GATTR.TYPTR := REALPTR                          PASCP 2735
                            END;                                                PASCP 2736
                          IF (LATTR.TYPTR = REALPTR)AND(GATTR.TYPTR = REALPTR)  PASCP 2737
                            THEN GEN0(3(*ADR*))                                 PASCP 2738
                         ELSE IF(LATTR.TYPTR'.FORM=POWER)                       PASCP 2739
                                 AND COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN    PASCP 2740
                                 GEN0(28(*UNI*))                                PASCP 2741
                               ELSE BEGIN ERROR(134);GATTR.TYPTR:=NIL END       PASCP 2742
                        END;                                                    PASCP 2743
          (*-*)       MINUS:                                                    PASCP 2744
                      IF (LATTR.TYPTR = INTPTR)AND(GATTR.TYPTR = INTPTR) THEN   PASCP 2745
                        GEN0(21(*SBI*))                                         PASCP 2746
                      ELSE                                                      PASCP 2747
                        BEGIN                                                   PASCP 2748
                          IF LATTR.TYPTR = INTPTR THEN                          PASCP 2749
                            BEGIN GEN0(9(*FLO*));                               PASCP 2750
                              LATTR.TYPTR := REALPTR                            PASCP 2751
                            END                                                 PASCP 2752
                          ELSE                                                  PASCP 2753
                            IF GATTR.TYPTR = INTPTR THEN                        PASCP 2754
                            BEGIN GEN0(10(*FLT*));                              PASCP 2755
                                GATTR.TYPTR := REALPTR                          PASCP 2756
                              END;                                              PASCP 2757
                          IF (LATTR.TYPTR = REALPTR)AND(GATTR.TYPTR = REALPTR)  PASCP 2758
                            THEN GEN0(22(*SBR*))                                PASCP 2759
                          ELSE                                                  PASCP 2760
                            IF (LATTR.TYPTR'.FORM = POWER)                      PASCP 2761
                              AND COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN       PASCP 2762
                              GEN0(5(*DIF*))                                    PASCP 2763
                            ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END       PASCP 2764
                        END;                                                    PASCP 2765
          (*OR*)      OROP:                                                     PASCP 2766
                      IF(LATTR.TYPTR=BOOLPTR)AND(GATTR.TYPTR=BOOLPTR)THEN       PASCP 2767
                        GEN0(13(*IOR*))                                         PASCP 2768
                      ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END             PASCP 2769
                  END (*CASE*)                                                  PASCP 2770
                ELSE GATTR.TYPTR := NIL                                         PASCP 2771
              END (*WHILE*)                                                     PASCP 2772
          END (*SIMPLEEXPRESSION*) ;                                            PASCP 2773
                                                                                PASCP 2774
        BEGIN (*EXPRESSION*)                                                    PASCP 2775
          SIMPLEEXPRESSION(FSYS + [RELOP]);                                     PASCP 2776
          IF SY = RELOP THEN                                                    PASCP 2777
            BEGIN                                                               PASCP 2778
              IF GATTR.TYPTR <> NIL THEN                                        PASCP 2779
                IF GATTR.TYPTR'.FORM <= POWER THEN LOAD                         PASCP 2780
                ELSE LOADADDRESS;                                               PASCP 2781
                LATTR := GATTR; LOP := OP;                                      PASCP 2782
             IF LOP = INOP THEN                                                 P      460
               IF NOT COMPTYPES(GATTR.TYPTR,INTPTR) THEN                        P      461
                 GEN0T(58(*ORD*),GATTR.TYPTR);                                  P      462
              INSYMBOL; SIMPLEEXPRESSION(FSYS);                                 PASCP 2783
              IF GATTR.TYPTR <> NIL THEN                                        PASCP 2784
                IF GATTR.TYPTR'.FORM <= POWER THEN LOAD                         PASCP 2785
                ELSE LOADADDRESS;                                               PASCP 2786
              IF (LATTR.TYPTR <> NIL) AND (GATTR.TYPTR <> NIL) THEN             PASCP 2787
                IF LOP = INOP THEN                                              PASCP 2788
                  IF GATTR.TYPTR'.FORM = POWER THEN                             PASCP 2789
                    IF COMPTYPES(LATTR.TYPTR,GATTR.TYPTR'.ELSET) THEN           PASCP 2790
                      GEN0(11(*INN*))                                           PASCP 2791
                    ELSE BEGIN ERROR(129); GATTR.TYPTR := NIL END               PASCP 2792
                  ELSE BEGIN ERROR(130); GATTR.TYPTR := NIL END                 PASCP 2793
                ELSE                                                            PASCP 2794
                  BEGIN                                                         PASCP 2795
                    IF LATTR.TYPTR <> GATTR.TYPTR THEN                          PASCP 2796
                      IF LATTR.TYPTR = INTPTR THEN                              PASCP 2797
                        BEGIN GEN0(9(*FLO*));                                   PASCP 2798
                          LATTR.TYPTR := REALPTR                                PASCP 2799
                        END                                                     PASCP 2800
                      ELSE                                                      PASCP 2801
                        IF GATTR.TYPTR = INTPTR THEN                            PASCP 2802
                          BEGIN GEN0(10(*FLT*));                                PASCP 2803
                            GATTR.TYPTR := REALPTR                              PASCP 2804
                          END;                                                  PASCP 2805
                    IF COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN                  PASCP 2806
                      BEGIN LSIZE := LATTR.TYPTR'.SIZE;                         PASCP 2807
                        CASE LATTR.TYPTR'.FORM OF                               PASCP 2808
                          SCALAR:                                               PASCP 2809
                            IF LATTR.TYPTR = REALPTR THEN TYPIND := #R#         PASCP 2810
                            ELSE                                                PASCP 2811
                              IF LATTR.TYPTR = BOOLPTR THEN TYPIND := #B#       PASCP 2812
                              ELSE                                              P      463
                                IF LATTR.TYPTR = CHARPTR THEN TYPIND := #C#     P      464
                                ELSE TYPIND := #I#;                             P      465
                          POINTER:                                              PASCP 2814
                            BEGIN                                               PASCP 2815
                              IF LOP IN [LTOP,LEOP,GTOP,GEOP] THEN ERROR(131);  PASCP 2816
                              TYPIND := #A#                                     PASCP 2817
                            END;                                                PASCP 2818
                          POWER:                                                PASCP 2819
                            BEGIN IF LOP IN [LTOP,GTOP] THEN ERROR(132);        PASCP 2820
                              TYPIND := #S#                                     PASCP 2821
                          END;                                                  PASCP 2822
                          ARRAYS:                                               PASCP 2823
                            BEGIN                                               PASCP 2824
                              IF NOT STRING(LATTR.TYPTR)                        PASCP 2825
                              AND(LOP IN[LTOP,LEOP,GTOP,GEOP])THEN ERROR(131);  PASCP 2826
                              TYPIND := #M#                                     PASCP 2827
                            END;                                                PASCP 2828
                          RECORDS:                                              PASCP 2829
                            BEGIN                                               PASCP 2830
                              IF LOP IN [LTOP,LEOP,GTOP,GEOP] THEN ERROR(131);  PASCP 2831
                              TYPIND := #M#                                     PASCP 2832
                            END;                                                PASCP 2833
                          FILES:                                                PASCP 2834
                            BEGIN ERROR(133); TYPIND := #F# END                 PASCP 2835
                        END;                                                    PASCP 2836
                        CASE LOP OF                                             PASCP 2837
                          LTOP: GEN2(53(*LES*),ORD(TYPIND),LSIZE);              PASCP 2838
                          LEOP: GEN2(52(*LEQ*),ORD(TYPIND),LSIZE);              PASCP 2839
                          GTOP: GEN2(49(*GRT*),ORD(TYPIND),LSIZE);              PASCP 2840
                          GEOP: GEN2(48(*GEQ*),ORD(TYPIND),LSIZE);              PASCP 2841
                          NEOP: GEN2(55(*NEQ*),ORD(TYPIND),LSIZE);              PASCP 2842
                          EQOP: GEN2(47(*EQU*),ORD(TYPIND),LSIZE)               PASCP 2843
                        END                                                     PASCP 2844
                      END                                                       PASCP 2845
                    ELSE ERROR(129)                                             PASCP 2846
                  END;                                                          PASCP 2847
              GATTR.TYPTR := BOOLPTR; GATTR.KIND := EXPR                        PASCP 2848
            END (*SY = RELOP*)                                                  PASCP 2849
        END (*EXPRESSION*) ;                                                    PASCP 2850
                                                                                PASCP 2851
        PROCEDURE ASSIGNMENT(FCP: CTP);                                         PASCP 2852
          VAR LATTR: ATTR;                                                      PASCP 2853
        BEGIN SELECTOR(FSYS + [BECOMES],FCP);                                   PASCP 2854
          IF SY = BECOMES THEN                                                  PASCP 2855
            BEGIN                                                               PASCP 2856
              IF GATTR.TYPTR <> NIL THEN                                        PASCP 2857
                IF (GATTR.ACCESS<>DRCT) OR (GATTR.TYPTR'.FORM>POWER) THEN       PASCP 2858
                  LOADADDRESS;                                                  PASCP 2859
              LATTR := GATTR;                                                   PASCP 2860
              INSYMBOL; EXPRESSION(FSYS);                                       PASCP 2861
              IF GATTR.TYPTR <> NIL THEN                                        PASCP 2862
                IF GATTR.TYPTR'.FORM <= POWER THEN LOAD                         PASCP 2863
                ELSE LOADADDRESS;                                               PASCP 2864
              IF (LATTR.TYPTR <> NIL) AND (GATTR.TYPTR <> NIL) THEN             PASCP 2865
                BEGIN                                                           PASCP 2866
                  IF COMPTYPES(REALPTR,LATTR.TYPTR)AND(GATTR.TYPTR=INTPTR)THEN  PASCP 2867
                    BEGIN GEN0(10(*FLT*));                                      PASCP 2868
                      GATTR.TYPTR := REALPTR                                    PASCP 2869
                    END;                                                        PASCP 2870
                  IF COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN                    PASCP 2871
                    CASE LATTR.TYPTR'.FORM OF                                   PASCP 2872
                      SCALAR,                                                   PASCP 2873
                      SUBRANGE: BEGIN                                           P      466
                                  IF DEBUG THEN CHECKBNDS(LATTR.TYPTR);         P      467
                                  STORE(LATTR)                                  P      468
                                END;                                            P      469
                      POINTER: BEGIN                                            P      470
                                 IF DEBUG THEN                                  P      471
                                   GEN2T(45(*CHK*),0,MAXADDR,NILPTR);           P      472
                                 STORE(LATTR)                                   P      473
                               END;                                             P      474
                      POWER:   STORE(LATTR);                                    PASCP 2876
                      ARRAYS,                                                   PASCP 2877
                      RECORDS: GEN1(40(*MOV*),LATTR.TYPTR'.SIZE);               PASCP 2878
                      FILES: ERROR(146)                                         PASCP 2879
                    END                                                         PASCP 2880
                  ELSE ERROR(129)                                               PASCP 2881
                END                                                             PASCP 2882
            END (*SY = BECOMES*)                                                PASCP 2883
          ELSE ERROR(51)                                                        PASCP 2884
        END (*ASSIGNMENT*) ;                                                    PASCP 2885
                                                                                PASCP 2886
        PROCEDURE GOTOSTATEMENT;                                                PASCP 2887
          VAR LLP: LBP; FOUND: BOOLEAN; TTOP,TTOP1: DISPRANGE;                  PASCP 2888
        BEGIN                                                                   PASCP 2889
          IF SY = INTCONST THEN                                                 PASCP 2890
            BEGIN                                                               PASCP 2891
              FOUND := FALSE;                                                   PASCP 2892
              TTOP := TOP;                                                      PASCP 2893
              WHILE DISPLAY[TTOP].OCCUR <> BLCK DO TTOP := TTOP - 1;            KEN     13
              TTOP1 := TTOP;                                                    KEN     20
              REPEAT                                                            PASCP 2894
                LLP := DISPLAY[TTOP].FLABEL;                                    KEN     21
                WHILE (LLP <> NIL) AND NOT FOUND DO                             PASCP 2897
                  WITH LLP' DO                                                  PASCP 2898
                    IF LABVAL = VAL.IVAL THEN                                   PASCP 2899
                      BEGIN FOUND := TRUE;                                      PASCP 2900
                        IF TTOP = TTOP1 THEN                                    PASCP 2901
                          GENUJPXJP(57(*UJP*),LABNAME)                          P      475
                        ELSE (*GOTO LEADS OUT OF PROCEDURE*) ERROR(399)         PASCP 2903
                      END                                                       PASCP 2904
                    ELSE LLP := NEXTLAB;                                        PASCP 2905
                TTOP := TTOP - 1                                                PASCP 2906
              UNTIL FOUND OR (TTOP = 0);                                        PASCP 2907
              IF NOT FOUND THEN ERROR(167);                                     PASCP 2908
              INSYMBOL                                                          PASCP 2909
            END                                                                 PASCP 2910
          ELSE ERROR(15)                                                        PASCP 2911
        END (*GOTOSTATEMENT*) ;                                                 PASCP 2912
                                                                                PASCP 2913
        PROCEDURE COMPOUNDSTATEMENT;                                            PASCP 2914
        BEGIN                                                                   PASCP 2915
          REPEAT                                                                PASCP 2916
            REPEAT STATEMENT(FSYS + [SEMICOLON,ENDSY])                          PASCP 2917
            UNTIL NOT (SY IN STATBEGSYS);                                       PASCP 2918
            TEST := SY <> SEMICOLON;                                            PASCP 2919
            IF NOT TEST THEN INSYMBOL                                           PASCP 2920
          UNTIL TEST;                                                           PASCP 2921
          IF SY = ENDSY THEN INSYMBOL ELSE ERROR(13)                            PASCP 2922
        END (*COMPOUNDSTATEMENET*) ;                                            PASCP 2923
                                                                                PASCP 2924
        PROCEDURE IFSTATEMENT;                                                  PASCP 2925
          VAR LCIX1,LCIX2: INTEGER;                                             PASCP 2926
        BEGIN EXPRESSION(FSYS + [THENSY]);                                      PASCP 2927
          GENLABEL(LCIX1); GENFJP(LCIX1);                                       PASCP 2928
          IF SY = THENSY THEN INSYMBOL ELSE ERROR(52);                          PASCP 2929
          STATEMENT(FSYS + [ELSESY]);                                           PASCP 2931
          IF SY = ELSESY THEN                                                   PASCP 2932
            BEGIN GENLABEL(LCIX2); GENUJPXJP(57(*UJP*),LCIX2);                  P      476
              PUTLABEL(LCIX1);                                                  PASCP 2934
              INSYMBOL; STATEMENT(FSYS);                                        PASCP 2935
              PUTLABEL(LCIX2)                                                   PASCP 2936
            END                                                                 PASCP 2937
          ELSE PUTLABEL(LCIX1)                                                  PASCP 2938
        END (*IFSTATEMENT*) ;                                                   PASCP 2939
                                                                                PASCP 2940
        PROCEDURE CASESTATEMENT;                                                PASCP 2941
          LABEL 1;                                                              PASCP 2942
          TYPE CIP = 'CASEINFO;                                                 PASCP 2943
               CASEINFO = PACKED                                                PASCP 2944
                          RECORD NEXT: CIP;                                     PASCP 2945
                            CSSTART: INTEGER;                                   PASCP 2946
                            CSLAB: INTEGER                                      PASCP 2947
                          END;                                                  PASCP 2948
          VAR LSP,LSP1: STP; FSTPTR,LPT1,LPT2,LPT3: CIP; LVAL: VALU;            PASCP 2949
              LADDR, LCIX, LCIX1, LMIN, LMAX: INTEGER;                          PASCP 2950
        BEGIN EXPRESSION(FSYS + [OFSY,COMMA,COLON]);                            PASCP 2951
          LOAD; GENLABEL(LCIX);                                                 X4       2
          LSP := GATTR.TYPTR;                                                   PASCP 2953
          IF LSP <> NIL THEN                                                    PASCP 2954
            IF (LSP'.FORM <> SCALAR) OR (LSP = REALPTR) THEN                    PASCP 2955
             BEGIN ERROR(144); LSP := NIL END                                   P      478
           ELSE IF NOT COMPTYPES(LSP,INTPTR) THEN GEN0T(58(*ORD*),LSP);         P      479
          GENUJPXJP(57(*UJP*),LCIX);                                            X4       3
          IF SY = OFSY THEN INSYMBOL ELSE ERROR(8);                             PASCP 2957
          FSTPTR := NIL; GENLABEL(LADDR);                                       PASCP 2958
          REPEAT                                                                PASCP 2959
            LPT3 := NIL; GENLABEL(LCIX1);                                       PASCP 2960
            IF NOT(SY IN [SEMICOLON,ENDSY]) THEN                                P      480
            BEGIN                                                               P      481
            REPEAT CONSTANT(FSYS + [COMMA,COLON],LSP1,LVAL);                    PASCP 2961
              IF LSP <> NIL THEN                                                PASCP 2962
                IF COMPTYPES(LSP,LSP1) THEN                                     PASCP 2963
                  BEGIN LPT1 := FSTPTR; LPT2 := NIL;                            PASCP 2964
                    WHILE LPT1 <> NIL DO                                        PASCP 2965
                      WITH LPT1' DO                                             PASCP 2966
                        BEGIN                                                   PASCP 2967
                          IF CSLAB <= LVAL.IVAL THEN                            PASCP 2968
                            BEGIN IF CSLAB = LVAL.IVAL THEN ERROR(156);         PASCP 2969
                              GOTO 1                                            PASCP 2970
                            END;                                                PASCP 2971
                          LPT2 := LPT1; LPT1 := NEXT                            PASCP 2972
                        END;                                                    PASCP 2973
        1:          NEW(LPT3);                                                  PASCP 2974
                    WITH LPT3' DO                                               PASCP 2975
                      BEGIN NEXT := LPT1; CSLAB := LVAL.IVAL;                   PASCP 2976
                        CSSTART := LCIX1                                        PASCP 2977
                      END;                                                      PASCP 2978
                    IF LPT2 = NIL THEN FSTPTR := LPT3                           PASCP 2979
                    ELSE LPT2'.NEXT := LPT3                                     PASCP 2980
                  END                                                           PASCP 2981
                ELSE ERROR(147);                                                PASCP 2982
              TEST := SY <> COMMA;                                              PASCP 2983
              IF NOT TEST THEN INSYMBOL                                         PASCP 2984
            UNTIL TEST;                                                         PASCP 2985
            IF SY = COLON THEN INSYMBOL ELSE ERROR(5);                          PASCP 2986
            PUTLABEL(LCIX1);                                                    PASCP 2987
            REPEAT STATEMENT(FSYS + [SEMICOLON])                                PASCP 2988
            UNTIL NOT (SY IN STATBEGSYS);                                       PASCP 2989
            IF LPT3 <> NIL THEN                                                 PASCP 2990
              GENUJPXJP(57(*UJP*),LADDR);                                       P      482
            END;                                                                P      483
            TEST := SY <> SEMICOLON;                                            PASCP 2992
            IF NOT TEST THEN INSYMBOL                                           PASCP 2993
          UNTIL TEST;                                                           PASCP 2994
          PUTLABEL(LCIX);                                                       PASCP 2995
          IF FSTPTR <> NIL THEN                                                 PASCP 2996
            BEGIN LMAX := FSTPTR'.CSLAB;                                        PASCP 2997
              (*REVERSE POINTERS*)                                              PASCP 2998
              LPT1 := FSTPTR; FSTPTR := NIL;                                    PASCP 2999
              REPEAT LPT2 := LPT1'.NEXT; LPT1'.NEXT := FSTPTR;                  PASCP 3000
                FSTPTR := LPT1; LPT1 := LPT2                                    PASCP 3001
              UNTIL LPT1 = NIL;                                                 PASCP 3002
              LMIN := FSTPTR'.CSLAB;                                            PASCP 3003
              IF LMAX - LMIN < CIXMAX THEN                                      PASCP 3004
                BEGIN                                                           P      484
                  GEN2T(45(*CHK*),LMIN,LMAX,INTPTR);                            P      485
                  GEN2(51(*LDC*),1,LMIN); GEN0(21(*SBI*)); GENLABEL(LCIX);      PASCP 3011
                  GENUJPXJP(44(*XJP*),LCIX); PUTLABEL(LCIX);                    P      486
                  REPEAT                                                        PASCP 3013
                    WITH FSTPTR' DO                                             PASCP 3014
                      BEGIN                                                     PASCP 3015
                        WHILE CSLAB > LMIN DO                                   PASCP 3016
                           BEGIN GEN0(60(*UJC ERROR*));                         P      487
                             LMIN := LMIN+1                                     P      488
                           END;                                                 P      489
                        GENUJPXJP(57(*UJP*),CSSTART);                           P      490
                        FSTPTR := NEXT; LMIN := LMIN + 1                        PASCP 3019
                      END                                                       PASCP 3020
                  UNTIL FSTPTR = NIL;                                           PASCP 3021
                  PUTLABEL(LADDR)                                               PASCP 3022
                END                                                             PASCP 3023
              ELSE ERROR(157)                                                   PASCP 3024
            END;                                                                PASCP 3025
            IF SY = ENDSY THEN INSYMBOL ELSE ERROR(13)                          PASCP 3026
        END (*CASESTATEMENT*) ;                                                 PASCP 3027
                                                                                PASCP 3028
        PROCEDURE REPEATSTATEMENT;                                              PASCP 3029
          VAR LADDR: INTEGER;                                                   PASCP 3030
        BEGIN GENLABEL(LADDR); PUTLABEL(LADDR);                                 PASCP 3031
          REPEAT STATEMENT(FSYS + [SEMICOLON,UNTILSY]);                         P      491
            IF SY IN STATBEGSYS THEN ERROR(14)                                  P      492
          UNTIL NOT(SY IN STATBEGSYS);                                          P      493
          WHILE SY = SEMICOLON DO                                               P      494
            BEGIN INSYMBOL;                                                     P      495
              REPEAT STATEMENT(FSYS + [SEMICOLON,UNTILSY]);                     P      496
                IF SY IN STATBEGSYS THEN ERROR(14)                              P      497
              UNTIL NOT (SY IN STATBEGSYS);                                     P      498
            END;                                                                P      499
          IF SY = UNTILSY THEN                                                  PASCP 3038
            BEGIN INSYMBOL; EXPRESSION(FSYS); GENFJP(LADDR)                     PASCP 3039
            END                                                                 PASCP 3040
          ELSE ERROR(53)                                                        PASCP 3041
        END (*REPEATSTATEMENT*) ;                                               PASCP 3042
                                                                                PASCP 3043
        PROCEDURE WHILESTATEMENT;                                               PASCP 3044
          VAR LADDR, LCIX: INTEGER;                                             PASCP 3045
        BEGIN GENLABEL(LADDR); PUTLABEL(LADDR);                                 PASCP 3046
          EXPRESSION(FSYS + [DOSY]); GENLABEL(LCIX); GENFJP(LCIX);              PASCP 3047
          IF SY = DOSY THEN INSYMBOL ELSE ERROR(54);                            PASCP 3048
          STATEMENT(FSYS); GENUJPXJP(57(*UJP*),LADDR); PUTLABEL(LCIX)           P      500
        END (*WHILESTATEMENT*) ;                                                PASCP 3050
                                                                                PASCP 3051
        PROCEDURE FORSTATEMENT;                                                 PASCP 3052
          VAR LATTR: ATTR; LSP: STP;  LSY: SYMBOL;                              PASCP 3053
              LCIX, LADDR: INTEGER;                                             PASCP 3054
                    LLC: ADDRRANGE;                                             P      501
        BEGIN LLC := LC;                                                        P      502
          WITH LATTR DO                                                         P      503
            BEGIN TYPTR := NIL; KIND := VARBL;                                  P      504
              ACCESS := DRCT; VLEVEL := LEVEL; DPLMT := 0                       P      505
            END;                                                                P      506
          IF SY = IDENT THEN                                                    PASCP 3056
            BEGIN SEARCHID([VARS],LCP);                                         PASCP 3057
              WITH LCP', LATTR DO                                               PASCP 3058
                BEGIN TYPTR := IDTYPE; KIND := VARBL;                           PASCP 3059
                  IF VKIND = ACTUAL THEN                                        PASCP 3060
                    BEGIN ACCESS := DRCT; VLEVEL := VLEV;                       PASCP 3061
                      DPLMT := VADDR                                            PASCP 3062
                    END                                                         PASCP 3063
                  ELSE BEGIN ERROR(155); TYPTR := NIL END                       PASCP 3064
                END;                                                            PASCP 3065
              IF LATTR.TYPTR <> NIL THEN                                        PASCP 3066
                IF (LATTR.TYPTR'.FORM > SUBRANGE)                               PASCP 3067
                   OR COMPTYPES(REALPTR,LATTR.TYPTR) THEN                       PASCP 3068
                  BEGIN ERROR(143); LATTR.TYPTR := NIL END;                     PASCP 3069
              INSYMBOL                                                          PASCP 3070
            END                                                                 PASCP 3071
          ELSE                                                                  PASCP 3072
            BEGIN ERROR(2); SKIP(FSYS + [BECOMES,TOSY,DOWNTOSY,DOSY]) END;      PASCP 3073
          IF SY = BECOMES THEN                                                  PASCP 3074
            BEGIN INSYMBOL; EXPRESSION(FSYS + [TOSY,DOWNTOSY,DOSY]);            PASCP 3075
              IF GATTR.TYPTR <> NIL THEN                                        PASCP 3076
                  IF GATTR.TYPTR'.FORM <> SCALAR THEN ERROR(144)                PASCP 3077
                  ELSE                                                          PASCP 3078
                    IF COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN                  PASCP 3079
                      BEGIN LOAD; STORE(LATTR) END                              PASCP 3080
                    ELSE ERROR(145)                                             PASCP 3081
            END                                                                 PASCP 3082
          ELSE                                                                  PASCP 3083
            BEGIN ERROR(51); SKIP(FSYS + [TOSY,DOWNTOSY,DOSY]) END;             PASCP 3084
          IF SY IN [TOSY,DOWNTOSY] THEN                                         PASCP 3085
            BEGIN LSY := SY; INSYMBOL; EXPRESSION(FSYS + [DOSY]);               PASCP 3086
              IF GATTR.TYPTR <> NIL THEN                                        PASCP 3087
              IF GATTR.TYPTR'.FORM <> SCALAR THEN ERROR(144)                    PASCP 3088
                ELSE                                                            PASCP 3089
                  IF COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN                    PASCP 3090
                    BEGIN LOAD;                                                 P      507
                      IF NOT COMPTYPES(LATTR.TYPTR,INTPTR) THEN                 P      508
                        GEN0T(58(*ORD*),GATTR.TYPTR);                           P      509
                      ALIGN(INTPTR,LC);                                         P      510
                      GEN2T(56(*STR*),0,LC,INTPTR);                             P      511
                      GENLABEL(LADDR); PUTLABEL(LADDR);                         PASCP 3092
                      GATTR := LATTR; LOAD;                                     P      512
                      IF NOT COMPTYPES(GATTR.TYPTR,INTPTR) THEN                 P      513
                        GEN0T(58(*ORD*),GATTR.TYPTR);                           P      514
                      GEN2T(54(*LOD*),0,LC,INTPTR);                             P      515
                      LC := LC + INTSIZE;                                       PASCP 3094
                      IF LC > LCMAX THEN LCMAX := LC;                           PASCP 3095
                      IF LSY = TOSY THEN GEN2(52(*LEQ*),ORD(#I#),1)             PASCP 3096
                      ELSE GEN2(48(*GEQ*),ORD(#I#),1);                          PASCP 3097
                    END                                                         PASCP 3098
                  ELSE ERROR(145)                                               PASCP 3099
            END                                                                 PASCP 3100
          ELSE BEGIN ERROR(55); SKIP(FSYS + [DOSY]) END;                        PASCP 3101
          GENLABEL(LCIX); GENUJPXJP(33(*FJP*),LCIX);                            P      516
          IF SY = DOSY THEN INSYMBOL ELSE ERROR(54);                            PASCP 3103
          STATEMENT(FSYS);                                                      PASCP 3104
          GATTR := LATTR; LOAD;                                                 PASCP 3105
          IF LSY=TOSY THEN GEN1T(34(*INC*),1,GATTR.TYPTR)                       P      517
          ELSE  GEN1T(31(*DEC*),1,GATTR.TYPTR);                                 P      518
          STORE(LATTR); GENUJPXJP(57(*UJP*),LADDR); PUTLABEL(LCIX);             P      519
          LC := LLC;                                                            P      520
        END (*FORSTATEMENT*) ;                                                  PASCP 3109
                                                                                PASCP 3110
                                                                                PASCP 3111
        PROCEDURE WITHSTATEMENT;                                                PASCP 3112
          VAR LCP: CTP; LCNT1: DISPRANGE; LLC: ADDRRANGE;                       P      521
        BEGIN LCNT1 := 0; LLC := LC;                                            P      522
          REPEAT                                                                PASCP 3115
            IF SY = IDENT THEN                                                  PASCP 3116
              BEGIN SEARCHID([VARS,FIELD],LCP); INSYMBOL END                    PASCP 3117
            ELSE BEGIN ERROR(2); LCP := UVARPTR END;                            PASCP 3118
            SELECTOR(FSYS + [COMMA,DOSY],LCP);                                  PASCP 3119
            IF GATTR.TYPTR <> NIL THEN                                          PASCP 3120
              IF GATTR.TYPTR'.FORM = RECORDS THEN                               PASCP 3121
                IF TOP < DISPLIMIT THEN                                         PASCP 3122
                  BEGIN TOP := TOP + 1; LCNT1 := LCNT1 + 1;                     PASCP 3123
                    WITH DISPLAY[TOP] DO                                        PASCP 3124
                      BEGIN FNAME := GATTR.TYPTR'.FSTFLD;                       PASCP 3125
                        FLABEL := NIL                                           PASCP 3126
                      END;                                                      PASCP 3127
                    IF GATTR.ACCESS = DRCT THEN                                 PASCP 3128
                      WITH DISPLAY[TOP] DO                                      PASCP 3129
                        BEGIN OCCUR := CREC; CLEV := GATTR.VLEVEL;              PASCP 3130
                          CDSPL := GATTR.DPLMT                                  PASCP 3131
                        END                                                     PASCP 3132
                    ELSE                                                        PASCP 3133
                      BEGIN LOADADDRESS;                                        P      523
                        ALIGN(NILPTR,LC);                                       P      524
                        GEN2T(56(*STR*),0,LC,NILPTR);                           P      525
                        WITH DISPLAY[TOP] DO                                    PASCP 3135
                          BEGIN OCCUR := VREC; VDSPL := LC END;                 PASCP 3136
                        LC := LC+PTRSIZE;                                       P      526
                        IF LC > LCMAX THEN LCMAX := LC                          PASCP 3138
                      END                                                       PASCP 3139
                  END                                                           PASCP 3140
                ELSE ERROR(250)                                                 PASCP 3141
              ELSE ERROR(140);                                                  PASCP 3142
            TEST := SY <> COMMA;                                                PASCP 3143
            IF NOT TEST THEN INSYMBOL                                           PASCP 3144
          UNTIL TEST;                                                           PASCP 3145
          IF SY = DOSY THEN INSYMBOL ELSE ERROR(54);                            PASCP 3146
          STATEMENT(FSYS);                                                      PASCP 3147
          TOP := TOP-LCNT1; LC := LLC;                                          P      527
        END (*WITHSTATEMENT*) ;                                                 PASCP 3149
                                                                                PASCP 3150
      BEGIN (*STATEMENT*)                                                       PASCP 3151
        IF SY = INTCONST THEN (*LABEL*)                                         PASCP 3152
          BEGIN LLP := DISPLAY[LEVEL].FLABEL;                                   PASCP 3153
            WHILE LLP <> NIL DO                                                 PASCP 3154
              WITH LLP' DO                                                      PASCP 3155
                IF LABVAL = VAL.IVAL THEN                                       PASCP 3156
                  BEGIN IF DEFINED THEN ERROR(165);                             PASCP 3157
                    PUTLABEL(LABNAME); DEFINED := TRUE;                         PASCP 3158
                    GOTO 1                                                      PASCP 3159
                  END                                                           PASCP 3160
                ELSE LLP := NEXTLAB;                                            PASCP 3161
            ERROR(167);                                                         PASCP 3162
      1:    INSYMBOL;                                                           PASCP 3163
            IF SY = COLON THEN INSYMBOL ELSE ERROR(5)                           PASCP 3164
          END;                                                                  PASCP 3165
        IF NOT (SY IN FSYS + [IDENT]) THEN                                      PASCP 3166
          BEGIN ERROR(6); SKIP(FSYS) END;                                       PASCP 3167
        IF SY IN STATBEGSYS + [IDENT] THEN                                      PASCP 3168
          BEGIN                                                                 PASCP 3169
            CASE SY OF                                                          PASCP 3170
              IDENT:    BEGIN SEARCHID([VARS,FIELD,FUNC,PROC],LCP); INSYMBOL;   PASCP 3171
                          IF LCP'.KLASS = PROC THEN CALL(FSYS,LCP)              PASCP 3172
                          ELSE ASSIGNMENT(LCP)                                  PASCP 3173
                        END;                                                    PASCP 3174
              BEGINSY:  BEGIN INSYMBOL; COMPOUNDSTATEMENT END;                  PASCP 3175
              GOTOSY:   BEGIN INSYMBOL; GOTOSTATEMENT END;                      PASCP 3176
              IFSY:     BEGIN INSYMBOL; IFSTATEMENT END;                        PASCP 3177
              CASESY:   BEGIN INSYMBOL; CASESTATEMENT END;                      PASCP 3178
              WHILESY:  BEGIN INSYMBOL; WHILESTATEMENT END;                     PASCP 3179
              REPEATSY: BEGIN INSYMBOL; REPEATSTATEMENT END;                    PASCP 3180
              FORSY:    BEGIN INSYMBOL; FORSTATEMENT END;                       PASCP 3181
              WITHSY:   BEGIN INSYMBOL; WITHSTATEMENT END                       PASCP 3182
            END;                                                                PASCP 3183
            IF NOT (SY IN [SEMICOLON,ENDSY,ELSESY,UNTILSY]) THEN                PASCP 3184
              BEGIN ERROR(6); SKIP(FSYS) END                                    PASCP 3185
          END                                                                   PASCP 3186
      END (*STATEMENT*) ;                                                       PASCP 3187
                                                                                PASCP 3188
    BEGIN (*BODY*)                                                              PASCP 3189
      IF FPROCP <> NIL THEN ENTNAME := FPROCP'.PFNAME                           PASCP 3190
      ELSE GENLABEL(ENTNAME);                                                   PASCP 3191
      CSTPTRIX := 0;                                                            X7       2
      TOPNEW   := LCAFTERMARKSTACK;                                             X7       3
      TOPMAX   := LCAFTERMARKSTACK;                                             X7       4
      PUTLABEL(ENTNAME); GENLABEL(SEGSIZE); GENLABEL(STACKTOP);                 P      529
      GENCUPENT(32(*ENT1*),1,SEGSIZE); GENCUPENT(32(*ENT2*),2,STACKTOP);        P      530
      IF FPROCP <> NIL THEN (*COPY MULTIPLE VALUES INTO LOACAL CELLS*)          PASCP 3195
        BEGIN LLC1 := LCAFTERMARKSTACK;                                         PASCP 3196
          LCP := FPROCP'.NEXT;                                                  PASCP 3197
          WHILE LCP <> NIL DO                                                   PASCP 3198
            WITH LCP' DO                                                        PASCP 3199
              BEGIN                                                             PASCP 3200
                ALIGN(PARMPTR,LLC1);                                            X6       1
                IF KLASS = VARS THEN                                            PASCP 3201
                  IF IDTYPE <> NIL THEN                                         PASCP 3202
                    IF IDTYPE'.FORM > POWER THEN                                X6       2
                      BEGIN                                                     PASCP 3204
                       IF VKIND = ACTUAL THEN                                   X6       3
                       BEGIN                                                    X6       4
                        GEN2(50(*LDA*),0,VADDR);                                PASCP 3205
                        GEN2T(54(*LOD*),0,LLC1,NILPTR);                         P      532
                        GEN1(40(*MOV*),IDTYPE'.SIZE);                           PASCP 3207
                       END;                                                     X6       5
                        LLC1 := LLC1 + PTRSIZE                                  PASCP 3208
                      END                                                       PASCP 3209
                    ELSE LLC1 := LLC1 + IDTYPE'.SIZE;                           PASCP 3210
                LCP := LCP'.NEXT;                                               PASCP 3211
              END;                                                              PASCP 3212
        END;                                                                    PASCP 3213
      LCMAX := LC;                                                              PASCP 3214
      REPEAT                                                                    PASCP 3215
        REPEAT STATEMENT(FSYS + [SEMICOLON,ENDSY])                              PASCP 3216
        UNTIL NOT (SY IN STATBEGSYS);                                           PASCP 3217
        TEST := SY <> SEMICOLON;                                                PASCP 3218
        IF NOT TEST THEN INSYMBOL                                               PASCP 3219
      UNTIL TEST;                                                               PASCP 3220
      IF SY = ENDSY THEN INSYMBOL ELSE ERROR(13);                               PASCP 3221
      LLP := DISPLAY[TOP].FLABEL; (*TEST FOR UNDEFINED LABELS*)                 PASCP 3222
      WHILE LLP <> NIL DO                                                       PASCP 3223
        WITH LLP' DO                                                            PASCP 3224
          BEGIN                                                                 PASCP 3225
            IF NOT DEFINED THEN                                                 PASCP 3226
              BEGIN ERROR(168);                                                 PASCP 3227
                WRITELN(OUTPUT); WRITELN(OUTPUT,# LABEL #,LABVAL);              PASCP 3228
               WRITE(OUTPUT,# #:CHCNT+16)                                       PASCP 3229
              END;                                                              PASCP 3230
            LLP := NEXTLAB                                                      PASCP 3231
          END;                                                                  PASCP 3232
      IF FPROCP <> NIL THEN                                                     PASCP 3233
        BEGIN                                                                   PASCP 3234
          IF FPROCP'.IDTYPE = NIL THEN GEN1(42(*RET*),ORD(#P#))                 PASCP 3235
              ELSE GEN0T(42(*RET*),FPROCP'.IDTYPE);                             J       11
          ALIGN(PARMPTR,LCMAX);                                                 P      533
          IF PRCODE THEN                                                        P      534
            BEGIN WRITELN(PRR,#L#,SEGSIZE:4,#=#,LCMAX);                         P      535
              WRITELN(PRR,#L#,STACKTOP:4,#=#,TOPMAX)                            P      536
            END                                                                 P      537
        END                                                                     PASCP 3248
      ELSE                                                                      PASCP 3249
        BEGIN GEN1(42(*RET*),ORD(#P#));                                         J       12
          ALIGN(PARMPTR,LCMAX);                                                 P      538
          IF PRCODE THEN                                                        P      539
            BEGIN WRITELN(PRR,#L#,SEGSIZE:4,#=#,LCMAX);                         P      540
              WRITELN(PRR,#L#,STACKTOP:4,#=#,TOPMAX);                           P      541
            WRITELN(PRR,#Q#)                                                    P      542
            END;                                                                P      543
          IC := 0;                                                              PASCP 3254
          (*GENERATE CALL OF MAIN PROGRAM; NOTE THAT THIS CALL MUST BE LOADED   PASCP 3255
           AT ABSOLUTE ADDRESS ZERO*)                                           PASCP 3256
          GEN1(41(*MST*),0); GENCUPENT(46(*CUP*),0,ENTNAME); GEN0(29(*STP*));   P      544
          IF PRCODE THEN                                                        PASCP 3258
            WRITELN(PRR,#Q#);                                                   P      545
          SAVEID := ID;                                                         PASCP 3260
          WHILE FEXTFILEP <> NIL DO                                             PASCP 3261
            BEGIN                                                               PASCP 3262
              WITH FEXTFILEP' DO                                                PASCP 3263
                IF NOT ((FILENAME = #INPUT   #) OR (FILENAME = #OUTPUT  #) OR   PASCP 3264
                        (FILENAME = #PRD     #) OR (FILENAME = #PRR     #))     PASCP 3265
                THEN BEGIN ID := FILENAME;                                      PASCP 3266
                       SEARCHID([VARS],LLCP);                                   PASCP 3267
                       IF LLCP'.IDTYPE<>NIL THEN                                P      546
                         IF LLCP'.IDTYPE'.FORM<>FILES THEN                      P      547
                         BEGIN WRITELN(OUTPUT);                                 PASCP 3272
                           WRITELN(OUTPUT,# #:8,#UNDECLARED #,#EXTERNAL #,      J       13
                                 #FILE#,FEXTFILEP'.FILENAME:8);                 J       14
                           WRITE(OUTPUT,# #:CHCNT+16)                           PASCP 3275
                         END                                                    PASCP 3276
                     END;                                                       PASCP 3277
                FEXTFILEP := FEXTFILEP'.NEXTFILE                                PASCP 3278
            END;                                                                PASCP 3279
          ID := SAVEID;                                                         PASCP 3280
          IF PRTABLES THEN                                                      P      548
            BEGIN WRITELN(OUTPUT); PRINTTABLES(TRUE)                            P      549
            END                                                                 P      550
        END;                                                                    PASCP 3284
    END (*BODY*) ;                                                              PASCP 3285
                                                                                PASCP 3286
  BEGIN (*BLOCK*)                                                               PASCP 3287
    DP := TRUE;                                                                 PASCP 3288
    REPEAT                                                                      PASCP 3289
      IF SY = LABELSY THEN                                                      PASCP 3290
        BEGIN INSYMBOL; LABELDECLARATION END;                                   PASCP 3291
      IF SY = CONSTSY THEN                                                      PASCP 3292
        BEGIN INSYMBOL; CONSTDECLARATION END;                                   PASCP 3293
      IF SY = TYPESY THEN                                                       PASCP 3294
        BEGIN INSYMBOL; TYPEDECLARATION END;                                    PASCP 3295
      IF SY = VARSY THEN                                                        PASCP 3296
        BEGIN INSYMBOL; VARDECLARATION END;                                     PASCP 3297
      WHILE SY IN [PROCSY,FUNCSY] DO                                            PASCP 3298
        BEGIN LSY := SY; INSYMBOL; PROCDECLARATION(LSY) END;                    PASCP 3299
      IF SY <> BEGINSY THEN                                                     PASCP 3300
        BEGIN ERROR(18); SKIP(FSYS) END                                         PASCP 3301
    UNTIL (SY IN STATBEGSYS) OR EOF(INPUT);                                     P      551
    DP := FALSE;                                                                PASCP 3303
    IF SY = BEGINSY THEN INSYMBOL ELSE ERROR(17);                               PASCP 3304
    REPEAT BODY(FSYS + [CASESY]);                                               PASCP 3305
      IF SY <> FSY THEN                                                         PASCP 3306
        BEGIN ERROR(6); SKIP(FSYS) END                                          P      552
    UNTIL ((SY = FSY) OR (SY IN BLOCKBEGSYS)) OR EOF(INPUT);                    P      553
  END (*BLOCK*) ;                                                               PASCP 3309
                                                                                PASCP 3310
  PROCEDURE PROGRAMME(FSYS:SETOFSYS);                                           PASCP 3311
    VAR EXTFP:EXTFILEP;                                                         PASCP 3312
  BEGIN                                                                         PASCP 3313
    IF SY = PROGSY THEN                                                         PASCP 3314
      BEGIN INSYMBOL; IF SY <> IDENT THEN ERROR(2); INSYMBOL;                   PASCP 3315
        IF NOT (SY IN [LPARENT,SEMICOLON]) THEN ERROR(14);                      PASCP 3316
        IF SY = LPARENT  THEN                                                   PASCP 3317
          BEGIN                                                                 PASCP 3318
            REPEAT INSYMBOL;                                                    PASCP 3319
              IF SY = IDENT THEN                                                PASCP 3320
                BEGIN NEW(EXTFP);                                               PASCP 3321
                  WITH EXTFP' DO                                                PASCP 3322
                    BEGIN FILENAME := ID; NEXTFILE := FEXTFILEP END;            PASCP 3323
                  FEXTFILEP := EXTFP;                                           PASCP 3324
                  INSYMBOL;                                                     PASCP 3325
                  IF NOT ( SY IN [COMMA,RPARENT] ) THEN ERROR(20)               PASCP 3326
                END                                                             PASCP 3327
              ELSE ERROR(2)                                                     PASCP 3328
            UNTIL SY <> COMMA;                                                  PASCP 3329
            IF SY <> RPARENT THEN ERROR(4);                                     PASCP 3330
            INSYMBOL                                                            PASCP 3331
          END;                                                                  PASCP 3332
        IF SY <> SEMICOLON THEN ERROR(14)                                       PASCP 3333
        ELSE INSYMBOL;                                                          PASCP 3334
      END;                                                                      PASCP 3335
    REPEAT BLOCK(FSYS,PERIOD,NIL);                                              PASCP 3336
      IF SY <> PERIOD THEN ERROR(21)                                            PASCP 3337
    UNTIL (SY = PERIOD) OR EOF(INPUT);                                          P      554
    IF LIST THEN WRITELN(OUTPUT);                                               KEN     17
    IF ERRINX > 0 THEN                                                          KEN     18
      BEGIN LIST := FALSE; ENDOFLINE END                                        KEN     19
  END (*PROGRAMME*) ;                                                           PASCP 3339
                                                                                PASCP 3340
                                                                                PASCP 3341
  PROCEDURE STDNAMES;                                                           PASCP 3342
  BEGIN                                                                         PASCP 3343
    NA[ 1] := #FALSE   #; NA[ 2] := #TRUE    #; NA[ 3] := #INPUT   #;           PASCP 3344
    NA[ 4] := #OUTPUT  #; NA[ 5] := #GET     #; NA[ 6] := #PUT     #;           PASCP 3345
    NA[ 7] := #RESET   #; NA[ 8] := #REWRITE #; NA[ 9] := #READ    #;           PASCP 3346
    NA[10] := #WRITE   #; NA[11] := #PACK    #; NA[12] := #UNPACK  #;           PASCP 3347
    NA[13] := #NEW     #; NA[14] := #RELEASE #; NA[15] := #READLN  #;           PASCP 3348
    NA[16] := #WRITELN #;                                                       PASCP 3349
    NA[17] := #ABS     #; NA[18] := #SQR     #; NA[19] := #TRUNC   #;           PASCP 3350
    NA[20] := #ODD     #; NA[21] := #ORD     #; NA[22] := #CHR     #;           PASCP 3351
    NA[23] := #PRED    #; NA[24] := #SUCC    #; NA[25] := #EOF     #;           PASCP 3352
    NA[26] := #EOLN    #;                                                       PASCP 3353
    NA[27] := #SIN     #; NA[28] := #COS     #; NA[29] := #EXP     #;           PASCP 3354
    NA[30] := #SQRT    #; NA[31] := #LN      #; NA[32] := #ARCTAN  #;           PASCP 3355
    NA[33] := #PRD     #; NA[34] := #PRR     #; NA[35] := #MARK    #;           PASCP 3356
  END (*STDNAMES*) ;                                                            PASCP 3357
                                                                                PASCP 3358
  PROCEDURE ENTERSTDTYPES;                                                      PASCP 3359
    VAR SP: STP;                                                                PASCP 3360
  BEGIN                                                 (*TYPE UNDERLIEING:*)   PASCP 3361
                                                         (*******************)  PASCP 3362
                                                                                PASCP 3363
    NEW(INTPTR,SCALAR,STANDARD);                              (*INTEGER*)       PASCP 3364
    WITH INTPTR' DO                                                             PASCP 3365
      BEGIN SIZE := INTSIZE; FORM := SCALAR; SCALKIND := STANDARD END;          PASCP 3366
    NEW(REALPTR,SCALAR,STANDARD);                             (*REAL*)          PASCP 3367
    WITH REALPTR' DO                                                            PASCP 3368
      BEGIN SIZE := REALSIZE; FORM := SCALAR; SCALKIND := STANDARD END;         PASCP 3369
    NEW(CHARPTR,SCALAR,STANDARD);                             (*CHAR*)          PASCP 3370
    WITH CHARPTR' DO                                                            PASCP 3371
      BEGIN SIZE := CHARSIZE; FORM := SCALAR; SCALKIND := STANDARD END;         PASCP 3372
    NEW(BOOLPTR,SCALAR,DECLARED);                             (*BOOLEAN*)       PASCP 3373
    WITH BOOLPTR' DO                                                            PASCP 3374
      BEGIN SIZE := BOOLSIZE; FORM := SCALAR; SCALKIND := DECLARED END;         PASCP 3375
    NEW(NILPTR,POINTER);                                      (*NIL*)           PASCP 3376
    WITH NILPTR' DO                                                             PASCP 3377
      BEGIN ELTYPE := NIL; SIZE := PTRSIZE; FORM := POINTER END;                PASCP 3378
    NEW(PARMPTR,SCALAR,STANDARD); (*FOR ALIGNMENT OF PARAMETERS*)               P      556
    WITH PARMPTR' DO                                                            P      557
      BEGIN SIZE := PARMSIZE; FORM := SCALAR; SCALKIND := STANDARD END ;        P      558
    NEW(TEXTPTR,FILES);                                       (*TEXT*)          PASCP 3379
    WITH TEXTPTR' DO                                                            PASCP 3380
      BEGIN FILTYPE := CHARPTR; SIZE := CHARSIZE; FORM := FILES END             PASCP 3381
  END (*ENTERSTDTYPES*) ;                                                       PASCP 3382
                                                                                PASCP 3383
  PROCEDURE ENTSTDNAMES;                                                        PASCP 3384
    VAR CP,CP1: CTP; I: INTEGER;                                                PASCP 3385
  BEGIN                                                       (*NAME:*)         PASCP 3386
                                                              (*******)         PASCP 3387
                                                                                PASCP 3388
    NEW(CP,TYPES);                                            (*INTEGER*)       PASCP 3389
    WITH CP' DO                                                                 PASCP 3390
      BEGIN NAME := #INTEGER #; IDTYPE := INTPTR; KLASS := TYPES END;           PASCP 3391
    ENTERID(CP);                                                                PASCP 3392
    NEW(CP,TYPES);                                            (*REAL*)          PASCP 3393
    WITH CP' DO                                                                 PASCP 3394
      BEGIN NAME := #REAL    #; IDTYPE := REALPTR; KLASS := TYPES END;          PASCP 3395
    ENTERID(CP);                                                                PASCP 3396
    NEW(CP,TYPES);                                            (*CHAR*)          PASCP 3397
    WITH CP' DO                                                                 PASCP 3398
      BEGIN NAME := #CHAR    #; IDTYPE := CHARPTR; KLASS := TYPES END;          PASCP 3399
    ENTERID(CP);                                                                PASCP 3400
    NEW(CP,TYPES);                                            (*BOOLEAN*)       PASCP 3401
    WITH CP' DO                                                                 PASCP 3402
      BEGIN NAME := #BOOLEAN #; IDTYPE := BOOLPTR; KLASS := TYPES END;          PASCP 3403
    ENTERID(CP);                                                                PASCP 3404
    CP1 := NIL;                                                                 PASCP 3405
    FOR I := 1 TO 2 DO                                                          PASCP 3406
      BEGIN NEW(CP,KONST);                                    (*FALSE,TRUE*)    PASCP 3407
        WITH CP' DO                                                             PASCP 3408
          BEGIN NAME := NA[I]; IDTYPE := BOOLPTR;                               PASCP 3409
            NEXT := CP1; VALUES.IVAL := I - 1; KLASS := KONST                   PASCP 3410
          END;                                                                  PASCP 3411
        ENTERID(CP); CP1 := CP                                                  PASCP 3412
      END;                                                                      PASCP 3413
    BOOLPTR'.FCONST := CP;                                                      PASCP 3414
    NEW(CP,KONST);                                             (*NIL*)          PASCP 3415
    WITH CP' DO                                                                 PASCP 3416
      BEGIN NAME := #NIL     #; IDTYPE := NILPTR;                               PASCP 3417
        NEXT := NIL; VALUES.IVAL := 0; KLASS := KONST                           PASCP 3418
      END;                                                                      PASCP 3419
    ENTERID(CP);                                                                PASCP 3420
    FOR I := 3 TO 4 DO                                                          PASCP 3421
      BEGIN NEW(CP,VARS);                                     (*INPUT,OUTPUT*)  PASCP 3422
        WITH CP' DO                                                             PASCP 3423
          BEGIN NAME := NA[I]; IDTYPE := TEXTPTR; KLASS := VARS;                PASCP 3424
            VKIND := ACTUAL; NEXT := NIL; VLEV := 1;                            PASCP 3425
            VADDR := LCAFTERMARKSTACK+(I-3)*CHARMAX;                            P      559
          END;                                                                  PASCP 3427
        ENTERID(CP)                                                             PASCP 3428
      END;                                                                      PASCP 3429
FOR I:=33 TO 34 DO                                                              PASCP 3430
      BEGIN NEW(CP,VARS);                                     (*PRD,PRR FILES*) PASCP 3431
         WITH CP' DO                                                            PASCP 3432
           BEGIN NAME := NA[I]; IDTYPE := TEXTPTR; KLASS := VARS;               PASCP 3433
              VKIND := ACTUAL; NEXT := NIL; VLEV := 1;                          PASCP 3434
              VADDR := LCAFTERMARKSTACK+(I-31)*CHARMAX;                         P      560
           END;                                                                 PASCP 3436
         ENTERID(CP)                                                            PASCP 3437
      END;                                                                      PASCP 3438
    FOR I := 5 TO 16 DO                                                         PASCP 3439
      BEGIN NEW(CP,PROC,STANDARD);                         (*GET,PUT,RESET*)    PASCP 3440
        WITH CP' DO                                           (*REWRITE,READ*)  PASCP 3441
          BEGIN NAME := NA[I]; IDTYPE := NIL;                 (*WRITE,PACK*)    PASCP 3442
            NEXT := NIL; KEY := I - 4;                        (*UNPACK,PACK*)   PASCP 3443
            KLASS := PROC; PFDECKIND := STANDARD                                PASCP 3444
          END;                                                                  PASCP 3445
        ENTERID(CP)                                                             PASCP 3446
      END;                                                                      PASCP 3447
    NEW(CP,PROC,STANDARD);                                                      PASCP 3448
    WITH CP' DO                                                                 PASCP 3449
        BEGIN NAME:=NA[35]; IDTYPE:=NIL;                                        PASCP 3450
              NEXT:= NIL; KEY:=13;                                              PASCP 3451
              KLASS:=PROC; PFDECKIND:= STANDARD                                 PASCP 3452
        END; ENTERID(CP);                                                       PASCP 3453
    FOR I := 17 TO 26 DO                                                        PASCP 3454
      BEGIN NEW(CP,FUNC,STANDARD);                         (*ABS,SQR,TRUNC*)    PASCP 3455
        WITH CP' DO                                           (*ODD,ORD,CHR*)   PASCP 3456
          BEGIN NAME := NA[I]; IDTYPE := NIL;              (*PRED,SUCC,EOF*)    PASCP 3457
            NEXT := NIL; KEY := I - 16;                                         PASCP 3458
            KLASS := FUNC; PFDECKIND := STANDARD                                PASCP 3459
          END;                                                                  PASCP 3460
        ENTERID(CP)                                                             PASCP 3461
      END;                                                                      PASCP 3462
    NEW(CP,VARS);                      (*PARAMETER OF PREDECLARED FUNCTIONS*)   PASCP 3463
    WITH CP' DO                                                                 PASCP 3464
      BEGIN NAME := #        #; IDTYPE := REALPTR; KLASS := VARS;               PASCP 3465
        VKIND := ACTUAL; NEXT := NIL; VLEV := 1; VADDR := 0                     PASCP 3466
      END;                                                                      PASCP 3467
    FOR I := 27 TO 32 DO                                                        PASCP 3468
      BEGIN NEW(CP1,FUNC,DECLARED,ACTUAL);                    (*SIN,COS,EXP*)   PASCP 3469
        WITH CP1' DO                                       (*SQRT,LN,ARCTAN*)   PASCP 3470
          BEGIN NAME := NA[I]; IDTYPE := REALPTR; NEXT := CP;                   PASCP 3471
            FORWDECL := FALSE; EXTERN := TRUE; PFLEV := 0; PFNAME := I - 12;    PASCP 3472
            KLASS := FUNC; PFDECKIND := DECLARED; PFKIND := ACTUAL              PASCP 3473
          END;                                                                  PASCP 3474
        ENTERID(CP1)                                                            PASCP 3475
      END                                                                       PASCP 3476
  END (*ENTSTDNAMES*) ;                                                         PASCP 3477
                                                                                PASCP 3478
  PROCEDURE ENTERUNDECL;                                                        PASCP 3479
  BEGIN                                                                         PASCP 3480
    NEW(UTYPPTR,TYPES);                                                         PASCP 3481
    WITH UTYPPTR' DO                                                            PASCP 3482
      BEGIN NAME := #        #; IDTYPE := NIL; KLASS := TYPES END;              PASCP 3483
    NEW(UCSTPTR,KONST);                                                         PASCP 3484
    WITH UCSTPTR' DO                                                            PASCP 3485
      BEGIN NAME := #        #; IDTYPE := NIL; NEXT := NIL;                     PASCP 3486
        VALUES.IVAL := 0; KLASS := KONST                                        PASCP 3487
      END;                                                                      PASCP 3488
    NEW(UVARPTR,VARS);                                                          PASCP 3489
    WITH UVARPTR' DO                                                            PASCP 3490
      BEGIN NAME := #        #; IDTYPE := NIL; VKIND := ACTUAL;                 PASCP 3491
        NEXT := NIL; VLEV := 0; VADDR := 0; KLASS := VARS                       PASCP 3492
      END;                                                                      PASCP 3493
    NEW(UFLDPTR,FIELD);                                                         PASCP 3494
    WITH UFLDPTR' DO                                                            PASCP 3495
      BEGIN NAME := #        #; IDTYPE := NIL; NEXT := NIL; FLDADDR := 0;       PASCP 3496
        KLASS := FIELD                                                          PASCP 3497
      END;                                                                      PASCP 3498
    NEW(UPRCPTR,PROC,DECLARED,ACTUAL);                                          PASCP 3499
    WITH UPRCPTR' DO                                                            PASCP 3500
      BEGIN NAME := #        #; IDTYPE := NIL; FORWDECL := FALSE;               PASCP 3501
        NEXT := NIL; EXTERN := FALSE; PFLEV := 0; GENLABEL(PFNAME);             PASCP 3502
        KLASS := PROC; PFDECKIND := DECLARED; PFKIND := ACTUAL                  PASCP 3503
      END;                                                                      PASCP 3504
    NEW(UFCTPTR,FUNC,DECLARED,ACTUAL);                                          PASCP 3505
    WITH UFCTPTR' DO                                                            PASCP 3506
      BEGIN NAME := #        #; IDTYPE := NIL; NEXT := NIL;                     PASCP 3507
        FORWDECL := FALSE; EXTERN := FALSE; PFLEV := 0; GENLABEL(PFNAME);       PASCP 3508
        KLASS := FUNC; PFDECKIND := DECLARED; PFKIND := ACTUAL                  PASCP 3509
      END                                                                       PASCP 3510
  END (*ENTERUNDECL*) ;                                                         PASCP 3511
                                                                                PASCP 3512
  PROCEDURE INITSCALARS;                                                        PASCP 3513
  BEGIN FWPTR := NIL;                                                           PASCP 3514
    PRTABLES := FALSE; LIST := TRUE; PRCODE := FALSE;                           PASCP 3515
    DEBUG := TRUE;                                                              P      561
    DP := TRUE; PRTERR := TRUE; ERRINX := 0;                                    PASCP 3516
    INTLABEL := 0; KK := 8; FEXTFILEP := NIL;                                   PASCP 3517
    LC := LCAFTERMARKSTACK+FILEBUFFER*CHARMAX;                                  P      562
    (* NOTE IN THE ABOVE RESERVATION OF BUFFER STORE FOR 2 TEXT FILES *)        PASCP 3519
    IC := 3; EOL := TRUE; LINECOUNT := 0;                                       PASCP 3520
    CH := # #; CHCNT := 0;                                                      PASCP 3521
    GLOBTESTP := NIL;                                                           PASCP 3522
    MXINT10 := MAXINT DIV 10; DIGMAX := STRGLGTH - 1;                           PASCP 3523
  END (*INITSCALARS*) ;                                                         PASCP 3525
                                                                                PASCP 3526
  PROCEDURE INITSETS;                                                           PASCP 3527
  BEGIN                                                                         PASCP 3528
    CONSTBEGSYS := [ADDOP,INTCONST,REALCONST,STRINGCONST,IDENT];                PASCP 3529
    SIMPTYPEBEGSYS := [LPARENT] + CONSTBEGSYS;                                  PASCP 3530
    TYPEBEGSYS:=[ARROW,PACKEDSY,ARRAYSY,RECORDSY,SETSY,FILESY]+SIMPTYPEBEGSYS;  PASCP 3531
    TYPEDELS := [ARRAYSY,RECORDSY,SETSY,FILESY];                                PASCP 3532
    BLOCKBEGSYS := [LABELSY,CONSTSY,TYPESY,VARSY,PROCSY,FUNCSY,                 PASCP 3533
                    BEGINSY];                                                   PASCP 3534
    SELECTSYS := [ARROW,PERIOD,LBRACK];                                         PASCP 3535
    FACBEGSYS := [INTCONST,REALCONST,STRINGCONST,IDENT,LPARENT,LBRACK,NOTSY];   PASCP 3536
    STATBEGSYS := [BEGINSY,GOTOSY,IFSY,WHILESY,REPEATSY,FORSY,WITHSY,           PASCP 3537
                   CASESY];                                                     PASCP 3538
  END (*INITSETS*) ;                                                            PASCP 3539
                                                                                PASCP 3540
  PROCEDURE INITTABLES;                                                         PASCP 3541
    PROCEDURE RESWORDS;                                                         PASCP 3542
    BEGIN                                                                       PASCP 3543
      RW[ 1] := #IF      #; RW[ 2] := #DO      #; RW[ 3] := #OF      #;         PASCP 3544
      RW[ 4] := #TO      #; RW[ 5] := #IN      #; RW[ 6] := #OR      #;         PASCP 3545
      RW[ 7] := #END     #; RW[ 8] := #FOR     #; RW[ 9] := #VAR     #;         PASCP 3546
      RW[10] := #DIV     #; RW[11] := #MOD     #; RW[12] := #SET     #;         PASCP 3547
      RW[13] := #AND     #; RW[14] := #NOT     #; RW[15] := #THEN    #;         PASCP 3548
      RW[16] := #ELSE    #; RW[17] := #WITH    #; RW[18] := #GOTO    #;         PASCP 3549
      RW[19] := #CASE    #; RW[20] := #TYPE    #;                               PASCP 3550
      RW[21] := #FILE    #; RW[22] := #BEGIN   #;                               PASCP 3551
      RW[23] := #UNTIL   #; RW[24] := #WHILE   #; RW[25] := #ARRAY   #;         PASCP 3552
      RW[26] := #CONST   #; RW[27] := #LABEL   #;                               PASCP 3553
      RW[28] := #REPEAT  #; RW[29] := #RECORD  #; RW[30] := #DOWNTO  #;         PASCP 3554
      RW[31] := #PACKED  #; RW[32] := #FORWARD #; RW[33] := #PROGRAM #;         PASCP 3555
      RW[34] := #FUNCTION#; RW[35] := #PROCEDUR#;                               PASCP 3556
      FRW[1] :=  1; FRW[2] :=  1; FRW[3] :=  7; FRW[4] := 15; FRW[5] := 22;     PASCP 3557
      FRW[6] := 28; FRW[7] := 32; FRW[8] := 34; FRW[9] := 36;                   PASCP 3558
    END (*RESWORDS*) ;                                                          PASCP 3559
                                                                                PASCP 3560
    PROCEDURE SYMBOLS;                                                          PASCP 3561
    BEGIN                                                                       PASCP 3562
      RSY[1] := IFSY; RSY[2] := DOSY; RSY[3] := OFSY; RSY[4] := TOSY;           PASCP 3563
      RSY[5] := RELOP; RSY[6] := ADDOP; RSY[7] := ENDSY; RSY[8] := FORSY;       PASCP 3564
      RSY[9] := VARSY; RSY[10] := MULOP; RSY[11] := MULOP; RSY[12] := SETSY;    PASCP 3565
      RSY[13] := MULOP; RSY[14] := NOTSY; RSY[15] := THENSY;                    PASCP 3566
      RSY[16] := ELSESY; RSY[17] := WITHSY; RSY[18] := GOTOSY;                  PASCP 3567
      RSY[19] := CASESY; RSY[20] := TYPESY;                                     PASCP 3568
      RSY[21] := FILESY; RSY[22] := BEGINSY;                                    PASCP 3569
      RSY[23] := UNTILSY; RSY[24] := WHILESY; RSY[25] := ARRAYSY;               PASCP 3570
      RSY[26] := CONSTSY; RSY[27] := LABELSY;                                   PASCP 3571
      RSY[28] := REPEATSY; RSY[29] := RECORDSY; RSY[30] := DOWNTOSY;            PASCP 3572
      RSY[31] := PACKEDSY; RSY[32] := FORWARDSY; RSY[33] := PROGSY;             PASCP 3573
      RSY[34] := FUNCSY; RSY[35] := PROCSY;                                     PASCP 3574
      SSY[#+#] := ADDOP; SSY[#-#] := ADDOP; SSY[#*#] := MULOP;                  PASCP 3575
      SSY[#/#] := MULOP; SSY[#(#] := LPARENT; SSY[#)#] := RPARENT;              PASCP 3576
      SSY[#$#] := OTHERSY; SSY[#=#] := RELOP; SSY[# #] := OTHERSY;              PASCP 3577
      SSY[#,#] := COMMA; SSY[#.#] := PERIOD; SSY[####] := OTHERSY;              PASCP 3578
      SSY[#[#] := LBRACK; SSY[#]#] := RBRACK; SSY[#:#] := COLON;                PASCP 3579
      SSY[#'#] := ARROW;                                                        PASCP 3580
      SSY[#<#] := RELOP; SSY[#>#] := RELOP;                                     PASCP 3581
      SSY[#;#] := SEMICOLON;                                                    PASCP 3582
    END (*SYMBOLS*) ;                                                           PASCP 3583
                                                                                PASCP 3584
    PROCEDURE RATORS;                                                           PASCP 3585
      VAR I: INTEGER; CH: CHAR;                                                 PASCP 3586
    BEGIN                                                                       PASCP 3587
      FOR I := 1 TO 35 (*NR OF RES WORDS*) DO ROP[I] := NOOP;                   PASCP 3588
      ROP[5] := INOP; ROP[10] := IDIV; ROP[11] := IMOD;                         PASCP 3589
      ROP[6] := OROP; ROP[13] := ANDOP;                                         PASCP 3590
    FOR I := ORDMINCHAR TO ORDMAXCHAR DO SOP[CHR(I)] := NOOP;                   BOOT     4
      SOP[#+#] := PLUS; SOP[#-#] := MINUS; SOP[#*#] := MUL; SOP[#/#] := RDIV;   PASCP 3592
      SOP[#=#] := EQOP;                                                         PASCP 3593
      SOP[#<#] := LTOP; SOP[#>#] := GTOP;                                       PASCP 3594
    END (*RATORS*) ;                                                            PASCP 3595
                                                                                PASCP 3596
    PROCEDURE PROCMNEMONICS;                                                    PASCP 3597
    BEGIN                                                                       PASCP 3598
      SNA[ 1] :=# GET#; SNA[ 2] :=# PUT#; SNA[ 3] :=# RDI#; SNA[ 4] :=# RDR#;   PASCP 3599
      SNA[ 5] :=# RDC#; SNA[ 6] :=# WRI#; SNA[ 7] :=# WRO#; SNA[ 8] :=# WRR#;   PASCP 3600
      SNA[ 9] :=# WRC#; SNA[10] :=# WRS#; SNA[11] :=# PAK#; SNA[12] :=# NEW#;   PASCP 3601
      SNA[13] :=# RST#; SNA[14] :=# ELN#; SNA[15] :=# SIN#; SNA[16] :=# COS#;   PASCP 3602
      SNA[17] :=# EXP#; SNA[18] :=# SQT#; SNA[19] :=# LOG#; SNA[20] :=# ATN#;   PASCP 3603
      SNA[21] :=# RLN#; SNA[22] :=# WLN#; SNA[23] :=# SAV#;                     PASCP 3604
    END (*PROCMNEMONICS*) ;                                                     PASCP 3605
                                                                                PASCP 3606
    PROCEDURE INSTRMNEMONICS;                                                   PASCP 3607
    BEGIN                                                                       PASCP 3608
      MN[0] :=# ABI#; MN[1] :=# ABR#; MN[2] :=# ADI#; MN[3] :=# ADR#;           PASCP 3609
      MN[4] :=# AND#; MN[5] :=# DIF#; MN[6] :=# DVI#; MN[7] :=# DVR#;           PASCP 3610
      MN[8] :=# EOF#; MN[9] :=# FLO#; MN[10] :=# FLT#; MN[11] :=# INN#;         PASCP 3611
      MN[12] :=# INT#; MN[13] :=# IOR#; MN[14] :=# MOD#; MN[15] :=# MPI#;       PASCP 3612
      MN[16] :=# MPR#; MN[17] :=# NGI#; MN[18] :=# NGR#; MN[19] :=# NOT#;       PASCP 3613
      MN[20] :=# ODD#; MN[21] :=# SBI#; MN[22] :=# SBR#; MN[23] :=# SGS#;       PASCP 3614
      MN[24] :=# SQI#; MN[25] :=# SQR#; MN[26] :=# STO#; MN[27] :=# TRC#;       PASCP 3615
      MN[28] :=# UNI#; MN[29] :=# STP#; MN[30] :=# CSP#; MN[31] :=# DEC#;       PASCP 3616
      MN[32] :=# ENT#; MN[33] :=# FJP#; MN[34] :=# INC#; MN[35] :=# IND#;       PASCP 3617
      MN[36] :=# IXA#; MN[37] :=# LAO#; MN[38] :=# LCA#; MN[39] :=# LDO#;       PASCP 3618
      MN[40] :=# MOV#; MN[41] :=# MST#; MN[42] :=# RET#; MN[43] :=# SRO#;       PASCP 3619
      MN[44] :=# XJP#; MN[45] :=# CHK#; MN[46] :=# CUP#; MN[47] :=# EQU#;       PASCP 3620
      MN[48] :=# GEQ#; MN[49] :=# GRT#; MN[50] :=# LDA#; MN[51] :=# LDC#;       PASCP 3621
      MN[52] :=# LEQ#; MN[53] :=# LES#; MN[54] :=# LOD#; MN[55] :=# NEQ#;       PASCP 3622
      MN[56] :=# STR#; MN[57] :=# UJP#; MN[58] :=# ORD#; MN[59] :=# CHR#;       P      563
   MN[60] :=# UJC#;                                                             P      564
    END (*INSTRMNEMONICS*) ;                                                    PASCP 3624
                                                                                P      565
                                                                                P      566
     PROCEDURE CHARTYPES;                                                       P      567
     VAR I : INTEGER;                                                           P      568
     BEGIN                                                                      P      569
       FOR I := ORDMINCHAR TO ORDMAXCHAR DO CHARTP[CHR(I)] := ILLEGAL;          P      570
       CHARTP[#A#] := LETTER ;                                                  P      571
       CHARTP[#B#] := LETTER ; CHARTP[#C#] := LETTER ;                          P      572
       CHARTP[#D#] := LETTER ; CHARTP[#E#] := LETTER ;                          P      573
       CHARTP[#F#] := LETTER ; CHARTP[#G#] := LETTER ;                          P      574
       CHARTP[#H#] := LETTER ; CHARTP[#I#] := LETTER ;                          P      575
       CHARTP[#J#] := LETTER ; CHARTP[#K#] := LETTER ;                          P      576
       CHARTP[#L#] := LETTER ; CHARTP[#M#] := LETTER ;                          P      577
       CHARTP[#N#] := LETTER ; CHARTP[#O#] := LETTER ;                          P      578
       CHARTP[#P#] := LETTER ; CHARTP[#Q#] := LETTER ;                          P      579
       CHARTP[#R#] := LETTER ; CHARTP[#S#] := LETTER ;                          P      580
       CHARTP[#T#] := LETTER ; CHARTP[#U#] := LETTER ;                          P      581
       CHARTP[#V#] := LETTER ; CHARTP[#W#] := LETTER ;                          P      582
       CHARTP[#X#] := LETTER ; CHARTP[#Y#] := LETTER ;                          P      583
       CHARTP[#Z#] := LETTER ; CHARTP[#0#] := NUMBER ;                          P      584
       CHARTP[#1#] := NUMBER ; CHARTP[#2#] := NUMBER ;                          P      585
       CHARTP[#3#] := NUMBER ; CHARTP[#4#] := NUMBER ;                          P      586
       CHARTP[#5#] := NUMBER ; CHARTP[#6#] := NUMBER ;                          P      587
       CHARTP[#7#] := NUMBER ; CHARTP[#8#] := NUMBER ;                          P      588
       CHARTP[#9#] := NUMBER ; CHARTP[#+#] := SPECIAL;                          P      589
       CHARTP[#-#] := SPECIAL; CHARTP[#*#] := SPECIAL;                          P      590
       CHARTP[#/#] := SPECIAL; CHARTP[#(#] := SPECIAL;                          P      591
       CHARTP[#)#] := SPECIAL; CHARTP[#$#] := SPECIAL;                          P      592
       CHARTP[#=#] := SPECIAL; CHARTP[# #] := SPECIAL;                          P      593
       CHARTP[#,#] := SPECIAL; CHARTP[#.#] := SPECIAL;                          P      594
       CHARTP[####] := SPECIAL; CHARTP[#[#] := SPECIAL;                         P      595
       CHARTP[#]#] := SPECIAL; CHARTP[#:#] := SPECIAL;                          P      596
       CHARTP[#'#] := SPECIAL; CHARTP[#;#] := SPECIAL;                          P      597
       CHARTP[#<#] := SPECIAL; CHARTP[#>#] := SPECIAL;                          P      598
       ORDINT[#0#] := 0; ORDINT[#1#] := 1; ORDINT[#2#] := 2;                    CH       4
       ORDINT[#3#] := 3;                                                        CH       5
       ORDINT[#4#] := 4; ORDINT[#5#] := 5; ORDINT[#6#] := 6;                    CH       6
      ORDINT[#7#] := 7; ORDINT[#8#] := 8; ORDINT[#9#] := 9;                     CH       7
     END;                                                                       P      599
                                                                                PASCP 3625
    PROCEDURE INITDX;                                                           P      600
    BEGIN                                                                       P      601
      CDX[ 0] :=  0; CDX[ 1] :=  0; CDX[ 2] := -1; CDX[ 3] := -1;               P      602
      CDX[ 4] := -1; CDX[ 5] := -1; CDX[ 6] := -1; CDX[ 7] := -1;               P      603
      CDX[ 8] :=  0; CDX[ 9] :=  0; CDX[10] :=  0; CDX[11] := -1;               P      604
      CDX[12] := -1; CDX[13] := -1; CDX[14] := -1; CDX[15] := -1;               P      605
      CDX[16] := -1; CDX[17] :=  0; CDX[18] :=  0; CDX[19] :=  0;               P      606
      CDX[20] :=  0; CDX[21] := -1; CDX[22] := -1; CDX[23] :=  0;               P      607
      CDX[24] :=  0; CDX[25] :=  0; CDX[26] := -2; CDX[27] :=  0;               P      608
      CDX[28] := -1; CDX[29] :=  0; CDX[30] :=  0; CDX[31] :=  0;               P      609
      CDX[32] :=  0; CDX[33] := -1; CDX[34] :=  0; CDX[35] :=  0;               P      610
      CDX[36] := -1; CDX[37] := +1; CDX[38] := +1; CDX[39] := +1;               P      611
      CDX[40] := -2; CDX[41] :=  0; CDX[42] :=  0; CDX[43] := -1;               P      612
      CDX[44] := -1; CDX[45] :=  0; CDX[46] :=  0; CDX[47] := -1;               P      613
      CDX[48] := -1; CDX[49] := -1; CDX[50] := +1; CDX[51] := +1;               P      614
      CDX[52] := -1; CDX[53] := -1; CDX[54] := +1; CDX[55] := -1;               P      615
      CDX[56] := -1; CDX[57] :=  0; CDX[58] :=  0; CDX[59] :=  0;               P      616
      CDX[60] := 0;                                                             P      617
      PDX[ 1] := -1; PDX[ 2] := -1; PDX[ 3] := -2; PDX[ 4] := -2;               P      618
      PDX[ 5] := -2; PDX[ 6] := -3; PDX[ 7] := -3; PDX[ 8] := -3;               P      619
      PDX[ 9] := -3; PDX[10] := -4; PDX[11] :=  0; PDX[12] := -2;               P      620
      PDX[13] := -1; PDX[14] :=  0; PDX[15] :=  0; PDX[16] :=  0;               P      621
      PDX[17] :=  0; PDX[18] :=  0; PDX[19] :=  0; PDX[20] :=  0;               P      622
      PDX[21] := -1; PDX[22] := -1; PDX[23] := -1;                              P      623
    END;                                                                        P      624
                                                                                P      625
  BEGIN (*INITTABLES*)                                                          PASCP 3626
    RESWORDS; SYMBOLS; RATORS;                                                  PASCP 3627
    INSTRMNEMONICS; PROCMNEMONICS;                                              PASCP 3628
    CHARTYPES; INITDX;                                                          P      626
  END (*INITTABLES*) ;                                                          PASCP 3629
                                                                                PASCP 3630
BEGIN                                                                           PASCP 3631
  (*INITIALIZE*)                                                                PASCP 3632
  (************)                                                                PASCP 3633
  INITSCALARS; INITSETS; INITTABLES;                                            PASCP 3634
                                                                                PASCP 3635
                                                                                PASCP 3636
  (*ENTER STANDARD NAMES AND STANDARD TYPES:*)                                  PASCP 3637
  (******************************************)                                  PASCP 3638
                                                                                PASCP 3639
  LEVEL := 0; TOP := 0;                                                         PASCP 3640
  WITH DISPLAY[0] DO                                                            PASCP 3641
    BEGIN FNAME := NIL; FLABEL := NIL; OCCUR := BLCK END;                       PASCP 3642
  ENTERSTDTYPES;   STDNAMES; ENTSTDNAMES;   ENTERUNDECL;                        PASCP 3643
  TOP := 1; LEVEL := 1;                                                         PASCP 3644
  WITH DISPLAY[1] DO                                                            PASCP 3645
    BEGIN FNAME := NIL; FLABEL := NIL; OCCUR := BLCK END;                       PASCP 3646
                                                                                PASCP 3647
                                                                                PASCP 3648
  (*COMPILE:*)                                                                  PASCP 3649
  (**********)                                                                  PASCP 3650
                                                                                PASCP 3651
  INSYMBOL;                                                                     PASCP 3652
  PROGRAMME(BLOCKBEGSYS+STATBEGSYS-[CASESY]);                                   PASCP 3653
                                                                                PASCP 3654
END.                                                                            PASCP 3655
                                                                                PASCP 3656
