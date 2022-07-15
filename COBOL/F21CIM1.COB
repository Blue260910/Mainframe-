F21CIM1 DFHMSD TYPE=MAP,MODE=INOUT,LANG=COBOL,CTRL=FREEKB,TIOAPFX=YES, *
               MAPATTS=(COLOR)
MAPA01  DFHMDI SIZE=(24,80)
*--------------------------------------------------------------
        DFHMDF POS=(01,30),LENGTH=23,INITIAL='-----------------------',*
               COLOR=TURQUOISE
        DFHMDF POS=(02,30),LENGTH=23,INITIAL='|CONSULTA FUNCIONARIOS|',*
               COLOR=YELLOW,ATTRB=PROT
        DFHMDF POS=(03,30),LENGTH=23,INITIAL='-----------------------',*
               COLOR=TURQUOISE
*--------------------------------------------------------------
        DFHMDF POS=(05,15),LENGTH=10,INITIAL='ID.......:',COLOR=NEUTRAL
ID      DFHMDF POS=(05,26),LENGTH=05,ATTRB=(UNPROT,IC,NUM),            *
               PICIN='9(05)',PICOUT='9(05)'
        DFHMDF POS=(05,32),LENGTH=01,ATTRB=ASKIP
*--------------------------------------------------------------
        DFHMDF POS=(07,15),LENGTH=10,INITIAL='NOME.....:',COLOR=NEUTRAL
NOME    DFHMDF POS=(07,26),LENGTH=30,ATTRB=UNPROT
        DFHMDF POS=(07,57),LENGTH=01,ATTRB=PROT
*--------------------------------------------------------------
        DFHMDF POS=(09,15),LENGTH=10,INITIAL='SETOR....:',COLOR=NEUTRAL
SETOR   DFHMDF POS=(09,26),LENGTH=04,ATTRB=UNPROT
        DFHMDF POS=(09,31),LENGTH=01,ATTRB=PROT
*--------------------------------------------------------------
        DFHMDF POS=(11,15),LENGTH=10,INITIAL='SALARIO..:',COLOR=NEUTRAL
SALARIO DFHMDF POS=(11,26),LENGTH=13,ATTRB=(UNPROT,NUM),               *
               PICIN='9(11)V99'
        DFHMDF POS=(11,40),LENGTH=01,ATTRB=PROT
*--------------------------------------------------------------
        DFHMDF POS=(13,15),LENGTH=10,INITIAL='DATA.....:',COLOR=NEUTRAL
DATADIA DFHMDF POS=(13,26),LENGTH=02,ATTRB=(UNPROT,NUM)
        DFHMDF POS=(13,29),LENGTH=01,INITIAL='/',COLOR=NEUTRAL,        *
               ATTRB=ASKIP
DATAMES DFHMDF POS=(13,31),LENGTH=02,ATTRB=(UNPROT,NUM)
        DFHMDF POS=(13,34),LENGTH=01,INITIAL='/',COLOR=NEUTRAL,        *
               ATTRB=ASKIP
DATAANO DFHMDF POS=(13,36),LENGTH=04,ATTRB=(UNPROT,NUM)
        DFHMDF POS=(13,41),LENGTH=01,ATTRB=ASKIP
*--------------------------------------------------------------
        DFHMDF POS=(15,15),LENGTH=10,INITIAL='EMAIL....:',COLOR=NEUTRAL
EMAIL   DFHMDF POS=(15,26),LENGTH=40,ATTRB=UNPROT
        DFHMDF POS=(15,67),LENGTH=01,ATTRB=PROT
*--------------------------------------------------------------
        DFHMDF POS=(21,03),LENGTH=04,INITIAL='MSG:',COLOR=YELLOW
MSG     DFHMDF POS=(21,08),LENGTH=40,ATTRB=PROT
*--------------------------------------------------------------
        DFHMDF POS=(23,07),LENGTH=70,                                  *
               INITIAL='TAB-NAVEGACAO  F5-CONSULTA  F6-INCLUSAO  F7-EXC*
               LUSAO  F3-SAIDA',                                       *
               COLOR=TURQUOISE

*--------------------------------------------------------------
        DFHMSD TYPE=FINAL
        END
