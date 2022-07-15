//FOUR021X JOB 'COMP DB2',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID
//*-------------------------------------------------------------------*
//*      JOB DE COMPILACAO DE PROGRAMA COBOL COM DB2 BATCH            *
//*      NAME = SEU PROGRAMA  -   INSTR = SUA CHAVE
//*-------------------------------------------------------------------*
//         JCLLIB ORDER=GR.GERAL.PROCLIB
//COMPDB2B EXEC DFHCODB2,NAME=FR21DB01,INSTR=FOUR021
//LKED.SYSIN    DD   *
  NAME FR21DB01(R)
//*
//BIND.SYSTSIN  DD *
 DSN  SYSTEM(DB8G)
 BIND PLAN(FR21DB01) MEM(FR21DB01) ACT(REP) ISOLATION(CS) -
       LIB('DSN810.DBRMLIB.DATA')
 END
