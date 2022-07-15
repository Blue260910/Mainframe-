//FOUR021X JOB 'CICS DB2',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID             00010003
//*-------------------------------------------------------------------* 00020000
//*      JOB DE COMPILACAO DE PROGRAMA COBOL COM DB2 CICS USANDO MENU * 00030000
//*      PLAN,,MEM,NAME = SEU PROGRAMA                                  00031002
//*      INSTR,PACKAGE  = SUA CHAVE                                     00031002
//*-------------------------------------------------------------------* 00040000
// JCLLIB ORDER=GR.GERAL.PROCLIB                                        00050007
//COMPCIDB EXEC DFHCIDB2,NAME=F21CIP1,INSTR=FOUR021                     00060008
//BIND.SYSTSIN  DD *                                                    00080000
DSN  SYSTEM(DB8G)
BIND PLAN(F21CIP1) MEM(F21CIP1) ACT(REP) ISOLATION(CS) -                00090004
       LIB('DSN810.DBRMLIB.DATA')                                       00100000
BIND PACKAGE(FOUR021) MEM(F21CIP1) ACT(REP) ISOLATION(CS) -             00090004
       LIB('DSN810.DBRMLIB.DATA') VALIDATE(BIND)                        00100000
END                                                                     00120000
//*                                                                     00130000
