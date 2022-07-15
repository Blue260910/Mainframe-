//FOUR021X JOB 'EXEC COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010001
//STEP01   EXEC PGM=SORT                                                00011000
//SORTIN   DD   DSN=PR.FOUR021.CLIENTES,DISP=SHR                        00012010
//SORTOUT  DD   DSN=PR.FOUR021.CLIENTES,DISP=OLD                        00012110
//SYSOUT   DD   SYSOUT=*                                                00013007
//SYSIN    DD   *                                                       00014007
  SORT FIELDS=(01,08,CH,A)                                              00014108
//STEP02   EXEC PGM=SORT                                                00014207
//SORTIN   DD   DSN=PR.FOUR021.MOV0106,DISP=SHR                         00014310
//SORTOUT  DD   DSN=PR.FOUR021.MOV0106,DISP=OLD                         00014410
//SYSOUT   DD   SYSOUT=*                                                00014507
//SYSIN    DD   *                                                       00014607
  SORT FIELDS=(01,08,CH,A;47,01,CH,A)                                   00014711
//*-------------------------------                                      00015000
//* EXECUTAR RELFUNC                                                    00016000
//*-------------------------------                                      00017000
//STEP02   EXEC PGM=FR21EX04                                            00020010
//STEPLIB  DD DSN=PR.FOUR021.OBJ,DISP=SHR                               00030001
//CLIENTES DD DSN=PR.FOUR021.CLIENTES,DISP=SHR                          00040010
//MOV0106  DD DSN=PR.FOUR021.MOV0106,DISP=SHR                           00040110
//MOV0106A DD DSN=PR.FOUR021.MOV0106A,DISP=SHR                          00040210
//MOV0106C DD DSN=PR.FOUR021.MOV0106C,DISP=SHR                          00040310
//LOGERROS DD DSN=PR.FOUR021.LOGERROS,DISP=SHR                          00041001
