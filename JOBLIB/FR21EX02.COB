//FOUR021X JOB 'EXEC COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010001
//STEP01   EXEC PGM=SORT                                                00011000
//SORTIN   DD   DSN=PR.FOUR021.ARQUIIN1,DISP=SHR                        00012007
//SORTOUT  DD   DSN=PR.FOUR021.ARQUIIN1,DISP=OLD                        00012107
//SYSOUT   DD   SYSOUT=*                                                00013007
//SYSIN    DD   *                                                       00014007
  SORT FIELDS=(01,08,CH,A)                                              00014108
//STEP02   EXEC PGM=SORT                                                00014207
//SORTIN   DD   DSN=PR.FOUR021.ARQUIIN2,DISP=SHR                        00014307
//SORTOUT  DD   DSN=PR.FOUR021.ARQUIIN2,DISP=OLD                        00014407
//SYSOUT   DD   SYSOUT=*                                                00014507
//SYSIN    DD   *                                                       00014607
  SORT FIELDS=(01,08,CH,A)                                              00014708
//*-------------------------------                                      00015000
//* EXECUTAR RELFUNC                                                    00016000
//*-------------------------------                                      00017000
//STEP02   EXEC PGM=FR21EX03                                            00020008
//STEPLIB  DD DSN=PR.FOUR021.OBJ,DISP=SHR                               00030001
//ARQUIIN1 DD DSN=PR.FOUR021.ARQUIIN1,DISP=SHR                          00040007
//ARQUIIN2 DD DSN=PR.FOUR021.ARQUIIN2,DISP=SHR                          00040107
//ARQUIOU1 DD DSN=PR.FOUR021.ARQUIOU1,DISP=SHR                          00040209
//ARQUIOU2 DD DSN=PR.FOUR021.ARQUIOU2,DISP=SHR                          00040309
//LOGERROS DD DSN=PR.FOUR021.LOGERROS,DISP=SHR                          00041001
