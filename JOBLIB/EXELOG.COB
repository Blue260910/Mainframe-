//FOUR021X JOB 'EXEC COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010005
//EXECUTAR EXEC PGM=FR21EX02                                            00020022
//STEPLIB  DD DSN=PR.FOUR021.OBJ,DISP=SHR                               00030002
//ARQUIIN1 DD DSN=PR.FOUR021.ARQUIIN1,DISP=SHR                          00031023
//ARQUIIN2 DD DSN=PR.FOUR021.ARQUIIN2,DISP=SHR                          00032023
//LOGERROS DD DSN=PR.FOUR021.LOGERROS,DISP=OLD                          00040016
