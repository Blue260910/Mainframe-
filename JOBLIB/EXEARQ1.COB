//FOUR021X JOB 'EXEC COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,02) 00010004
//EXECUTAR EXEC PGM=ARQ2102                                             00020011
//STEPLIB  DD DSN=PR.FOUR021.OBJ,DISP=SHR                               00030003
//CLIENTES DD DSN=PR.FOUR021.CLIENTES,DISP=SHR                          00031010
//SAIDACLI DD DSN=PR.FOUR021.SAIDACLI,DISP=SHR                          00032010
//SYSIN    DD *                                                         00040000
