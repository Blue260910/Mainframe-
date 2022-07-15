//FOUR021X JOB 'CRIAGDG',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01)  00010001
//STEP1     EXEC PGM=IDCAMS                                             00020000
//SYSPRINT  DD   SYSOUT=*                                               00030000
//SYSIN     DD   *                                                      00040000
  DEFINE GENERATIONDATAGROUP   -                                        00050000
   (NAME(GR.FOUR021.EXCSAIDA)  -                                        00060001
   NOEMPTY   -                                                          00061000
   NOSCRATCH -                                                          00062000
   LIMIT(03) )                                                          00070000
