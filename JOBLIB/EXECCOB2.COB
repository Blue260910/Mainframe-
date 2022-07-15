//FOUR021X JOB 'EXEC CO',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01)  00010006
//*****************************************************************     00020000
//* EXECUTAR PROGRAMA MD022103                                          00030026
//*****************************************************************     00031000
//*********************************123456789012345678*************      00032028
//STEP1    EXEC PGM=MD022103,PARM='000100199000200015'                  00040033
//STEPLIB  DD   DSN=GR.GERAL.LOADLIB,DISP=SHR                           00050000
//EVSA0407 DD   DSN=GR.FOUR021.EVSA0407,DISP=SHR                        00060034
//SAIVSAM  DD   SYSOUT=*                                                00061030
//SYSOUT   DD   SYSOUT=*                                                00070008
