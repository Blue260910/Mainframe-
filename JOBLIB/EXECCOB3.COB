//FOUR021X JOB 'EXEC CO',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01)  00010006
//*****************************************************************     00020000
//* EXECUTAR PROGRAMA MD022104                                          00030036
//*****************************************************************     00031000
//*********************************123456789012345678*************      00032028
//EXEC     EXEC PGM=MD022104                                            00040037
//STEPLIB  DD   DSN=GR.GERAL.LOADLIB,DISP=SHR                           00050000
//EVSA0407 DD   DSN=GR.FOUR021.EVSA0407,DISP=SHR                        00060034
//MOV1207  DD   DSN=GR.FOUR021.MOV1207,DISP=SHR                         00060135
//EXCSAIDA DD   DSN=GR.FOUR021.EXCSAIDA(+1),DISP=(NEW,CATLG,DELETE),    00060237
//         LIKE=GR.FOUR021.MOV1207                                      00060337
//SYSOUT   DD   SYSOUT=*                                                00070008
