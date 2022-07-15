//FOUR021X JOB 'EXEC COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010005
//STEP01   EXEC PGM=SORT                                                00020005
//SORTIN   DD   DSN=GR.FOUR021.SRELBENE,DISP=SHR                        00030005
//SORTOUT  DD   DSN=GR.FOUR021.SRELBENE,DISP=SHR                        00040005
//SYSOUT   DD   SYSOUT=*                                                00050005
//SYSIN    DD   *                                                       00060005
  SORT FIELDS=(01,05,CH,A)                                              00070005
                                                                        00080005
//STEP02   EXEC PGM=FR21RELB                                            00140005
//STEPLIB  DD   DSN=GR.GERAL.LOADLIB,DISP=SHR                           00220001
//SYSOUT   DD   SYSOUT=*                                                00221007
//SRELBENE DD   DSN=GR.FOUR021.SRELBENE,DISP=SHR                        00230003
//RELATBEN DD   SYSOUT=*                                                00240006
