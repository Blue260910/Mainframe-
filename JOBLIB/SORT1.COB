//FOUR021X JOB 'EXE COB',CLASS=C,MSGCLASS=X,NOTIFY=&SYSUID              00010001
//STEP01   EXEC PGM=SORT                                                00020000
//SORTIN   DD   DSN=GR.FOUR021.MASSASRT,DISP=SHR                        00030001
//SORTOUT  DD   SYSOUT=*                                                00040000
//SYSOUT   DD   SYSOUT=*                                                00050000
//SYSIN    DD  *                                                        00090000
  SORT FIELDS=COPY                                                      00100002
    OUTREC FIELDS=(01,04,6X,05,05,6X,34,8,6X,10,19)                     00100106
  SORT FIELDS=COPY                                                      00100205
  OUTFIL REMOVECC,                                                      00101105
  HEADER1=(1:C'AGENCIA',11:C'CONTA',22:C'SALARIO',36:C'NOME')           00101208
                                                                        00102002
                                                                        00120003
