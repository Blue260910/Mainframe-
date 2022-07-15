//FOUR021X JOB 'EXEC CO',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01)  00010000
//STEP2    EXEC PGM=SORT                                                00011000
//SORTIN   DD   DSN=GR.FOUR021.SAIVSAM,DISP=SHR                         00012000
//SORTOUT  DD   DSN=GR.FOUR021.SAIVSAM,DISP=OLD                         00013000
//SYSIN    DD *                                                         00014000
 SORT FIELDS=(01,09,CH,D)                                               00015000
                                                                        00020000
