//FOUR021X JOB 'EXE COB',CLASS=C,MSGCLASS=X,NOTIFY=&SYSUID,TIME=(0,01)  00010005
//STEP1     EXEC PGM=IDCAMS                                             00020000
//SYSPRINT  DD   SYSOUT=*                                               00030000
//SYSIN     DD   *                                                      00040000
 REPRO INDATASET(GR.FOUR021.DADOS) -                                    00050007
       OUTDATASET(GR.FOUR021.EVSA0407)                                  00060006
