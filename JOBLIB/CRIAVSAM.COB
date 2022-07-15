//FOUR021X JOB 'CRIAVSAM',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010002
//STEP1     EXEC PGM=IDCAMS                                             00020000
//SYSPRINT  DD   SYSOUT=*                                               00030000
//SYSIN     DD   *                                                      00040000
 DEFINE CLUSTER (NAME(GR.FOUR021.EVSA0407) -                            00050005
    IXD -                                                               00060000
    VOL(PR39X7) -                                                       00070000
    RECORDSIZE(19 19) -                                                 00080005
    SHR(2) -                                                            00090000
    TRK(2 1) -                                                          00100005
    UNIQUE -                                                            00110000
    KEYS(09 0) -                                                        00120003
    FREESPACE(04 04) ) -                                                00130000
    DATA(NAME(GR.FOUR021.EVSA0407.DATA)) -                              00140005
    INDEX(NAME(GR.FOUR021.EVSA0407.INDEX))                              00150005
                                                                        00160000
