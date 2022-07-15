      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. DESAFIO.                                             00040002
                                                                        00050000
      *=====================================================            00060000
      *  AUTOR: VICTOR                                                  00070000
      *  EMPRESA: FOURSYS                                               00080000
      *=====================================================            00090000
                                                                        00100000
      *====================================================             00101000
       ENVIRONMENT                               DIVISION.              00102000
      *====================================================             00103000
                                                                        00104000
      *====================================================             00105000
       DATA                                      DIVISION.              00106000
      *====================================================             00107000
       WORKING-STORAGE                           SECTION.               00108000
       77 WRK-NUM1 PIC 9(01).                                           00109001
       77 WRK-NUM2 PIC 9(01).                                           00109101
       77 WRK-NUM3 PIC 9(01).                                           00109201
       77 WRK-NUM4 PIC 9(01).                                           00109301
       77 WRK-RESTO-TOTAL PIC 9(04).                                    00130201
       77 WRK-CONTADOR    PIC 9(04).                                    00130301
                                                                        00130401
       77 WRK-RESTO1  PIC 9(02).                                        00130501
       77 WRK-RESTO2  PIC 9(02).                                        00130601
       77 WRK-RESTO3  PIC 9(02).                                        00130701
       77 WRK-RESTO4  PIC 9(02).                                        00130801
      *====================================================             00131000
       PROCEDURE                                 DIVISION.              00140000
      *====================================================             00150000
          ACCEPT  WRK-NUM1  FROM SYSIN.                                 00170000
          ACCEPT  WRK-NUM2  FROM SYSIN.                                 00180000
          ACCEPT  WRK-NUM3  FROM SYSIN.                                 00181000
          ACCEPT  WRK-NUM4  FROM SYSIN.                                 00181100
                                                                        00181200
          PERFORM 1000-CONTAS VARYING WRK-CONTADOR FROM 1 BY 1          00181301
                  UNTIL WRK-RESTO-TOTAL EQUAL 0.                        00181401
                                                                        00181501
          STOP RUN.                                                     00181601
      *----------------------------------------------------             00181701
       1000-CONTAS                                 SECTION.             00181801
                                                                        00181900
          DIVIDE WRK-NUM1 BY WRK-CONTADOR GIVING WRK-RESULTADO          00182001
                             REMAINDER WRK-RESTO1.                      00182101
                                                                        00182201
          DIVIDE WRK-NUM1 BY WRK-CONTADOR GIVING WRK-RESULTADO          00182301
                             REMAINDER WRK-RESTO2.                      00182401
                                                                        00182501
          DIVIDE WRK-NUM1 BY WRK-CONTADOR GIVING WRK-RESULTADO          00182601
                             REMAINDER WRK-RESTO3.                      00182701
                                                                        00182801
          DIVIDE WRK-NUM1 BY WRK-CONTADOR GIVING WRK-RESULTADO          00182901
                             REMAINDER WRK-RESTO4.                      00183001
                                                                        00183101
          COMPUTE WRK-RESTO-TOTAL =( WRK-RESTO1 + WRK-RESTO2 +          00183201
                                    WRK-RESTO3 + WRK-RESTO4 )           00183301
                                                                        00183401
          IF WRK-RESTO-TOTAL EQUAL ZERO                                 00183501
             DISPLAY 'ELES BIPAM JUNTOS EM ' WRK-CONTADOR               00183601
          END-IF.                                                       00183701
                                                                        00183801
                                                                        00183901
                                                                        00184000
       1000-99-FIM.                                   EXIT.             00185001
