      *================================================================*00010000
       IDENTIFICATION                            DIVISION.              00020000
      *================================================================*00030000
       PROGRAM-ID. FR21CB07.                                            00040000
      *=============================================================*   00050000
      *   AUTOR....:VICTOR ARANDA                                   *   00051000
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00052000
      *   DATA ....:06/06/2022                                      *   00053001
      *-------------------------------------------------------------*   00054000
      *   OBJETIVO:CALCULADORA SIMPLES                              *   00055001
      *                                                             *   00060001
      *                                                             *   00070000
      *-------------------------------------------------------------*   00080000
      *   ARQUIVOS:                                                 *   00090001
      *    DDNAME              I/O                  COPY/BOOK       *   00100000
      *                                                             *   00101001
      *                                                             *   00102000
      *=============================================================*   00103001
                                                                        00104000
      *================================================================*00105000
       DATA                                      DIVISION.              00106000
      *================================================================*00107000
       WORKING-STORAGE                           SECTION.               00108000
                                                                        00108100
       01 WRK-DATA                                                      00108200
          05 WRK-N1                 PIC 9(02)     VALUE ZEROS .         00109100
          05 WRK-OP                 PIC X(01)     VALUE SPACE .         00109200
          05 WRK-N2                 PIC 9(02)     VALUE ZEROES.         00109300
                                                                        00109400
       77 WRK-RES                PIC S9(03)    VALUE ZEROES.            00109500
       77 WRK-RES-EDT            PIC -ZZ9                               00109800
                                                                        00109900
      *================================================================*00110000
       PROCEDURE                                 DIVISION.              00110100
      *================================================================*00110200
           ACCEPT WRK-DATA FROM SYSIN.                                  00110300
                                                                        00110400
           EVALUATE WRK-OP                                              00110600
                                                                        00110700
               WHEN '+'                                                 00110800
                  ADD WRK-N1 TO WRK-N2 GIVING WRK-RES                   00110900
               WHEN '-'                                                 00111000
                  SUBTRACT WRK-N2 FROM WRK-N1 GIVING WRK-RES            00111100
               WHEN '/'                                                 00111200
                  DIVIDE WRK-N1 BY WRK-N2 GIVING WRK-RES                00112000
                       ON SIZE ERROR                                    00112100
                         DISPLAY 'ERRO NA DIVISAO'                      00112200
                       NOT ON SIZE ERROR                                00112300
                         DISPLAY WRK-RES                                00112500
                    END-DIVIDE.                                         00112600
               WHEN '*'                                                 00113000
                  MULTIPLY WRK-N1 BY WRK-N2 GIVING WRK-RES              00113100
               WHEN OTHER                                               00113200
                  DISPLAY 'ERRO'                                        00113300
           END-EVALUATE.                                                00113500
                                                                        00113600
           MOVE WRK-RES TO WRK-RES-EDT.                                 00113700
           DISPLAY WRK-RES-EDT.                                         00113800
           STOP RUN.                                                    00114000
