      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR21CB15.                                            00040000
      *=============================================================*   00050000
      *   AUTOR....:VICTOR ARANDA                                   *   00051000
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00052000
      *   DATA ....:13/06/2022                                      *   00053000
      *-------------------------------------------------------------*   00054000
      *   OBJETIVO:CALCULAR A TABUADA DE UM NUMERO                  *   00055000
      *                                                             *   00056000
      *                                                             *   00057000
      *-------------------------------------------------------------*   00058000
      *   ARQUIVOS:                                                 *   00059000
      *    DDNAME              I/O                  COPY/BOOK       *   00060000
      *                                                             *   00070000
      *                                                             *   00080000
      *=============================================================*   00090000
       ENVIRONMENT                               DIVISION.              00102000
      *====================================================             00103000
       CONFIGURATION                             SECTION .              00104000
                                                                        00104100
       SPECIAL-NAMES.                                                   00104200
           DECIMAL-POINT IS COMMA.                                      00104300
      *====================================================             00105000
       DATA                                      DIVISION.              00106000
      *====================================================             00107000
       WORKING-STORAGE                            SECTION.              00108000
                                                                        00108200
       01 WRK-DADOS.                                                    00108300
          05 WRK-N1         PIC 9(02)     VALUE ZEROS.                  00108400
                                                                        00109000
                                                                        00109100
       77 WRK-RESULTADO      PIC 9(03)     VALUE ZEROS.                 00109400
       77 WRK-CONTADOR       PIC 9(02)     VALUE 1    .                 00109800
      *====================RESULTADOS======================             00110000
                                                                        00110100
      *             SEM CALCULOS NECESSARIOS                            00110200
                                                                        00110300
      *====================FORMATACAO=====================              00110400
                                                                        00110500
                                                                        00110700
      *====================================================             00110800
       PROCEDURE                                 DIVISION.              00110900
      *====================================================             00111000
                                                                        00111100
           PERFORM 1000-INICIO.                                         00111200
                                                                        00111300
           PERFORM 2000-PROCESSO UNTIL WRK-N1 EQUAL ZEROS.              00111500
                                                                        00111700
           PERFORM 3000-FIM.                                            00111900
                                                                        00112000
      *====================================================             00112100
       1000-INICIO                              SECTION.                00112200
      *====================================================             00112300
                                                                        00112400
                                                                        00112500
      *    RECEBIMENTO DE DADOS                                         00112600
           ACCEPT WRK-DADOS FROM SYSIN.                                 00112700
                                                                        00112800
                                                                        00112900
                                                                        00113000
      *====================================================             00113100
       1000-99-FIM.                                 EXIT.               00113200
      *====================================================             00113300
                                                                        00113400
                                                                        00113500
      *====================================================             00113600
       2000-PROCESSO                            SECTION.                00113700
      *====================================================             00113800
                                                                        00120000
                                                                        00120100
            DISPLAY '--------TABUADA DO- ' WRK-N1 '-------'.            00120200
           PERFORM 2100-CALCULAR 10 TIMES.                              00120300
            MOVE 1 TO WRK-CONTADOR.                                     00120500
            INITIALIZE WRK-DADOS.                                       00120600
            ACCEPT WRK-DADOS FROM SYSIN.                                00120700
                                                                        00120900
      *====================================================             00121000
       2000-99-FIM.                                 EXIT.               00121100
      *====================================================             00121200
                                                                        00121300
      *====================================================             00121400
       2100-CALCULAR                   SECTION.                         00121500
      *====================================================             00121600
                                                                        00121700
           COMPUTE WRK-RESULTADO = WRK-N1 * WRK-CONTADOR.               00121800
           DISPLAY WRK-RESULTADO.                                       00122100
           ADD 1 TO WRK-CONTADOR.                                       00122200
                                                                        00122300
      *====================================================             00122400
       2100-99-FIM.                                 EXIT.               00122500
      *====================================================             00122600
                                                                        00122700
                                                                        00122800
      *====================================================             00122900
       2200-99-FIM.                                 EXIT.               00123000
      *====================================================             00123100
                                                                        00123200
      *====================================================             00123300
       2300-SOMATORIA                       SECTION.                    00123400
      *====================================================             00123500
                                                                        00123900
                                                                        00124000
                                                                        00124100
      *====================================================             00124200
       2300-99-FIM.                                 EXIT.               00124300
      *====================================================             00124400
                                                                        00124500
      *====================================================             00124600
       3000-FIM                                   SECTION.              00124700
      *====================================================             00124800
                                                                        00124900
           STOP RUN.                                                    00125000
                                                                        00130000
      *====================================================             00131000
       3000-99-FIM.                                  EXIT.              00132000
      *====================================================             00133000
                                                                        00140000
