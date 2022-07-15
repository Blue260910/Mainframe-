      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR21CB08.                                            00040000
      *=============================================================*   00050000
      *   AUTOR....:VICTOR ARANDA                                   *   00051000
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00052000
      *   DATA ....:07/06/2022                                      *   00053000
      *-------------------------------------------------------------*   00054000
      *   OBJETIVO:CONTAS USANDO GIVVING E MASCARA                  *   00055000
      *                                                             *   00056000
      *                                                             *   00057000
      *-------------------------------------------------------------*   00058000
      *   ARQUIVOS:                                                 *   00059000
      *    DDNAME              I/O                  COPY/BOOK       *   00059100
      *                                                             *   00059200
      *                                                             *   00059300
      *=============================================================*   00059400
       ENVIRONMENT                               DIVISION.              00102000
      *====================================================             00103000
                                                                        00104000
      *====================================================             00105000
       DATA                                      DIVISION.              00106000
      *====================================================             00107000
       WORKING-STORAGE                         SECTION.                 00108000
       01 WRK-DADOS.                                                    00108100
          05 WRK-MES        PIC 9(02) VALUE ZEROS.                      00108200
          05 WRK-VEND       PIC 9(08) VALUE ZEROS.                      00108300
          05 WRK-FUNC       PIC 9(03) VALUE ZEROS.                      00108400
                                                                        00108500
       77 WRK-RES           PIC 9(14) VALUE ZEROS.                      00108600
       77 WRK-RES-EDT       PIC ZZZZZZZZZZZZZ9   .                      00108700
                                                                        00108800
      *====================================================             00108900
       PROCEDURE                                 DIVISION.              00109000
      *====================================================             00109100
           ACCEPT WRK-DADOS FROM SYSIN.                                 00109200
           DISPLAY 'MES           ..  ' WRK-MES.                        00109300
           DISPLAY 'VENDAS        ..  ' WRK-VEND.                       00109400
           DISPLAY 'FUNCIONARIOS  ..  ' WRK-FUNC.                       00109500
                                                                        00109600
           DIVIDE WRK-VEND BY WRK-FUNC GIVING WRK-RES                   00109700
               ON SIZE ERROR                                            00110100
                 DISPLAY 'ERRO NA DIVISAO'                              00110200
               NOT ON SIZE ERROR                                        00110300
                 MOVE WRK-RES TO WRK-RES-EDT                            00110400
                 DISPLAY 'MEDIA DE VENDAS POR FUNC É..  ' WRK-RES-EDT   00110500
            END-DIVIDE.                                                 00111000
           STOP RUN.                                                    00120000
