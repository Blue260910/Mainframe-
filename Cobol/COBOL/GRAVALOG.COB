      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. GRAVALOG.                                            00040000
      *=============================================================*   00050000
      *   AUTOR....:VICTOR ARANDA                                   *   00051000
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00052000
      *   DATA ....:08/06/2022                                      *   00053000
      *-------------------------------------------------------------*   00054000
      *   OBJETIVO:GRAVA LOG DE ERROS DOS PROGRAMAS                 *   00055000
      *                                                             *   00056000
      *                                                             *   00057000
      *-------------------------------------------------------------*   00058000
      *   ARQUIVOS:                                                 *   00060000
      *    DDNAME              I/O                  COPY/BOOK       *   00070000
      *                                                             *   00080000
      *                                                             *   00081000
      *=============================================================*   00090000
                                                                        00100000
      *====================================================             00101000
       ENVIRONMENT                               DIVISION.              00102000
      *====================================================             00103000
       CONFIGURATION                             SECTION.               00103100
       SPECIAL-NAMES.                                                   00103200
           DECIMAL-POINT  IS COMMA.                                     00103300
                                                                        00103400
       INPUT-OUTPUT                              SECTION.               00103500
       FILE-CONTROL.                                                    00103600
           SELECT LOGERROS  ASSIGN TO LOGERROS                          00103700
               FILE STATUS  IS WRK-FS-LOGERROS.                         00103800
                                                                        00103900
      *====================================================             00104000
       DATA                                      DIVISION.              00105000
      *====================================================             00106000
       FILE                                      SECTION.               00107000
       FD LOGERROS                                                      00107100
           RECORDING  MODE IS F                                         00107200
           BLOCK CONTAINS 0 RECORDS.                                    00107300
                                                                        00107400
       01 FD-LOGERROS PIC X(58).                                        00107500
                                                                        00107900
       WORKING-STORAGE                           SECTION.               00108000
      *====================================================             00109000
                                                                        00110700
       77 WRK-FS-LOGERROS PIC 9(02).                                    00111000
       77 WRK-SIS-DATA    PIC 9(08).                                    00111100
       77 WRK-SIS-HORA    PIC X(06).                                    00111200
                                                                        00111300
       LINKAGE                                   SECTION.               00111400
       01 COMMAREA.                                                     00111500
          05 LNK-PROGRAMA   PIC X(08).                                  00111600
          05 LNK-SECAO      PIC X(04).                                  00111700
          05 LNK-MENSAGEM   PIC X(30).                                  00111800
          05 LNK-STATUS     PIC X(02).                                  00111900
          05 LNK-DATA       PIC 9(08).                                  00112000
          05 LNK-HORA       PIC 9(06).                                  00112100
                                                                        00112200
       PROCEDURE   DIVISION USING COMMAREA.                             00112300
      *====================================================             00112400
       0000-PRINCIPAL                            SECTION.               00112500
           OPEN EXTEND LOGERROS.                                        00112600
                                                                        00112700
           ACCEPT WRK-SIS-DATA FROM DATE YYYYMMDD.                      00112800
           ACCEPT WRK-SIS-HORA FROM TIME.                               00112900
                                                                        00113000
           MOVE WRK-SIS-DATA TO LNK-DATA.                               00113100
           MOVE WRK-SIS-HORA TO LNK-HORA.                               00113200
           MOVE COMMAREA  TO FD-LOGERROS.                               00113300
                                                                        00113400
                                                                        00113500
              WRITE FD-LOGERROS.                                        00113600
           CLOSE LOGERROS.                                              00113700
                                                                        00113800
           STOP RUN.                                                    00114000
