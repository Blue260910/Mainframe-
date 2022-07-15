      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR21CB04.                                            00040000
      *=============================================================*   00050000
      *   AUTOR....:VICTOR ARANDA                                   *   00051000
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00052000
      *   DATA ....:02/06/2022                                      *   00053000
      *-------------------------------------------------------------*   00054000
      *   OBJETIVO:REDEFINE DE DADOS                                *   00055000
      *                                                             *   00056000
      *                                                             *   00057000
      *-------------------------------------------------------------*   00058000
      *   ARQUIVOS:                                                 *   00059000
      *    DDNAME              I/O                  COPY/BOOK       *   00059100
      *                                                             *   00060000
      *                                                             *   00070000
      *=============================================================*   00080000
                                                                        00100000
      *====================================================             00101000
       ENVIRONMENT                               DIVISION.              00102000
      *====================================================             00103000
                                                                        00104000
      *====================================================             00105000
       DATA                                      DIVISION.              00106000
      *====================================================             00107000
       WORKING-STORAGE                           SECTION.               00108000
                                                                        00108100
       01 WRK-DADOS.                                                    00108500
          05 WRK-ID         PIC 9(05) VALUE ZEROS.                      00108600
          05 WRK-NOME       PIC X(30) VALUE SPACES.                     00108700
          05 WRK-ENDERECO   PIC X(30) VALUE SPACES.                     00108800
                                                                        00108900
       01 WRK-DADOS-RED1    REDEFINES WRK-DADOS.                        00109000
          05 WRK-ID-RED1    PIC 9(05).                                  00109100
          05 WRK-NOME-RED1  PIC X(30).                                  00109200
                                                                        00109500
       01 WRK-DADOS-RED2    REDEFINES WRK-DADOS.                        00109600
          05 WRK-ID-RED2    PIC 9(05).                                  00109700
                                                                        00109800
       01 WRK-DADOS-RED3    REDEFINES WRK-DADOS.                        00109900
          05 FILLER         PIC X(35).                                  00110000
          05 WRK-ENDERECO-RED3 PIC X(30).                               00110100
                                                                        00110200
      *====================================================             00110300
       PROCEDURE                                 DIVISION.              00111000
      *====================================================             00120000
           ACCEPT WRK-DADOS FROM SYSIN.                                 00130000
           DISPLAY 'WRK-DADOS     ..' WRK-DADOS.                        00131000
           DISPLAY 'WRK-DADOS-RED1..' WRK-DADOS-RED1.                   00131100
           DISPLAY 'WRK-DADOS-RED2..' WRK-DADOS-RED2.                   00131200
           DISPLAY 'WRK-DADOS-RED3..' WRK-ENDERECO-RED3.                00131300
           STOP RUN.                                                    00134000
                                                                        00135000
                                                                        00136000
