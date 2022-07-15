      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. ARQ2103.                                             00040000
                                                                        00050000
      *=============================================================*   00060000
      *   AUTOR....:VICTOR ARANDA                                   *   00061000
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00062000
      *   DATA ....:25/04/2022                                      *   00063000
      *-------------------------------------------------------------*   00064000
      *   OBJETIVO:LER CLIENTES E FAZER RELATORIO DE ACORDO COM     *   00065000
      *            O SETOR                                          *   00066000
      *                                                             *   00067000
      *-------------------------------------------------------------*   00068000
      *   ARQUIVOS:                                                 *   00069000
      *    DDNAME              I/O                  COPY/BOOK       *   00069100
      *    CLIENTES             I                                   *   00070000
      *    SAIDACLI             O                                   *   00080000
      *=============================================================*   00090000
                                                                        00100000
      *====================================================             00101000
       ENVIRONMENT                               DIVISION.              00102000
      *====================================================             00103000
       CONFIGURATION                             SECTION .              00104000
                                                                        00104100
       SPECIAL-NAMES.                                                   00104200
           DECIMAL-POINT IS COMMA.                                      00104300
                                                                        00104400
       INPUT-OUTPUT                              SECTION.               00104500
                                                                        00104600
       FILE-CONTROL.                                                    00104700
           SELECT CLIENTES ASSIGN TO CLIENTES                           00104800
               FILE STATUS     IS    WRK-STATUS.                        00104900
                                                                        00105000
           SELECT SAIDACLI ASSIGN TO SAIDACLI                           00105400
               FILE STATUS     IS    WRK-STATUS-SAI.                    00105500
                                                                        00105600
      *====================================================             00105700
       DATA                                      DIVISION.              00106000
      *====================================================             00107000
       FILE                                       SECTION.              00107100
      *====================================================             00107200
                                                                        00107300
      *===================CLIENTES=========================             00107400
                                                                        00107500
       FD CLIENTES                                                      00108200
           RECORDING  MODE IS F                                         00108300
           BLOCK CONTAINS 0 RECORDS.                                    00108400
                                                                        00108500
        01 FD-CLIENTES.                                                 00108700
           05 FD-CODIGO      PIC 9(05).                                 00108800
           05 FD-NOME        PIC X(20).                                 00108900
           05 FD-CARGO       PIC X(04).                                 00109000
           05 FD-SALARIO     PIC 9(06)V99.                              00109100
                                                                        00109400
      *===================SAIDACLI=========================             00109500
                                                                        00109600
       FD SAIDACLI                                                      00109700
           RECORDING  MODE IS F                                         00109800
           BLOCK CONTAINS 0 RECORDS.                                    00109900
                                                                        00110000
        01 FD-CLIENTES-S.                                               00110100
           05 FD-CODIGO-S    PIC 9(05).                                 00110200
           05 FD-NOME-S      PIC X(20).                                 00110300
           05 FD-CARGO-S     PIC X(04).                                 00110400
           05 FD-SALARIO-S   PIC 9(06)V99.                              00110500
                                                                        00110800
      *====================================================             00110900
       WORKING-STORAGE                           SECTION.               00111000
      *====================================================             00111100
                                                                        00111200
       77 WRK-STATUS         PIC 9(02).                                 00111300
       77 WRK-STATUS-SAI     PIC 9(02).                                 00111400
                                                                        00111500
      *====================RESULTADOS======================             00111600
                                                                        00111700
      *             SEM CALCULOS NECESSARIOS                            00111800
                                                                        00111900
      *====================FORMATACAO=====================              00112000
                                                                        00112100
       77 WRK-DISPLAY        PIC X(29).                                 00112300
                                                                        00112400
       77 WRK-CONT-LIDOS     PIC 9(03).                                 00112500
       77 WRK-CONT-ANALISTAS PIC 9(03).                                 00112600
                                                                        00112700
                                                                        00112800
       77 WRK-CONT-SALARIO   PIC 9(06)V99.                              00112900
       77 WRK-CONT-SALARIO-A PIC 9(06)V99.                              00113000
                                                                        00113100
       77 WRK-SALARIO-EDT    PIC ZZZ.ZZ9,99.                            00113200
       77 WRK-SALARIO-A-EDT  PIC ZZZ.ZZ9,99.                            00113300
                                                                        00113400
       77 WRK-ESPACO         PIC X(01) VALUE SPACE.                     00113500
                                                                        00113600
                                                                        00113700
      *====================================================             00113800
       PROCEDURE                                 DIVISION.              00113900
      *====================================================             00114000
                                                                        00114100
           PERFORM 1000-INICIO.                                         00114200
                                                                        00114300
           IF WRK-STATUS = 00                                           00114400
              PERFORM 2000-PROCESSO UNTIL WRK-STATUS EQUAL 10           00114500
           END-IF.                                                      00114600
                                                                        00114700
           PERFORM 2100-RELATORIO.                                      00114800
                                                                        00114900
           PERFORM 3000-FIM.                                            00115000
                                                                        00115100
      *====================================================             00115200
       1000-INICIO                              SECTION.                00115300
      *====================================================             00115400
                                                                        00115500
                                                                        00115600
      *    RECEBIMENTO DE DADOS                                         00115700
           OPEN INPUT CLIENTES.                                         00115800
                                                                        00115900
              DISPLAY 'ENTRADA ABERTA..' WRK-STATUS.                    00116000
                                                                        00116100
           OPEN OUTPUT SAIDACLI.                                        00116200
                                                                        00116300
              DISPLAY 'SAIDA   ABERTA..' WRK-STATUS-SAI.                00116400
              DISPLAY WRK-ESPACO.                                       00116500
              DISPLAY '----------RELATORIO DE CLIENTES---------------'. 00116600
              DISPLAY WRK-ESPACO.                                       00116700
                                                                        00116800
      *====================================================             00116900
       1000-99-FIM.                                 EXIT.               00117000
      *====================================================             00117100
                                                                        00117200
                                                                        00117300
      *====================================================             00117400
       2000-PROCESSO                            SECTION.                00117500
      *====================================================             00118000
                                                                        00118100
                                                                        00120000
                                                                        00120500
                                                                        00120600
           IF FD-CARGO > 0                                              00120700
                                                                        00120800
              IF FD-CARGO = 0234                                        00120900
                 ADD 1 TO WRK-CONT-ANALISTAS                            00121000
                 ADD 1 TO WRK-CONT-LIDOS                                00121100
                 ADD FD-SALARIO TO WRK-CONT-SALARIO-A                   00121200
                 MOVE  FD-CLIENTES TO FD-CLIENTES-S                     00121300
                 WRITE FD-CLIENTES-S                                    00121400
              ELSE                                                      00121500
                 ADD 1 TO WRK-CONT-LIDOS                                00121600
                 ADD FD-SALARIO TO WRK-CONT-SALARIO                     00121700
              END-IF                                                    00121800
                                                                        00121900
           MOVE FD-CLIENTES TO WRK-DISPLAY                              00122000
           DISPLAY WRK-DISPLAY                                          00122100
           END-IF.                                                      00122200
                                                                        00122300
           READ CLIENTES.                                               00122400
                                                                        00122500
      *====================================================             00122600
       2000-99-FIM.                                 EXIT.               00122700
      *====================================================             00122800
                                                                        00125300
      *====================================================             00125400
       2100-RELATORIO                         SECTION.                  00125500
      *====================================================             00125600
                                                                        00125700
           MOVE WRK-CONT-SALARIO   TO  WRK-SALARIO-EDT.                 00125800
           MOVE WRK-CONT-SALARIO-A TO WRK-SALARIO-A-EDT.                00125900
                                                                        00126000
                                                                        00126100
                                                                        00126200
           DISPLAY WRK-ESPACO.                                          00126300
           DISPLAY '-----------RELATORIO----------------- '.            00126400
           DISPLAY WRK-ESPACO.                                          00126500
           DISPLAY 'TOTAL DE REGISTROS LIDOS ' WRK-CONT-LIDOS.          00126600
           DISPLAY 'REGISTROS COM ANALISTAS  ' WRK-CONT-ANALISTAS.      00126700
           DISPLAY WRK-ESPACO.                                          00126800
           DISPLAY 'SALARIOS                 ' WRK-SALARIO-EDT.         00126900
           DISPLAY 'SALARIOS DE ANALISTAS    ' WRK-SALARIO-A-EDT.       00127000
           DISPLAY WRK-ESPACO.                                          00127100
           DISPLAY '-----------FIM-DO-RELATORIO---------- '.            00127200
           DISPLAY WRK-ESPACO.                                          00127300
                                                                        00127400
                                                                        00127500
                                                                        00127600
                                                                        00127700
                                                                        00127800
      *====================================================             00127900
       2100-99-FIM.                                 EXIT.               00128000
      *====================================================             00128100
      *====================================================             00128200
       3000-FIM                              SECTION.                   00128300
      *====================================================             00128400
                                                                        00128500
           EVALUATE WRK-STATUS                                          00128600
                                                                        00128700
              WHEN 10                                                   00128800
                CLOSE CLIENTES                                          00128900
                DISPLAY 'FIM DO ARQUIVO ' WRK-STATUS                    00129000
                DISPLAY '-------------------------------------------'   00129100
                                                                        00129200
              WHEN 35                                                   00129300
                DISPLAY 'ARQUIVO NAO ENCONTRADO ' WRK-STATUS            00129400
                DISPLAY '-------------------------------------------'   00129500
                                                                        00129600
              WHEN OTHER                                                00129700
                DISPLAY 'ERRO NO ARQUIVO        ' WRK-STATUS            00129800
                DISPLAY '-------------------------------------------'   00129900
                                                                        00130000
            END-EVALUATE.                                               00130100
                                                                        00130200
                                                                        00130300
            IF WRK-STATUS-SAI = 96                                      00130400
              DISPLAY 'SAIDA NAO ENCONTRADA ' WRK-STATUS-SAI            00130500
              DISPLAY '-------------------------------------------'     00130600
                                                                        00130700
            ELSE                                                        00130800
              CLOSE SAIDACLI                                            00130900
                                                                        00131000
            END-IF.                                                     00131100
                                                                        00131200
            STOP RUN.                                                   00131300
                                                                        00131400
      *====================================================             00131500
       3000-99-FIM.                                  EXIT.              00132000
      *====================================================             00133000
                                                                        00140000
