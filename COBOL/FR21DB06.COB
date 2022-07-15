      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR19DB06.                                            00005000
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:VICTOR ARANDA                                   *   00007104
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00007200
      *   DATA ....:10/06/2022                                      *   00007300
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO:LER TODA A INFORMACAO DA TABELA DB2 E GERAR      *   00007504
      *            UM ARQUIVO DE SAIDA.                             *   00007607
      *                                                             *   00007701
      *-------------------------------------------------------------*   00007900
      *   BASE DE DADOS:                                            *   00008000
      *   TABELA.DB2..                                              *   00008100
      *    ------              I/O                 INCLUDE/BOOK     *   00008200
      *   IVAN.FUNC             I                  #BKFUNC----      *   00008300
      *-------------------------------------------------------------*   00008400
      *   ARQUIVOS:                                                 *   00008501
      *    DDNAME              I/O                  COPY/BOOK       *   00008701
      *   RELDB2                O                  -----------      *   00008801
      *=============================================================*   00009000
                                                                        00009100
      *=============================================================*   00009200
       ENVIRONMENT                               DIVISION.              00009300
      *=============================================================*   00009400
                                                                        00009500
      *=============================================================*   00009600
       CONFIGURATION                               SECTION.             00009700
      *=============================================================*   00009800
       SPECIAL-NAMES.                                                   00009900
           DECIMAL-POINT IS COMMA.                                      00010000
      *-------------------------------------------------------------*   00010100
       INPUT-OUTPUT                                 SECTION.            00010200
       FILE-CONTROL.                                                    00010300
            SELECT RELDB2 ASSIGN TO RELDB2                              00010400
                FILE STATUS IS WRK-FS-RELDB2.                           00010500
                                                                        00010600
      *=============================================================*   00010700
       DATA                                      DIVISION.              00010800
      *=============================================================*   00010900
       FILE                                        SECTION.             00011000
       FD RELDB2                                                        00011100
           RECORDING MODE IS F                                          00011200
           LABEL RECORD IS STANDARD                                     00011300
           BLOCK CONTAINS 0 RECORDS.                                    00011400
                                                                        00011504
      *--------LRECL 99---------------------------------------------*   00011600
                                                                        00011704
       01 FD-RELDB2 PIC X(99).                                          00011810
                                                                        00011910
      *=============================================================*   00012600
       WORKING-STORAGE                             SECTION.             00012700
      *=============================================================*   00012800
                                                                        00012900
           EXEC SQL                                                     00013000
              INCLUDE #BKFUNC                                           00013100
           END-EXEC.                                                    00013200
                                                                        00013300
           EXEC SQL                                                     00013400
              INCLUDE SQLCA                                             00013500
           END-EXEC.                                                    00013600
                                                                        00013700
           EXEC SQL                                                     00014000
              DECLARE CFUNC CURSOR FOR                                  00015000
               SELECT * FROM IVAN.FUNC                                  00016000
                ORDER BY ID                                             00017000
           END-EXEC.                                                    00018000
                                                                        00019000
       77 WRK-FS-RELDB2       PIC 9(02).                                00019102
                                                                        00019210
       77 WRK-REGLIDOS        PIC 9(03).                                00020000
                                                                        00020110
       77 WRK-GRAVADOS        PIC 9(03).                                00021004
                                                                        00021110
       77 WRK-SAL-ACUM        PIC S9(08)V9(2) COMP.                     00022018
                                                                        00022121
       77 WRK-SAL-ACUM-EDT    PIC ZZ.ZZZ.ZZ9,99.                        00022226
                                                                        00023010
       77 WRK-SQLCODE         PIC -999.                                 00024200
                                                                        00024310
       77 WRK-NULL-EMAIL      PIC S9(4) COMP.                           00024400
                                                                        00024520
      *------------------GRAVACAO-----------------------------------*   00024620
       01 WRK-RELDB2.                                                   00024713
          05 WRK-ID                PIC 99999.                           00024810
          05 WRK-NOME              PIC X(30).                           00024910
          05 WRK-SETOR             PIC X(04).                           00025010
          05 WRK-SALARIO           PIC 9999999999.                      00025110
          05 WRK-DATAADM           PIC X(10).                           00025210
          05 WRK-EMAIL             PIC X(40).                           00025310
                                                                        00025421
      *=============================================================*   00025500
       PROCEDURE DIVISION.                                              00025600
      *=============================================================*   00025700
                                                                        00025800
      *-------------------------------------------------------------*   00025900
       0000-PRINCIPAL                           SECTION.                00026000
      *-------------------------------------------------------------*   00026100
                                                                        00026200
            PERFORM  1000-INICIAR.                                      00026300
            PERFORM  2000-PROCESSAR UNTIL SQLCODE EQUAL 100.            00026408
            PERFORM  3000-FINALIZAR.                                    00026500
            STOP RUN.                                                   00026600
                                                                        00026700
                                                                        00026800
      *-------------------------------------------------------------*   00026900
       1000-INICIAR                             SECTION.                00027000
                                                                        00027204
            EXEC SQL                                                    00027300
               OPEN CFUNC                                               00027400
            END-EXEC.                                                   00027500
                                                                        00029300
            OPEN OUTPUT RELDB2.                                         00029402
                                                                        00029515
            PERFORM  1100-TESTAR-STATUS1.                               00029620
            PERFORM  1200-TESTAR-SQL.                                   00029723
                                                                        00029820
       1000-99-FIM.              EXIT.                                  00029920
      *-------------------------------------------------------------*   00030020
                                                                        00030120
      *-------------------------------------------------------------*   00030220
       1100-TESTAR-STATUS1                      SECTION.                00030320
                                                                        00030404
            IF WRK-FS-RELDB2 NOT EQUAL 0                                00030502
               DISPLAY ' ERRO NA ABERTURA DO ARQUIVO'                   00030602
                  STOP RUN                                              00030702
            END-IF.                                                     00030802
                                                                        00030902
       1100-99-FIM.              EXIT.                                  00032102
      *-------------------------------------------------------------*   00032200
                                                                        00032320
      *-------------------------------------------------------------*   00032423
       1200-TESTAR-SQL                          SECTION.                00032524
                                                                        00032620
            EVALUATE SQLCODE                                            00032721
              WHEN 0                                                    00032819
                PERFORM 2500-LER-FUNCIONARIO                            00032919
              WHEN 100                                                  00033019
                DISPLAY 'SEM INFORMACOES'                               00033119
              WHEN OTHER                                                00033219
                MOVE SQLCODE TO WRK-SQLCODE                             00033319
                DISPLAY 'ERRO ' WRK-SQLCODE ' NO OPEN DO CURSOR.'       00033419
                MOVE 200 TO RETURN-CODE                                 00033519
                STOP RUN                                                00033619
            END-EVALUATE.                                               00033721
                                                                        00033820
       1200-99-FIM.              EXIT.                                  00033920
      *-------------------------------------------------------------*   00034020
                                                                        00034120
      *-------------------------------------------------------------*   00034220
       2000-PROCESSAR                           SECTION.                00034300
                                                                        00034420
               INITIALIZE WRK-RELDB2.                                   00034511
                                                                        00034611
               MOVE DB2-ID       TO WRK-ID.                             00034711
               MOVE DB2-NOME     TO WRK-NOME.                           00034811
               MOVE DB2-SETOR    TO WRK-SETOR.                          00034911
               MOVE DB2-SALARIO  TO WRK-SALARIO.                        00035011
               MOVE DB2-DATAADM  TO WRK-DATAADM.                        00035111
                 IF WRK-NULL-EMAIL EQUAL -1                             00035214
                  MOVE SPACES  TO WRK-EMAIL                             00035314
                 ELSE                                                   00035414
                  MOVE DB2-EMAIL    TO WRK-EMAIL                        00035514
                 END-IF.                                                00035614
               WRITE FD-RELDB2 FROM WRK-RELDB2.                         00035711
               ADD 1             TO WRK-GRAVADOS.                       00035811
               PERFORM 2500-LER-FUNCIONARIO.                            00035911
                                                                        00036000
       2000-99-FIM.              EXIT.                                  00036100
      *-------------------------------------------------------------*   00036204
                                                                        00036320
      *-------------------------------------------------------------*   00036420
       2500-LER-FUNCIONARIO                         SECTION.            00036520
                                                                        00036604
           EXEC SQL                                                     00036704
            FETCH CFUNC                                                 00036804
             INTO :DB2-ID,                                              00036904
                  :DB2-NOME,                                            00037004
                  :DB2-SETOR,                                           00037104
                  :DB2-SALARIO,                                         00037204
                  :DB2-DATAADM,                                         00037304
                  :DB2-EMAIL     :WRK-NULL-EMAIL                        00037404
            END-EXEC.                                                   00037504
                                                                        00037604
            EVALUATE SQLCODE                                            00037704
             WHEN 0                                                     00037804
               ADD 1 TO WRK-REGLIDOS                                    00037904
               ADD DB2-SALARIO TO WRK-SAL-ACUM                          00038018
               CONTINUE                                                 00038108
             WHEN 100                                                   00038204
              DISPLAY ' FINAL DE ARQUIVO'                               00038304
             WHEN OTHER                                                 00038404
               MOVE SQLCODE TO WRK-SQLCODE                              00038504
               DISPLAY 'ERRO NA LEITURA ' WRK-SQLCODE                   00038604
             END-EVALUATE.                                              00038704
                                                                        00038804
       2500-99-FIM.              EXIT.                                  00038904
      *-------------------------------------------------------------*   00039000
                                                                        00039120
      *-------------------------------------------------------------*   00039220
       3000-FINALIZAR                               SECTION.            00039320
                                                                        00039420
              EXEC SQL                                                  00039520
                CLOSE CFUNC                                             00039620
              END-EXEC.                                                 00039720
                                                                        00039820
              CLOSE RELDB2.                                             00039920
                                                                        00040020
              DISPLAY ' -----FIM DO PROGRAMA----- '.                    00040120
              DISPLAY ' REGISTROS LIDOS.......' WRK-REGLIDOS.           00040220
              DISPLAY ' REGISTROS GRAVADOS....' WRK-GRAVADOS.           00040320
              MOVE WRK-SAL-ACUM TO WRK-SAL-ACUM-EDT.                    00040421
              DISPLAY ' SALARIO ACUMULADO.....' WRK-SAL-ACUM-EDT.       00041021
                                                                        00041100
       3000-99-FIM.              EXIT.                                  00041200
      *-------------------------------------------------------------*   00041300
                                                                        00041420
      *-------------------------------------------------------------*   00044100
       9000-TRATAERROS                              SECTION.            00045000
      *-------------------------------------------------------------*   00045100
                                                                        00046000
       9000-99-FIM.              EXIT.                                  00047000
