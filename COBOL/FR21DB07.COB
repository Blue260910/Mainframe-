      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR19DB07.                                            00005036
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:VICTOR ARANDA                                   *   00007104
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00007200
      *   DATA ....:13/06/2022                                      *   00007336
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO:LER TODA A INFORMACAO DA TABELA DB2 DE ACORDO COM*   00007536
      *            TABELA DE DESCRICAO DE SETORES                   *   00007636
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
      *--------LRECL 135--------------------------------------------*   00011636
                                                                        00011704
       01 FD-RELDB2 PIC X(135).                                         00011831
                                                                        00011910
      *=============================================================*   00012600
       WORKING-STORAGE                             SECTION.             00012700
      *=============================================================*   00012800
                                                                        00012900
           EXEC SQL                                                     00013000
              INCLUDE #BKFUNC                                           00013100
           END-EXEC.                                                    00013200
                                                                        00013300
           EXEC SQL                                                     00013429
              INCLUDE #BKSETOR                                          00013529
           END-EXEC.                                                    00013629
                                                                        00013729
           EXEC SQL                                                     00013800
              INCLUDE SQLCA                                             00013900
           END-EXEC.                                                    00014000
                                                                        00014100
           EXEC SQL                                                     00014200
              DECLARE CFUNC CURSOR FOR                                  00015000
               SELECT ID,NOME,SALARIO,DATAADM,EMAIL,DESCSETOR           00016027
                FROM IVAN.FUNC F , IVAN.SETOR S                         00016131
                WHERE F.SETOR = S.IDSETOR                               00016227
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
          05 WRK-DESCSETOR         PIC X(40).                           00025035
          05 WRK-EMAIL             PIC X(40).                           00025135
          05 WRK-SALARIO           PIC 9999999999.                      00025231
          05 WRK-DATAADM           PIC X(10).                           00025535
                                                                        00025628
      *=============================================================*   00025728
       PROCEDURE DIVISION.                                              00025828
      *=============================================================*   00025928
                                                                        00026028
      *-------------------------------------------------------------*   00026128
       0000-PRINCIPAL                           SECTION.                00026228
      *-------------------------------------------------------------*   00026328
                                                                        00026428
            PERFORM  1000-INICIAR.                                      00026528
            PERFORM  2000-PROCESSAR UNTIL SQLCODE EQUAL 100.            00026628
            PERFORM  3000-FINALIZAR.                                    00026728
            STOP RUN.                                                   00026828
                                                                        00026928
                                                                        00027028
      *-------------------------------------------------------------*   00027128
       1000-INICIAR                             SECTION.                00027228
                                                                        00027304
            EXEC SQL                                                    00027400
               OPEN CFUNC                                               00027500
            END-EXEC.                                                   00027600
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
               MOVE DB2-ID        TO WRK-ID.                            00034732
               MOVE DB2-NOME      TO WRK-NOME.                          00034832
               MOVE DB2-DESCSETOR TO WRK-DESCSETOR.                     00034932
               MOVE DB2-SALARIO   TO WRK-SALARIO.                       00035032
               MOVE DB2-DATAADM   TO WRK-DATAADM.                       00035132
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
                  :DB2-SALARIO,                                         00037204
                  :DB2-DATAADM,                                         00037304
                  :DB2-EMAIL     :WRK-NULL-EMAIL,                       00037532
                  :DB2-DESCSETOR                                        00037632
            END-EXEC.                                                   00037732
                                                                        00037832
            EVALUATE SQLCODE                                            00037932
             WHEN 0                                                     00038032
               ADD 1 TO WRK-REGLIDOS                                    00038132
               ADD DB2-SALARIO TO WRK-SAL-ACUM                          00038232
             WHEN 100                                                   00038432
              DISPLAY ' FINAL DE ARQUIVO'                               00038532
             WHEN OTHER                                                 00038632
               MOVE SQLCODE TO WRK-SQLCODE                              00038732
               DISPLAY 'ERRO NA LEITURA ' WRK-SQLCODE                   00038832
             END-EVALUATE.                                              00038932
                                                                        00039032
       2500-99-FIM.              EXIT.                                  00039132
      *-------------------------------------------------------------*   00039232
                                                                        00039332
      *-------------------------------------------------------------*   00039432
       3000-FINALIZAR                               SECTION.            00039532
                                                                        00039632
              EXEC SQL                                                  00039732
                CLOSE CFUNC                                             00039832
              END-EXEC.                                                 00039932
                                                                        00040032
              CLOSE RELDB2.                                             00040132
                                                                        00040232
              DISPLAY ' -----FIM DO PROGRAMA----- '.                    00040332
              DISPLAY ' REGISTROS LIDOS.......' WRK-REGLIDOS.           00040432
              DISPLAY ' REGISTROS GRAVADOS....' WRK-GRAVADOS.           00040532
              MOVE WRK-SAL-ACUM TO WRK-SAL-ACUM-EDT.                    00040632
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
