      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR21JOIN.                                            00005039
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:VICTOR ARANDA                                   *   00007104
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00007200
      *   DATA ....:14/06/2022                                      *   00007337
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO:LER TODA A INFORMACAO DA TABELA DB2 (FUNC) E USAR*   00007537
      *            JOIN PARA COMBINAR COM A TABELA (BENEF).         *   00007637
      *                                                             *   00007701
      *-------------------------------------------------------------*   00007900
      *   BASE DE DADOS:                                            *   00008000
      *   TABELA.DB2..                                              *   00008100
      *    ------              I/O                 INCLUDE/BOOK     *   00008200
      *   IVAN.FUNC             I                  #BKFUNC----      *   00008300
      *   IVAN.BENEF            I                  #BKBENEF---      *   00008437
      *-------------------------------------------------------------*   00008537
      *   ARQUIVOS:                                                 *   00008637
      *    DDNAME              I/O                  COPY/BOOK       *   00008701
      *   SRELBENE              O                  -----------      *   00008837
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
            SELECT SRELBENE ASSIGN TO SRELBENE                          00010437
                FILE STATUS IS WRK-FS-SRELBENE.                         00010537
                                                                        00010600
      *=============================================================*   00010700
       DATA                                      DIVISION.              00010800
      *=============================================================*   00010900
       FILE                                        SECTION.             00011000
      *--------LRECL 68---------------------------------------------*   00011140
                                                                        00011240
       FD SRELBENE                                                      00011337
           RECORDING MODE IS F                                          00011400
           LABEL RECORD IS STANDARD                                     00011500
           BLOCK CONTAINS 0 RECORDS.                                    00011600
                                                                        00011704
                                                                        00011804
       01 FD-SRELBENE PIC X(68).                                        00011937
                                                                        00012010
      *=============================================================*   00012600
       WORKING-STORAGE                             SECTION.             00012700
      *=============================================================*   00012800
                                                                        00012900
           EXEC SQL                                                     00013000
              INCLUDE #BKFUNC                                           00013100
           END-EXEC.                                                    00013200
                                                                        00013300
           EXEC SQL                                                     00013429
              INCLUDE #BKBENEF                                          00013537
           END-EXEC.                                                    00013629
                                                                        00013729
           EXEC SQL                                                     00013800
              INCLUDE SQLCA                                             00013900
           END-EXEC.                                                    00014000
                                                                        00014100
           EXEC SQL                                                     00014200
              DECLARE CFUNC CURSOR FOR                                  00015000
               SELECT ID,NOME,SALARIO,PLANMED,PLANDENT,COTAS            00016037
                FROM IVAN.FUNC F , IVAN.BENEF B                         00016137
                WHERE F.ID = B.IDFUNC                                   00016237
           END-EXEC.                                                    00018000
                                                                        00019000
      *----------------VARIAVEIS DE APOIO---------------------------*   00019140
       01 FILLER                                      PIC X(64) VALUE   00019240
           '-----------VARIAVEIS DE APOIO-------------------'.          00019340
      *-------------------------------------------------------------*   00019440
       77 WRK-REGLIDOS        PIC 9(03).                                00020000
                                                                        00020110
       77 WRK-GRAVADOS        PIC 9(03).                                00021004
                                                                        00021110
       77 WRK-SAL-ACUM        PIC S9(08)V9(2) COMP.                     00022018
                                                                        00022121
       77 WRK-SAL-ACUM-EDT    PIC ZZ.ZZZ.ZZ9,99.                        00022226
                                                                        00023010
       77 WRK-NULL-EMAIL      PIC S9(4) COMP.                           00024400
                                                                        00024540
      *----------------VARIAVEIS DE STATUS--------------------------*   00024640
       01 FILLER                                     PIC X(64) VALUE    00024740
          '------------VARIAVEIS DE STATUS-----------------'.           00024840
      *-------------------------------------------------------------*   00024940
       77 WRK-SQLCODE         PIC -999.                                 00025240
                                                                        00025320
       77 WRK-FS-SRELBENE     PIC 9(02).                                00025440
                                                                        00025540
      *----------------WRK-SRELBENE-TAMANHO 68----------------------*   00025640
       01 FILLER                                     PIC X(64) VALUE    00025740
           '-----------WRK-SRELBENE-TAMANHO 68--------------'.          00025840
      *-------------------------------------------------------------*   00025940
       01 WRK-SRELBENE.                                                 00026037
          05 WRK-ID                PIC 99999.                           00026110
          05 WRK-NOME              PIC X(30).                           00026210
          05 WRK-SALARIO           PIC 9999999999.                      00026331
          05 WRK-PLANMED           PIC X(10).                           00026437
          05 WRK-PLANDENT          PIC X(10).                           00026537
          05 WRK-COTAS             PIC 9(03).                           00026637
                                                                        00026728
      *=============================================================*   00026828
       PROCEDURE DIVISION.                                              00026928
      *=============================================================*   00027028
                                                                        00027128
      *-------------------------------------------------------------*   00027228
       0000-PRINCIPAL                                       SECTION.    00027340
                                                                        00027528
            PERFORM  1000-INICIAR.                                      00027628
            PERFORM  2000-PROCESSAR UNTIL SQLCODE EQUAL 100.            00027728
            PERFORM  3000-FINALIZAR.                                    00027828
            STOP RUN.                                                   00027928
                                                                        00028128
       0000-99-FIM.                                            EXIT.    00028240
      *-------------------------------------------------------------*   00028340
                                                                        00028440
      *-------------------------------------------------------------*   00028540
       1000-INICIAR                                         SECTION.    00028640
                                                                        00028740
            EXEC SQL                                                    00028840
               OPEN CFUNC                                               00028940
            END-EXEC.                                                   00029000
                                                                        00029300
            OPEN OUTPUT SRELBENE.                                       00029437
                                                                        00029515
            PERFORM  1100-TESTAR-STATUS1.                               00029620
            PERFORM  1200-TESTAR-SQL.                                   00029723
                                                                        00029820
       1000-99-FIM.                                            EXIT.    00029940
      *-------------------------------------------------------------*   00030020
                                                                        00030120
      *-------------------------------------------------------------*   00030220
       1100-TESTAR-STATUS1                                  SECTION.    00030340
                                                                        00030404
            IF WRK-FS-SRELBENE NOT EQUAL 0                              00030537
               DISPLAY ' ERRO NA ABERTURA DO ARQUIVO'                   00030602
                  STOP RUN                                              00030702
            END-IF.                                                     00030802
                                                                        00030902
       1100-99-FIM.                                            EXIT.    00032140
      *-------------------------------------------------------------*   00032200
                                                                        00032320
      *-------------------------------------------------------------*   00032423
       1200-TESTAR-SQL                                      SECTION.    00032540
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
       1200-99-FIM.                                            EXIT.    00033940
      *-------------------------------------------------------------*   00034020
                                                                        00034120
      *-------------------------------------------------------------*   00034220
       2000-PROCESSAR                                       SECTION.    00034340
                                                                        00034420
               INITIALIZE WRK-SRELBENE.                                 00034538
                                                                        00034611
               MOVE DB2-ID        TO WRK-ID.                            00034732
               MOVE DB2-NOME      TO WRK-NOME.                          00034832
               MOVE DB2-SALARIO   TO WRK-SALARIO.                       00035032
               MOVE DB2-PLANMED   TO WRK-PLANMED.                       00035137
               MOVE DB2-PLANDENT  TO WRK-PLANDENT.                      00035237
               MOVE DB2-COTAS     TO WRK-COTAS.                         00035337
               WRITE FD-SRELBENE FROM WRK-SRELBENE.                     00035938
               ADD 1             TO WRK-GRAVADOS.                       00036037
               PERFORM 2500-LER-FUNCIONARIO.                            00036137
                                                                        00036237
       2000-99-FIM.                                            EXIT.    00036340
      *-------------------------------------------------------------*   00036437
                                                                        00036537
      *-------------------------------------------------------------*   00036637
       2500-LER-FUNCIONARIO                                 SECTION.    00036740
                                                                        00036837
           EXEC SQL                                                     00036937
            FETCH CFUNC                                                 00037037
             INTO :DB2-ID,                                              00037137
                  :DB2-NOME,                                            00037237
                  :DB2-SALARIO,                                         00037337
                  :DB2-PLANMED,                                         00037437
                  :DB2-PLANDENT,                                        00037537
                  :DB2-COTAS                                            00037637
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
       2500-99-FIM.                                            EXIT.    00039140
      *-------------------------------------------------------------*   00039232
                                                                        00039332
      *-------------------------------------------------------------*   00039432
       3000-FINALIZAR                                       SECTION.    00039540
                                                                        00039632
              EXEC SQL                                                  00039732
                CLOSE CFUNC                                             00039832
              END-EXEC.                                                 00039932
                                                                        00040032
              CLOSE SRELBENE.                                           00040137
                                                                        00040232
              DISPLAY ' -----FIM DO PROGRAMA----- '.                    00040332
              DISPLAY ' REGISTROS LIDOS.......' WRK-REGLIDOS.           00040432
              DISPLAY ' REGISTROS GRAVADOS....' WRK-GRAVADOS.           00040532
              MOVE WRK-SAL-ACUM TO WRK-SAL-ACUM-EDT.                    00040632
              DISPLAY ' SALARIO ACUMULADO.....' WRK-SAL-ACUM-EDT.       00041021
                                                                        00041100
       3000-99-FIM.                                            EXIT.    00041240
      *-------------------------------------------------------------*   00041300
                                                                        00041420
      *-------------------------------------------------------------*   00044340
       9000-TRATAERROS                                      SECTION.    00045040
                                                                        00046000
       9000-99-FIM.                                            EXIT.    00047040
      *-------------------------------------------------------------*   00048040
