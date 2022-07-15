      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR21RELB.                                            00005010
                                                                        00006000
      *=============================================================*   00006100
      *   AUTOR....:VICTOR ARANDA                                   *   00006200
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00006300
      *   DATA ....:14/06/2022                                      *   00006400
      *-------------------------------------------------------------*   00006500
      *   OBJETIVO: LER TODOS OS REGISTROS DO ARQUIVO, CLASSIFICAR  *   00006600
      *      POR ORDEM DE ID E FAZER UM RELATORIO.                  *   00006700
      *-------------------------------------------------------------*   00006900
      *   ARQUIVOS...:                                              *   00007000
      *    DDNAME              I/O                 INCLUDE/BOOK     *   00007100
      *    SRELBENE             I                                   *   00007200
      *    RELATBEN             O                  -----------      *   00007311
      *-------------------------------------------------------------*   00007400
      *   MODULOS....:                             INCLUDE/BOOK     *   00007500
      *=============================================================*   00007700
                                                                        00012000
      *=============================================================*   00013000
       ENVIRONMENT                                         DIVISION.    00014017
      *=============================================================*   00015000
                                                                        00015100
      *=============================================================*   00015200
       CONFIGURATION                                        SECTION.    00015317
      *=============================================================*   00015400
       SPECIAL-NAMES.                                                   00015500
           DECIMAL-POINT IS COMMA.                                      00015600
                                                                        00015700
                                                                        00015800
       INPUT-OUTPUT                                         SECTION.    00015917
       FILE-CONTROL.                                                    00016000
           SELECT SRELBENE  ASSIGN TO SRELBENE                          00016100
               FILE STATUS  IS WRK-FS-SRELBENE.                         00016200
                                                                        00016300
                                                                        00016415
           SELECT RELATBEN  ASSIGN TO RELATBEN                          00016511
               FILE STATUS  IS WRK-FS-RELATBEN.                         00016611
      *=============================================================*   00016700
       DATA                                                DIVISION.    00016817
      *=============================================================*   00016900
                                                                        00017017
       FILE                                                 SECTION.    00017217
      *--------------------LRECR-68---------------------------------*   00017317
       FD SRELBENE                                                      00017400
           RECORDING  MODE IS F                                         00017500
           BLOCK CONTAINS 0 RECORDS.                                    00017600
                                                                        00017700
       01 FD-SRELBENE.                                                  00017800
          05 FD-ID                PIC X(05).                            00017901
          05 FD-NOME              PIC X(30).                            00018001
          05 FD-SALARIO           PIC 9(10).                            00018101
          05 FD-PLANMED           PIC X(10).                            00018201
          05 FD-PLANDENT          PIC X(10).                            00018301
          05 FD-COTAS             PIC 9(03).                            00018401
                                                                        00018517
      *--------------------LRECR-98---------------------------------*   00018617
       FD RELATBEN                                                      00018712
           RECORDING  MODE IS F                                         00018812
           BLOCK CONTAINS 0 RECORDS.                                    00018912
                                                                        00019017
       01 FD-RELATBEN             PIC X(98).                            00019113
                                                                        00019216
      *=============================================================*   00019400
       WORKING-STORAGE                             SECTION.             00019500
      *=============================================================*   00020000
                                                                        00020117
      *----------------WRK-SRELBENE-TAMANHO 98----------------------*   00020217
       01 FILLER                                     PIC X(64) VALUE    00020317
           '-----------WRK-SRELBENE-TAMANHO 98--------------'.          00020417
      *-------------------------------------------------------------*   00020500
       01 WRK-SRELBENE.                                                 00020604
          05 WRK-ID          PIC X(05).                                 00020701
          05 FILLER          PIC X(05).                                 00020801
          05 WRK-NOME        PIC X(30).                                 00020901
          05 FILLER          PIC X(05).                                 00021001
          05 WRK-SALARIO     PIC 9(10).                                 00021101
          05 FILLER          PIC X(04).                                 00021301
          05 WRK-PLANMED     PIC X(10).                                 00021401
          05 FILLER          PIC X(04).                                 00021500
          05 WRK-PLANDENT    PIC X(10).                                 00021701
          05 FILLER          PIC X(03).                                 00021801
          05 WRK-COTAS       PIC 9(03).                                 00021901
          05 FILLER          PIC X(06).                                 00022316
          05 WRK-LIN-SR      PIC 9(02).                                 00022416
                                                                        00022517
                                                                        00022617
      *-----------------CABECALHO-----------------------------------*   00022717
       01 FILLER                                     PIC X(64) VALUE    00022817
           '------------CABECALHO---------------------------'.          00022917
      *-------------------------------------------------------------*   00023017
       01 WRK-CABEC.                                                    00023100
          05 WRK-CABEC1.                                                00023200
            10 FILLER   PIC X(13).                                      00023300
            10 FILLER   PIC X(37) VALUE                                 00023400
               'RELATORIO DE FUNCIONARIOS       PAG: '.                 00023500
            10 WRK-PAG  PIC 9(02) VALUE 1.                              00023600
            10 FILLER   PIC X(46).                                      00023713
          05 WRK-CABEC2.                                                00023800
            10 FILLER   PIC X(02) VALUE 'ID'.                           00023905
            10 FILLER   PIC X(08).                                      00024005
            10 FILLER   PIC X(04) VALUE 'NOME'.                         00024105
            10 FILLER   PIC X(31).                                      00024205
            10 FILLER   PIC X(07) VALUE 'SALARIO'.                      00024305
            10 FILLER   PIC X(07).                                      00024405
            10 FILLER   PIC X(09) VALUE 'PLANO MED'.                    00024505
            10 FILLER   PIC X(05).                                      00024605
            10 FILLER   PIC X(10) VALUE 'PLANO DENT'.                   00024705
            10 FILLER   PIC X(03).                                      00024806
            10 FILLER   PIC X(05) VALUE 'COTAS'.                        00024905
            10 FILLER   PIC X(01).                                      00025018
            10 FILLER   PIC X(03) VALUE '- '.                           00025117
            10 WRK-LINHA PIC 9(02).                                     00025220
                                                                        00025312
      *-----------------STATUS DE ARQUIVOS--------------------------*   00025617
       01 FILLER                                     PIC X(64) VALUE    00025717
           '----------- STATUS DOS ARQUIVOS--------'.                   00025817
      *-------------------------------------------------------------*   00025917
       77 WRK-FS-SRELBENE PIC 9(02).                                    00026012
                                                                        00026117
       77 WRK-FS-RELATBEN PIC 9(02).                                    00026212
                                                                        00026317
      *----------------ACUMULAR REGISTROS --------------------------*   00026417
       01 FILLER                                     PIC X(64) VALUE    00026517
           '-----------ACUMULAR REGISTROS ---------------'.             00026617
      *-------------------------------------------------------------*   00026717
       77 WRK-ACUM-LIDOS  PIC 9(03).                                    00026812
                                                                        00026917
       77 WRK-ACUM-GRAVADOS PIC 9(03).                                  00027117
                                                                        00027220
       01 WRK-BRANCO.                                                   00027321
          05 FILLER         PIC X(98) VALUE SPACES.                     00027420
      *=============================================================*   00027520
       PROCEDURE DIVISION.                                              00027620
      *=============================================================*   00027720
                                                                        00027820
      *-------------------------------------------------------------*   00027920
       0000-PRINCIPAL                                       SECTION.    00028020
                                                                        00028120
           PERFORM 1000-INICIAR.                                        00028224
           PERFORM 2000-PROCESSAR UNTIL WRK-FS-SRELBENE EQUAL 10.       00028424
           PERFORM 3000-FINALIZAR.                                      00028524
           STOP RUN.                                                    00028624
                                                                        00028720
       0000-99-FIM.                                            EXIT.    00028820
      *-------------------------------------------------------------*   00028901
                                                                        00029017
      *-------------------------------------------------------------*   00029117
       1000-INICIAR                                         SECTION.    00029217
                                                                        00029317
           OPEN INPUT SRELBENE.                                         00029424
           OPEN OUTPUT RELATBEN.                                        00029524
           PERFORM 1100-TESTARSTATUS.                                   00029624
           PERFORM 1200-CABECALHO.                                      00029724
                                                                        00029817
       1000-99-FIM.                                            EXIT.    00029917
      *-------------------------------------------------------------*   00030001
                                                                        00030117
      *-------------------------------------------------------------*   00030217
       1100-TESTARSTATUS                                    SECTION.    00030317
                                                                        00030417
           IF WRK-FS-SRELBENE NOT EQUAL 0                               00030524
              DISPLAY 'ERRO DE OPEN'                                    00030624
           END-IF.                                                      00030724
                                                                        00030901
       1100-99-FIM.                                            EXIT.    00031017
      *-------------------------------------------------------------*   00031101
                                                                        00031217
      *-------------------------------------------------------------*   00031317
       1200-CABECALHO                                       SECTION.    00031417
                                                                        00031517
           IF WRK-FS-SRELBENE EQUAL 0                                   00031624
              WRITE FD-RELATBEN FROM WRK-BRANCO                         00031724
              ADD 2 TO WRK-LINHA                                        00031824
                                                                        00031920
              IF WRK-PAG EQUAL 1                                        00032024
                 WRITE FD-RELATBEN FROM WRK-CABEC1                      00032124
              ELSE                                                      00032224
                 WRITE FD-RELATBEN FROM WRK-CABEC1 AFTER PAGE           00032324
              END-IF                                                    00032424
                                                                        00032520
           ADD 2 TO WRK-LINHA                                           00032624
           WRITE FD-RELATBEN FROM WRK-BRANCO                            00032724
           WRITE FD-RELATBEN FROM WRK-CABEC2                            00032824
           END-IF.                                                      00033024
                                                                        00033120
                                                                        00033220
           READ SRELBENE.                                               00033324
                                                                        00033417
       1200-99-FIM.                                            EXIT.    00033520
      *-------------------------------------------------------------*   00033617
                                                                        00033717
      *-------------------------------------------------------------*   00033817
       2000-PROCESSAR                                       SECTION.    00033917
                                                                        00034017
           ADD 1 TO WRK-ACUM-LIDOS.                                     00034124
           IF WRK-FS-SRELBENE NOT EQUAL 0                               00034224
              DISPLAY ' FINAL DE ARQUIVO'                               00034324
           ELSE                                                         00034424
              MOVE FD-ID        TO   WRK-ID                             00034524
              MOVE FD-NOME      TO   WRK-NOME                           00034624
              MOVE FD-SALARIO   TO   WRK-SALARIO                        00034724
              MOVE FD-PLANMED   TO   WRK-PLANMED                        00034824
              MOVE FD-PLANDENT  TO   WRK-PLANDENT                       00034924
              MOVE FD-COTAS     TO   WRK-COTAS                          00035024
              ADD  1            TO   WRK-LINHA                          00035124
              MOVE WRK-LINHA    TO   WRK-LIN-SR                         00035224
              WRITE FD-RELATBEN FROM WRK-SRELBENE                       00035424
              ADD  1            TO   WRK-ACUM-GRAVADOS                  00035524
           END-IF.                                                      00035624
                                                                        00035724
           IF WRK-LINHA EQUAL 10                                        00035824
              MOVE 0 TO WRK-LINHA                                       00035924
              ADD  1 TO WRK-PAG                                         00036024
              PERFORM 1200-CABECALHO                                    00036124
           ELSE                                                         00036224
              READ SRELBENE                                             00036324
           END-IF.                                                      00036424
                                                                        00036517
       2000-99-FIM.                                            EXIT.    00036617
      *-------------------------------------------------------------*   00036717
                                                                        00036817
      *-------------------------------------------------------------*   00036917
       3000-FINALIZAR                                       SECTION.    00037017
                                                                        00037117
           CLOSE SRELBENE.                                              00037224
           CLOSE RELATBEN.                                              00037324
                                                                        00037424
           DISPLAY 'FINAL DE ARQUIVO'.                                  00037524
           DISPLAY '---------------------------'.                       00037624
           DISPLAY 'LIDOS.... ' WRK-ACUM-LIDOS.                         00037724
           DISPLAY 'GRAVADOS. ' WRK-ACUM-GRAVADOS.                      00037824
                                                                        00037917
       3000-99-FIM.                                            EXIT.    00038017
      *-------------------------------------------------------------*   00038117
                                                                        00038217
      *-------------------------------------------------------------*   00038317
       9000-TRATARERROS                                     SECTION.    00038417
                                                                        00039000
       9000-99-FIM.                                            EXIT.    00040017
      *-------------------------------------------------------------*   00050017
