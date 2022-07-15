      *=============================================================*   00010000
       IDENTIFICATION                            DIVISION.              00020000
      *=============================================================*   00030000
                                                                        00040000
       PROGRAM-ID. FR21EX04.                                            00050099
                                                                        00060000
      *=============================================================*   00070000
      *   AUTOR...........: VICTOR ARANDA                           *   00080001
      *                                                             *   00081059
      *   DATA ...........: 31/05/2022                              *   00090099
      *                                                             *   00091059
      *   OBJETIVO........: ATUALIZAR O SALDO DE CLIENTES DE ACORDO *   00100099
      *                     COM ARQUIVO DE MOVIMENTACOES, DISTINGUI-*   00101099
      *                     NDO CLIENTES COM SALDOS ACIMA DE 10000  *   00102099
      *                     REAIS.                                  *   00103099
      *=============================================================*   00110000
      *                                                             *   00120059
      *   ARQUIVOS.....:                                            *   00122059
      *                                                             *   00123059
      *   NAME                I/O                                   *   00124059
      *                                                             *   00125099
      *   CLIENTES             I                                    *   00126099
      *   MOV0106              I                                    *   00126199
      *   MOV0106A             O                                    *   00127099
      *   MOV0106C             O                                    *   00128099
      *=============================================================*   00129059
      *                                                             *   00131059
      *   MODULOS....:                                              *   00132059
      *                                                             *   00133059
      *   NAME                                                      *   00134059
      *   GRAVALOG             GRAVA LOG DE ERROS                   *   00135059
      *   DATAEXE              OBTEM DATA E HORA ATUAL              *   00136059
      *                                                             *   00137059
      *=============================================================*   00138059
       ENVIRONMENT                               DIVISION.              00140000
      *=============================================================*   00150000
                                                                        00160000
      *-------------------------------------------------------------*   00170000
       CONFIGURATION                                        SECTION.    00180001
      *-------------------------------------------------------------*   00190000
       SPECIAL-NAMES.                                                   00200000
           DECIMAL-POINT IS COMMA.                                      00210000
                                                                        00220000
                                                                        00230000
       INPUT-OUTPUT                                         SECTION.    00240001
       FILE-CONTROL.                                                    00250000
           SELECT CLIENTES ASSIGN TO CLIENTES                           00260099
               FILE STATUS  IS WRK-FS-CLIENTES.                         00270099
                                                                        00271044
           SELECT MOV0106  ASSIGN TO MOV0106                            00272199
               FILE STATUS  IS WRK-FS-MOV0106 .                         00272299
                                                                        00272361
           SELECT MOV0106A ASSIGN TO MOV0106A                           00273099
               FILE STATUS  IS WRK-FS-MOV0106A.                         00274099
                                                                        00275044
           SELECT MOV0106C ASSIGN TO MOV0106C                           00276099
               FILE STATUS  IS WRK-FS-MOV0106C.                         00277099
                                                                        00280000
      *=============================================================*   00290000
       DATA                                                DIVISION.    00300001
      *=============================================================*   00310000
       FILE                                                 SECTION.    00320001
      *=============================================================*   00330001
                                                                        00330160
      *-------------------------------------------------------------*   00330299
      *       ARQUIVO DE ENTRADA CLIENTES LRECL = 46                *   00330399
      *-------------------------------------------------------------*   00330499
       FD CLIENTES                                                      00340099
           RECORDING  MODE IS F                                         00350000
           BLOCK CONTAINS 0 RECORDS.                                    00360000
                                                                        00370000
                                                                        00390000
       01 FD-CLIENTES.                                                  00400099
          05 FD-CHAVE1.                                                 00410099
             10 FD-AGENCIA1 PIC X(04).                                  00411099
             10 FD-CONTA1   PIC X(04).                                  00411199
          05 FD-NOME        PIC X(30).                                  00420099
          05 FD-SALDO       PIC 9(08).                                  00430099
                                                                        00440099
      *-------------------------------------------------------------*   00451099
      *       ARQUIVO DE ENTRADA MOV0106  LRECL = 47                *   00460099
      *-------------------------------------------------------------*   00460399
       FD MOV0106                                                       00460499
           RECORDING  MODE IS F                                         00460561
           BLOCK CONTAINS 0 RECORDS.                                    00460661
                                                                        00460761
                                                                        00460861
       01 FD-MOV0106.                                                   00460999
          05 FD-CHAVE2.                                                 00461199
             10 FD-AGENCIA2  PIC X(04).                                 00461299
             10 FD-CONTA2    PIC X(04).                                 00461399
          05 FD-MOVIMENTACAO PIC X(30).                                 00461499
          05 FD-VALOR-MOV    PIC 9(08).                                 00461599
          05 FD-TIPO-MOV     PIC X(01).                                 00461699
                                                                        00461999
      *-------------------------------------------------------------*   00462099
      *       ARQUIVO DE SAIDA   MOV0106A LRECL = 46                *   00462199
      *-------------------------------------------------------------*   00462299
       FD MOV0106A                                                      00462399
           RECORDING  MODE IS F                                         00462499
           BLOCK CONTAINS 0 RECORDS.                                    00463045
                                                                        00463158
       01 FD-MOV0106A  PIC X(46).                                       00465099
                                                                        00466099
      *-------------------------------------------------------------*   00467099
      *       ARQUIVO DE SAIDA   MOV0106C LRECL = 46                *   00467199
      *-------------------------------------------------------------*   00467299
       FD MOV0106C                                                      00467499
           RECORDING  MODE IS F                                         00467599
           BLOCK CONTAINS 0 RECORDS.                                    00467699
                                                                        00467799
       01 FD-MOV0106C  PIC X(46).                                       00467899
                                                                        00469045
      *-------------------------------------------------------------*   00470000
       WORKING-STORAGE                                      SECTION.    00480002
      *-------------------------------------------------------------*   00490000
                                                                        00490130
      *-------------------------------------------------------------*   00490299
       01 FILLER                                     PIC X(46) VALUE    00490399
                          ' LOG DE ERROS '.                             00490499
      *-------------------------------------------------------------*   00490599
       01 WRK-DADOS.                                                    00491099
           05 WRK-PROGRAMA  PIC X(08).                                  00492099
           05 WRK-SECAO     PIC X(04).                                  00493099
           05 WRK-MENSAGEM  PIC X(30).                                  00494099
           05 WRK-STATUS    PIC X(02).                                  00496099
                                                                        00497099
      *-------------------------------------------------------------*   00502099
       01 FILLER                                     PIC X(46) VALUE    00504099
                            ' LOGICA '.                                 00505099
      *-------------------------------------------------------------*   00506099
       77 WRK-LIDOS           PIC 9(03).                                00530061
                                                                        00530138
       77 WRK-GRAVADOS        PIC 9(03).                                00530261
                                                                        00530399
       77 WRK-GRAVADOS-A      PIC 9(03).                                00530499
                                                                        00530599
       77 WRK-GRAVADOS-C      PIC 9(03).                                00530799
                                                                        00530899
       77 WRK-LINHA-VAZIA     PIC X(01) VALUE SPACE.                    00531065
                                                                        00532066
       77 WRK-MSG-FINAL       PIC X(17) VALUE 'FINAL DE PROGRAMA'.      00533066
                                                                        00533199
      *-------------------------------------------------------------*   00541099
       01 FILLER                                     PIC X(46) VALUE    00542099
                            ' STATUS '.                                 00543099
      *-------------------------------------------------------------*   00544099
       77 WRK-FS-CLIENTES PIC 9(02).                                    00550099
                                                                        00550161
       77 WRK-FS-MOV0106  PIC 9(02).                                    00550299
                                                                        00551045
       77 WRK-FS-MOV0106A PIC 9(02).                                    00560099
                                                                        00561099
       77 WRK-FS-MOV0106C PIC 9(02).                                    00580099
                                                                        00590010
      *-------------------------------------------------------------*   00601099
       01 FILLER                                     PIC X(46) VALUE    00601199
                         ' LAYOUT SAIDA '.                              00601299
      *-------------------------------------------------------------*   00601399
       01 WRK-SAIDA.                                                    00601479
           05 WRK-CHAVE-SAI PIC X(08).                                  00601599
           05 WRK-NOME-SAI  PIC X(30).                                  00601699
           05 WRK-SALDO-SAI PIC 9(08).                                  00601799
                                                                        00601898
      *-------------------------------------------------------------*   00602010
       LINKAGE                                              SECTION.    00610010
      *-------------------------------------------------------------*   00620010
       01 LNK-INFO-DATA.                                                00640099
          02 LINK-DATA       PIC X(23).                                 00641099
          02 LINK-HORA       PIC X(8).                                  00642099
                                                                        00650010
      *=============================================================*   00690001
       PROCEDURE DIVISION USING LNK-INFO-DATA.                          00700009
      *=============================================================*   00710000
                                                                        00720001
      *=============================================================*   00730001
       0000-PRINCIPAL                                       SECTION.    00740001
                                                                        00760001
            PERFORM 1000-INICIAR.                                       00770000
            PERFORM 1500-TESTAR-VAZIO.                                  00771099
            PERFORM 2000-PROCESSAR UNTIL WRK-FS-CLIENTES EQUAL 10       00780099
                                   AND   WRK-FS-MOV0106  EQUAL 10.      00781099
            PERFORM 3000-FINALIZAR.                                     00790000
                                                                        00800071
       0000-99-FIM.                                            EXIT.    00830006
      *=============================================================*   00840001
                                                                        00850000
                                                                        00860001
                                                                        00870001
      *=============================================================*   00880001
       1000-INICIAR                                         SECTION.    00890001
                                                                        00910000
            OPEN INPUT  CLIENTES.                                       00920099
            OPEN INPUT  MOV0106.                                        00920199
                                                                        00920299
            OPEN OUTPUT MOV0106A.                                       00921099
            OPEN OUTPUT MOV0106C.                                       00921199
                                                                        00922058
            PERFORM 1200-TESTARSTATUS.                                  00922199
                                                                        00924099
       1000-99-FIM.                                            EXIT.    00970006
      *=============================================================*   00980001
                                                                        00990001
                                                                        01000001
      *=============================================================*   01001058
       1500-TESTAR-VAZIO                                    SECTION.    01002099
                                                                        01003058
              READ CLIENTES.                                            01008099
              READ MOV0106.                                             01008199
                                                                        01008261
              IF WRK-FS-CLIENTES NOT EQUAL ZERO OR                      01008399
                 WRK-FS-MOV0106  NOT EQUAL ZERO                         01008499
                                                                        01008567
                 DISPLAY WRK-MSG-FINAL                                  01009399
                 STOP RUN                                               01009499
              END-IF.                                                   01009558
                                                                        01009658
       1500-99-FIM.                                            EXIT.    01009761
      *=============================================================*   01009858
                                                                        01010001
                                                                        01011058
      *=============================================================*   01020001
       1200-TESTARSTATUS                                    SECTION.    01030058
                                                                        01050001
               IF WRK-FS-CLIENTES NOT EQUAL ZEROS                       01060099
                                                                        01062068
                  MOVE 'FR21EX03'                  TO WRK-PROGRAMA      01070099
                  MOVE '1000'                      TO WRK-SECAO         01080099
                  MOVE 'ERRO OPEN CLIENTES       ' TO WRK-MENSAGEM      01090099
                  MOVE WRK-FS-CLIENTES             TO WRK-STATUS        01100099
                  PERFORM 9000-TRATARERROS                              01110099
               END-IF.                                                  01120000
                                                                        01130001
               IF WRK-FS-MOV0106  NOT EQUAL ZEROS                       01131099
                                                                        01132075
                  MOVE 'FR21EX03'                  TO WRK-PROGRAMA      01133099
                  MOVE '1000'                      TO WRK-SECAO         01134099
                  MOVE 'ERRO OPEN MOVIMENTACAO   ' TO WRK-MENSAGEM      01135099
                  MOVE WRK-FS-MOV0106              TO WRK-STATUS        01136099
                  PERFORM 9000-TRATARERROS                              01137099
               END-IF.                                                  01138075
                                                                        01140031
       1200-99-FIM.                                            EXIT.    01150058
      *=============================================================*   01160001
                                                                        01170001
                                                                        01190001
      *=============================================================*   01200001
       2000-PROCESSAR                                       SECTION.    01210001
                                                                        01700031
           EVALUATE TRUE                                                01701099
                                                                        01701185
           WHEN FD-CHAVE1 LESS FD-CHAVE2                                01701299
                PERFORM 2100-GRAVA-ARQUIVO                              01702599
                ADD 1 TO WRK-LIDOS                                      01702699
                READ CLIENTES                                           01702799
                                                                        01702999
           WHEN FD-CHAVE1 EQUAL FD-CHAVE2                               01703099
                PERFORM 2200-COMPUTA-SALDO                              01703299
                READ MOV0106                                            01703999
                                                                        01704099
           END-EVALUATE.                                                01707086
                                                                        01707181
                IF WRK-FS-MOV0106  NOT EQUAL 0                          01707399
                   MOVE HIGH-VALUES TO FD-CHAVE2                        01707599
                                                                        01707699
               END-IF.                                                  01707799
                                                                        01707899
       2000-99-FIM.                                            EXIT.    01710006
      *=============================================================*   01720001
                                                                        01720199
      *=============================================================*   01720299
       2100-GRAVA-ARQUIVO                                   SECTION.    01720399
                                                                        01720499
      *                 GRAVAMENTO DOS ARQUIVOS                         01720599
                                                                        01720699
                                                                        01720799
               MOVE FD-CHAVE1       TO WRK-CHAVE-SAI.                   01720899
               MOVE FD-NOME         TO WRK-NOME-SAI.                    01720999
               MOVE FD-SALDO        TO WRK-SALDO-SAI.                   01721099
                                                                        01721199
               IF WRK-SALDO-SAI IS GREATER THAN OR EQUAL TO 10000       01721299
                  MOVE WRK-SAIDA    TO FD-MOV0106A                      01721399
                  WRITE FD-MOV0106A                                     01721499
                  ADD 1 TO WRK-GRAVADOS-A                               01721599
                                                                        01721699
               ELSE                                                     01721799
                  MOVE WRK-SAIDA    TO FD-MOV0106C                      01721899
                  WRITE FD-MOV0106C                                     01721999
                  ADD 1 TO WRK-GRAVADOS-C                               01722099
               END-IF.                                                  01722199
                                                                        01722299
       2100-99-FIM.                                            EXIT.    01722399
      *=============================================================*   01722499
                                                                        01722599
      *=============================================================*   01722699
       2200-COMPUTA-SALDO                                   SECTION.    01722799
                                                                        01722899
      *                 ATUALIZACAO DE SALDO                            01722999
                                                                        01723099
           IF FD-TIPO-MOV EQUAL 'C'                                     01723199
              ADD FD-VALOR-MOV TO FD-SALDO                              01723299
                                                                        01723399
           ELSE                                                         01724099
              IF FD-VALOR-MOV IS LESS THAN OR EQUAL TO FD-SALDO         01725099
                 SUBTRACT FD-VALOR-MOV FROM FD-SALDO                    01726099
                                                                        01726199
              END-IF                                                    01727099
           END-IF.                                                      01728099
                                                                        01729099
       2200-99-FIM.                                            EXIT.    01760099
      *=============================================================*   01770099
                                                                        01770199
      *=============================================================*   01771099
       3000-FINALIZAR                                       SECTION.    01772099
                                                                        01773099
      *                 DISPLAY DE ESTATISTICAS                         01773199
                                                                        01773299
             ADD WRK-GRAVADOS-A TO WRK-GRAVADOS-C GIVING WRK-GRAVADOS.  01774099
             IF  WRK-GRAVADOS NOT EQUAL WRK-LIDOS                       01780099
                 DISPLAY 'ERRO DE PROCESSAMENTO'                        01790099
             END-IF.                                                    01791099
                                                                        01792099
             DISPLAY 'ARQUIVOS LIDOS....' WRK-LIDOS.                    01800099
             DISPLAY 'ARQUIVOS GRAVADOS.' WRK-GRAVADOS.                 01810099
             DISPLAY 'GRAVADOS EM MOV0106A.' WRK-GRAVADOS-A.            01811099
             DISPLAY 'GRAVADOS EM MOV0106C.' WRK-GRAVADOS-C.            01812099
             DISPLAY WRK-MSG-FINAL.                                     01813099
             CLOSE CLIENTES MOV0106.                                    01813199
             CLOSE MOV0106A MOV0106C.                                   01813399
                                                                        01813599
                                                                        01814058
      *                 DISPLAY DE HORARIO                              01815058
                                                                        01816058
             CALL 'DATAEXT' USING LNK-INFO-DATA.                        01820099
             DISPLAY WRK-LINHA-VAZIA.                                   01830099
             DISPLAY LINK-DATA.                                         01840099
             DISPLAY LINK-HORA.                                         01841099
             DISPLAY WRK-LINHA-VAZIA.                                   01850099
             STOP RUN.                                                  01851099
                                                                        01852075
       3000-99-FIM.                                            EXIT.    01860006
      *=============================================================*   01870001
                                                                        01880001
                                                                        01900001
      *=============================================================*   01910001
       9000-TRATARERROS                                     SECTION.    01920001
                                                                        01930031
                                                                        01940001
             CALL 'GRAVALOG' USING WRK-DADOS.                           01950000
             STOP RUN.                                                  01960099
                                                                        01970001
                                                                        01980031
       9000-99-FIM.                                            EXIT.    01990006
      *=============================================================*   02000001
