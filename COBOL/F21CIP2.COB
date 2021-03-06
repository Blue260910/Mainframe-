      *===================================================              00010000
       IDENTIFICATION                            DIVISION.              00020000
      *===================================================              00030000
                                                                        00031000
       PROGRAM-ID. F21CIP2.                                             00032054
      *=============================================================*   00033015
      *   AUTOR....:VICTOR ARANDA                                   *   00033115
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00033215
      *   DATA ....:29/06/2022                                      *   00033382
      *-------------------------------------------------------------*   00033415
      *   OBJETIVO:FAZER UMA CONSULTA DE FUNCIONARIO PELO CICS      *   00033515
      *            UTILIZANDO UM ARQUIVO VISAM                      *   00033654
      *                                                             *   00033715
      *-------------------------------------------------------------*   00033815
      *   BASE DE DADOS:                                            *   00033915
      *   TABELA.DB2..         I/O                                  *   00034015
      *                                           INCLUDE/BOOK      *   00034115
      *                                            -----------      *   00034215
      *                                                             *   00034315
      *-------------------------------------------------------------*   00034415
      *   ARQUIVOS:                                                 *   00034515
      *    DDNAME              I/O                  COPY/BOOK       *   00034615
      *                                            -----------      *   00034715
      *=============================================================*   00035015
                                                                        00040400
      *===================================================              00040500
       ENVIRONMENT                               DIVISION.              00040600
      *===================================================              00040700
       CONFIGURATION                              SECTION.              00040800
       SPECIAL-NAMES.                                                   00040900
           DECIMAL-POINT IS COMMA.                                      00041030
                                                                        00042000
      *===================================================              00043000
       DATA                                      DIVISION.              00044000
      *==================================================               00044100
       WORKING-STORAGE                            SECTION.              00044200
                                                                        00044300
           COPY DFHAID.                                                 00045715
                                                                        00046115
        01 WRK-ENTRADA.                                                 00047183
           05 WRK-TRANS           PIC X(04).                            00047399
           05 WRK-DADOS.                                                00047499
              10 WRK-CHAVE           PIC X(05).                         00047599
              10 WRK-CONTENT.                                           00047699
                 15 WRK-NOME      PIC X(30) VALUE SPACES.               00047799
                 15 WRK-SETOR     PIC X(04) VALUE SPACES.               00047899
                 15 WRK-SALARIO   PIC X(10) VALUE SPACES.               00047999
                 15 WRK-DATAADM   PIC X(10) VALUE SPACES.               00048099
                 15 WRK-EMAIL     PIC X(40) VALUE SPACES.               00048199
           05 WRK-STATUS          PIC X(01) VALUE SPACES.               00048799
                                                                        00048883
                                                                        00049083
         77 WRK-RESP     PIC S9(04) COMP.                               00049183
                                                                        00049554
      *===================================================              00049630
       PROCEDURE                                 DIVISION.              00049730
      *===================================================              00049830
      *---------------------------------------------------              00049930
       0000-PRINCIPAL                              SECTION.             00050030
      *---------------------------------------------------              00050130
            PERFORM 1000-INICIALIZAR.                                   00050230
                                                                        00050363
            PERFORM 2000-PROCESSAR                                      00050583
                                                                        00050763
            PERFORM 3000-FINALIZAR.                                     00050863
                                                                        00052083
      *---------------------------------------------------              00052130
       0000-99-FIM.                                EXIT.                00052230
      *----------------------------------------------------             00052330
       1000-INICIALIZAR                          SECTION.               00052430
      *----------------------------------------------------             00052530
           INITIALIZE  WRK-DADOS.                                       00052690
                                                                        00052783
           EXEC CICS RECEIVE                                            00052883
                INTO(WRK-ENTRADA)                                       00052983
           END-EXEC.                                                    00053083
                                                                        00053183
      *---------------------------------------------------              00053963
       1000-99-FIM.                                EXIT.                00054063
      *----------------------------------------------------             00054163
      *-----------------------------------------------------            00054263
       2000-PROCESSAR                            SECTION.               00054363
      *-----------------------------------------------------            00054463
                                                                        00055383
           EXEC CICS READ                                               00055483
                FILE('FUNC')                                            00056083
                RIDFLD(WRK-CHAVE)                                       00060883
                INTO(WRK-DADOS)                                         00060983
                RESP(WRK-RESP)                                          00061083
           END-EXEC.                                                    00061299
                                                                        00061383
           EVALUATE WRK-RESP                                            00061684
                                                                        00061798
             WHEN DFHRESP(NORMAL)                                       00061884
                  MOVE '1' TO WRK-STATUS                                00061984
                  EVALUATE TRUE                                         00062097
                                                                        00062198
                  WHEN WRK-SALARIO EQUAL ZEROES                         00062297
                       MOVE '3' TO WRK-STATUS                           00062397
                                                                        00062598
                  WHEN WRK-EMAIL EQUAL SPACES                           00062697
                       MOVE '4' TO WRK-STATUS                           00062797
                  END-EVALUATE                                          00062997
                                                                        00063098
             WHEN DFHRESP(NOTFND)                                       00063184
                  MOVE '2' TO WRK-STATUS                                00063284
                                                                        00063498
             WHEN OTHER                                                 00063584
                  MOVE '5' TO WRK-STATUS                                00063684
                                                                        00063898
           END-EVALUATE.                                                00063984
                                                                        00064084
           IF WRK-STATUS NOT EQUAL 1                                    00064198
              INITIALIZE  WRK-CONTENT                                   00064298
                                                                        00064399
                                                                        00064498
      *---------------------------------------------------              00064837
       2000-99-FIM.                                EXIT.                00064937
      *----------------------------------------------------             00065037
                                                                        00420043
      *--------------------------------------------------               00430076
       3000-FINALIZAR                            SECTION.               00440043
      *--------------------------------------------------               00450043
                                                                        00460043
            EXEC CICS SEND                                              00460183
                 FROM(WRK-ENTRADA)                                      00460283
                 ERASE                                                  00460383
            END-EXEC.                                                   00460483
                                                                        00460583
            EXEC CICS                                                   00460683
                 RETURN                                                 00460783
            END-EXEC.                                                   00460883
      *--------------------------------------------------               00520043
       3000-99-FIM.                                 EXIT.               00530043
      *---------------------------------------------------              00540043
