      *===================================================              00010000
       IDENTIFICATION                            DIVISION.              00020000
      *===================================================              00030000
                                                                        00031000
       PROGRAM-ID. F21CIP1.                                             00032008
      *=============================================================*   00033015
      *   AUTOR....:VICTOR ARANDA                                   *   00033115
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00033215
      *   DATA ....:22/06/2022                                      *   00033315
      *-------------------------------------------------------------*   00033415
      *   OBJETIVO:FAZER UMA CONSULTA DE FUNCIONARIO PELO CICS      *   00033515
      *                                                             *   00033615
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
           EXEC SQL                                                     00044415
              INCLUDE #BKFUNC                                           00044915
           END-EXEC.                                                    00045015
                                                                        00045115
           EXEC SQL                                                     00045215
              INCLUDE SQLCA                                             00045315
           END-EXEC.                                                    00045415
                                                                        00045515
           COPY F21CIM1.                                                00045615
           COPY DFHAID.                                                 00045715
                                                                        00046115
        77 WRK-ID            PIC 9(05) VALUE ZEROS.                     00046229
        77 WRK-SQLCODE       PIC -999.                                  00046329
        77 WRK-NULL-EMAIL    PIC S9(4) COMP.                            00046429
        77 WRK-POS           PIC 9(02).                                 00047046
        77 WRK-STATUS-MAP    PIC S9(04) COMP.                           00047154
        77 WRK-FLAG          PIC 9(1).                                  00047254
        77 WRK-SAIDA         PIC X(30) VALUE 'FIM DE PROGRAMA'.         00047354
                                                                        00047454
        77 WRK-SALARIO       PIC ZZ.ZZZ.ZZ9,99.                         00047839
                                                                        00047930
        01 WRK-DATA.                                                    00048030
           05 WRK-ANO    PIC X(04).                                     00048137
           05 FILLER     PIC X(01) VALUE '-'.                           00048237
           05 WRK-MES    PIC X(02).                                     00048337
           05 FILLER     PIC X(01) VALUE '-'.                           00048437
           05 WRK-DIA    PIC X(02).                                     00048530
                                                                        00048630
      *===================================================              00048830
       PROCEDURE                                 DIVISION.              00048930
      *===================================================              00049030
      *---------------------------------------------------              00049130
       0000-PRINCIPAL                              SECTION.             00049230
      *---------------------------------------------------              00049330
            PERFORM 1000-INICIALIZAR.                                   00049458
                                                                        00049554
            IF WRK-FLAG EQUAL 1                                         00049654
               PERFORM 2000-PROCESSAR                                   00049754
            END-IF.                                                     00049854
                                                                        00049954
            PERFORM 3000-FINALIZAR.                                     00050054
                                                                        00050154
            EXEC CICS                                                   00050254
                 RETURN TRANSID('T211')                                 00050354
            END-EXEC.                                                   00050454
                                                                        00050554
      *---------------------------------------------------              00050830
       0000-99-FIM.                                EXIT.                00050930
      *----------------------------------------------------             00051030
       1000-INICIALIZAR                         SECTION.                00051158
      *----------------------------------------------------             00051230
                                                                        00051354
            EXEC CICS RECEIVE                                           00051454
                  MAPSET('F21CIM1')                                     00051554
                  MAP('MAPA01')                                         00051654
                  INTO(MAPA01I)                                         00051754
                  RESP(WRK-STATUS-MAP)                                  00051854
            END-EXEC.                                                   00051954
                                                                        00052054
            IF WRK-STATUS-MAP EQUAL DFHRESP(MAPFAIL)                    00052154
               MOVE 2 TO WRK-FLAG                                       00052254
            ELSE                                                        00052454
               MOVE 1 TO WRK-FLAG                                       00052554
            END-IF.                                                     00052654
                                                                        00052908
      *---------------------------------------------------              00053000
       1000-99-FIM.                                EXIT.                00053100
      *----------------------------------------------------             00053200
      *-----------------------------------------------------            00053300
       2000-PROCESSAR                            SECTION.               00053400
      *-----------------------------------------------------            00053500
           INITIALIZE MAPA01O.                                          00053661
           INITIALIZE MAPA01I.                                          00053761
                                                                        00053861
           EVALUATE TRUE                                                00053938
               WHEN EIBAID = '5'                                        00054038
                    PERFORM 2100-PESQUISAR                              00055038
                                                                        00060837
               WHEN EIBAID = '6'                                        00060938
                    PERFORM 2400-VALIDACAO                              00061043
                    IF WRK-POS EQUAL 7                                  00061143
                       PERFORM 2200-INSERIR                             00061243
                    END-IF                                              00061543
                                                                        00061643
               WHEN EIBAID = '7'                                        00061741
                    PERFORM 2300-DELETAR                                00062062
                                                                        00062261
               WHEN EIBAID = '8'                                        00062360
                    PERFORM 2400-VALIDACAO                              00062561
                    IF WRK-POS EQUAL 7                                  00062661
                       PERFORM 2500-UPDATE                              00062761
                    END-IF                                              00062861
                                                                        00062961
                                                                        00063061
      *                      SAIR DO PROGRAMA                           00063137
               WHEN EIBAID = '3'                                        00063238
                    EXEC CICS SEND                                      00063354
                         FROM(WRK-SAIDA)                                00063454
                         ERASE                                          00063554
                    END-EXEC                                            00063654
                                                                        00063754
                    EXEC CICS                                           00063854
                         RETURN                                         00063954
                    END-EXEC                                            00064054
                                                                        00064154
                                                                        00064237
      *                      LIMPAR A TELA                              00064337
               WHEN EIBAID = ''''                                       00064456
                    INITIALIZE MAPA01O                                  00064554
                    INITIALIZE MAPA01I                                  00064659
                                                                        00064738
            END-EVALUATE.                                               00064838
                                                                        00064937
      *---------------------------------------------------              00065037
       2000-99-FIM.                                EXIT.                00065137
      *----------------------------------------------------             00065237
      *--------------------------------------------------               00065337
       2100-PESQUISAR                            SECTION.               00065437
      *--------------------------------------------------               00065537
                                                                        00065637
              MOVE IDI TO DB2-ID.                                       00065737
                EXEC SQL                                                00065837
                  SELECT ID,NOME,SETOR,SALARIO,DATAADM,EMAIL            00065937
                  INTO  :DB2-ID,                                        00066037
                        :DB2-NOME,                                      00066137
                        :DB2-SETOR,                                     00066237
                        :DB2-SALARIO,                                   00066337
                        :DB2-DATAADM,                                   00066437
                        :DB2-EMAIL  :WRK-NULL-EMAIL                     00066537
                  FROM IVAN.FUNC                                        00066637
                   WHERE ID =:DB2-ID                                    00066737
                   FETCH FIRST ROW ONLY                                 00066837
                END-EXEC.                                               00066937
                                                                        00067038
           EVALUATE TRUE                                                00067138
           WHEN (SQLCODE NOT EQUAL ZEROS AND 100) OR                    00067238
                (SQLWARN0 EQUAL 'W')                                    00067338
                                                                        00067437
              MOVE SQLCODE TO WRK-SQLCODE                               00067537
              MOVE    'ERRO NA LEITURA ' TO MSGO                        00067637
                                                                        00067738
                                                                        00067837
           WHEN (SQLCODE EQUAL 100)                                     00067938
               INITIALIZE MAPA01O                                       00068061
               INITIALIZE MAPA01I                                       00068161
               MOVE    'REGISTRO: ' TO MSGO                             00068261
               MOVE     DB2-ID      TO MSGO(11:5)                       00068361
               MOVE    'NAO ENCONTRADO ' TO MSGO(17:15)                 00068461
                                                                        00068561
           WHEN (SQLCODE EQUAL 0)                                       00068638
               MOVE    'ENCONTRADO     ' TO MSGO                        00068759
              MOVE DB2-NOME           TO NOMEO                          00068840
              MOVE DB2-SETOR          TO SETORO                         00068940
              MOVE DB2-SALARIO        TO WRK-SALARIO                    00069040
              MOVE WRK-SALARIO        TO SALARIOO                       00069140
              MOVE DB2-DATAADM        TO WRK-DATA                       00069240
              MOVE WRK-DIA            TO DATADIAO                       00069340
              MOVE WRK-MES            TO DATAMESO                       00069440
              MOVE WRK-ANO            TO DATAANOO                       00069540
                                                                        00069637
           IF WRK-NULL-EMAIL = 0                                        00069737
                 MOVE DB2-EMAIL       TO EMAILO                         00069840
           ELSE                                                         00069937
                 MOVE 'EMAIL NULO'    TO EMAILO                         00070040
           END-IF                                                       00070137
                                                                        00070237
           END-EVALUATE.                                                00070338
      *--------------------------------------------------               00070437
       2100-99-FIM.                                 EXIT.               00070537
      *---------------------------------------------------              00070637
                                                                        00070737
      *--------------------------------------------------               00070837
       2200-INSERIR                              SECTION.               00070937
      *--------------------------------------------------               00071037
                                                                        00072043
           MOVE  IDI             TO DB2-ID.                             00073037
           MOVE  NOMEI           TO DB2-NOME.                           00074037
           MOVE  SETORI          TO DB2-SETOR.                          00075037
           MOVE  SALARIOI        TO DB2-SALARIO.                        00076037
           MOVE  DATADIAI        TO WRK-DIA.                            00076137
           MOVE  DATAMESI        TO WRK-MES.                            00076237
           MOVE  DATAANOI        TO WRK-ANO.                            00076337
           MOVE  WRK-DATA        TO DB2-DATAADM.                        00077037
           MOVE  EMAILI          TO DB2-EMAIL.                          00078037
                                                                        00078137
           EXEC SQL                                                     00078237
           INSERT INTO                                                  00078337
                  IVAN.FUNC(ID,NOME,SETOR,SALARIO,DATAADM,EMAIL)        00079037
                  VALUES(:DB2-ID,                                       00079137
                         :DB2-NOME,                                     00079237
                         :DB2-SETOR,                                    00079337
                         :DB2-SALARIO,                                  00079437
                         :DB2-DATAADM,                                  00079537
                         :DB2-EMAIL)                                    00080037
                END-EXEC.                                               00090037
                                                                        00100037
           EVALUATE TRUE                                                00100138
           WHEN (SQLCODE NOT EQUAL ZEROS AND 100) OR                    00100238
                (SQLWARN0 EQUAL 'W')                                    00100338
                                                                        00100438
              MOVE SQLCODE TO WRK-SQLCODE                               00100538
              MOVE 'ERRO NA GRAVACAO: ' TO MSGO                         00100648
              MOVE WRK-SQLCODE TO MSGO(19:4)                            00100748
                                                                        00101238
           WHEN (SQLCODE EQUAL 0)                                       00101338
              EXEC SQL                                                  00101461
                   COMMIT                                               00102061
              END-EXEC                                                  00103061
              MOVE    'REGISTRO: ' TO MSGO                              00104061
              MOVE     DB2-ID      TO MSGO(11:5)                        00105061
              MOVE    'INSERIDO'   TO MSGO(17:8)                        00106061
                                                                        00106141
           END-EVALUATE.                                                00107045
                                                                        00108043
                                                                        00120045
      *--------------------------------------------------               00130037
       2200-99-FIM.                                 EXIT.               00140037
      *---------------------------------------------------              00150037
                                                                        00160037
      *--------------------------------------------------               00170037
       2300-DELETAR                              SECTION.               00180041
      *--------------------------------------------------               00190037
                                                                        00200037
           MOVE IDI  TO  DB2-ID.                                        00210042
                                                                        00211041
           EXEC SQL                                                     00212042
               DELETE FROM IVAN.FUNC WHERE ID = :DB2-ID                 00213041
           END-EXEC.                                                    00214042
                                                                        00215041
           EVALUATE SQLCODE                                             00216042
            WHEN 0                                                      00217041
                 MOVE    'REGISTRO: ' TO MSGO                           00218041
                 MOVE     DB2-ID      TO MSGO(11:5)                     00218141
                 MOVE    'DELETADO'   TO MSGO(17:8)                     00218241
            WHEN 100                                                    00219041
                 MOVE    'REGISTRO: ' TO MSGO                           00219141
                 MOVE     DB2-ID      TO MSGO(11:5)                     00219241
                 MOVE    'NAO ENCONTRADO ' TO MSGO(17:15)               00219341
            WHEN OTHER                                                  00219441
                 MOVE SQLCODE TO WRK-SQLCODE                            00219541
                 MOVE WRK-SQLCODE TO MSGO                               00220041
           END-EVALUATE.                                                00230042
                                                                        00240041
                                                                        00250037
      *--------------------------------------------------               00260037
       2300-99-FIM.                                 EXIT.               00270041
      *---------------------------------------------------              00280037
                                                                        00290041
      *--------------------------------------------------               00300041
       2400-VALIDACAO                            SECTION.               00310043
      *--------------------------------------------------               00320041
           MOVE 7 TO WRK-POS.                                           00321046
           MOVE 'ERRO: ' TO MSGO.                                       00330043
                                                                        00350043
           IF IDL EQUAL 0 OR  IDI EQUAL ZEROES                          00351061
              MOVE 'ID,' TO MSGO(WRK-POS:3)                             00352061
              ADD 3 TO WRK-POS                                          00353061
           END-IF.                                                      00354061
                                                                        00355043
           IF NOMEL EQUAL 0 OR  NOMEI  EQUAL SPACES                     00356061
              MOVE 'NOME,' TO MSGO(WRK-POS:5)                           00357043
              ADD 5 TO WRK-POS                                          00358043
           END-IF.                                                      00359043
                                                                        00359143
           IF SETORL EQUAL 0 OR SETORI  EQUAL SPACES                    00360061
              MOVE 'SETOR,' TO MSGO(WRK-POS:6)                          00370043
              ADD 6 TO WRK-POS                                          00380043
           END-IF.                                                      00380143
                                                                        00380243
           IF SALARIOL EQUAL 0 OR SALARIOI EQUAL ZEROES                 00380361
              MOVE 'SALARIO,' TO MSGO(WRK-POS:8)                        00380443
              ADD 8 TO WRK-POS                                          00381043
           END-IF.                                                      00382043
                                                                        00383043
           IF DATAANOL EQUAL ZERO OR DATAMESL EQUAL ZERO                00384061
                                  OR DATADIAL EQUAL ZERO                00385061
              MOVE 'DATAADM,'  TO MSGO(WRK-POS:8)                       00386047
              ADD 8 TO WRK-POS                                          00387043
           END-IF.                                                      00390043
                                                                        00390143
           IF WRK-POS NOT EQUAL 7                                       00390243
              SUBTRACT 1 FROM WRK-POS                                   00390343
              MOVE '.' TO MSGO(WRK-POS:1)                               00390443
              ADD 1 TO WRK-POS                                          00390543
           END-IF.                                                      00390643
                                                                        00390743
      *---------------------------------------------------              00391043
       2400-99-FIM.                                 EXIT.               00400043
      *---------------------------------------------------              00410041
      *--------------------------------------------------               00411060
       2500-UPDATE                               SECTION.               00412060
      *--------------------------------------------------               00413060
                                                                        00414060
            MOVE  IDI             TO DB2-ID.                            00414161
            MOVE  NOMEI           TO DB2-NOME.                          00414261
            MOVE  SETORI          TO DB2-SETOR.                         00414361
            MOVE  SALARIOI        TO DB2-SALARIO.                       00414461
            MOVE  DATADIAI        TO WRK-DIA.                           00414561
            MOVE  DATAMESI        TO WRK-MES.                           00414661
            MOVE  DATAANOI        TO WRK-ANO.                           00414761
            MOVE  WRK-DATA        TO DB2-DATAADM.                       00414861
            MOVE  EMAILI          TO DB2-EMAIL.                         00414961
                                                                        00420160
                                                                        00420261
            EXEC SQL                                                    00420361
                 UPDATE IVAN.FUNC SET NOME    = :DB2-NOME,              00420461
                                      SETOR   = :DB2-SETOR,             00420561
                                      SALARIO = :DB2-SALARIO,           00420661
                                      DATAADM = :DB2-DATAADM,           00420761
                                      EMAIL   = :DB2-EMAIL              00420861
                                 WHERE ID= :DB2-ID                      00420961
            END-EXEC.                                                   00421061
                                                                        00421161
                                                                        00421261
            EVALUATE TRUE                                               00421361
            WHEN (SQLCODE NOT EQUAL ZEROS AND 100) OR                   00421461
                 (SQLWARN0 EQUAL 'W')                                   00421561
                                                                        00421661
            MOVE SQLCODE TO WRK-SQLCODE                                 00421761
            MOVE WRK-SQLCODE TO MSGO(19:4)                              00421961
                                                                        00422061
            WHEN (SQLCODE EQUAL 0)                                      00422161
                 EXEC SQL                                               00422261
                      COMMIT                                            00422361
                 END-EXEC                                               00422461
            MOVE    'REGISTRO: '  TO MSGO                               00422561
            MOVE     DB2-ID       TO MSGO(11:5)                         00422661
            MOVE    'ATUALIZADO ' TO MSGO(17:11)                        00422761
                                                                        00422861
            END-EVALUATE.                                               00422961
                                                                        00423061
      *--------------------------------------------------               00423860
       2500-99-FIM.                                 EXIT.               00423960
                                                                        00424043
      *--------------------------------------------------               00430043
       3000-FINALIZAR                            SECTION.               00440043
      *--------------------------------------------------               00450043
                                                                        00460043
           IF WRK-FLAG EQUAL 2                                          00470054
              EXEC CICS SEND                                            00471058
                   MAPSET('F21CIM1')                                    00472058
                   MAP('MAPA01')                                        00473058
                   ERASE                                                00473158
                   MAPONLY                                              00473258
              END-EXEC                                                  00473358
           ELSE                                                         00474054
              EXEC CICS SEND                                            00475058
                   MAPSET('F21CIM1')                                    00476058
                   MAP('MAPA01')                                        00480058
                   DATAONLY                                             00481058
                   FROM(MAPA01O)                                        00482058
              END-EXEC                                                  00490058
           END-IF.                                                      00500058
                                                                        00510043
      *--------------------------------------------------               00520043
       3000-99-FIM.                                 EXIT.               00530043
      *---------------------------------------------------              00540043
