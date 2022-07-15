      *=============================================================*   00010000
       IDENTIFICATION                            DIVISION.              00020000
      *=============================================================*   00030000
                                                                        00040000
       PROGRAM-ID. FR21EX02.                                            00050000
                                                                        00060000
      *=============================================================*   00070000
      *   AUTOR...........: VICTOR ARANDA                           *   00080000
      *                                                             *   00081000
      *   DATA ...........: 30/05/2022                              *   00090000
      *                                                             *   00091000
      *   OBJETIVO........: BALANCE LINE                            *   00100000
      *                                                             *   00101000
      *                                                             *   00102000
      *=============================================================*   00110000
      *                                                             *   00120000
      *   ARQUIVOS.....:                                            *   00122000
      *                                                             *   00123000
      *   NAME                I/O                                   *   00124000
      *   ARQUIIN1             I                                    *   00126000
      *   ARQUIIN2             I                                    *   00126100
      *   ARQUIOUT             O                                    *   00127000
      *                                                             *   00128000
      *=============================================================*   00129000
      *                                                             *   00131000
      *   MODULOS....:                                              *   00132000
      *                                                             *   00133000
      *   NAME                                                      *   00134000
      *   GRAVALOG             GRAVA LOG DE ERROS                   *   00135000
      *   DATAEXE              OBTEM DATA E HORA ATUAL              *   00136000
      *                                                             *   00137000
      *=============================================================*   00138000
       ENVIRONMENT                               DIVISION.              00140000
      *=============================================================*   00150000
                                                                        00160000
      *-------------------------------------------------------------*   00170000
       CONFIGURATION                                        SECTION.    00180000
      *-------------------------------------------------------------*   00190000
       SPECIAL-NAMES.                                                   00200000
           DECIMAL-POINT IS COMMA.                                      00210000
                                                                        00220000
                                                                        00230000
       INPUT-OUTPUT                                         SECTION.    00240000
       FILE-CONTROL.                                                    00250000
           SELECT ARQUIIN1 ASSIGN TO ARQUIIN1                           00260000
               FILE STATUS  IS WRK-FS-ARQUIIN1.                         00270000
                                                                        00271000
           SELECT ARQUIIN2 ASSIGN TO ARQUIIN2                           00272100
               FILE STATUS  IS WRK-FS-ARQUIIN2.                         00272200
                                                                        00272300
           SELECT ARQUIOUT ASSIGN TO ARQUIOUT                           00273000
               FILE STATUS  IS WRK-FS-ARQUIOUT.                         00274000
                                                                        00275000
                                                                        00280000
      *=============================================================*   00290000
       DATA                                                DIVISION.    00300000
      *=============================================================*   00310000
       FILE                                                 SECTION.    00320000
      *=============================================================*   00330000
                                                                        00330100
      *      ARQUIVO DE ENTRADA 1                                       00331000
       FD ARQUIIN1                                                      00340000
           RECORDING  MODE IS F                                         00350000
           BLOCK CONTAINS 0 RECORDS.                                    00360000
                                                                        00370000
                                                                        00390000
       01 FD-ARQUIIN1.                                                  00400000
          05 FD-IDFUNC    PIC X(05).                                    00410000
          05 FD-NOMEFUNC  PIC X(30).                                    00420000
          05 FD-SETORFUNC PIC X(04).                                    00430000
          05 FILLER       PIC X(19).                                    00450000
                                                                        00451000
      *      ARQUIVO DE ENTRADA 2                                       00460000
                                                                        00460300
       FD ARQUIIN2                                                      00460400
           RECORDING  MODE IS F                                         00460500
           BLOCK CONTAINS 0 RECORDS.                                    00460600
                                                                        00460700
                                                                        00460800
       01 FD-ARQUIIN2.                                                  00460900
          05 FD-IDFUNC-PROJ  PIC X(05).                                 00461000
          05 FD-PROJETO      PIC X(20).                                 00461100
          05 FD-QUANTHORAS   PIC X(03).                                 00461200
          05 FILLER          PIC X(30).                                 00461300
                                                                        00461400
      *      ARQUIVO DE SAIDA                                           00461500
                                                                        00461600
       FD ARQUIOUT                                                      00461700
           RECORDING  MODE IS F                                         00462000
           BLOCK CONTAINS 0 RECORDS.                                    00463000
                                                                        00463100
                                                                        00464100
       01 FD-ARQUIOUT     PIC X(67).                                    00465000
                                                                        00466000
                                                                        00467000
                                                                        00468000
      *-------------------------------------------------------------*   00470000
       WORKING-STORAGE                                      SECTION.    00480000
      *-------------------------------------------------------------*   00490000
                                                                        00490100
      *------------------LOG-DE-ERROS-------------------------------*   00490200
                                                                        00490300
       01 WRK-DADOS.                                                    00491000
           05 WRK-PROGRAMA  PIC X(08).                                  00492000
           05 WRK-SECAO     PIC X(04).                                  00493000
           05 WRK-MENSAGEM  PIC X(30).                                  00494000
           05 WRK-STATUS    PIC X(02).                                  00495000
                                                                        00496000
      *--------------------LOGICA-----------------------------------*   00502000
                                                                        00503000
       77 WRK-LIDOS           PIC 9(03).                                00530000
                                                                        00530100
       77 WRK-GRAVADOS        PIC 9(03).                                00530200
                                                                        00530300
       77 WRK-LINHA-VAZIA     PIC X(01) VALUE SPACE.                    00531000
                                                                        00532000
       77 WRK-MSG-FINAL       PIC X(17) VALUE 'FINAL DE PROGRAMA'.      00533000
                                                                        00533100
       77 WRK-CONTADOR        PIC 9(02).                                00533200
                                                                        00534000
      *--------------------STATUS-----------------------------------*   00540000
                                                                        00541000
       77 WRK-FS-ARQUIIN1 PIC 9(02).                                    00550000
                                                                        00550100
       77 WRK-FS-ARQUIIN2 PIC 9(02).                                    00550200
                                                                        00551000
       77 WRK-FS-ARQUIOUT PIC 9(02).                                    00560000
                                                                        00590000
      *--------------------LAYOUT SAIDA-----------------------------*   00600000
                                                                        00601200
       01 WRK-SAIDA.                                                    00601300
          05 WRK-IDFUNC       PIC 9(05).                                00601400
          05 WRK-NOMEFUNC     PIC X(30).                                00601500
          05 WRK-PROJETO      PIC X(20).                                00601600
          05 WRK-HORAS        PIC 9(03).                                00601700
                                                                        00601800
       77 WRK-ESPACO-PROJ     PIC X(35) VALUE SPACES.                   00601900
      *-------------------------------------------------------------*   00602000
       LINKAGE                                              SECTION.    00610000
      *-------------------------------------------------------------*   00620000
                                                                        00630000
       01 LNK-INFO-DATA    PIC X(31).                                   00640000
                                                                        00650000
      *=============================================================*   00690000
       PROCEDURE DIVISION USING LNK-INFO-DATA.                          00700000
      *=============================================================*   00710000
                                                                        00720000
      *=============================================================*   00730000
       0000-PRINCIPAL                                       SECTION.    00740000
                                                                        00750000
                                                                        00760000
            PERFORM 1000-INICIAR.                                       00770000
            PERFORM 1500-PRE-PROCESSAR.                                 00771000
            PERFORM 2000-PROCESSAR UNTIL WRK-FS-ARQUIIN2 EQUAL 10       00780000
                                   AND   WRK-FS-ARQUIIN1 EQUAL 10.      00781000
            PERFORM 3000-FINALIZAR.                                     00790000
                                                                        00800000
                                                                        00810000
                                                                        00820000
       0000-99-FIM.                                            EXIT.    00830000
      *=============================================================*   00840000
      *=============================================================*   00880000
       1000-INICIAR                                         SECTION.    00890000
                                                                        00910000
            OPEN INPUT  ARQUIIN1.                                       00920000
            OPEN INPUT  ARQUIIN2.                                       00920100
            OPEN OUTPUT ARQUIOUT.                                       00921000
                                                                        00922000
       1000-99-FIM.                                            EXIT.    00970000
      *=============================================================*   00980000
                                                                        00990000
                                                                        01000000
      *=============================================================*   01001000
       1500-PRE-PROCESSAR                                   SECTION.    01002000
                                                                        01003000
              PERFORM 1200-TESTARSTATUS.                                01007000
              READ ARQUIIN1.                                            01008000
              READ ARQUIIN2.                                            01008100
                                                                        01008200
              IF WRK-FS-ARQUIIN1 NOT EQUAL ZERO OR                      01008300
                 WRK-FS-ARQUIIN2 NOT EQUAL ZERO                         01008400
                                                                        01008500
              DISPLAY WRK-MSG-FINAL                                     01009300
              STOP RUN                                                  01009400
              END-IF.                                                   01009500
                                                                        01009600
       1500-99-FIM.                                            EXIT.    01009700
      *=============================================================*   01009800
                                                                        01010000
                                                                        01011000
      *=============================================================*   01020000
       1200-TESTARSTATUS                                    SECTION.    01030000
                                                                        01050000
               IF WRK-FS-ARQUIIN1 NOT EQUAL ZEROS                       01060000
                                                                        01062000
                MOVE 'FR21EX02'                  TO WRK-PROGRAMA        01070000
                MOVE '1000'                      TO WRK-SECAO           01080000
                MOVE 'ERRO OPEN FUNCIONARIOS - ' TO WRK-MENSAGEM        01090000
                MOVE WRK-FS-ARQUIIN1             TO WRK-STATUS          01100000
                PERFORM 9000-TRATARERROS                                01110000
               END-IF.                                                  01120000
                                                                        01130000
               IF WRK-FS-ARQUIIN2 NOT EQUAL ZEROS                       01131000
                                                                        01132000
                MOVE 'FR21EX02'                  TO WRK-PROGRAMA        01133000
                MOVE '1000'                      TO WRK-SECAO           01134000
                MOVE 'ERRO OPEN PROJETOS     - ' TO WRK-MENSAGEM        01135000
                MOVE WRK-FS-ARQUIIN2             TO WRK-STATUS          01136000
                PERFORM 9000-TRATARERROS                                01137000
               END-IF.                                                  01138000
                                                                        01140000
       1200-99-FIM.                                            EXIT.    01150000
      *=============================================================*   01160000
                                                                        01170000
                                                                        01180000
      *=============================================================*   01181000
       1300-CABEC                                           SECTION.    01182000
                                                                        01189000
                                                                        01189100
                                                                        01189200
       1300-99-FIM.                                            EXIT.    01189600
      *=============================================================*   01189700
                                                                        01190000
      *=============================================================*   01200000
       2000-PROCESSAR                                       SECTION.    01210000
                                                                        01700000
           EVALUATE TRUE                                                01701000
                                                                        01701100
           WHEN FD-IDFUNC LESS FD-IDFUNC-PROJ                           01701200
                                                                        01701300
                IF WRK-CONTADOR EQUAL ZERO                              01701400
                DISPLAY FD-IDFUNC FD-NOMEFUNC                           01701500
                        'FUNCIONARIO SEM PROJETO'                       01701600
                MOVE 0 TO WRK-CONTADOR                                  01701700
                END-IF                                                  01701800
                MOVE 0 TO WRK-CONTADOR                                  01701900
                READ ARQUIIN1                                           01702000
                                                                        01702100
           WHEN FD-IDFUNC EQUAL FD-IDFUNC-PROJ                          01702200
                MOVE FD-IDFUNC       TO WRK-IDFUNC                      01702300
                MOVE FD-NOMEFUNC     TO WRK-NOMEFUNC                    01702400
                MOVE FD-PROJETO      TO WRK-PROJETO                     01702500
                MOVE FD-QUANTHORAS   TO WRK-HORAS                       01702600
                DISPLAY WRK-SAIDA                                       01702700
                READ ARQUIIN2                                           01702800
                ADD 1 TO WRK-CONTADOR                                   01703000
                                                                        01703100
           WHEN FD-IDFUNC GREATER FD-IDFUNC-PROJ                        01704000
                DISPLAY WRK-ESPACO-PROJ 'PROJETO SEM FUNCIONARIO'       01706100
                READ ARQUIIN2                                           01706200
           END-EVALUATE.                                                01707000
                                                                        01707100
                                                                        01707200
                IF WRK-FS-ARQUIIN2 NOT EQUAL 0                          01707300
                   MOVE HIGH-VALUES TO FD-IDFUNC-PROJ                   01707500
               END-IF.                                                  01707600
                                                                        01707700
                IF WRK-FS-ARQUIIN1 NOT EQUAL 0                          01707800
                   MOVE HIGH-VALUES TO FD-IDFUNC                        01707900
               END-IF.                                                  01708000
                                                                        01709000
                                                                        01709400
                                                                        01709500
       2000-99-FIM.                                            EXIT.    01710000
      *=============================================================*   01720000
                                                                        01730000
      *=============================================================*   01760000
       3000-FINALIZAR                                       SECTION.    01770000
               DISPLAY WRK-MSG-FINAL.                                   01813000
               CLOSE ARQUIIN1.                                          01813100
               CLOSE ARQUIIN2.                                          01813200
               CLOSE ARQUIOUT.                                          01813300
                                                                        01813400
                                                                        01814000
      *                 DISPLAY DE HORARIO                              01815000
                                                                        01816000
               CALL 'DATAEXT' USING LNK-INFO-DATA.                      01820000
                DISPLAY WRK-LINHA-VAZIA.                                01830000
                DISPLAY LNK-INFO-DATA.                                  01840000
                DISPLAY WRK-LINHA-VAZIA.                                01850000
                STOP RUN.                                               01851000
                                                                        01852000
       3000-99-FIM.                                            EXIT.    01860000
      *=============================================================*   01870000
                                                                        01880000
                                                                        01900000
      *=============================================================*   01910000
       9000-TRATARERROS                                     SECTION.    01920000
                                                                        01930000
                                                                        01940000
             CALL 'GRAVALOG' USING WRK-DADOS.                           01950000
                 GOBACK.                                                01960000
                                                                        01970000
                                                                        01980000
       9000-99-FIM.                                            EXIT.    01990000
      *=============================================================*   02000000
