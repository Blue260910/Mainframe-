      *===============================================================*
       IDENTIFICATION                                        DIVISION.
      *===============================================================*
       PROGRAM-ID. F21CIP3.
      *===============================================================*
      *   AUTOR....:VICTOR ARANDA                                     *
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR        *
      *   DATA ....:30/06/2022                                        *
      *---------------------------------------------------------------*
      *   OBJETIVO:PROGRAMA PARA CONSULTAR                            *
      *    REGISTROS EM ARQUIVO (VSAM) EXECUTANDO NO CICS.            *
      *      APLICANDO LOGICA PSEUDO COM METODO (EIBCALEN)            *
      *                                                               *
      *---------------------------------------------------------------*
      *   ARQUIVOS:                                                   *
      *   DDNAME......                                                *
      *    DDNAME              I/O                 INCLUDE/BOOK       *
      *---------------------------------------------------------------*
      *   MODULOS....:                             INCLUDE/BOOK       *
      *===============================================================*

      *===============================================================*
       ENVIRONMENT                                            DIVISION.
      *===============================================================*
       CONFIGURATION                                           SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *===============================================================*
       DATA                                                   DIVISION.
      *===============================================================*
       WORKING-STORAGE SECTION.

           COPY F21CIM1.

       01 WRK-DADOS.
          05 WRK-CHAVE        PIC X(05).
          05 WRK-NOME         PIC X(30).
          05 WRK-SETOR        PIC X(04).
          05 WRK-SALARIO      PIC 9(08)V99.
          05 WRK-DATAADM.
             10 WRK-DATADIA   PIC X(02).
             10 FILLER        PIC X(01) VALUE '-'.
             10 WRK-DATAMES   PIC X(02).
             10 FILLER        PIC X(01) VALUE '-'.
             10 WRK-DATAANO   PIC X(04).
          05 WRK-EMAIL        PIC X(40).
          05 WRK-STATUS       PIC X(01).

       77 WK-RESP            PIC S9(04) COMP.
       77 WRK-RET-MAPA       PIC S9(04) COMP.
       77 WRK-FASE           PIC 9(01).
       77 WRK-FLAG           PIC 9(01).
       77 WRK-SAIDA          PIC X(30) VALUE 'FIM DE PROGRAMA'.
       77 WRK-SALARIO-ED     PIC ZZ.ZZZ.ZZ9,99.
      *===============================================================*
       PROCEDURE                                 DIVISION.
      *===============================================================*
      *---------------------------------------------------------------*
       0000-PRINCIPAL                                         SECTION.
      *---------------------------------------------------------------*

               PERFORM 1000-INICIALIZAR.
              IF WRK-FLAG EQUAL 1
                PERFORM 2000-PROCESSAR
              END-IF
                PERFORM 3000-FINALIZAR.


               EXEC CICS
                 RETURN TRANSID ('T213')
                 COMMAREA(WRK-FASE)
               END-EXEC.
       0000-99-FIM.                        EXIT.

      *---------------------------------------------------------------*
       1000-INICIALIZAR                                       SECTION.
      *---------------------------------------------------------------*

              IF EIBCALEN EQUAL 0
                EXEC CICS SEND
                  MAPSET('F21CIM1')
                  MAP('MAPA01')
                  ERASE
                  MAPONLY
                END-EXEC
               MOVE 1 TO WRK-FASE
              END-IF.
              INITIALIZE MAPA01I.
              EXEC CICS RECEIVE
                 MAPSET('F21CIM1')
                 MAP('MAPA01')
                 INTO(MAPA01I)
                 RESP(WRK-RET-MAPA)
              END-EXEC.

             IF WRK-RET-MAPA EQUAL DFHRESP(MAPFAIL)
                MOVE 2 TO WRK-FLAG
             ELSE
                MOVE 1 TO WRK-FLAG
             END-IF.
       1000-99-FIM.                        EXIT.

      *---------------------------------------------------------------*
       2000-PROCESSAR                                         SECTION.
      *---------------------------------------------------------------*


            EVALUATE EIBAID
             WHEN '3'
              EXEC CICS SEND
                  FROM(WRK-SAIDA)
                  ERASE
              END-EXEC

              EXEC CICS
                 RETURN
              END-EXEC

             WHEN ''''
              INITIALIZE MAPA01O
      *-------------CONSULTA---------------------------------------
             WHEN '5'
                MOVE IDI TO WRK-CHAVE
               EXEC CICS LINK
                   PROGRAM('F21COVSA')
                   COMMAREA(WRK-DADOS)
                   RESP(WK-RESP)
               END-EXEC
              PERFORM 2200-VERIFICACAO-CONSULTA
             END-EVALUATE.
       2000-99-FIM.                        EXIT.
      *---------------------------------------------------------------*
       2200-VERIFICACAO-CONSULTA                              SECTION.
      *---------------------------------------------------------------*
               EVALUATE WRK-STATUS
                WHEN '1'
                 MOVE WRK-NOME       TO NOMEO
                 MOVE WRK-SETOR      TO SETORO
                 MOVE WRK-SALARIO    TO WRK-SALARIO-ED
                 MOVE WRK-SALARIO-ED TO SALARIOO
                 MOVE WRK-DATADIA    TO DATADIAO
                 MOVE WRK-DATAMES    TO DATAMESO
                 MOVE WRK-DATAANO    TO DATAANOO
                 MOVE WRK-EMAIL      TO EMAILO
                 MOVE 'ENCONTRADO  ' TO MSGO
                WHEN OTHER
                     INITIALIZE MAPA01I
                     MOVE WRK-CHAVE TO IDO
                     EVALUATE WRK-STATUS

                       WHEN '2'
                            MOVE 'NAO ENCONTRADO ' TO MSGO
                       WHEN '3'
                            MOVE 'SALARIO COM INCONSISTENCIA' TO MSGO
                       WHEN '4'
                            MOVE 'EMAIL EM BRANCO' TO MSGO
                     END-EVALUATE
               END-EVALUATE.

       2200-99-FIM.                        EXIT.
      *---------------------------------------------------------------*
       3000-FINALIZAR                                         SECTION.
      *---------------------------------------------------------------*

              IF WRK-FLAG EQUAL 2
                 EXEC CICS SEND
                    MAPSET('F21CIM1')
                    MAP('MAPA01')
                    ERASE
                    MAPONLY
                 END-EXEC
              ELSE
                EXEC CICS SEND
                   MAPSET('F21CIM1')
                   MAP('MAPA01')
                   DATAONLY
                   FROM(MAPA01O)
                END-EXEC
              END-IF.

       3000-99-FIM.                        EXIT.
