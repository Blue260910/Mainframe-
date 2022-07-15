      *===============================================================*
       IDENTIFICATION                                        DIVISION.
      *===============================================================*
       PROGRAM-ID. F21COVSA.
      *===============================================================*
      *   AUTOR....:VICTOR ARANDA                                     *
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR        *
      *   DATA ....:04/07/2022                                        *
      *---------------------------------------------------------------*
      *   OBJETIVO: PROGRAMA COMPLEMENTAR F21CIP3                     *
      *                                                               *
      *                                                               *
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
       WORKING-STORAGE                                         SECTION.

           COPY DFHAID.
           COPY F21CIM1.

       01 WRK-DADOS.
          05 WRK-CHAVE        PIC X(05).
          05 WRK-NOME         PIC X(30).
          05 WRK-SETOR        PIC X(04).
          05 WRK-SALARIO      PIC X(10).
          05 WRK-DATAADM.
             10 WRK-DATADIA      PIC X(02).
             10 FILLER           PIC X(01) VALUE '-'.
             10 WRK-DATAMES      PIC X(02).
             10 FILLER           PIC X(01) VALUE '-'.
             10 WRK-DATAANO      PIC X(04).
          05 WRK-EMAIL        PIC X(40).
          05 WRK-STATUS       PIC X(01).

       77 WK-RESP            PIC S9(04) COMP.

      *===============================================================*
       LINKAGE                                                SECTION.
      *===============================================================*

       01 DFHCOMMAREA.
          05 LNK-CHAVE        PIC X(05).
          05 LNK-NOME         PIC X(30).
          05 LNK-SETOR        PIC X(04).
          05 LNK-SALARIO      PIC X(10).
          05 WRK-DATAADM.
             10 LNK-DATADIA      PIC X(02).
             10 FILLER           PIC X(01) VALUE '-'.
             10 LNK-DATAMES      PIC X(02).
             10 FILLER           PIC X(01) VALUE '-'.
             10 LNK-DATAANO      PIC X(04).
          05 LNK-EMAIL        PIC X(40).
          05 LNK-STATUS       PIC X(01).

      *===============================================================*
       PROCEDURE                                 DIVISION.
      *===============================================================*
      *---------------------------------------------------------------*
       0000-PRINCIPAL                                         SECTION.
      *---------------------------------------------------------------*

                PERFORM 1000-INICIALIZAR.
                PERFORM 2000-PROCESSAR.
                PERFORM 3000-FINALIZAR.

       0000-99-FIM.                        EXIT.

      *---------------------------------------------------------------*
       1000-INICIALIZAR                                       SECTION.
      *---------------------------------------------------------------*
               INITIALIZE WRK-DADOS.
               MOVE LNK-CHAVE TO WRK-CHAVE.

       1000-99-FIM.                        EXIT.

      *---------------------------------------------------------------*
       2000-PROCESSAR                                         SECTION.
      *---------------------------------------------------------------*

               EXEC CICS READ
                      FILE('FUNC')
                      RIDFLD(WRK-CHAVE)
                      INTO(WRK-DADOS)
                      RESP(WK-RESP)
               END-EXEC.
             IF WK-RESP EQUAL DFHRESP(NORMAL)
               EVALUATE TRUE
                WHEN WRK-SALARIO EQUAL ZEROES
                 MOVE '3' TO LNK-STATUS
                WHEN WRK-EMAIL EQUAL SPACES
                 MOVE '4' TO LNK-STATUS
                WHEN OTHER
                MOVE WRK-NOME      TO LNK-NOME
                MOVE WRK-SETOR     TO LNK-SETOR
                MOVE WRK-SALARIO   TO LNK-SALARIO
                MOVE WRK-DATADIA   TO LNK-DATADIA
                MOVE WRK-DATAMES   TO LNK-DATAMES
                MOVE WRK-DATAANO   TO LNK-DATAANO
                MOVE WRK-EMAIL     TO LNK-EMAIL
                MOVE '1'           TO LNK-STATUS
               END-EVALUATE
             ELSE
              MOVE '2' TO LNK-STATUS
             END-IF.
       2000-99-FIM.                        EXIT.
      *---------------------------------------------------------------*
       3000-FINALIZAR                                         SECTION.
      *---------------------------------------------------------------*
             EXEC CICS
                 RETURN
             END-EXEC.
       3000-99-FIM.                        EXIT.
