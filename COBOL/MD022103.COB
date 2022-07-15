      *===============================================================*
       IDENTIFICATION                                        DIVISION.
      *===============================================================*
       PROGRAM-ID. MD022103.
      *===============================================================*
      *   AUTOR....:VICTOR ARANDA                                     *
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR        *
      *   DATA ....:07/07/2022                                        *
      *---------------------------------------------------------------*
      * VERSAO   DATA         AUTOR \      DESCRICAO DA MODIFICACAO   *
      *  1.0   07/07/22   VICTOR ARANDA        VERSAO INICIAL         *
      *---------------------------------------------------------------*
      *   OBJETIVO: RECEBER DUAS CHAVES PELA PARM E LER O CONTEUDO *
      *             QUE ESTAO ENTRE ELAS.                             *
      *                                                               *
      *---------------------------------------------------------------*
      *   ARQUIVOS:                                 INCLUDE/BOOK      *
      *   DDNAME......         I/O                   FR21WLNK         *
      *   EVSA0407              I                    FR21W040         *
      *   SAIVSAM               O                                     *
      *---------------------------------------------------------------*
      *   MODULOS....:                                                *
      *   DATAEXE (OBTEM DATA POR EXTENSO                             *
      *---------------------------------------------------------------*
      *//////////////////  A L T E R A C O E S  //////////////////////*
      *---------------------------------------------------------------*
      *   ANALISTA......:VICTOR ARANDA                                *
      *   DATA..........:07/07/2022                                   *
      *   CONSULTORIA...:FOURSYS|4SYSV                                *
      *---------------------------------------------------------------*
      *                                                               *
      *                                                               *
      *                                                               *
      *===============================================================*
       ENVIRONMENT                                            DIVISION.
      *===============================================================*
       CONFIGURATION                                           SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT                                            SECTION.
        FILE-CONTROL.
             SELECT EVSA0407 ASSIGN TO EVSA0407
                ORGANIZATION  IS INDEXED
                ACCESS MODE   IS DYNAMIC
                RECORD KEY    IS ARQ-CHAVE
                FILE STATUS   IS FS-EVSA0407.

             SELECT SAIVSAM ASSIGN TO SAIVSAM
                FILE STATUS   IS FS-SAIVSAM.

      *===============================================================*
       DATA                                                   DIVISION.
      *===============================================================*

      *===============================================================*
       FILE SECTION.
       FD EVSA0407.

       COPY 'FR21W040'.

      *=====================LRECL 19===================================*

       FD SAIVSAM
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

        01 REG-SAIVSAM     PIC X(19).

      *===============================================================*
       WORKING-STORAGE                                        SECTION.
      *===============================================================*
       01 FILLER                                       PIC X(70) VALUE
           '---------VARIAVEIS DE APOIO--------------------'.

      *==============VARIAVEIS DE APOIO===============================*

       77 FS-EVSA0407             PIC 9(2).

       77 FS-SAIVSAM              PIC 9(2).

       77 WRK-CONT-LIDOS          PIC 9(03).

       77 WRK-CONT-GRAVADOS       PIC 9(03).

       77 WRK-CONT-EXCESSAO       PIC 9(03).

       77 WRK-LEN-ED              PIC Z99.

      *===============================================================*
       01 FILLER                                       PIC X(70) VALUE
           '---------VARIAVEIS DE MENSAGENS----------------'.

      *==============VARIAVEIS DE MENSAGENS===========================*

       77 WRK-MSG             PIC X(30).
       77 WRK-MSGCODIGO-VSAM  PIC X(13) VALUE 'CODIGO VSAM: '.
       77 WRK-ERRO-OPEN       PIC X(21) VALUE 'ERRO NO OPEN EVSA0407'.
       77 WRK-MSG-ERROTAMANHO PIC X(16) VALUE 'ERRO NO TAMANHO '.
       77 WRK-ERRO-CLOSE      PIC X(22) VALUE 'ERRO NO CLOSE EVSA0407'.
       77 WRK-SEPARACAO       PIC X(37) VALUE ALL '-'.

      *===============================================================*
       01 FILLER                                       PIC X(70) VALUE
           '---------VARIAVEIS DO MODULO-------------------'.

      *==============VARIAVEIS DO MODULO==============================*

       77 WRK-DATAEXT         PIC X(07) VALUE 'DATAEXT'.

      *----------------------------------------------------------------
       LINKAGE                                                 SECTION.
      *----------------------------------------------------------------

       COPY 'FR21WLNK'.

      *===============================================================*
       PROCEDURE DIVISION USING LNK-ENTRADA.
      *===============================================================*

      *----------------------------------------------------------------
       0000-PRINCIPAL                                         SECTION.
      *----------------------------------------------------------------

            PERFORM 1000-INICIALIZAR.
            PERFORM 2000-PROCESSAR.
            PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------
       0000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       1000-INICIALIZAR                                       SECTION.
      *----------------------------------------------------------------

           OPEN INPUT EVSA0407.
           OPEN OUTPUT SAIVSAM.
           PERFORM 1200-TESTAR-STATUS.

      *----------------------------------------------------------------
       1000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       1200-TESTAR-STATUS                                     SECTION.
      *----------------------------------------------------------------

           IF FS-EVSA0407 NOT EQUAL ZEROS
              MOVE WRK-ERRO-OPEN TO WRK-MSG
              PERFORM 9000-TRATAERROS
           END-IF.

      *----------------------------------------------------------------
       1200-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       2000-PROCESSAR                                         SECTION.
      *----------------------------------------------------------------

           MOVE LNK-CHAVE TO ARQ-CHAVE.
           START EVSA0407 KEY EQUAL ARQ-CHAVE
            INVALID KEY
             DISPLAY 'CHAVE NAO ENCONTRADA'
            NOT INVALID KEY
             IF LNK-LEN EQUAL 18
                PERFORM 2100-LEITURA UNTIL ARQ-CHAVE EQUAL LNK-CHAVE2
             ELSE
                MOVE WRK-MSG-ERROTAMANHO TO WRK-MSG
             END-IF.

      *----------------------------------------------------------------
       2000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       2100-LEITURA                                           SECTION.
      *----------------------------------------------------------------

            READ EVSA0407 NEXT.
            ADD 1 TO WRK-CONT-LIDOS.
            IF ARQ-CHAVE LESS OR EQUAL LNK-CHAVE2
               IF REG-EVSA0407 IS NUMERIC
                  ADD 1 TO WRK-CONT-GRAVADOS
                  WRITE REG-SAIVSAM FROM REG-EVSA0407
               ELSE
                  ADD 1 TO WRK-CONT-EXCESSAO
                  DISPLAY REG-EVSA0407
               END-IF
            END-IF.

      *----------------------------------------------------------------
       2100-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       3000-FINALIZAR                                         SECTION.
      *----------------------------------------------------------------
           CLOSE SAIVSAM.
           CLOSE EVSA0407.
           DISPLAY 'REGISTROS LIDOS : ' WRK-CONT-LIDOS.
           DISPLAY 'REGISTROS GRAVADOS: ' WRK-CONT-GRAVADOS.
           DISPLAY 'REGISTROS COM INCONSISTENCIA: ' WRK-CONT-EXCESSAO.
           DISPLAY WRK-SEPARACAO.
            CALL WRK-DATAEXT USING LNK-INFO-DATA.
                 DISPLAY 'DATA ' WRK-DIA-EXT.
                 DISPLAY 'HORA ' WRK-HORA-EXT.
           DISPLAY WRK-SEPARACAO.
           GOBACK.

      *----------------------------------------------------------------
       3000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       9000-TRATAERROS                                        SECTION.
      *----------------------------------------------------------------


           DISPLAY WRK-SEPARACAO.
           DISPLAY WRK-MSG.
           DISPLAY WRK-MSGCODIGO-VSAM FS-EVSA0407.
           DISPLAY WRK-SEPARACAO.
           GOBACK.

      *----------------------------------------------------------------
       9000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------
