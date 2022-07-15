      *===============================================================*
       IDENTIFICATION                                        DIVISION.
      *===============================================================*
       PROGRAM-ID. MD022104.
      *===============================================================*
      *   AUTOR....:VICTOR ARANDA                                     *
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR        *
      *   DATA ....:12/07/2022                                        *
      *---------------------------------------------------------------*
      * VERSAO   DATA         AUTOR \      DESCRICAO DA MODIFICACAO   *
      *  1.0   12/07/22   VICTOR ARANDA        VERSAO INICIAL         *
      *---------------------------------------------------------------*
      *   OBJETIVO:                                                   *
      *                                                               *
      *                                                               *
      *---------------------------------------------------------------*
      *   ARQUIVOS:                                 INCLUDE/BOOK      *
      *   DDNAME......         I/O                                    *
      *                                              FR21WLNK         *
      *   EVSA0407             I-O                   FR21W040         *
      *   MOV1207               I                    FR21WMOV         *
      *---------------------------------------------------------------*
      *   MODULOS....:                                                *
      *   DATAEXE (OBTEM DATA POR EXTENSO)                            *
      *---------------------------------------------------------------*
      *//////////////////  A L T E R A C O E S  //////////////////////*
      *---------------------------------------------------------------*
      *   ANALISTA......:VICTOR ARANDA                                *
      *   DATA..........:12/07/2022                                   *
      *   CONSULTORIA...:FOURSYS|4SYSV                                *
      *---------------------------------------------------------------*
      *   ARQUIVOS:                                 INCLUDE/BOOK      *
      *   DDNAME......         I/O                                    *
      *                                              FR21WLNK         *
      *   EVSA0407             I-O                   FR21W040         *
      *   MOV1207               I                    FR21WMOV         *
      *   EXCSAIDA              O                                     *
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

             SELECT MOV1207  ASSIGN     TO MOV1207
                        FILE STATUS     IS FS-MOV1207.

             SELECT EXCSAIDA ASSIGN     TO EXCSAIDA
                        FILE STATUS     IS FS-EXCSAIDA.

      *===============================================================*
       DATA                                                   DIVISION.
      *===============================================================*

      *===============================================================*
       FILE SECTION.
       FD EVSA0407.

       COPY 'FR21W040'.

      *=====================LRECL 19===================================*

       FD MOV1207
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       COPY 'FR21WMOV'.

      *=====================LRECL 19===================================*

       FD EXCSAIDA
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       01 FD-REG-EXCSAIDA         PIC X(19).

      *===============================================================*
       WORKING-STORAGE                                        SECTION.
      *===============================================================*
       01 FILLER                                       PIC X(70) VALUE
           '---------VARIAVEIS DE APOIO--------------------'.

      *==============VARIAVEIS DE APOIO===============================*

       77 FS-EVSA0407             PIC 9(02).

4SYSVA 77 FS-EXCSAIDA             PIC 9(02).

       77 FS-MOV1207              PIC 9(02).

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
       77 WRK-ERRO-OPEN1      PIC X(21) VALUE 'ERRO NO OPEN EVSA0407'.
4SYSVA 77 WRK-ERRO-OPEN2      PIC X(20) VALUE 'ERRO NO OPEN MOV1207'.
4SYSVA 77 WRK-ERRO-OPEN3      PIC X(21) VALUE 'ERRO NO OPEN EXCSAIDA'.

       77 WRK-MSG-ERROTAMANHO PIC X(16) VALUE 'ERRO NO TAMANHO '.

       77 WRK-ERRO-CLOSE1     PIC X(22) VALUE 'ERRO NO CLOSE EVSA0407'.
4SYSVA 77 WRK-ERRO-CLOSE2     PIC X(21) VALUE 'ERRO NO CLOSE MOV1207'.
4SYSVA 77 WRK-ERRO-CLOSE3     PIC X(22) VALUE 'ERRO NO CLOSE EXCSAIDA'.
       77 WRK-SEPARACAO       PIC X(50) VALUE ALL '-'.

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
            PERFORM 1500-TESTAR-VAZIO.
            PERFORM 2000-PROCESSAR UNTIL FS-MOV1207 EQUAL 10.
            PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------
       0000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       1000-INICIALIZAR                                       SECTION.
      *----------------------------------------------------------------

           OPEN I-O    EVSA0407.
           OPEN INPUT  MOV1207.
4SYSVA     OPEN OUTPUT EXCSAIDA.
           PERFORM 1200-TESTAR-STATUS.

      *----------------------------------------------------------------
       1000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       1200-TESTAR-STATUS                                     SECTION.
      *----------------------------------------------------------------

           IF FS-EVSA0407 NOT EQUAL ZEROS
              MOVE WRK-ERRO-OPEN1 TO WRK-MSG
              PERFORM 9000-TRATAERROS
           END-IF.

           IF FS-MOV1207  NOT EQUAL ZEROS
              MOVE WRK-ERRO-OPEN2 TO WRK-MSG
              PERFORM 9000-TRATAERROS
           END-IF.
4SYSVA
  |        IF FS-EXCSAIDA NOT EQUAL ZEROS
  |           MOVE WRK-ERRO-OPEN3 TO WRK-MSG
  |           PERFORM 9000-TRATAERROS
  |        END-IF.
4SYSVA
      *----------------------------------------------------------------
       1200-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------*
       1500-TESTAR-VAZIO                                       SECTION.
      *----------------------------------------------------------------*

           READ MOV1207
           IF FS-MOV1207 NOT EQUAL ZEROS
              DISPLAY ' ARQUIVO VAZIO '
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                              EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------
       2000-PROCESSAR                                         SECTION.
      *----------------------------------------------------------------

           MOVE ARQ-MOV1207-CHAVE TO ARQ-CHAVE.
           READ EVSA0407.

           EVALUATE TRUE
           WHEN (FS-EVSA0407 EQUAL ZEROS
                AND ARQ-MOV1207-SALARIO NOT EQUAL ZEROS
                AND ARQ-MOV1207-SALARIO IS NUMERIC)
                    REWRITE REG-EVSA0407 FROM ARQ-MOV1207
                    IF FS-EVSA0407 EQUAL ZEROS
                       ADD 1 TO WRK-CONT-GRAVADOS
                       DISPLAY 'ATUALIZADO............: ' ARQ-MOV1207
                       DISPLAY WRK-SEPARACAO
                    END-IF

            WHEN FS-EVSA0407 NOT EQUAL ZEROS
                 ADD 1 TO WRK-CONT-EXCESSAO
4SYSVA           WRITE FD-REG-EXCSAIDA FROM ARQ-CHAVE
                 DISPLAY 'CHAVE NAO ENCONTRADA. : ' ARQ-CHAVE
                 DISPLAY WRK-SEPARACAO

            WHEN ARQ-MOV1207-SALARIO EQUAL ZEROS OR
                 ARQ-MOV1207-SALARIO IS NOT NUMERIC
                 ADD 1 TO WRK-CONT-EXCESSAO
4SYSVA           WRITE FD-REG-EXCSAIDA FROM ARQ-CHAVE
                 DISPLAY 'SALARIO INVALIDO......: ' ARQ-MOV1207
                 DISPLAY WRK-SEPARACAO

           END-EVALUATE.
           READ MOV1207.
           ADD 1 TO WRK-CONT-LIDOS.

      *----------------------------------------------------------------
       2000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       3000-FINALIZAR                                         SECTION.
      *----------------------------------------------------------------

           CLOSE MOV1207.
           CLOSE EVSA0407.
4SYSVA     CLOSE EXCSAIDA.

           DISPLAY 'REGISTROS LIDOS.............: ' WRK-CONT-LIDOS.
           DISPLAY 'REGISTROS GRAVADOS..........: ' WRK-CONT-GRAVADOS.
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
