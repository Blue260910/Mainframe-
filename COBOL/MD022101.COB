      *===============================================================*
       IDENTIFICATION                                        DIVISION.
      *===============================================================*
       PROGRAM-ID. FR21LVSA.
      *===============================================================*
      *   AUTOR....:VICTOR ARANDA                                     *
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR        *
      *   DATA ....:27/06/2022                                        *
      *---------------------------------------------------------------*
      *   OBJETIVO:LEITURA DE UM ARQUIVO (VSAM) E GRAVACAO EM AR1QUIVO*
      *                                                               *
      *                                                               *
      *                                                               *
      *---------------------------------------------------------------*
      *   ARQUIVOS:                                                   *
      *   DDNAME......                                                *
      *    DDNAME              I/O                 INCLUDE/BOOK       *
      *                                             I#BV0407          *
      *    EVSA0407             I                                     *
      *    SVSA0407             O                                     *
      *---------------------------------------------------------------*
      *   MODULOS....:                             INCLUDE/BOOK       *
      *===============================================================*

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
                ACCESS MODE   IS SEQUENTIAL
                RECORD KEY    IS ARQ-CHAVE
                FILE STATUS   IS FS-EVSA0407.

      *-----------------------LRECL 27--------------------------------*
             SELECT SVSA0407 ASSIGN TO SVSA0407
                FILE STATUS   IS FS-SVSA0407.


      *-----------------------LRECL 27--------------------------------*
             SELECT SV0407EX ASSIGN TO SV0407EX
                FILE STATUS   IS FS-SV0407EX.
      *===============================================================*
       DATA                                                   DIVISION.
      *===============================================================*
       FILE SECTION.
       FD EVSA0407.

       01 REG-EVSA0407.
          02 ARQ-CHAVE .
             03 ARQ-AGENCIA     PIC X(04).
             03 ARQ-CONTA       PIC X(05).
          02 ARQ-SALARIO        PIC X(10).


      *---------------------------------------------------------------*

       FD SVSA0407
             RECORDING MODE IS F
             BLOCK CONTAINS 0 RECORDS.

       01 FD-SVSA0407-REG       PIC X(27).

      *---------------------------------------------------------------*

       FD SV0407EX
             RECORDING MODE IS F
             BLOCK CONTAINS 0 RECORDS.

       01 FD-SV0407EX-REG       PIC X(27).

      *===============================================================*
       WORKING-STORAGE                                        SECTION.
      *===============================================================*

       01 FILLER                                       PIC X(70) VALUE
           '---------VARIAVEIS DE STATUS-------------------'.

      *==============VARIAVEIS DE STATUS==============================*

       77 FS-EVSA0407            PIC 9(2).
       77 FS-SVSA0407            PIC 9(2).
       77 FS-SV0407EX            PIC 9(2).

      *===============================================================*

       01 FILLER                                       PIC X(70) VALUE
           '---------VARIAVEIS DE APOIO--------------------'.

      *==============VARIAVEIS DE APOIO===============================*

       77 WRK-CONT-LIDOS         PIC 9(03).
       77 WRK-CONT-ERRO          PIC 9(03).
       77 WRK-CONT-GRAVADOS      PIC 9(03).

       77 WRK-DATAEXT            PIC X(07) VALUE 'DATAEXT'.


       01 FILLER                                       PIC X(70) VALUE
           '---------VARIAVEIS DE ARQUIVO------------------'.

      *==============VARIAVEIS DE ARQUIVO=============================*

          COPY 'I#BV0407'.


      *----------------------------------------------------------------
       LINKAGE                                                 SECTION.
      *----------------------------------------------------------------
       01 LNK-INFO-DATA.
          05 WRK-DIA-EXT          PIC X(23).
          05 WRK-HORA-EXT         PIC X(08).

      *===============================================================*
       PROCEDURE DIVISION USING LNK-INFO-DATA.
      *===============================================================*

      *----------------------------------------------------------------
       0000-PRINCIPAL                                         SECTION.
      *----------------------------------------------------------------

            PERFORM 1000-INICIALIZAR.
             PERFORM 2000-PROCESSAR UNTIL FS-EVSA0407 EQUAL 10.
              PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------
       0000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       1000-INICIALIZAR                                       SECTION.
      *----------------------------------------------------------------

           OPEN INPUT  EVSA0407.
           PERFORM 1200-TESTAR-STATUS1.
           OPEN OUTPUT SVSA0407.
           PERFORM 1200-TESTAR-STATUS2.
           OPEN OUTPUT SV0407EX.
           PERFORM 1200-TESTAR-STATUS3.

           DISPLAY '---------ARQUIVOS COM EXCECAO-----------'.

      *----------------------------------------------------------------
       1000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       1200-TESTAR-STATUS1                                     SECTION.
      *----------------------------------------------------------------

           IF FS-EVSA0407 NOT EQUAL ZEROS
              DISPLAY 'ERRO NA ABERTURA DO ARQUIVO EVSA0407'
              DISPLAY 'CODIGO VSAM ' FS-EVSA0407
              PERFORM 3000-FINALIZAR
           ELSE
              READ EVSA0407
           END-IF.

      *----------------------------------------------------------------
       1200-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       1200-TESTAR-STATUS2                                     SECTION.
      *----------------------------------------------------------------

           IF FS-SVSA0407 NOT EQUAL ZEROS
              DISPLAY 'ERRO NA ABERTURA DO ARQUIVO SVSA0407'
              DISPLAY 'CODIGO VSAM ' FS-SVSA0407
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------
       1200-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       1200-TESTAR-STATUS3                                     SECTION.
      *----------------------------------------------------------------

           IF FS-SV0407EX NOT EQUAL ZEROS
              DISPLAY 'ERRO NA ABERTURA DO ARQUIVO SV0407EX'
              DISPLAY 'CODIGO VSAM ' FS-SV0407EX
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------
       1200-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       2000-PROCESSAR                                         SECTION.
      *----------------------------------------------------------------

           ADD 1 TO WRK-CONT-LIDOS.
           MOVE ARQ-AGENCIA TO WRK-SVSA0407-DADOS-AGENCIA.
           MOVE ARQ-CONTA   TO WRK-SVSA0407-DADOS-CONTA.
           MOVE ARQ-SALARIO TO WRK-SVSA0407-DADOS-SALARIO.
           ACCEPT WRK-SVSA0407-DADOS-DATA FROM DATE YYYYMMDD.

           IF WRK-SVSA0407-DADOS-SALARIO EQUAL ZEROS OR
              WRK-SVSA0407-DADOS-SALARIO IS NOT NUMERIC

              WRITE FD-SV0407EX-REG FROM WRK-CORPO
              ADD 1 TO WRK-CONT-ERRO
           ELSE

              WRITE FD-SVSA0407-REG FROM WRK-CORPO
              ADD 1 TO WRK-CONT-GRAVADOS
           END-IF.

           READ EVSA0407.

      *----------------------------------------------------------------
       2000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       3000-FINALIZAR                                         SECTION.
      *----------------------------------------------------------------

           CLOSE EVSA0407.
           CLOSE SVSA0407.
           CLOSE SV0407EX.

           DISPLAY '----------------------------------------'
           DISPLAY ' REGISTROS LIDOS ' WRK-CONT-LIDOS.
           DISPLAY ' REGISTROS GRAVADOS ' WRK-CONT-GRAVADOS.
           DISPLAY ' REGISTROS COM ERROS ' WRK-CONT-ERRO.
           DISPLAY '----------------------------------------'

           CALL WRK-DATAEXT USING LNK-INFO-DATA.
           DISPLAY 'DATA ' WRK-DIA-EXT.
           DISPLAY 'HORA ' WRK-HORA-EXT.

           DISPLAY '----------------------------------------'
           DISPLAY ' FINAL DE PROCESSAMENTO '.
           GOBACK.

      *----------------------------------------------------------------
       3000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------
