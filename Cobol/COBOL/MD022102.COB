      *===============================================================*
       IDENTIFICATION                                        DIVISION.
      *===============================================================*
       PROGRAM-ID. MD022102.
      *===============================================================*
      *   AUTOR....:VICTOR ARANDA                                     *
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR        *
      *   DATA ....:07/07/2022                                        *
      *---------------------------------------------------------------*
      * VERSAO   DATA         AUTOR        DESCRICAO DA MODIFICACAO   *
      *  1.0   07/07/22   VICTOR ARANDA       VERSAO INICIAL          *
      *---------------------------------------------------------------*
      *   OBJETIVO: CONSULTAR/GRAVAR/EXCLUIR/REESCREVER UM REGISTRO   *
      *             NO ARQUIVO VSAM UTILIZANDO PARM                   *
      *                                                               *
      *             L (LEITURA)                                       *
      *             G (GRAVACAO)                                      *
      *             E (EXCLUSAO)                                      *
      *             R (REGRAVACAO)                                    *
      *---------------------------------------------------------------*
      *   ARQUIVOS:                                 INCLUDE/BOOK      *
      *   DDNAME......         I/O                   LNKEVSA          *
      *   EVSA0407             I-O                   EVSABOOK         *
      *---------------------------------------------------------------*
      *   MODULOS....:                                                *
      *   DATAEXE (OBTEM DATA POR EXTENSO                             *
4SYSV *---------------------------------------------------------------*
  |   *//////////////////  A L T E R A C O E S  //////////////////////*
  |   *---------------------------------------------------------------*
  |   *   ANALISTA......:VICTOR ARANDA                                *
  |   *   DATA..........:07/07/2022                                   *
  |   *   CONSULTORIA...:FOURSYS|4SYSV                                *
  |   *---------------------------------------------------------------*
  |   *   OBJETIVO: IMPLEMENTAR VERIFICACOES PARA GRAVACAO, EXCLUSAO  *
  |   *             E REGRAVACAO | IMPLEMENTACAO DE FILLERS           *
  |   *                                                               *
4SYSV *===============================================================*
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
      *===============================================================*
       DATA                                                   DIVISION.
      *===============================================================*
       FILE SECTION.
       FD EVSA0407.

4SYSV  COPY 'EVSABOOK'.

      *===============================================================*
       WORKING-STORAGE                                        SECTION.
4SYSV *===============================================================*
  |    01 FILLER                                       PIC X(70) VALUE
  |        '---------VARIAVEIS DE APOIO--------------------'.
  |
4SYSV *==============VARIAVEIS DE APOIO===============================*

       77 FS-EVSA0407         PIC 9(2).
       77 WRK-LEN             PIC 9(04) COMP.
       77 WRK-MSG             PIC X(30).

4SYSV *===============================================================*
  |    01 FILLER                                       PIC X(70) VALUE
  |        '---------VARIAVEIS DE MENSAGENS----------------'.
  |
4SYSV *==============VARIAVEIS DE MENSAGENS===========================*

       77 WRK-MSGCODIGO-VSAM  PIC X(13) VALUE 'CODIGO VSAM: '.
       77 WRK-ERRO-OPEN       PIC X(21) VALUE 'ERRO NO OPEN EVSA0407'.
       77 WRK-ERRO-CLOSE      PIC X(22) VALUE 'ERRO NO CLOSE EVSA0407'.
       77 WRK-MSGVAZIO        PIC X(22) VALUE 'ARQUIVO EVSA0407 VAZIO'.
       77 WRK-MSGTAMANHO      PIC X(17) VALUE 'TAMANHO INCORRETO'.


       77 WRK-ERRO-LEITURA    PIC X(14) VALUE 'NAO ENCONTRADO'.
       77 WRK-ERRO-GRAVACAO   PIC X(21) VALUE 'REGISTRO JA EXISTENTE'.
       77 WRK-ERRO-SALARIO    PIC X(16) VALUE 'SALARIO INVALIDO'.
       77 WRK-ERRO-EXCLUSAO   PIC X(12) VALUE 'NAO EXCLUIDO'.
       77 WRK-ERRO-REGRAVADO  PIC X(13) VALUE 'NAO REGRAVADO'.


       77 WRK-MSG-GRAVADO     PIC X(07) VALUE 'GRAVADO'.
       77 WRK-MSG-EXCLUIDO    PIC X(08) VALUE 'EXCLUIDO'.
       77 WRK-MSG-REGRAVADO   PIC X(09) VALUE 'REGRAVADO'.


       77 WRK-SEPARACAO       PIC X(37) VALUE ALL '-'.

4SYSV *===============================================================*
  |    01 FILLER                                       PIC X(70) VALUE
  |        '---------VARIAVEIS DO MODULO-------------------'.
  |
4SYSV *==============VARIAVEIS DO MODULO==============================*

       77 WRK-DATAEXT         PIC X(07) VALUE 'DATAEXT'.

      *----------------------------------------------------------------
       LINKAGE                                                 SECTION.
      *----------------------------------------------------------------

4SYSV  COPY 'LNKEVSA'.

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

           OPEN I-O EVSA0407.
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

           EVALUATE LNK-OPERACAO
             WHEN 'L'
              PERFORM  2100-LEITURA
             WHEN 'G'
              PERFORM  2200-GRAVACAO
             WHEN 'E'
              PERFORM  2300-EXCLUSAO
             WHEN 'R'
              PERFORM  2400-REGRAVAR
           END-EVALUATE.

      *----------------------------------------------------------------
       2000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       2100-LEITURA                                           SECTION.
      *----------------------------------------------------------------
            IF LNK-LEN EQUAL 10
               MOVE LNK-CHAVE TO ARQ-CHAVE
               READ EVSA0407

               IF FS-EVSA0407 EQUAL ZEROS
                  DISPLAY REG-EVSA0407
               ELSE
                  MOVE WRK-ERRO-LEITURA TO WRK-MSG
                  MOVE FS-EVSA0407 TO WRK-MSG(24:02)
                  PERFORM 3000-FINALIZAR
               END-IF

            ELSE
                 MOVE WRK-MSGTAMANHO TO WRK-MSG
                 MOVE LNK-LEN TO WRK-MSG(24:04)
                 PERFORM 3000-FINALIZAR
            END-IF.
      *----------------------------------------------------------------
       2100-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       2200-GRAVACAO                                          SECTION.
      *----------------------------------------------------------------
4SYSV
  |        IF LNK-LEN EQUAL 20
  |           IF LNK-SALARIO IS NUMERIC
  |              MOVE LNK-CHAVE   TO ARQ-CHAVE
  |              READ EVSA0407
  |
  |              IF FS-EVSA0407 EQUAL ZEROS
  |                 MOVE WRK-ERRO-GRAVACAO TO WRK-MSG
  |                 MOVE FS-EVSA0407 TO WRK-MSG(24:02)
  |                 PERFORM 3000-FINALIZAR
  |              ELSE
  |              MOVE LNK-SALARIO TO ARQ-SALARIO
  |                 WRITE REG-EVSA0407
  |                 DISPLAY REG-EVSA0407
4SYSV               MOVE WRK-MSG-GRAVADO TO WRK-MSG
                 END-IF
              ELSE
                 MOVE WRK-ERRO-SALARIO TO WRK-MSG
                 PERFORM 3000-FINALIZAR
              END-IF
           ELSE
               MOVE WRK-MSGTAMANHO TO WRK-MSG
               MOVE LNK-LEN TO WRK-MSG(24:04)
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------
       2200-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       2300-EXCLUSAO                                          SECTION.
      *----------------------------------------------------------------
4SYSV      IF LNK-LEN EQUAL 10
  |           MOVE LNK-CHAVE TO ARQ-CHAVE
  |           READ EVSA0407
  |
  |           IF FS-EVSA0407 EQUAL ZEROS
  |              DELETE EVSA0407
  |              DISPLAY ARQ-CHAVE
  |              MOVE WRK-MSG-EXCLUIDO TO WRK-MSG
4SYSV         ELSE
                 MOVE WRK-ERRO-EXCLUSAO TO WRK-MSG
                 MOVE FS-EVSA0407 TO WRK-MSG(24:02)
                 PERFORM 3000-FINALIZAR
              END-IF

           ELSE
              MOVE WRK-MSGTAMANHO TO WRK-MSG
              MOVE LNK-LEN TO WRK-MSG(24:04)
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------
       2300-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       2400-REGRAVAR                                          SECTION.
      *----------------------------------------------------------------
4SYSV      IF LNK-LEN EQUAL 20
  |           MOVE LNK-CHAVE   TO ARQ-CHAVE
  |           READ EVSA0407
  |           IF LNK-SALARIO IS NUMERIC AND FS-EVSA0407 EQUAL ZEROS
  |              MOVE LNK-SALARIO TO ARQ-SALARIO
  |              REWRITE REG-EVSA0407
  |
  |              IF FS-EVSA0407 EQUAL ZEROS
  |                 DISPLAY REG-EVSA0407
  |                 MOVE WRK-MSG-REGRAVADO TO WRK-MSG
  |              ELSE
  |                 MOVE WRK-ERRO-REGRAVADO TO WRK-MSG
  |                 MOVE FS-EVSA0407 TO WRK-MSG(24:02)
  |                 PERFORM 3000-FINALIZAR
  |              END-IF
  |
  |           IF LNK-SALARIO IS NOT NUMERIC
  |              MOVE WRK-ERRO-SALARIO TO WRK-MSG
  |              PERFORM 3000-FINALIZAR
  |           ELSE
  |              MOVE WRK-ERRO-REGRAVADO TO WRK-MSG
  |              MOVE FS-EVSA0407 TO WRK-MSG(24:02)
  |              PERFORM 3000-FINALIZAR
4SYSV         END-IF
           ELSE
              MOVE WRK-MSGTAMANHO TO WRK-MSG
              MOVE LNK-LEN TO WRK-MSG(24:04)
              PERFORM 3000-FINALIZAR
           END-IF.
      *----------------------------------------------------------------
       2400-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       3000-FINALIZAR                                         SECTION.
      *----------------------------------------------------------------

           CLOSE EVSA0407.
           DISPLAY WRK-MSG.
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
