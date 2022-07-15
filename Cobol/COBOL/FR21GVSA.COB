      *===============================================================*
       IDENTIFICATION                                        DIVISION.
      *===============================================================*
       PROGRAM-ID. FR21GVSA.
      *===============================================================*
      *   AUTOR....:VICTOR ARANDA                                     *
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR        *
      *   DATA ....:27/06/2022                                        *
      *---------------------------------------------------------------*
      *   OBJETIVO:LEITURA DE UM ARQUIVO (VSAM)                       *
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

       INPUT-OUTPUT                                            SECTION.
        FILE-CONTROL.
             SELECT ARQVSAM  ASSIGN TO ARQVSAM
                ORGANIZATION  IS INDEXED
                ACCESS MODE   IS DYNAMIC
                RECORD KEY    IS ARQ-CHAVE
                FILE STATUS   IS FS-ARQVSAM.
      *===============================================================*
       DATA                                                   DIVISION.
      *===============================================================*
       FILE SECTION.
        FD ARQVSAM.

        01 REG-ARQVSAM.
           02 ARQ-CHAVE   PIC X(5).
           02 ARQ-NOME    PIC X(30).
           02 ARQ-SETOR   PIC X(04).
           02 ARQ-SALARIO PIC X(10).
           02 ARQ-DATAADM PIC X(10).
           02 ARQ-EMAIL   PIC X(40).

      *===============================================================*
       WORKING-STORAGE                                        SECTION.
      *===============================================================*

       77 FS-ARQVSAM     PIC 9(2).

       01 WRK-ARQVSAM.
          02 WRK-CHAVE   PIC X(5).
          02 WRK-NOME    PIC X(30).
          02 WRK-SETOR   PIC X(04).
          02 WRK-SALARIO PIC X(10).
          02 WRK-DATAADM PIC X(10).
          02 WRK-EMAIL   PIC X(40).




      *===============================================================*
       PROCEDURE                                             DIVISION.
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

           OPEN I-O   ARQVSAM.
           IF FS-ARQVSAM NOT EQUAL ZEROS
              DISPLAY 'ERRO NA ABERTURA DO ARQUIVO ARQVSAM'
              DISPLAY 'CODIGO VSAM ' FS-ARQVSAM
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------
       1000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       2000-PROCESSAR                                         SECTION.
      *----------------------------------------------------------------

           ACCEPT WRK-CHAVE    FROM SYSIN.
           ACCEPT WRK-NOME     FROM SYSIN.
           ACCEPT WRK-SETOR    FROM SYSIN.
           ACCEPT WRK-SALARIO  FROM SYSIN.
           ACCEPT WRK-DATAADM  FROM SYSIN.
           ACCEPT WRK-EMAIL    FROM SYSIN.

           MOVE   WRK-CHAVE    TO ARQ-CHAVE.
           MOVE   WRK-NOME     TO ARQ-NOME.
           MOVE   WRK-SETOR    TO ARQ-SETOR.
           MOVE   WRK-SALARIO  TO ARQ-SALARIO.
           MOVE   WRK-DATAADM  TO ARQ-DATAADM.
           MOVE   WRK-EMAIL    TO ARQ-EMAIL.

           READ ARQVSAM.

            IF (FS-ARQVSAM EQUAL 0)
                  DISPLAY 'REGISTRO ' ARQ-CHAVE ' JA EXISTENTE'
                  DISPLAY 'FILESTATUS VSAM: ' FS-ARQVSAM
                ELSE
                  WRITE REG-ARQVSAM FROM WRK-ARQVSAM
                  IF (FS-ARQVSAM NOT EQUAL 0)
                    DISPLAY 'ERRO DURANTE GRAVACAO'
                    DISPLAY 'FILESTATUS VSAM: ' FS-ARQVSAM
                  ELSE
                    DISPLAY 'REGISTRO ' ARQ-CHAVE ' CADASTRADO'
                  END-IF
                END-IF.




      *----------------------------------------------------------------
       2000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       3000-FINALIZAR                                         SECTION.
      *----------------------------------------------------------------

           CLOSE ARQVSAM.
           DISPLAY REG-ARQVSAM
           DISPLAY 'REGISTRO INCLUIDO '.
           GOBACK.

      *----------------------------------------------------------------
       3000-99-FIM.                                              EXIT.
      *----------------------------------------------------------------
