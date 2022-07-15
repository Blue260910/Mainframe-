
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. DATAEXT.

      *=============================================================*
      *   AUTOR....:VICTOR ARANDA                                   *
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *
      *   DATA ....:26/05/2022                                      *
      *-------------------------------------------------------------*
      *   OBJETIVO:DATA POR EXTENSO                                 *
      *                                                             *
      *                                                             *
      *-------------------------------------------------------------*
      *   ARQUIVOS:                                                 *
      *    DDNAME              I/O                  COPY/BOOK       *
      *                                                             *
      *                                                             *
      *=============================================================*
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                                          SECTION.

       01 FILLER.
          05 MESES       PIC X(09) OCCURS 12 TIMES.

       01 WRK-DATA1.
          05 WRK-ANO     PIC 9(04).
          05 WRK-MES     PIC 9(02).
          05 WRK-DIA     PIC 9(02).

       01 WRK-DATA2.
          05 WRK-DIA     PIC 9(02).
          05 FILLER      PIC X(04) VALUE ' DE '.
          05 WRK-MES-EXT PIC X(09).
          05 FILLER      PIC X(04) VALUE ' DE '.
          05 WRK-ANO     PIC 9(04).


      *--------------------------HORARIO--------------------------------

       01 WRK-HORARIO1.
          05 WRK-HORA     PIC 9(02).
          05 WRK-MINUTO   PIC 9(02).
          05 WRK-SEGUNDO  PIC 9(02).


       01 WRK-HORARIO2.
          05 WRK-HORA     PIC 9(02).
          05 FILLER       PIC X(01) VALUE ':'.
          05 WRK-MINUTO   PIC 9(02).
          05 FILLER       PIC X(01) VALUE ':'.
          05 WRK-SEGUNDO  PIC 9(02).

      *-----------------------------------------------------------------
       LINKAGE                                                  SECTION.

       01 AREA-TRANSFERENCIA.
          02 LINK-DATA       PIC X(23).
          02 LINK-HORA       PIC X(8).
      *-----------------------------------------------------------------

       PROCEDURE DIVISION USING AREA-TRANSFERENCIA.

       0000-PRINCIPAL                                           SECTION.

           PERFORM 1000-TABELA-DE-MES.
           ACCEPT WRK-DATA1 FROM DATE YYYYMMDD.
           ACCEPT WRK-HORARIO1 FROM TIME.
           MOVE CORRESPONDING WRK-HORARIO1 TO WRK-HORARIO2.
           MOVE CORRESPONDING WRK-DATA1 TO WRK-DATA2.
           MOVE MESES(WRK-MES) TO WRK-MES-EXT.
           MOVE WRK-HORARIO2 TO LINK-HORA.
           MOVE WRK-DATA2 TO LINK-DATA.
           GOBACK.








       1000-TABELA-DE-MES                                       SECTION.

           MOVE 'JANEIRO  ' TO MESES(1).
           MOVE 'FEVEREIRO' TO MESES(2).
           MOVE 'MARCO    ' TO MESES(3).
           MOVE 'ABRIL    ' TO MESES(4).
           MOVE 'MAIO     ' TO MESES(5).
           MOVE 'JUNHO    ' TO MESES(6).
           MOVE 'JULHO    ' TO MESES(7).
           MOVE 'AGOSTO   ' TO MESES(8).
           MOVE 'SETEMBRO ' TO MESES(9).
           MOVE 'OUTUBRO  ' TO MESES(10).
           MOVE 'NOVEMBRO ' TO MESES(11).
           MOVE 'DEZEMBRO ' TO MESES(12).
