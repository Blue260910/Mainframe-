      *=====================================================            00010000
       IDENTIFICATION                             DIVISION.             00020000
      *=====================================================            00030000
       PROGRAM-ID. FR21CB02                                             00040000
      *=============================================================*   00050000
      *   AUTOR....:VICTOR ARANDA                                   *   00051000
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00052000
      *   DATA ....:03/05/2022                                      *   00053000
      *-------------------------------------------------------------*   00054000
      *   OBJETIVO:ACCEPT DE DADOS E PROCESSAMENTO                  *   00055000
      *                                                             *   00056000
      *                                                             *   00057000
      *-------------------------------------------------------------*   00058000
      *   ARQUIVOS:                                                 *   00059000
      *    DDNAME              I/O                  COPY/BOOK       *   00059100
      *                                                             *   00060000
      *                                                             *   00070000
      *=============================================================*   00080000
                                                                        00090100
      *=====================================================            00091000
       ENVIRONMENT                                 DIVISION.            00092000
      *=====================================================            00093000
                                                                        00100000
      *=====================================================            00101000
       DATA                                        DIVISION.            00102000
      *=====================================================            00103000
       WORKING-STORAGE                              SECTION.            00103100
       77 NOME        PIC       X(20)         VALUE SPACES.             00103200
       77 IDADE       PIC       9(02)         VALUE ZEROS.              00103300
       77 ENDER       PIC       X(20)         VALUE SPACES.             00103400
       77 SALARIO     PIC       9(04)         VALUE ZEROS.              00103500
       01 WRK-DATA.                                                     00103800
          05 WRK-ANO    PIC 9(04).                                      00103900
          05 WRK-MES    PIC 9(02).                                      00104000
          05 WRK-DIA    PIC 9(02).                                      00105000
       77 WRK-ANO-2     PIC 9(04).                                      00106000
      *====================================================             00106100
       PROCEDURE                                 DIVISION.              00106200
      *====================================================             00106300
           ACCEPT   NOME         FROM  SYSIN.                           00107000
           ACCEPT   IDADE        FROM  SYSIN.                           00108000
           ACCEPT   ENDER        FROM  SYSIN.                           00109000
           ACCEPT   SALARIO      FROM  SYSIN.                           00109100
           ACCEPT   WRK-DATA     FROM  DATE    YYYYMMDD.                00109300
           MOVE     WRK-DATA(1:4) TO   WRK-ANO-2                        00109400
           DISPLAY 'DATA ... ' WRK-DIA '/'  WRK-MES ' DE ' WRK-ANO.     00109500
           SUBTRACT IDADE        FROM  WRK-ANO.                         00109600
           DISPLAY 'MEU NOME... '      NOME.                            00110000
           DISPLAY 'SUA IDADE.. '      IDADE.                           00110100
           IF      IDADE >=  18                                         00110200
              DISPLAY 'MAIOR DE IDADE'                                  00110300
           ELSE                                                         00110400
              DISPLAY 'MENOR DE IDADE'                                  00110500
           END-IF.                                                      00110600
           DISPLAY 'NASCEU EM '        WRK-ANO.                         00110700
           DISPLAY 'ENDECO... '        ENDER.                           00111000
           DISPLAY 'SALARIO... '       SALARIO.                         00111100
           DISPLAY ' SO ANO  '         WRK-ANO-2.                       00111200
           STOP RUN.                                                    00120000
