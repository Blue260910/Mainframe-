      ******************************************************************00010042
      * Author: VICTOR ARANDA                                           00020042
      * Date:                                                           00030042
      * Purpose:                                                        00040042
      * Tectonics FOURSYS                                               00050042
      ******************************************************************00060042
       IDENTIFICATION                                          DIVISION.00070042
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-00080042
       PROGRAM-ID. PRGTEST4.                                            00090042
       ENVIRONMENT                                             DIVISION.00100042
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-00110042
       CONFIGURATION                                            SECTION.00120042
       SPECIAL-NAMES.                                                   00130042
           DECIMAL-POINT IS COMMA.                                      00140042
                                                                        00150042
       INPUT-OUTPUT                                             SECTION.00160042
                                                                        00170042
       FILE-CONTROL.                                                    00180042
           SELECT CLIENTES ASSIGN TO CLIENTES                           00190042
               FILE STATUS     IS    WRK-STATUS-IN.                     00200043
                                                                        00210042
           SELECT SAIDACLI ASSIGN TO SAIDACLI                           00220043
               FILE STATUS     IS    WRK-STATUS-OUT.                    00221043
                                                                        00222043
                                                                        00223043
      *-----------------------------------------------------------------00230042
       DATA                                                    DIVISION.00240042
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-00250042
       FILE                                                     SECTION.00260042
      *-----------------------------------------------------------------00270042
                                                                        00280042
       FD CLIENTES                                                      00290042
           RECORDING MODE IS F                                          00300042
           BLOCK CONTAINS 0 RECORDS.                                    00310042
                                                                        00320044
       01 FD-CLIENTES.                                                  00330044
           05 FD-CODIGO            PIC 9(02).                           00340042
           05 FILLER               PIC X(56).                           00350042
                                                                        00360042
                                                                        00361044
       FD SAIDACLI                                                      00362044
           RECORDING MODE IS F                                          00363044
           BLOCK CONTAINS 0 RECORDS.                                    00364044
                                                                        00365044
       01 FD-SAIDACLI.                                                  00366044
        05 WRK-NUM-VEND-OUT           PIC 9(02).                        00370044
        05 WRK-PRODUTO-NOME-OUT       PIC X(14).                        00371044
        05 WRK-VALOR-PROD-ED-OUT      PIC 9(05).                        00372044
        05 FILLER                     PIC X(37).                        00372144
                                                                        00373044
                                                                        00374044
                                                                        00375044
      *-----------------------------------------------------------------00380042
       WORKING-STORAGE                                          SECTION.00390042
      *-----------------------TABELA------------------------------------00400042
                                                                        00410042
       01 WRK-TAB-PRODUTO.                                              00420042
          05 FILLER PIC X(21) VALUE '01TELEVISAO     02500'.            00430042
          05 FILLER PIC X(21) VALUE '02SECADORA      02000'.            00440042
          05 FILLER PIC X(21) VALUE '03GELADEIRA     03500'.            00450042
          05 FILLER PIC X(21) VALUE '04MESA          01000'.            00460042
          05 FILLER PIC X(21) VALUE '05COMPUTADOR    04550'.            00470042
          05 FILLER PIC X(21) VALUE '06CAFETEIRA     00300'.            00480042
          05 FILLER PIC X(21) VALUE '07CAMERA        01500'.            00490042
          05 FILLER PIC X(21) VALUE '08FOGAO         00800'.            00500042
          05 FILLER PIC X(21) VALUE '09QUADRO        09500'.            00510042
          05 FILLER PIC X(21) VALUE '10LAPIS         00001'.            00520042
          05 FILLER PIC X(21) VALUE '11CANETA        00002'.            00530042
          05 FILLER PIC X(21) VALUE '12VIDEO GAME    05000'.            00540042
          05 FILLER PIC X(21) VALUE '13CELULAR       03200'.            00550042
          05 FILLER PIC X(21) VALUE '14VIOLAO        01100'.            00560042
          05 FILLER PIC X(21) VALUE '15GUITARRA      02100'.            00570042
                                                                        00580042
                                                                        00590042
                                                                        00600042
                                                                        00610042
                                                                        00620042
       01 FILLER REDEFINES WRK-TAB-PRODUTO.                             00630042
          05 WRK-TAB-PROD OCCURS 15 TIMES.                              00640042
             07 WRK-CODIGO-TAB     PIC 9(02).                           00650042
             07 WRK-PRODUTO-TAB    PIC X(14).                           00660042
             07 WRK-VALOR-TAB      PIC 9(05).                           00670042
                                                                        00680042
                                                                        00690042
       01 FILLER.                                                       00700042
          05 PROD-FINAL OCCURS 15 TIMES.                                00710042
             07 WRK-NUM-VEND        PIC 9(02).                          00720042
             07 WRK-PRODUTO-NOME    PIC X(14).                          00730042
             07 WRK-VALOR-PROD      PIC 9(05).                          00740042
                                                                        00750042
       01 WRK-INFORMACAO.                                               00760042
          05 WRK-DATA-INFO            PIC X(23).                        00770042
          05 WRK-HORARIO-INFO         PIC X(08).                        00780042
                                                                        00790042
                                                                        00800042
                                                                        00810042
                                                                        00820042
       77 WRK-IND                  PIC 9(02).                           00830042
                                                                        00840042
       77 WRK-ACHOU                PIC X(01) VALUE 'N'.                 00850042
                                                                        00860042
                                                                        00870042
                                                                        00880042
      *-----------------------LOGICA------------------------------------00890042
                                                                        00900042
       77 WRK-LIDOS                PIC 9(02) VALUE ZEROS.               00910042
                                                                        00920042
       77 WRK-ACUM-PRODUTOS        PIC 9(02).                           00930042
                                                                        00940042
       77 WRK-ACUM-VALOR           PIC 9(08).                           00950042
                                                                        00960042
       01 WRK-TEMPO.                                                    00970042
          05 WRK-DATA                 PIC X(23).                        00980042
          05 WRK-HORARIO              PIC X(08).                        00990042
                                                                        01000042
       77 WRK-STATUS-IN               PIC 9(02).                        01010044
       77 WRK-STATUS-OUT              PIC 9(02).                        01011044
      *-----------------------ERROS-------------------------------------01020042
                                                                        01030042
       01 COMMAREA.                                                     01040042
          05 WRK-PRGLOG-NOME          PIC X(08).                        01050042
          05 WRK-PRGLOG-SECAO         PIC X(04).                        01060042
          05 WRK-PRGLOG-MENSAGEM      PIC X(30).                        01070042
          05 WRK-PRGLOG-STATUS        PIC X(02).                        01080042
          05 FILLER                   PIC X(14).                        01090042
      *-----------------------MASCARA-----------------------------------01100042
                                                                        01110042
       77 WRK-ACUM-VALOR-ED        PIC ZZ.ZZZ,ZZ.                       01120042
                                                                        01130042
       77 WRK-VALOR-PROD-ED        PIC ZZ.ZZZ,ZZ.                       01140042
                                                                        01150042
      *-----------------------DISPLAYS----------------------------------01151047
                                                                        01152047
       77 DISP-ESPACOS             PIC X(01) VALUE SPACES.              01153047
       77 DISP-SEPARACAO           PIC X(30) VALUE ALL '-'.             01153147
       77 DISP-MENSAGEM            PIC X(30).                           01153247
                                                                        01153347
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-01160042
                                                                        01170042
                                                                        01180042
       PROCEDURE                                               DIVISION.01190042
                                                                        01200042
                                                                        01210042
                                                                        01220042
           PERFORM 1000-INICIAR.                                        01230042
                                                                        01240042
                                                                        01250042
           PERFORM 2000-PROCESSAR UNTIL WRK-STATUS-IN NOT EQUAL ZEROS.  01260044
                                                                        01270042
                                                                        01280042
           PERFORM 3000-FINALIZAR.                                      01290042
                                                                        01300042
                                                                        01310042
                                                                        01320042
       1000-INICIAR                                             SECTION.01330042
           OPEN INPUT CLIENTES.                                         01340042
           OPEN OUTPUT SAIDACLI.                                        01341044
           PERFORM 9000-ERROS.                                          01342049
           READ CLIENTES.                                               01350042
                                                                        01360042
       1000-99-FIM.                                             EXIT.   01370042
                                                                        01380042
       2000-PROCESSAR                                           SECTION.01390042
           MOVE 'N' TO WRK-ACHOU.                                       01400042
           MOVE  ZEROS  TO WRK-IND.                                     01410042
                                                                        01420042
                                                                        01430042
           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND > 15 OR    01440042
                                                 WRK-ACHOU = 'S'        01450042
           IF FD-CODIGO  EQUAL WRK-CODIGO-TAB(WRK-IND)                  01460042
                                                                        01470042
                                                                        01480042
             MOVE 'S' TO WRK-ACHOU                                      01490042
             ADD WRK-VALOR-TAB(WRK-IND) TO WRK-ACUM-VALOR               01500042
             ADD WRK-VALOR-TAB(WRK-IND) TO WRK-VALOR-PROD(WRK-IND)      01510042
             ADD 1 TO WRK-NUM-VEND(WRK-IND)                             01520042
             MOVE WRK-PRODUTO-TAB(WRK-IND) TO WRK-PRODUTO-NOME(WRK-IND) 01530042
             DISPLAY WRK-PRODUTO-TAB(WRK-IND)  'VALIDO'                 01540042
                                                                        01550042
           END-IF                                                       01560042
                                                                        01570042
           END-PERFORM.                                                 01580042
                                                                        01590042
           IF WRK-ACHOU EQUAL 'S'                                       01600042
                                                                        01610042
           ADD 1 TO WRK-ACUM-PRODUTOS                                   01620042
                                                                        01630042
           ELSE                                                         01640042
           DISPLAY 'NAO ENCONTRADO'                                     01650042
           END-IF.                                                      01660042
                                                                        01670042
           ADD 1 TO WRK-LIDOS.                                          01680042
                                                                        01690042
                                                                        01700042
                                                                        01710042
           READ CLIENTES.                                               01720042
                                                                        01730042
                                                                        01740042
                                                                        01750042
       2000-99-FIM.                                             EXIT.   01760042
                                                                        01770042
       3000-FINALIZAR                                           SECTION.01780042
           MOVE ZEROS TO WRK-IND.                                       01790042
           DISPLAY DISP-MENSAGEM.                                       01800047
                                                                        01810042
           DISPLAY DISP-SEPARACAO.                                      01820047
           CALL 'DATAEXT' USING  WRK-INFORMACAO.                        01830042
           DISPLAY WRK-HORARIO-INFO WRK-DATA-INFO.                      01840042
           DISPLAY DISP-SEPARACAO.                                      01850047
                                                                        01860042
                                                                        01870042
           MOVE WRK-ACUM-VALOR TO WRK-ACUM-VALOR-ED.                    01880042
           DISPLAY DISP-MENSAGEM.                                       01890047
           DISPLAY 'TOTAL DE LIDOS   ' WRK-LIDOS.                       01900042
           DISPLAY 'TOTAL DE VALIDOS ' WRK-ACUM-PRODUTOS.               01910042
           DISPLAY DISP-MENSAGEM.                                       01920047
           DISPLAY DISP-SEPARACAO.                                      01930048
           DISPLAY 'VALOR FINAL DA COMPRA ' WRK-ACUM-VALOR-ED.          01940042
           DISPLAY DISP-SEPARACAO.                                      01950047
           DISPLAY DISP-MENSAGEM.                                       01960047
                                                                        01970042
           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND > 15       01980042
           IF WRK-NUM-VEND(WRK-IND) NOT NUMERIC                         01990042
              MOVE ZEROS TO WRK-NUM-VEND(WRK-IND)                       02000042
           END-IF                                                       02010042
           IF WRK-NUM-VEND(WRK-IND) GREATER ZEROS                       02020042
           MOVE WRK-VALOR-PROD(WRK-IND) TO WRK-VALOR-PROD-ED            02030042
            DISPLAY WRK-NUM-VEND(WRK-IND) ' ' WRK-PRODUTO-NOME(WRK-IND) 02040042
                    WRK-VALOR-PROD-ED                                   02050042
                                                                        02051044
           MOVE WRK-NUM-VEND(WRK-IND)     TO  WRK-NUM-VEND-OUT          02052046
           MOVE WRK-PRODUTO-NOME(WRK-IND) TO  WRK-PRODUTO-NOME-OUT      02053046
           MOVE WRK-VALOR-PROD-ED         TO  WRK-VALOR-PROD-ED-OUT     02054046
           WRITE FD-SAIDACLI                                            02055046
                                                                        02056044
           END-IF                                                       02060042
           END-PERFORM.                                                 02070042
           DISPLAY DISP-MENSAGEM.                                       02080047
           DISPLAY DISP-SEPARACAO.                                      02090047
           DISPLAY 'FIM DE PROGRAMA'.                                   02100042
                                                                        02110049
           CLOSE CLIENTES.                                              02190042
           CLOSE SAIDACLI.                                              02191044
           STOP RUN.                                                    02200042
                                                                        02210042
       3000-99-FIM.                                             EXIT.   02220042
                                                                        02230049
       9000-ERROS                                               SECTION.02240049
                                                                        02250049
           IF WRK-STATUS-IN NOT EQUAL ZEROS                             02251052
           MOVE 'PRGTEST4' TO WRK-PRGLOG-NOME                           02252050
           MOVE '1000' TO WRK-PRGLOG-SECAO                              02253050
           MOVE 'ERRO DE OPEN INPUT' TO WRK-PRGLOG-MENSAGEM             02254050
           MOVE WRK-STATUS-IN TO  WRK-PRGLOG-STATUS                     02255050
           CALL 'GRAVALOG' USING COMMAREA                               02256050
           DISPLAY COMMAREA                                             02257050
           END-IF.                                                      02258051
           EXIT.                                                        02260051
