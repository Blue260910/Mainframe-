      *=============================================================*   00010000
       IDENTIFICATION                            DIVISION.              00020000
      *=============================================================*   00030000
                                                                        00040000
       PROGRAM-ID. RELAT01.                                             00050029
                                                                        00060000
      *=============================================================*   00070000
      *   AUTOR...........: VICTOR ARANDA                           *   00080001
      *                                                             *   00081059
      *   DATA ...........: 24/05/2022                              *   00090001
      *                                                             *   00091059
      *   OBJETIVO........: FAZER RELATORIO DE FUNCIONARIOS         *   00100059
      *                     E SEPARAR POR DEPARTAMENTO, FAZENDO     *   00101059
      *                     AGRUPAMENTO DE SALARIOS POR SETOR.      *   00102059
      *=============================================================*   00110000
      *                                                             *   00120059
      *   ARQUIVOS.....:                                            *   00122059
      *                                                             *   00123059
      *   NAME                I/O                                   *   00124059
      *   ARQUIIN              I                                    *   00126059
      *   ARQUIOUT             O                                    *   00127059
      *                                                             *   00128059
      *=============================================================*   00129059
      *                                                             *   00131059
      *   MODULOS....:                                              *   00132059
      *                                                             *   00133059
      *   NAME                                                      *   00134059
      *   GRAVALOG             GRAVA LOG DE ERROS                   *   00135059
      *   DATAEXE              OBTEM DATA E HORA ATUAL              *   00136059
      *                                                             *   00137059
      *=============================================================*   00138059
       ENVIRONMENT                               DIVISION.              00140000
      *=============================================================*   00150000
                                                                        00160000
      *-------------------------------------------------------------*   00170000
       CONFIGURATION                                        SECTION.    00180001
      *-------------------------------------------------------------*   00190000
       SPECIAL-NAMES.                                                   00200000
           DECIMAL-POINT IS COMMA.                                      00210000
                                                                        00220000
                                                                        00230000
       INPUT-OUTPUT                                         SECTION.    00240001
       FILE-CONTROL.                                                    00250000
           SELECT ARQUIIN  ASSIGN TO ARQUIIN                            00260003
               FILE STATUS  IS WRK-FS-ARQUIIN.                          00270003
                                                                        00271044
           SELECT ARQUIOUT ASSIGN TO ARQUIOUT                           00273050
               FILE STATUS  IS WRK-FS-ARQUIOUT.                         00274050
                                                                        00275044
                                                                        00280000
      *=============================================================*   00290000
       DATA                                                DIVISION.    00300001
      *=============================================================*   00310000
       FILE                                                 SECTION.    00320001
      *=============================================================*   00330001
                                                                        00330160
      *      ARQUIVO DE ENTRADA                                         00331060
       FD ARQUIIN                                                       00340003
           RECORDING  MODE IS F                                         00350000
           BLOCK CONTAINS 0 RECORDS.                                    00360000
                                                                        00370000
                                                                        00390000
       01 FD-ARQUIIN.                                                   00400003
          05 FD-CODIGO    PIC X(05).                                    00410030
          05 FD-NOME      PIC X(30).                                    00420011
          05 FD-SETOR     PIC 9(04).                                    00430011
          05 FD-SALARIO   PIC 9(06).                                    00440011
          05 FILLER       PIC X(13).                                    00450012
                                                                        00460011
                                                                        00460345
      *      ARQUIVO DE SAIDA                                           00460460
                                                                        00460560
       FD ARQUIOUT                                                      00461045
           RECORDING  MODE IS F                                         00462045
           BLOCK CONTAINS 0 RECORDS.                                    00463045
                                                                        00463158
                                                                        00464158
       01 FD-ARQUIOUT     PIC X(67).                                    00465051
                                                                        00466045
                                                                        00467045
                                                                        00468045
      *-------------------------------------------------------------*   00470000
       WORKING-STORAGE                                      SECTION.    00480002
      *-------------------------------------------------------------*   00490000
                                                                        00490130
      *------------------LOG-DE-ERROS-------------------------------*   00490230
                                                                        00490330
       01 WRK-DADOS.                                                    00491030
           05 WRK-PROGRAMA  PIC X(08).                                  00492030
           05 WRK-SECAO     PIC X(04).                                  00493030
           05 WRK-MENSAGEM  PIC X(30).                                  00494030
           05 WRK-STATUS    PIC X(02).                                  00495030
                                                                        00496030
      *------------------CABECALHO----------------------------------*   00500031
                                                                        00500130
           COPY '#CAB'.                                                 00500443
                                                                        00500558
      *--------------------LOGICA-----------------------------------*   00502034
                                                                        00503034
       77 WRK-LINHAS          PIC 9(03) VALUE 1.                        00530044
                                                                        00530138
       77 WRK-SETOR-ATUAL     PIC X(04).                                00530244
                                                                        00530339
       77 WRK-ACUM-SALARIO    PIC 9(08).                                00530439
                                                                        00530558
       77 WRK-ACUM-LIDOS      PIC 9(02).                                00530658
                                                                        00531031
      *--------------------STATUS-----------------------------------*   00540031
                                                                        00541031
       77 WRK-FS-ARQUIIN  PIC 9(02).                                    00550031
                                                                        00551045
       77 WRK-FS-ARQUIOUT PIC 9(02).                                    00560045
                                                                        00590010
      *--------------------SAIDA------------------------------------*   00591037
                                                                        00591137
       01 WRK-SAIDA.                                                    00592031
                                                                        00592152
          05 WRK-LINHAS-SAIDA    PIC 9(03).                             00592252
          05 FILLER              PIC X(03).                             00592353
          05 WRK-CODIGO-SAIDA    PIC X(05).                             00593031
          05 FILLER              PIC X(04).                             00593131
          05 WRK-NOME-SAIDA      PIC X(30).                             00594031
          05 WRK-SETOR-SAIDA     PIC X(04).                             00595040
          05 FILLER              PIC X(07).                             00595137
          05 WRK-SALARIO-ED      PIC ZZZ.ZZZ,ZZ.                        00596037
                                                                        00597047
      *--------------------LAYOUT SAIDA-----------------------------*   00599545
                                                                        00599645
       01 WRK-TOTSAL.                                                   00599758
          05 FILLER              PIC X(17) VALUE 'SALARIO TOTAL DE '.   00599858
          05 WRK-SETOR-ATUAL-A   PIC X(04).                             00599958
          05 FILLER              PIC X(02) VALUE ': '.                  00600058
          05 WRK-SALARIO-ED-A    PIC ZZZ.ZZZ,ZZ.                        00600158
                                                                        00600260
       01 WRK-MSG-FINAL.                                                00600547
          05 FILLER       PIC X(16) VALUE 'FINAL DE ARQUIVO'.           00600658
                                                                        00600760
       01 WRK-MSG-LIDOS.                                                00600858
          05 FILLER       PIC X(09) VALUE 'LIDOS... '.                  00600958
                                                                        00601060
       77 WRK-LINHA-VAZIA PIC X(01) VALUE SPACE.                        00601158
                                                                        00601258
      *-------------------------------------------------------------*   00602010
       LINKAGE                                              SECTION.    00610010
      *-------------------------------------------------------------*   00620010
                                                                        00630010
       01 LNK-INFO-DATA    PIC X(31).                                   00640010
                                                                        00650010
      *=============================================================*   00690001
       PROCEDURE DIVISION USING LNK-INFO-DATA.                          00700009
      *=============================================================*   00710000
                                                                        00720001
      *=============================================================*   00730001
       0000-PRINCIPAL                                       SECTION.    00740001
                                                                        00750031
                                                                        00760001
            PERFORM 1000-INICIAR.                                       00770000
            PERFORM 1500-PRE-PROCESSAR.                                 00771058
            PERFORM 2000-PROCESSAR UNTIL WRK-FS-ARQUIIN EQUAL 10.       00780032
            PERFORM 3000-FINALIZAR.                                     00790000
            STOP RUN.                                                   00800000
                                                                        00810000
                                                                        00820031
       0000-99-FIM.                                            EXIT.    00830006
      *=============================================================*   00840001
                                                                        00850000
                                                                        00860001
                                                                        00870001
      *=============================================================*   00880001
       1000-INICIAR                                         SECTION.    00890001
                                                                        00910000
            OPEN INPUT ARQUIIN.                                         00920003
            OPEN OUTPUT ARQUIOUT.                                       00921045
                                                                        00922058
       1000-99-FIM.                                            EXIT.    00970006
      *=============================================================*   00980001
                                                                        00990001
                                                                        01000001
      *=============================================================*   01001058
       1100-PRE-PROCESSAR                                   SECTION.    01002058
                                                                        01003058
              PERFORM 1200-TESTARSTATUS.                                01007058
              READ ARQUIIN.                                             01008058
                                                                        01008158
              IF WRK-FS-ARQUIIN EQUAL ZERO                              01008258
              PERFORM 1300-CABEC                                        01009058
              MOVE FD-SETOR TO WRK-SETOR-ATUAL                          01009158
              ELSE                                                      01009258
              DISPLAY WRK-MSG-FINAL                                     01009358
              STOP RUN                                                  01009458
              END-IF.                                                   01009558
                                                                        01009658
       1100-99-FIM.                                            EXIT.    01009758
      *=============================================================*   01009858
                                                                        01010001
                                                                        01011058
      *=============================================================*   01020001
       1200-TESTARSTATUS                                    SECTION.    01030058
                                                                        01040031
                                                                        01050001
               IF WRK-FS-ARQUIIN  NOT EQUAL 0                           01060003
                MOVE 'RELAT01 '                  TO WRK-PROGRAMA        01070044
                MOVE '1000'                      TO WRK-SECAO           01080044
                MOVE 'ERRO OPEN FUNCIONARIOS - ' TO WRK-MENSAGEM        01090044
                MOVE WRK-FS-ARQUIIN              TO WRK-STATUS          01100044
                PERFORM 9000-TRATARERROS                                01110058
               END-IF.                                                  01120000
                                                                        01130001
                                                                        01140031
       1200-99-FIM.                                            EXIT.    01150058
      *=============================================================*   01160001
                                                                        01170001
                                                                        01180001
      *=============================================================*   01181031
       1300-CABEC                                           SECTION.    01182058
                                                                        01183031
               WRITE FD-ARQUIOUT FROM WRK-LINHA-VAZIA.                  01184045
               WRITE FD-ARQUIOUT FROM WRK-CABEC1.                       01185045
               WRITE FD-ARQUIOUT FROM WRK-LINHA-VAZIA.                  01186045
               WRITE FD-ARQUIOUT FROM WRK-CABEC2.                       01187045
               ADD 4 TO WRK-LINHAS.                                     01188031
                                                                        01189031
       1300-99-FIM.                                            EXIT.    01189658
      *=============================================================*   01189745
                                                                        01190001
      *=============================================================*   01200001
       2000-PROCESSAR                                       SECTION.    01210001
                                                                        01220031
      *                 MUDANCA DE SETOR                                01220258
                                                                        01220358
              IF FD-SETOR NOT EQUAL WRK-SETOR-ATUAL                     01220438
                                                                        01220539
                 MOVE WRK-ACUM-SALARIO TO WRK-SALARIO-ED                01220658
                 MOVE WRK-SALARIO-ED TO WRK-SALARIO-ED-A                01220758
                 MOVE WRK-SETOR-ATUAL TO WRK-SETOR-ATUAL-A              01220858
                  WRITE FD-ARQUIOUT FROM WRK-LINHA-VAZIA                01220958
                  WRITE FD-ARQUIOUT FROM WRK-TOTSAL                     01221558
                  WRITE FD-ARQUIOUT FROM WRK-LINHA-VAZIA                01221658
                 MOVE 0 TO WRK-ACUM-SALARIO                             01221758
                 ADD 1 TO WRK-PAG                                       01221858
                 MOVE 1 TO WRK-LINHAS                                   01221958
                  WRITE FD-ARQUIOUT FROM WRK-LINHA-VAZIA                01222058
                 PERFORM 1200-CABEC                                     01222158
                 MOVE FD-SETOR TO WRK-SETOR-ATUAL                       01222258
              END-IF.                                                   01222338
                                                                        01222458
      *                 COMPUTACAO DE LINHAS                            01222558
                                                                        01222638
              ADD 1 TO WRK-ACUM-LIDOS                                   01222738
                                                                        01222838
                                                                        01222958
                                                                        01223058
      *                 FINAL DE ARQUIVO                                01223158
                                                                        01223258
              IF WRK-FS-ARQUIIN EQUAL 10                                01223338
                  WRITE FD-ARQUIOUT FROM WRK-MSG-FINAL                  01223458
                                                                        01224031
                                                                        01224158
      *                 SAIDA DE DADOS                                  01224258
              ELSE                                                      01225038
                 MOVE FD-CODIGO   TO  WRK-CODIGO-SAIDA                  01226038
                 MOVE FD-NOME     TO  WRK-NOME-SAIDA                    01227038
                 MOVE FD-SETOR    TO  WRK-SETOR-SAIDA                   01228038
                 MOVE FD-SALARIO  TO  WRK-SALARIO-ED                    01230038
                 MOVE WRK-LINHAS  TO  WRK-LINHAS-SAIDA                  01231052
                  WRITE FD-ARQUIOUT FROM  WRK-SAIDA                     01240058
                 ADD 1 TO WRK-LINHAS                                    01240238
                 ADD FD-SALARIO TO WRK-ACUM-SALARIO                     01240339
              END-IF.                                                   01243038
                                                                        01244058
              READ ARQUIIN.                                             01250038
                                                                        01690001
                                                                        01700031
       2000-99-FIM.                                            EXIT.    01710006
      *=============================================================*   01720001
                                                                        01730031
      *=============================================================*   01760001
       3000-FINALIZAR                                       SECTION.    01770001
                                                                        01780031
      *                 SAIDA FINAL DE DADOS                            01781058
                                                                        01785058
               MOVE WRK-ACUM-SALARIO TO WRK-SALARIO-ED.                 01790057
               MOVE WRK-SALARIO-ED TO WRK-SALARIO-ED-A.                 01791057
               MOVE WRK-SETOR-ATUAL TO WRK-SETOR-ATUAL-A.               01791157
                WRITE FD-ARQUIOUT FROM WRK-LINHA-VAZIA.                 01791258
                WRITE FD-ARQUIOUT FROM WRK-TOTSAL.                      01792058
                WRITE FD-ARQUIOUT FROM WRK-LINHA-VAZIA.                 01793058
                                                                        01793258
                                                                        01793358
      *                 FECHAMENTO DE ARQUIVOS                          01794058
                                                                        01795058
               CLOSE ARQUIIN.                                           01800003
               CLOSE ARQUIOUT.                                          01801045
                                                                        01802058
                                                                        01803058
      *                 DISPLAYS DE FINAL DE ARQUIVO                    01804058
                                                                        01805058
                DISPLAY WRK-LINHA-VAZIA.                                01810058
                DISPLAY WRK-MSG-FINAL.                                  01811058
                DISPLAY WRK-MSG-LIDOS WRK-ACUM-LIDOS.                   01812058
                                                                        01813031
                                                                        01814058
      *                 DISPLAY DE HORARIO                              01815058
                                                                        01816058
               CALL 'DATAEXT' USING LNK-INFO-DATA.                      01820002
                DISPLAY WRK-LINHA-VAZIA.                                01830058
                DISPLAY LNK-INFO-DATA.                                  01840058
                DISPLAY WRK-LINHA-VAZIA.                                01850058
                                                                        01851031
       3000-99-FIM.                                            EXIT.    01860006
      *=============================================================*   01870001
                                                                        01880001
                                                                        01900001
      *=============================================================*   01910001
       9000-TRATARERROS                                     SECTION.    01920001
                                                                        01930031
                                                                        01940001
             CALL 'GRAVALOG' USING WRK-DADOS.                           01950000
                 GOBACK.                                                01960000
                                                                        01970001
                                                                        01980031
       9000-99-FIM.                                            EXIT.    01990006
      *=============================================================*   02000001
