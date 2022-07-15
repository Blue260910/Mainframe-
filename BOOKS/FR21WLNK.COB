       01 LNK-ENTRADA.
          05 LNK-LEN            PIC 9(04) COMP.
          05 LNK-CORPO.
             10 LNK-CHAVE.
                15 LNK-AGENCIA  PIC X(04).
                15 LNK-CONTA    PIC X(05).
             10 LNK-CHAVE2.
                15 LNK-AGENCIA2  PIC X(04).
                15 LNK-CONTA2    PIC X(05).
          05 LNK-INFO-DATA.
             10 WRK-DIA-EXT       PIC X(23).
             10 WRK-HORA-EXT      PIC X(08).
