      ******************************************************************
      * DCLGEN TABLE(IVAN.SETOR)                                       *
      *        LIBRARY(GR.IVAN.BOOKLIB(#BKSETOR))                      *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(DB2-)                                             *
      *        STRUCTURE(REG-SETOR)                                    *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE IVAN.SETOR TABLE
           ( IDSETOR                        CHAR(4),
             DESCSETOR                      CHAR(40)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE IVAN.SETOR                         *
      ******************************************************************
       01  REG-SETOR.
      *                       IDSETOR
           10 DB2-IDSETOR          PIC X(4).
      *                       DESCSETOR
           10 DB2-DESCSETOR        PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
