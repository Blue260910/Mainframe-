      ******************************************************************
      * DCLGEN TABLE(IVAN.BENEF)                                       *
      *        LIBRARY(GR.IVAN.BOOKLIB(#BKBENEF))                      *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(DB2-)                                             *
      *        STRUCTURE(REG-BENEF)                                    *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE IVAN.BENEF TABLE
           ( IDFUNC                         DECIMAL(5, 0),
             PLANMED                        CHAR(10),
             PLANDENT                       CHAR(10),
             COTAS                          DECIMAL(3, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE IVAN.BENEF                         *
      ******************************************************************
       01  REG-BENEF.
      *                       IDFUNC
           10 DB2-IDFUNC           PIC S9(5)V USAGE COMP-3.
      *                       PLANMED
           10 DB2-PLANMED          PIC X(10).
      *                       PLANDENT
           10 DB2-PLANDENT         PIC X(10).
      *                       COTAS
           10 DB2-COTAS            PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
