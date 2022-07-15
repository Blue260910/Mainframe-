      ******************************************************************
      * DCLGEN TABLE(IBMUSER.FUNC)                                     *
      *        LIBRARY(GR.IVAN.BOOKLIB(BOOKFUNC))                      *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(DB2-)                                             *
      *        STRUCTURE(REG-FUNC)                                     *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE IBMUSER.FUNC TABLE
           ( IDFUNC                         DECIMAL(5, 0),
             NOMEFUNC                       CHAR(30),
             IDADEFUNC                      DECIMAL(3, 0),
             SALFUNC                        DECIMAL(9, 2),
             EMAILFUNC                      CHAR(30)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE IBMUSER.FUNC                       *
      ******************************************************************
       01  REG-FUNC.
      *    *************************************************************
      *                       IDFUNC
           10 DB2-IDFUNC           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
      *                       NOMEFUNC
           10 DB2-NOMEFUNC         PIC X(30).
      *    *************************************************************
      *                       IDADEFUNC
           10 DB2-IDADEFUNC        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
      *                       SALFUNC
           10 DB2-SALFUNC          PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
      *                       EMAILFUNC
           10 DB2-EMAILFUNC        PIC X(30).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
