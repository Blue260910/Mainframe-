      ******************************************************************
      * DCLGEN TABLE(IVAN.FUNC)                                        *
      *        LIBRARY(GR.IVAN.BOOKLIB(#BKFUNC))                       *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(DB2-)                                             *
      *        STRUCTURE(REG-FUNC)                                     *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE IVAN.FUNC TABLE
           ( ID                             DECIMAL(5, 0),
             NOME                           CHAR(30) NOT NULL,
             SETOR                          CHAR(4) NOT NULL,
             SALARIO                        DECIMAL(10, 2) NOT NULL,
             DATAADM                        DATE NOT NULL,
             EMAIL                          CHAR(40)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE IVAN.FUNC                          *
      ******************************************************************
       01  REG-FUNC.
      *                       ID
           10 DB2-ID               PIC S9(5)V USAGE COMP-3.
      *                       NOME
           10 DB2-NOME             PIC X(30).
      *                       SETOR
           10 DB2-SETOR            PIC X(4).
      *                       SALARIO
           10 DB2-SALARIO          PIC S9(8)V9(2) USAGE COMP-3.
      *                       DATAADM
           10 DB2-DATAADM          PIC X(10).
      *                       EMAIL
           10 DB2-EMAIL            PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
