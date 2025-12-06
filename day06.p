
/*------------------------------------------------------------------------
    File        : day06.p
    Purpose     : Solve Day 02 of Advent of Code 2025

    Syntax      :

    Description : Solution for Day 06 of Advent of Code 2025 in Progress OpenEdge

    Author(s)   : Wim van der Ham
    Created     : Sat Dec 06 06:26:33 CET 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for GET */
DEFINE VARIABLE cURL       AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/&1/day/&2".
DEFINE VARIABLE cCommand   AS CHARACTER NO-UNDO.
/* Variables for input handling */
DEFINE VARIABLE lDownload   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cSession    AS CHARACTER NO-UNDO INITIAL "53616c7465645f5f0aa2b48889c4ecb0a71e7086a3ce378be60c9c62fff2ce2f0a803b3cf401a90e48d12df95cfd2383f2923a50c7378e392a1b5d4ce4438c7e".
DEFINE VARIABLE iYear       AS INTEGER   NO-UNDO INITIAL 2025.
DEFINE VARIABLE iDay        AS INTEGER   NO-UNDO INITIAL 6.
DEFINE VARIABLE hPLIP       AS HANDLE    NO-UNDO.
DEFINE VARIABLE cInputFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutputFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInput     AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOpenURL    AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE lPart       AS LOGICAL   NO-UNDO EXTENT 2.
DEFINE VARIABLE iMaxLine    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxLength  AS INTEGER   NO-UNDO.

/* Variables for solving */
/* Generic */
DEFINE VARIABLE iSolution   AS INT64     NO-UNDO.
DEFINE VARIABLE lOk         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvlDebug    AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lvlShow     AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lvlOutput   AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE iPart       AS INTEGER   NO-UNDO.
/* Specific */
DEFINE TEMP-TABLE ttLine
   FIELD IDLine         AS INTEGER 
   FIELD cInputLine     AS CHARACTER FORMAT "X(10)"

   INDEX indLine IS UNIQUE IDLine.
   
/* Specific variables */   
DEFINE TEMP-TABLE ttProblem
   FIELD iColumn    AS INTEGER 
   FIELD cOperation AS CHARACTER 
   FIELD iResult    AS INT64 
   
   /* Extra fields for Part Two */
   FIELD iStartPos AS INTEGER /* Left  most position of this problem (column) */
   FIELD iEndPos   AS INTEGER /* Right most position of this problem (column) */
INDEX indColumn IS UNIQUE iColumn.

DEFINE TEMP-TABLE ttTerm
   FIELD iColumn AS INTEGER 
   FIELD iRow    AS INTEGER 
   FIELD iValue  AS INT64 
INDEX indTerm IS UNIQUE iColumn iRow.

DEFINE TEMP-TABLE ttTermColumn
   FIELD iColumn   AS INTEGER 
   FIELD iRow      AS INTEGER 
   FIELD iPosition AS INTEGER 
   FIELD cValue    AS CHARACTER 
INDEX indTermPos IS UNIQUE iPosition iColumn iRow.

DEFINE TEMP-TABLE ttTermByColumn
   FIELD iColumn   AS INTEGER 
   FIELD iRow      AS INTEGER 
   FIELD iPosition AS INTEGER 
   FIELD cValue    AS CHARACTER 
   FIELD iValue    AS INT64 
INDEX indColumn IS UNIQUE iColumn iPosition.

DEFINE VARIABLE iColumn   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPosition AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRow      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cValue    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSection  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iScanPos  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cNumber   AS CHARACTER NO-UNDO.
   
/* ********************  Preprocessor Definitions  ******************** */

{AOC_session.i}

DISPLAY
   SUBSTITUTE ("Year &1 Day &2", iYear, iDay) FORMAT "X(16)" NO-LABELS SKIP
   lOpenURL  LABEL "Open URL?"       VIEW-AS TOGGLE-BOX SKIP
   lDownload LABEL "Download Input?" VIEW-AS TOGGLE-BOX SKIP   
   lPart[1]  LABEL "Solve Part 1?"   VIEW-AS TOGGLE-BOX SKIP
   lPart[2]  LABEL "Solve Part 2?"   VIEW-AS TOGGLE-BOX SKIP 
   lvlDebug  LABEL "Debug?"          VIEW-AS TOGGLE-BOX SKIP 
   lvlShow   LABEL "Show?"           VIEW-AS TOGGLE-BOX SKIP
   lvlOutput LABEL "Output?"         VIEW-AS TOGGLE-BOX SKIP
   WITH FRAME fr-Parameters SIDE-LABELS ROW 3 CENTERED TITLE " Parameters ".
ASSIGN 
   lDownload  = FALSE
   cInputfile = SUBSTITUTE ("C:\OpenEdge\WRK_12.8\AOC&1\source\input\&2.txt", iYear, STRING (iDay, "99"))
   cURL       = SUBSTITUTE (cURL, iYear, iDay)
   .
FILE-INFO:FILE-NAME = cInputFile.
IF FILE-INFO:FILE-TYPE EQ ? THEN 
DO:
   lDownload = TRUE.
END.
UPDATE
   lOpenURL
   lDownload
   lPart
   lvlDebug
   lvlShow
   lvlOutput
   WITH FRAME fr-Parameters.
RUN plip_aoc.p PERSISTENT SET hPLIP.
IF lOpenURL THEN 
DO:
   RUN chkURL IN hPLIP
      (INPUT  iYear,
       INPUT  iDay,
       OUTPUT lOk,
       OUTPUT cMessage).
   IF lOk EQ FALSE THEN 
   DO:
      MESSAGE cMessage
         VIEW-AS ALERT-BOX WARNING.
      RETURN.
   END.
   cCommand = SUBSTITUTE ("start &1", cURL).
   OS-COMMAND SILENT VALUE (cCommand).
END.
IF lDownload THEN 
DO:
   RUN getInput IN hPLIP
      (INPUT  cSession,
       INPUT  iYear,
       INPUT  iDay,
       INPUT  cInputFile,
       OUTPUT lOk,
       OUTPUT cMessage).
   IF lOk EQ FALSE THEN 
   DO:
      MESSAGE cMessage
         VIEW-AS ALERT-BOX WARNING.
      RETURN.
   END.
END.
/* Start Processing */
iSolution = 0.
ETIME (YES).
COPY-LOB FROM FILE cInputfile TO OBJECT lcInput.
IF lvlDebug THEN 
DO:
   lcInput = "".
   cInputFile = REPLACE (cInputFile, ".txt", "_debug.txt").
   COPY-LOB FROM FILE cInputfile TO OBJECT lcInput.
END.

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = RIGHT-TRIM (ENTRY (iLine, lcInput, "~n")).

   IF cLine EQ "" THEN 
      NEXT.
      
   CREATE ttLine.
   ASSIGN 
      ttLine.IDLine     = iLine
      ttLine.cInputLine = cLine
      .

   iMaxLine = ttLine.IDLine.
   iMaxLength = MAXIMUM (iMaxLength, LENGTH (ttLine.cInputLine)).
   
END. /* ReadBlock: */

IF lvlShow THEN 
DO:
   MESSAGE iMaxLength
   VIEW-AS ALERT-BOX.
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   FOR EACH ttLine:
      DO WHILE INDEX (ttLine.cInputLine, "  ") GT 0:
         /* Replace double spaces with single space */
         ttLine.cInputLine = REPLACE (ttLine.cInputLine, "  ", " ").
      END.
      
      DO iColumn = 1 TO NUM-ENTRIES (ttLine.cInputLine, " "):
         IF ttLine.IDLine EQ 1 THEN DO:
            CREATE ttProblem.
            ASSIGN 
               ttProblem.iColumn = iColumn
            .
         END.
         IF ttLine.IDLine LT iMaxLine THEN DO:
            CREATE ttTerm.
            ASSIGN 
               ttTerm.iColumn = iColumn
               ttTerm.iRow    = ttLine.IDLine
               ttTerm.iValue  = INT64 (ENTRY (iColumn, ttLine.cInputLine, " "))
            .
         END.
         ELSE DO:
            /* Last line contains the operation */
            FIND  ttProblem
            WHERE ttProblem.iColumn EQ iColumn.
            ASSIGN 
               ttProblem.cOperation = ENTRY (iColumn, ttLine.cInputLine, " ")
            .
            IF ttProblem.cOperation EQ "*" THEN
               /* Initialize with * neutral value */ 
               ttProblem.iResult = 1.
         END.
      END.
   END.
      
   IF lvlShow THEN 
   DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttLine:HANDLE).
         
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttProblem:HANDLE).
      
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttProblem:HANDLE).         
   END.
      
   FOR EACH ttProblem:
      FOR EACH ttTerm OF ttProblem:
         CASE ttProblem.cOperation:
            WHEN "*" THEN
               ttProblem.iResult *= ttTerm.iValue.
            WHEN "+" THEN
               ttProblem.iResult += ttTerm.iValue.
            OTHERWISE 
               UNDO, THROW NEW Progress.Lang.AppError(SUBSTITUTE ("Unknown operation '&1'.", ttProblem.cOperation), 99).
         END CASE.
      END.
      iSolution += ttProblem.iResult.
   END.
       
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2025 - Day 06 - Part One".
END. /* Process Part One */

IF lPart[2] THEN 
DO:
   /* Process Part Two */
   IF lPart[1] THEN DO:
      /* Reset counters */
      iSolution = 0.
   END. /* Reset counters */

   IF lvlOutput EQ TRUE THEN DO:   
      FILE-INFO:FILE-NAME = "output".
   
      cOutputFile = SUBSTITUTE ("&1\&2.out",
                                FILE-INFO:FULL-PATHNAME,
                                STRING (iDay, "99")).
      
      OUTPUT TO VALUE (cOutputFile).
   END.
      
   FIND LAST ttLine.
   iColumn = 0.
   DO iPosition = 1 TO LENGTH (ttLine.cInputLine):
      cChar = SUBSTRING (ttLine.cInputLine, iPosition, 1).
      IF INDEX ("+*", cChar) NE 0 THEN DO:
         iColumn += 1.
         CREATE ttProblem.
         ASSIGN 
            ttProblem.iColumn    = iColumn
            ttProblem.iStartPos  = iPosition
            ttProblem.cOperation = cChar
         .
         IF ttProblem.cOperation EQ "*" THEN 
            ttProblem.iResult = 1.
      END.
      ELSE DO:
         FIND  ttProblem 
         WHERE ttProblem.iColumn EQ iColumn.
         ASSIGN 
            ttProblem.iEndPos = iPosition
         .
      END.
      IF iPosition EQ LENGTH (ttLine.cInputLine) THEN DO:
         MESSAGE ttProblem.iEndPos SKIP iMaxLength
         VIEW-AS ALERT-BOX. 
         ttProblem.iEndPos = iMaxLength.
         MESSAGE ttProblem.iEndPos
         VIEW-AS ALERT-BOX.
      END.
      ELSE DO:
         ttProblem.iEndPos -= 1.
      END.
   END.

   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttProblem:HANDLE).
   END.
   
   FOR EACH ttLine
   WHERE ttLine.IDLine LT iMaxLine:
      DO iPosition = 1 TO LENGTH (ttLine.cInputLine):
         cChar = SUBSTRING (ttLine.cInputLine, iPosition, 1).
         IF INDEX("0123456789", cChar) NE 0 THEN DO:
            /* Found a number */
            FIND  ttProblem
            WHERE ttProblem.iStartPos LE iPosition
            AND   ttProblem.iEndPos   GE iPosition.
            CREATE ttTermColumn.
            ASSIGN 
               ttTermColumn.iColumn   = ttProblem.iColumn
               ttTermColumn.iRow      = ttLine.IDLine
               ttTermColumn.iPosition = iPosition
               ttTermColumn.cValue    = cChar
            .
         END.
      END.
   END.
   
   FOR EACH ttProblem
   BY ttProblem.iColumn DESCENDING:
      DO iPosition = ttProblem.iEnd TO ttProblem.iStartPos BY -1:
         CREATE ttTermByColumn.
         ASSIGN 
            ttTermByColumn.iColumn   = ttProblem.iColumn
            ttTermByColumn.iPosition = iPosition
         .
                              
         FOR EACH ttTermColumn OF ttProblem
         WHERE ttTermColumn.iPosition EQ iPosition
         BY    ttTermColumn.iRow:
            ASSIGN 
               ttTermByColumn.cValue += ttTermColumn.cValue
            .
            IF lvlDebug THEN DO:
               MESSAGE SUBSTITUTE ("Column: &1, Position: &2, Row: &3, Value: &4, Total Value: &5.",
                                   ttTermColumn.iColumn,
                                   ttTermColumn.iPosition,
                                   ttTermColumn.iRow,
                                   ttTermColumn.cValue,
                                   ttTermByColumn.cValue)
               VIEW-AS ALERT-BOX.
            END.
         END.
         ttTermByColumn.iValue = INT64 (ttTermByColumn.cValue).
         IF lvlDebug THEN DO:
            MESSAGE SUBSTITUTE ("Column: &1, Position: &2, Value: &3",
                                ttTermByColumn.iColumn,
                                ttTermByColumn.iPosition,
                                ttTermByColumn.cValue)
            VIEW-AS ALERT-BOX.
         END.
      END.
      FOR EACH ttTermByColumn OF ttProblem:
         CASE ttProblem.cOperation:
            WHEN "*" THEN
               ttProblem.iResult *= ttTermByColumn.iValue.
            WHEN "+" THEN 
               ttProblem.iResult += ttTermByColumn.iValue.
            OTHERWISE
               UNDO, THROW NEW Progress.Lang.AppError(SUBSTITUTE ("Unknown operation '&1'.", ttProblem.cOperation), 99).
         END CASE. 
      END.
      iSolution += ttProblem.iResult.
   END.
   
   IF lvlDebug THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttProblem:HANDLE).
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttTermColumn:HANDLE).
   END.

   IF lvlOutput EQ TRUE THEN DO:
      OUTPUT CLOSE.
   END.
      
   IF lvlShow THEN 
   DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttProblem:HANDLE).
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttTermByColumn:HANDLE).
   END.
      
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2025 - Day 06 - Part Two".
END. /* Process Part One */



CATCH oError AS Progress.Lang.Error :
   DEFINE VARIABLE iMessage      AS INTEGER   NO-UNDO.
   DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
   cErrorMessage = oError:GetMessage(1).
   iMessage = 2.
   DO WHILE iMessage LT oError:NumMessages:
      cErrorMessage = SUBSTITUTE ("&1~n&2", cErrorMessage, oError:GetMessage(iMessage)).
      iMessage = iMessage + 1.
   END.
   IF oError:CallStack NE ? THEN 
   DO:
      cErrorMessage = SUBSTITUTE ("&1~n~nCall Stack:~n&2", cErrorMessage, oError:CallStack).
   END.
   MESSAGE "Error!" SKIP (1)
      SUBSTITUTE ("At line #: &1: &2", iLine, cLine) SKIP
      cErrorMessage SKIP(1) 
      VIEW-AS ALERT-BOX ERROR.
   IF lvlShow THEN 
   DO:
   END.
   RETURN.      
END CATCH.
