
/*------------------------------------------------------------------------
    File        : day01.p
    Purpose     : Solve Day 01 of Advent of Code 2025

    Syntax      :

    Description : Solution for Day 01 of Advent of Code 2025 in Progress OpenEdge

    Author(s)   : Wim van der Ham
    Created     : Mon Dec 01 17:18:33 CET 2025
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
DEFINE VARIABLE iDay        AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE hPLIP       AS HANDLE    NO-UNDO.
DEFINE VARIABLE cInputFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutputFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInput     AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lOpenURL    AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE lPart       AS LOGICAL   NO-UNDO EXTENT 2.
/* Variables for solving */
/* Generic */
DEFINE VARIABLE iSolution   AS INT64     NO-UNDO.
DEFINE VARIABLE lOk         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvlDebug    AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lvlShow     AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE iPart       AS INTEGER   NO-UNDO.
/* Specific */
DEFINE TEMP-TABLE ttLine
   FIELD IDLine         AS INTEGER 
   FIELD cInputLine     AS CHARACTER FORMAT "X(10)"
   /* Extra fields to keep track of the dial */
   FIELD cDirection     AS CHARACTER FORMAT "X"
   FIELD iMoveValue     AS INTEGER 
   FIELD iStartPosition AS INTEGER 
   FIELD iCalculation   AS INTEGER
   FIELD iEndPosition   AS INTEGER 
   FIELD iZeroCounter   AS INTEGER 
   INDEX indLine IS UNIQUE IDLine.
   
DEFINE VARIABLE iPosition  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cDirection AS CHARACTER NO-UNDO.
DEFINE VARIABLE iValue     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLoop1     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLoop2     AS INTEGER   NO-UNDO.
   
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
   MESSAGE cSession
   VIEW-AS ALERT-BOX.
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
   lcInput = REPLACE ("L68,L30,R48,L5,R60,L55,L1,L99,R14,L82", ",", "~n").
   lcInput = "R1000".
END.
/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).

   IF cLine EQ "" THEN 
      NEXT.
      
   CREATE ttLine.
   ASSIGN 
      ttLine.IDLine     = iLine
      ttLine.cInputLine = cLine
      .

END. /* ReadBlock: */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   iPosition = 50.
   
   FOR EACH ttLine:
      ASSIGN 
         ttLine.iStartPosition = iPosition
      . 
      ASSIGN 
         cDirection = SUBSTRING (ttLine.cInputLine, 1, 1)
         iValue     = INTEGER (SUBSTRING (ttLine.cInputLine, 2))
      .
      ASSIGN 
         ttLine.cDirection = cDirection
         ttLine.iMoveValue = iValue
      .
      CASE cDirection:
         WHEN "L" THEN 
            iPosition = iPosition - iValue.
         WHEN "R" THEN
            iPosition = iPosition + iValue.
         OTHERWISE
            UNDO, THROW NEW Progress.Lang.AppError(SUBSTITUTE ("Unknown direction '&1'.", cDirection), 99).
      END CASE.
      
      ASSIGN 
         ttLine.iCalculation   = iPosition
      .
      
      DO WHILE iPosition LT 0:
         iPosition = iPosition + 100.
      END.
      DO WHILE iPosition GE 100:
         iPosition = iPosition - 100.
      END.
      
      ASSIGN 
         ttLine.iEndPosition = iPosition
      . 

      IF iPosition EQ 0 THEN
         ASSIGN 
            ttLine.iZeroCounter = 1
            iSolution           = iSolution + 1
         .
   END.
      
   IF lvlShow THEN 
   DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttLine:HANDLE).
   END.
      
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2025 - Day 01 - Part One".
END. /* Process Part One */

IF lPart[2] THEN 
DO:
   /* Process Part Two */
   IF lPart[1] THEN DO:
      /* Reset counters */
      iSolution = 0.
      
      FOR EACH ttLine:
         ASSIGN 
            ttLine.cDirection     = ""
            tTLine.iStartPosition = 0
            ttLine.iCalculation   = 0
            ttLine.iMoveValue     = 0
            ttLine.iEndPosition   = 0
            ttLine.iZeroCounter   = 0
         .
      END.
   END. /* Reset counters */
   
   FILE-INFO:FILE-NAME = "output".

   cOutputFile = SUBSTITUTE ("&1\&2",
                             FILE-INFO:FULL-PATHNAME,
                             "01.out").
   
   OUTPUT TO VALUE (cOutputFile).
   
   iPosition = 50.
   FOR EACH ttLine:
      ASSIGN 
         ttLine.iStartPosition = iPosition
      . 
      ASSIGN 
         cDirection = SUBSTRING (ttLine.cInputLine, 1, 1)
         iValue     = INTEGER (SUBSTRING (ttLine.cInputLine, 2))
      .
      ASSIGN 
         ttLine.cDirection = cDirection
         ttLine.iMoveValue = iValue
      .
      CASE cDirection:
         WHEN "L" THEN DO:
            IF iPosition EQ 0 THEN 
               iPosition = 100.
            ttLine.iStartPosition = iPosition.
            iPosition -= iValue.
         END.
         WHEN "R" THEN DO:
            IF iPosition EQ 100 THEN 
               iPosition = 0.
            ttLine.iStartPosition = iPosition.
            iPosition += iValue.
         END.
         OTHERWISE
            UNDO, THROW NEW Progress.Lang.AppError(SUBSTITUTE ("Unknown direction '&1'.", cDirection), 99).
      END CASE.
      
      ASSIGN 
         ttLine.iCalculation   = iPosition
      .
      
      ASSIGN 
         iLoop1 = 0
         iLoop2 = 0
      .
      
      IF iPosition LT 0 THEN DO:
         
         DO WHILE iPosition LT 0:
            iLoop1              += 1.
            iPosition           += 100.
            ttLine.iZeroCounter += 1. 
         END.
      END.
          
      IF iPosition GT 100 THEN DO:
         DO WHILE iPosition GT 100:
            iLoop2              += 1.
            iPosition           -= 100.
            ttLine.iZeroCounter += 1.
         END.
      END.
                     
      ASSIGN 
         ttLine.iEndPosition = iPosition
      . 

      IF ttLine.iEndPosition EQ 0 
      OR ttLine.iEndPosition EQ 100 THEN 
         ttLine.iZeroCounter += 1.
                  
      iSolution = iSolution + ttLine.iZeroCounter.

      DISPLAY
      ttLine.iStartPosition MOD 100   LABEL "iStart"
      ttLine.cInputLine FORMAT "X(8)" LABEL "cEntry"
      ttLine.iMoveValue               LABEL "iRotate"
      ttLine.iEndPosition   MOD 100   LABEL "iDial"
      ttLine.iZeroCounter             LABEL "iLineZero"
      iSolution                       LABEL "iZero"
      WITH WIDTH 132 STREAM-IO.            
   END.
   OUTPUT CLOSE.
      
   IF lvlShow THEN 
   DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttLine:HANDLE).
   END.
      
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2025 - Day 01 - Part Two".
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
