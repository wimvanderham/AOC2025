
/*------------------------------------------------------------------------
    File        : day07.p
    Purpose     : Solve Day 02 of Advent of Code 2025

    Syntax      :

    Description : Solution for Day 07 of Advent of Code 2025 in Progress OpenEdge

    Author(s)   : Wim van der Ham
    Created     : Sun Dec 07 22:35:51 CET 2025
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
DEFINE VARIABLE iDay        AS INTEGER   NO-UNDO INITIAL 07.
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
DEFINE TEMP-TABLE ttGrid
   FIELD iRow  AS INTEGER 
   FIELD iCol  AS INTEGER 
   FIELD cChar AS CHARACTER 
INDEX indGrid IS UNIQUE iRow iCol.

DEFINE TEMP-TABLE ttBeam
   FIELD IDBeam   AS INTEGER 
   FIELD iRow     AS INTEGER 
   FIELD iCol     AS INTEGER 
   FIELD iStep    AS INTEGER 
   FIELD lRunning AS LOGICAL
INDEX indBeam IS UNIQUE IDBeam
INDEX indRun  IS PRIMARY iStep lRunning DESCENDING iRow iCol.

DEFINE BUFFER ttRunBeam FOR ttBeam.
DEFINE BUFFER ttNewBeam FOR ttBeam.
DEFINE BUFFER ttChkBeam FOR ttBeam.

DEFINE TEMP-TABLE ttSeen
   FIELD iRow AS INTEGER 
   FIELD iCol AS INTEGER
INDEX indSeen IS UNIQUE iRow iCol.
 
DEFINE VARIABLE iNewID AS INTEGER NO-UNDO.   
DEFINE VARIABLE lSplit AS LOGICAL NO-UNDO. 
DEFINE VARIABLE iStep  AS INTEGER NO-UNDO.
   
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
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).

   IF cLine EQ "" THEN 
      NEXT.
      
   CREATE ttLine.
   ASSIGN 
      ttLine.IDLine     = iLine
      ttLine.cInputLine = cLine
      .

   DO iChar = 1 TO LENGTH (ttLine.cInputLine):
      cChar = SUBSTRING (ttLine.cInputLine, iChar, 1).
      CREATE ttGrid.
      ASSIGN 
         ttGrid.iRow  = ttLine.IDLine
         ttGrid.iCol  = iChar
         ttGrid.cChar = cChar
      .
      IF ttGrid.cChar EQ "S" THEN DO:
         iNewID += 1.
         CREATE ttBeam.
         ASSIGN 
            ttBeam.IDBeam   = iNewID
            ttBeam.iRow     = ttGrid.iRow
            ttBeam.iCol     = ttGrid.iCol
            ttBeam.iStep    = 1
            ttBeam.lRunning = TRUE
         .
      END.
   END.
END. /* ReadBlock: */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   iStep = 1.
   FIND FIRST ttBeam 
   WHERE ttBeam.iStep    EQ iStep
   AND   ttBeam.lRunning EQ TRUE NO-ERROR.
   IF lvlShow THEN 
      MESSAGE AVAILABLE ttBeam
      VIEW-AS ALERT-BOX.
   DO WHILE AVAILABLE ttBeam:
      /* While there are running beams available */
      FOR EACH ttRunBeam
      WHERE ttRunBeam.iStep    EQ iStep
      AND   ttRunBeam.lRunning EQ TRUE:
         /* All running beams of this step */
         FIND  ttSeen 
         WHERE ttSeen.iRow EQ ttRunBeam.iRow
         AND   ttSeen.iCol EQ ttRunBeam.iCol NO-ERROR.
         IF AVAILABLE ttSeen THEN
            /* Already passed here, don't check again */
            NEXT.
         ELSE DO:
            CREATE ttSeen.
            ASSIGN 
               ttSeen.iRow = ttRunBeam.iRow
               ttSeen.iCol = ttRunBeam.iCol
            .
         END.
         FIND  ttGrid
         WHERE ttGrid.iRow EQ ttRunBeam.iRow + 1
         AND   ttGrid.iCol EQ ttRunBeam.iCol NO-ERROR.
         IF NOT AVAILABLE ttGrid THEN DO:
            /* Beam reached the end, stop running */
            ttRunBeam.lRunning EQ FALSE.
         END.
         ELSE DO:
            CASE ttGrid.cChar:
               WHEN "." THEN DO:
                  ASSIGN 
                     ttRunBeam.iRow  =  ttGrid.iRow
                     ttRunBeam.iCol  =  ttGrid.iCol
                     ttRunBeam.iStep += 1
                  .
               END.
               WHEN "^" THEN DO:
                  /* Found a splitter */
                  ttRunBeam.lRunning = FALSE.
                  lSplit = FALSE.
                  FIND  ttChkBeam
                  WHERE ttChkBeam.iRow EQ ttGrid.iRow
                  AND   ttChkBeam.iCol EQ ttGrid.iCol - 1 NO-ERROR.
                  IF NOT AVAILABLE ttChkBeam THEN DO:
                     iNewID += 1.
                     CREATE ttNewBeam.
                     ASSIGN 
                        ttNewBeam.IDBeam   = iNewID
                        ttNewBeam.iRow     = ttGrid.iRow
                        ttNewBeam.iCol     = ttGrid.iCol - 1
                        ttNewBeam.iStep    = iStep + 1
                        ttNewBeam.lRunning = TRUE
                     .
                     lSplit = TRUE.
                  END.
                  FIND  ttChkBeam
                  WHERE ttChkBeam.iRow EQ ttGrid.iRow
                  AND   ttChkBeam.iCol EQ ttGrid.iCol + 1 NO-ERROR.
                  IF NOT AVAILABLE ttChkBeam THEN DO: 
                     iNewID += 1.
                     CREATE ttNewBeam.
                     ASSIGN 
                        ttNewBeam.IDBeam   = iNewID
                        ttNewBeam.iRow     = ttGrid.iRow
                        ttNewBeam.iCol     = ttGrid.iCol + 1
                        ttNewBeam.iStep    = iStep + 1
                        ttNewBeam.lRunning = TRUE
                     .
                     lSplit = TRUE.
                  END.
                  IF lSplit EQ TRUE THEN
                     iSolution += 1.
               END.   
            END CASE.
         END.
      END. /* All running beams of this step */
      IF lvlShow THEN DO:
         RUN sy\win\wbrowsett.w
            (INPUT TEMP-TABLE ttBeam:HANDLE).
      END.
      iStep += 1.
      FIND FIRST ttBeam 
      WHERE ttBeam.iStep    EQ iStep
      AND   ttBeam.lRunning EQ TRUE NO-ERROR.
   END. /* While there are running beams available */
   
   IF lvlShow THEN 
   DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttBeam:HANDLE).
   END.
      
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2025 - Day 07 - Part One".
END. /* Process Part One */

IF lPart[2] THEN 
DO:
   /* Process Part Two */
   IF lPart[1] THEN DO:
      /* Reset counters */
      iSolution = 0.
      
      FOR EACH ttLine:

      END.
   END. /* Reset counters */
   
   IF lvlOutput EQ TRUE THEN DO:
      FILE-INFO:FILE-NAME = "output".
   
      cOutputFile = SUBSTITUTE ("&1\&2.out",
                                FILE-INFO:FULL-PATHNAME,
                                STRING (iDay, "99")).
      
      OUTPUT TO VALUE (cOutputFile).
   END.
      
   FOR EACH ttLine:

   END.

   IF lvlOutput EQ TRUE THEN DO:
      OUTPUT CLOSE.
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
      VIEW-AS ALERT-BOX TITLE " 2025 - Day 07 - Part Two".
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
