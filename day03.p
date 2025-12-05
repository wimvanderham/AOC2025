
/*------------------------------------------------------------------------
    File        : day03.p
    Purpose     : Solve Day 03 of Advent of Code 2025

    Syntax      :

    Description : Solution for Day 03 of Advent of Code 2025 in Progress OpenEdge

    Author(s)   : Wim van der Ham
    Created     : Fri Dec 05 01:22:22 CET 2025
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
DEFINE VARIABLE iDay        AS INTEGER   NO-UNDO INITIAL 3.
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
   
   /* Specific fields */
   FIELD iMaxJoltage    AS INTEGER 
   
   INDEX indLine IS UNIQUE IDLine.
   
/* Specific variables */
   
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION getMaxJoltage RETURNS INTEGER 
   (INPUT ipcBatteries AS CHARACTER) FORWARD.

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
   lcInput = "987654321111111,811111111111119,234234234234278,818181911112111".
   lcInput = REPLACE (lcInput, ",", "~n").
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
   FOR EACH ttLine:
      ttLine.iMaxJoltage = getMaxJoltage(ttLine.cInputLine). 
      
      iSolution += ttLine.iMaxJoltage.
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
      VIEW-AS ALERT-BOX TITLE " 2025 - Day 03 - Part One".
END. /* Process Part One */

IF lPart[2] THEN 
DO:
   /* Process Part Two */
   
   FILE-INFO:FILE-NAME = "output".

   cOutputFile = SUBSTITUTE ("&1\&2.out",
                             FILE-INFO:FULL-PATHNAME,
                             STRING (iDay, "99")).
   
   OUTPUT TO VALUE (cOutputFile).
   
   FOR EACH ttLine:

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
      VIEW-AS ALERT-BOX TITLE " 2025 - Day 02 - Part Two".
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


/* ************************  Function Implementations ***************** */

FUNCTION getMaxJoltage RETURNS INTEGER 
(INPUT ipcBatteries AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Get the maximum joltage in two numbers from a list of batteries
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iChar   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMax    AS INTEGER   NO-UNDO EXTENT 2.
DEFINE VARIABLE iPos    AS INTEGER   NO-UNDO EXTENT 2.
DEFINE VARIABLE iStart  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLength AS INTEGER   NO-UNDO.

   iStart = 1.
   DO iLoop = 1 TO 2:
      /* Search for the two highest batteries */
      iLength = LENGTH (ipcBatteries).
      IF iLoop EQ 1 THEN 
         /* Have to stop before the last position */
         iLength -= 1. 
      DO iChar = iStart TO iLength:
         cChar = SUBSTRING (ipcBatteries, iChar, 1).
         IF INTEGER (cChar) GT iMax[iLoop] THEN DO:
            ASSIGN 
               iPos[iLoop] = iChar
               iMax[iLoop] = INTEGER (cChar)
            .
         END.
      END.
      iStart = iPos[iLoop] + 1.
   END.
   
   RETURN iMax[1] * 10 + iMax[2].
      
END FUNCTION.
