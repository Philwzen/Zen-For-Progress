/** Definitions Section **/

DEFINE VARIABLE hCol AS HANDLE NO-UNDO. /* Stores reference to Current Widget */
DEFINE VARIABLE winState AS INTEGER NO-UNDO. /* Stores Current Window State */

DEFINE TEMP-TABLE tt_Size NO-UNDO /* Stores Widget Dimensions &
                    Positions */
   FIELD wg_Name AS CHARACTER
   FIELD wg_Width AS DECIMAL
   FIELD wg_Height AS DECIMAL
   FIELD wg_Xpos AS DECIMAL
   FIELD wg_Ypos AS DECIMAL
   INDEX wg_Name IS PRIMARY wg_Name.

DEFINE BUFFER bf_Size FOR tt_Size.


/**     MAIN-BLOCK      **/

 /* Sets the maximum width and height to that of the current
    Windows Resolution, that is, 800 x 600 etc.. */
 ASSIGN CURRENT-WINDOW:MAX-WIDTH = SESSION:WIDTH-CHARS
        CURRENT-WINDOW:MAX-HEIGHT = SESSION:HEIGHT-CHARS.

 /* Save current window information to the temp-table */
 ASSIGN hCol = CURRENT-WINDOW NO-ERROR.
 CREATE tt_Size.
 ASSIGN tt_Size.wg_Name = STRING(hCol)
        tt_Size.wg_Width = hCol:WIDTH-PIXELS
        tt_Size.wg_Height = hCol:HEIGHT-PIXELS
        tt_Size.wg_Xpos = hCol:X
        tt_Size.wg_Ypos = hCol:Y NO-ERROR.


/** WINDOW-MAXIMIZED Trigger **/

 ASSIGN winState = 1 NO-ERROR. /* 1 = Maximized */
 ASSIGN hCol = FRAME {&FRAME-NAME}:HANDLE NO-ERROR.

 /* Get Window Dimensions using a buffer */
 FIND FIRST bf_Size WHERE bf_Size.wg_Name =
       STRING(CURRENT-WINDOW) NO-ERROR.
 IF AVAILABLE bf_Size THEN DO:
   ASSIGN hCol:HEIGHT-PIXELS = (hCol:HEIGHT-PIXELS *
       CURRENT-WINDOW:HEIGHT-PIXELS) / bf_Size.wg_Height
          hCol:WIDTH-PIXELS = (hCol:WIDTH-PIXELS *
       CURRENT-WINDOW:WIDTH-PIXELS) / bf_Size.wg_Width
                   NO-ERROR.
   /*  Find the Entry for the Frame, and if it doesn't exist,
   create one */
   FIND FIRST tt_Size WHERE tt_Size.wg_Name =
               STRING(hCol) NO-ERROR.
   IF NOT AVAILABLE tt_Size THEN DO:
     CREATE tt_Size.
     ASSIGN tt_Size.wg_Name = STRING(hCol)
            tt_Size.wg_Width = hCol:WIDTH-PIXELS
            tt_Size.wg_Height = hCol:HEIGHT-PIXELS
            tt_Size.wg_xPos = hCol:X
            tt_Size.wg_yPos = hCol:Y NO-ERROR.
   END.
   ASSIGN hCol = hCol:FIRST-CHILD NO-ERROR.
   ASSIGN hCol = hCol:FIRST-CHILD NO-ERROR.
   /* Walk the Widget Tree and if there is no entry in the
      Temp-Table, create one, then resize & replace */
   DO WHILE VALID-HANDLE(hCol):
     FIND FIRST tt_Size WHERE tt_Size.wg_Name =
       STRING(hCol) NO-ERROR.
     IF NOT AVAILABLE tt_Size THEN DO:
       CREATE tt_Size.
       ASSIGN tt_Size.wg_Name = STRING(hCol)
              tt_Size.wg_Width = hCol:WIDTH-PIXELS
              tt_Size.wg_Height = hCol:HEIGHT-PIXELS
              tt_Size.wg_xPos = hCol:X
              tt_Size.wg_yPos = hCol:Y NO-ERROR.
     END.
     ASSIGN hCol:HEIGHT-PIXELS = (hCol:HEIGHT-PIXELS *
       CURRENT-WINDOW:HEIGHT-PIXELS) / bf_Size.wg_Height
            hCol:WIDTH-PIXELS = (hCol:WIDTH-PIXELS *
       CURRENT-WINDOW:WIDTH-PIXELS) / bf_Size.wg_Width
            hCol:X = (hCol:X * CURRENT-WINDOW:WIDTH-PIXELS) /
       bf_Size.wg_Width
            hCol:Y = (hCol:Y * CURRENT-WINDOW:HEIGHT-PIXELS) /
       bf_Size.wg_Height NO-ERROR.
     ASSIGN hCol = hCol:NEXT-SIBLING NO-ERROR. /* Step to next
                          Widget */
   END.
END.


/**  WINDOW-RESTORED Trigger **/
/* This trigger restores the original Widget's Dimensions and
  Positions */

IF winState = 3 THEN
   ASSIGN winState = 0 NO-ERROR. /* If the window has been
                    minimized, do nothing */
 ELSE DO: /* If the window was in Maximized State, find the
         Temp-Table record for each Widget and Restore it */
   IF winState = 1 THEN DO:
     ASSIGN winState = 0 NO-ERROR.
   END.
   ASSIGN hCol = FRAME {&FRAME-NAME}:HANDLE NO-ERROR.
   FIND FIRST tt_Size WHERE wg_Name = STRING(hCol) NO-ERROR.
   IF AVAILABLE tt_Size THEN DO:
     ASSIGN hCol:HEIGHT-PIXELS = tt_Size.wg_Height
            hCol:WIDTH-PIXELS = tt_Size.wg_Width
            hCol:X = tt_Size.wg_xPos
            hCol:Y = tt_Size.wg_yPos NO-ERROR.
   END.
   ASSIGN hCol = hCol:FIRST-CHILD NO-ERROR.
   ASSIGN hCol = hCol:FIRST-CHILD NO-ERROR.
   DO WHILE VALID-HANDLE(hCol):
     FIND FIRST tt_Size WHERE wg_Name = STRING(hCol) NO-ERROR.
     IF AVAILABLE tt_Size THEN DO:
       ASSIGN hCol:HEIGHT-PIXELS = tt_Size.wg_Height
              hCol:WIDTH-PIXELS = tt_Size.wg_Width
              hCol:X = tt_Size.wg_xPos
              hCol:Y = tt_Size.wg_yPos NO-ERROR.
     END.
     ASSIGN hCol = hCol:NEXT-SIBLING NO-ERROR.
   END.
 END.


/** WINDOW-MINIMIZED Trigger **/

ASSIGN winState = 3. /* 3 = Minimized */

/** WINDOW-RESIZED Trigger **/
/* This trigger is simply in place to trap resizing to the other
  triggers */
IF winState <> 1 THEN DO:
   FIND FIRST tt_Size WHERE tt_Size.wg_Name =
       STRING(CURRENT-WINDOW) NO-ERROR.
   IF AVAILABLE tt_Size THEN
     ASSIGN CURRENT-WINDOW:HEIGHT-PIXELS = tt_Size.wg_Height
            CURRENT-WINDOW:WIDTH-PIXELS =
       tt_Size.wg_Width NO-ERROR.
END.
