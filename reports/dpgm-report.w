&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME philwin-main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS philwin-main 
/******************************************************************************/
/*  PROGRAM ID.     : ????????                                                */
/*  PROGRAM TITLE   : ????????                                                */
/*                                                                            */
/*  CREATE DATE     : ??/??/??                                                */
/*                                                                            */
/*  COMPANY NAME    : Zennor Computing LTD                                    */
/*  VERSION NO.     : 1.0                                                     */
/******************************************************************************/
/******************************************************************************/
/*                          PATCH HISTORY                                     */
/*                                                                            */
/*           PATCH  USR     Job                                               */
/* DATE      NO.    ID      REF DESCRIPTION                                   */
/* -------------------------------------------------------------------------- */
/* ??/??/??  P00    Philw   00  initial release                               */
/******************************************************************************/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&glob title-text ?????
/* &glob table-name zen-dpgm              */
/* &glob Unique-key {&table-name}tableid  */
&glob KeepRefreshButton 
&glob NoButtons
&global-define suppresswindow
/* ***************************  Definitions  ************************** */
{app-paths.i}
{{&core}{&rep}reportdefs.i} /* problem with unixpath in here */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-params lv-print-type 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-params lv-print-type 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 FILL-IN-params lv-print-type 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR philwin-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-params AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "Author" 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE lv-print-type AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Immediate", 1,
"Spool", 2,
"Batch", 3
     SIZE 34.6 BY 1.1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     FILL-IN-params AT ROW 1.71 COL 17 COLON-ALIGNED WIDGET-ID 4
     lv-print-type AT ROW 4.81 COL 22 NO-LABEL WIDGET-ID 8
     "uses matchs so can be p* etc." VIEW-AS TEXT
          SIZE 38 BY .86 AT ROW 3.14 COL 22 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 75.8 BY 5.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW philwin-main ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         COLUMN             = 4
         ROW                = 8.71
         HEIGHT             = 5.81
         WIDTH              = 75.8
         MAX-HEIGHT         = 12.1
         MAX-WIDTH          = 112
         VIRTUAL-HEIGHT     = 12.1
         VIRTUAL-WIDTH      = 112
         SHOW-IN-TASKBAR    = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN philwin-main = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME f-main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-params IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR RADIO-SET lv-print-type IN FRAME f-main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK philwin-main 


/* ***************************  Main Block  *************************** */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{{&core}commonmaint.i &path = "{&sys}{&srv}"}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {{&core}sec-chk.i}
    RUN enable_UI.
    run initialise.
    {{&core}wid-chk.i}
  {{&core}focus.i}
IF NOT THIS-PROCEDURE:PERSISTENT OR SESSION:DISPLAY-TYPE ne "GUI":U then
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI philwin-main  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Do-Report philwin-main 
PROCEDURE Do-Report :
def var lv-params      as char no-undo.     
def var lv-paramtitles as char no-undo.
def var lv-repdest     as char no-undo init 'window'.
def var lv-opfile      as char no-undo.
def var x              as int  no-undo.

/********************** Start Custom bit ****************************/
/* put paramater names and fill string of parameter values here */
/*
assign
    lv-paramtitles = "from-contract{&Delim2}to-contract{&Delim2}area{&Delim2}op-unit{&Delim2}zone{&Delim2}sort-by{&Delim2}permanent-records" /* {&Delim2} Delimited list */
    lv-params  = string(fil-from-cont) + '{&Delim2}' + 
                 string(fil-to-cont)   + '{&Delim2}' + 
                 fil-area    + '{&Delim2}' + 
                 fil-op-unit + '{&Delim2}' + 
                 fil-zone    + '{&Delim2}' + 
                 rs-order    + '{&Delim2}' +
                 rs-permanent:screen-value in frame {&frame-name}.
                      */

   lv-params = fill-in-params.
   lv-paramtitles = 'Author'.

/********************** End Custom Bit  ****************************/


{{&core}{&rep}doreport.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI philwin-main  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FILL-IN-params lv-print-type 
      WITH FRAME f-main.
  ENABLE FILL-IN-params lv-print-type 
      WITH FRAME f-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW philwin-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetRepDefaults philwin-main 
PROCEDURE GetRepDefaults :
/*  lv-repname   = entry(1,lv-value,'{&Delim3}') 
    lv-extrprog  = entry(2,lv-value,'{&Delim3}')
    lv-datfile   = entry(3,lv-value,'{&Delim3}')
    lv-cryfile   = entry(4,lv-value,'{&Delim3}')
    lv-params    = entry(5,lv-value,'{&Delim3}')}
*/

{{&core}{&rep}getrepdefaults.i &program-name = this-procedure:name}

/*
/********************* Start Custom Bit ****************************/
*/ fill-in-params:screen-value in frame {&frame-name} = lv-params.
/*
do x = 1 to num-entries(lv-params,'{&Delim2}'):
    case x:   /* screen variable Saved defaults */
        when 1 then fil-from-cont:screen-value = entry(x,lv-params,'{&Delim2}'). 
        when 2 then fil-to-cont:screen-value   = entry(x,lv-params,'{&Delim2}'). 
        when 3 then fil-area:screen-value    = entry(x,lv-params,'{&Delim2}'). 
        when 4 then fil-op-unit:screen-value  = entry(x,lv-params,'{&Delim2}'). 
        when 5 then fil-zone:screen-value    = entry(x,lv-params,'{&Delim2}'). 
        when 6 then rs-order:screen-value   = entry(x,lv-params,'{&Delim2}'). 
    end case.
end.
/********************* End Custom Bit ****************************/ 
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-initialise philwin-main 
PROCEDURE Local-initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CreateButs ("Ok^TTF,Exit^TTF{&Delim2}" +
            string(this-procedure) + "," +
            string({&window-name}:handle) + "," +
            string(frame {&frame-name}:handle) + "," + 
            "{&btnhorizontal},{&btnflat},{&btnstartcol},{&btnstartrow},{&btnheight},{&btnwidth},{&btncenter}").
          
run GetRepDefaults in this-procedure no-error.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok-trigger philwin-main 
PROCEDURE ok-trigger :
DEF VAR LV-HAND AS HANDLE NO-UNDO.
    run validate-screen in this-procedure.
        
    if return-value ne 'passed' then do:
        lv-hand = widget-handle(return-value).
        apply 'entry' to lv-hand.
        Return 'failed'.
    end.
    else do:
        assign frame {&frame-name}
            {&list-1}.
    
        run do-report in this-procedure no-error.
    
        if return-value = "passed" 
            then RUN exit-trigger.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh philwin-main 
PROCEDURE refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
message '' {&dbt}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen philwin-main 
PROCEDURE validate-screen :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
/* validate the screen widgets etc if any fail 
    display message put focus on that widget and return 'failed' 
 e.g.

  IF lv-new THEN
      IF CAN-FIND(FIRST b-{&Table-name} WHERE b-{&Table-name}.keyfield = t-{&Table-name}.keyfield) 
      THEN DO:
          MESSAGE msg (120,"","","","") VIEW-AS ALERT-BOX.
          RETURN STRING(t-{&Table-name}.keyfield:HANDLE in frame one).
      END.
   else return 'passed'.
*/
    return 'passed'.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

