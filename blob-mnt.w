&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME window-maint


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-blob NO-UNDO LIKE zen-blob.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS window-maint 
/******************************************************************************/
/*  PROGRAM ID.     :                                                         */
/*  PROGRAM TITLE   :                                                         */
/*                                                                            */
/*  CREATE DATE     : ??/??/??                                                */
/*                                                                            */
/*  COMPANY NAME    :                                                         */
/*  VERSION NO.     : 1.0                                                     */
/******************************************************************************/
/******************************************************************************/
/*                          PATCH HISTORY                                     */
/*                                                                            */
/*           PATCH  USR     Job                                               */
/* DATE      NO.    ID      REF DESCRIPTION                                   */
/* -------------------------------------------------------------------------- */
/* ??/??/??  P00    Philw   00  initial release                               
   01/19/10  P78    Annerik 2F  Set memptrs to zero to avoid Progress bug.    */
/******************************************************************************/
/****************************************************************************** 
 this is a semi generic routine you only need to paint the screen widgets
 then edit pc-save for any fields not on the screen ie time stamps etc
 and edit validate-screen for any validation logic you require to allow record 
 to be saved 
********************************************************************************/

create widget-pool.
{app-paths.i}
&glob title-text Blob Maintenance
&glob zenscreen true

&glob table-name zen-blob
&glob unique-key {&table-name}tableid

/* ***************************  Definitions  ************************** */
/* Parameters Definitions ---                                           */
/* Local var Definitions ---                                            */
/*
&glob bug               /* turn on debug mesasges */
&glob tree              /* turn on procedure tree listing */
&glob NoExitCheck       /* disble exit question */
&glob NoButtons         /* no buttons displayed */
&glob NoNew             /* no new button */
&glob NoEdit            /* no edit button */
&glob NoSave            /* no save button */
&glob NoDelete          /* no delete button */
&glob NoUndo            /* no undo button */
&glob NoExit            /* no exit button */
&glob NoHelp            /* no help button */
&glob NoImmediateQuery  /* do not openquery */
&Glob ImmediateQuery    /* force open query */
&glob NotFoundMessage   /* no record not found message */
*/

&glob suppresswindow   /* no window creation */
&glob NoAudit           /* no audit button */
&glob NoQuery           /* no query button */
&glob NoExport          /* no export button */
&glob NoPrint           /* no print button */
&glob NoChangedCheck    /* disable changed onleave check */
def var lv-parentname as char no-undo.
def var lv-parentid as dec no-undo.
def var lv-desc as char no-undo.
def var lv-filename as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME a-frame-maint
&Scoped-define BROWSE-NAME br-maint

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES t-zen-blob

/* Definitions for BROWSE br-maint                                      */
&Scoped-define FIELDS-IN-QUERY-br-maint t-zen-blob.description ~
t-zen-blob.Blob-Filename 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-maint 
&Scoped-define QUERY-STRING-br-maint FOR EACH t-zen-blob NO-LOCK
&Scoped-define OPEN-QUERY-br-maint OPEN QUERY br-maint FOR EACH t-zen-blob NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-maint t-zen-blob
&Scoped-define FIRST-TABLE-IN-QUERY-br-maint t-zen-blob


/* Definitions for FRAME a-frame-maint                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-maint cb-type TOGGLE-1 btn-load 
&Scoped-Define DISPLAYED-FIELDS t-zen-blob.description ~
t-zen-blob.Blob-Filename 
&Scoped-define DISPLAYED-TABLES t-zen-blob
&Scoped-define FIRST-DISPLAYED-TABLE t-zen-blob
&Scoped-Define DISPLAYED-OBJECTS cb-type TOGGLE-1 

/* Custom List Definitions                                              */
/* add-list,edit-list,List-3,List-4,List-5,List-6                       */
&Scoped-define add-list t-zen-blob.description t-zen-blob.Blob-Filename ~
cb-type TOGGLE-1 
&Scoped-define edit-list t-zen-blob.description t-zen-blob.Blob-Filename ~
cb-type TOGGLE-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR window-maint AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-load 
     LABEL "Load" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cb-type AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL no 
     LABEL "Auto Install" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-maint FOR 
      t-zen-blob SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-maint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-maint window-maint _STRUCTURED
  QUERY br-maint NO-LOCK DISPLAY
      t-zen-blob.description FORMAT "X(50)":U WIDTH 42.2
      t-zen-blob.Blob-Filename FORMAT "x(50)":U WIDTH 43.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 91 BY 10.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME a-frame-maint
     br-maint AT ROW 1.43 COL 13 HELP
          "Select the record to edit."
     t-zen-blob.description AT ROW 12.43 COL 13 WIDGET-ID 4 FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 73 BY 1
     t-zen-blob.Blob-Filename AT ROW 13.38 COL 15.2 WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 73 BY 1
     cb-type AT ROW 14.43 COL 22.8 COLON-ALIGNED WIDGET-ID 8
     TOGGLE-1 AT ROW 14.48 COL 80 WIDGET-ID 10
     btn-load AT ROW 15.52 COL 4 WIDGET-ID 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.1
         SIZE 106.4 BY 16.24 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: t-zen-blob T "?" NO-UNDO schadm zen-blob
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW window-maint ASSIGN
         HIDDEN             = YES
         TITLE              = "Browse Maintenance"
         HEIGHT             = 16.33
         WIDTH              = 106.6
         MAX-HEIGHT         = 45.33
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.33
         VIRTUAL-WIDTH      = 256
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN window-maint = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW window-maint
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME a-frame-maint
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB br-maint 1 a-frame-maint */
/* SETTINGS FOR FILL-IN t-zen-blob.Blob-Filename IN FRAME a-frame-maint
   NO-ENABLE ALIGN-L 1 2                                                */
/* SETTINGS FOR COMBO-BOX cb-type IN FRAME a-frame-maint
   1 2                                                                  */
/* SETTINGS FOR FILL-IN t-zen-blob.description IN FRAME a-frame-maint
   NO-ENABLE ALIGN-L 1 2 EXP-FORMAT                                     */
/* SETTINGS FOR TOGGLE-BOX TOGGLE-1 IN FRAME a-frame-maint
   1 2                                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME a-frame-maint
/* Query rebuild information for FRAME a-frame-maint
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME a-frame-maint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-maint
/* Query rebuild information for BROWSE br-maint
     _TblList          = "Temp-Tables.t-zen-blob"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER,"
     _FldNameList[1]   > Temp-Tables.t-zen-blob.description
"t-zen-blob.description" ? "X(50)" "character" ? ? ? ? ? ? no ? no no "42.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.t-zen-blob.Blob-Filename
"t-zen-blob.Blob-Filename" ? "x(50)" "character" ? ? ? ? ? ? no ? no no "43.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-maint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-load
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-load window-maint
ON CHOOSE OF btn-load IN FRAME a-frame-maint /* Load */
do:
   define var lv-memptr   as memptr no-undo.


   /* make sure there is nothing left over in the memory pointer! */
   set-size(lv-memptr) = 0.

   lv-filename = getosfile(lv-filename).

   if not filenotfound(lv-filename) then do:
/*       lv-filename = getfullpath(lv-filename).  */

      copy-lob from file lv-filename to lv-memptr.
      assign
         t-zen-blob.Blob-Filename = lv-filename
         t-zen-blob.blob-data     = lv-memptr.

      disp t-zen-blob.Blob-Filename
      with frame {&frame-name}.
   end.
   else message 'Invalid File Name' skip
                lv-filename
        view-as alert-box error.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-maint
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK window-maint 


/* ***************************  Main Block  *************************** */
/* Set current-window: this will parent dialog-boxes and frames. */

assign current-window                = {&window-name} 
       this-procedure:current-window = {&window-name}.
/* main core logic */
{{&core}commonmaint.i &path = "{&core}{&srv}"
                &extraparams = "lv-parentname,lv-parentid,"  }
                   /* &extratables = ",???" */
/* Best default for GUI applications is... */
pause 0 before-hide.
/* Now enable the interface and wait for the exit condition. */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire. */
main-block:
do on error undo main-block, leave main-block:
   {{&core}sec-chk.i} /* program security check */
   run enable_UI.     
   {{&core}wid-chk.i} /* widget level security check */
   {{&core}focus.i}   /* find first enabled widget */
   if not this-procedure:persistent then
      wait-for close of this-procedure. 
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-validate window-maint 
PROCEDURE delete-validate :
/*------------------------------------------------------------------------------
  Purpose:   return 'passed' if ok to delete record  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    return "passed".

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI window-maint  _DEFAULT-DISABLE
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
  HIDE FRAME a-frame-maint.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disp-wids window-maint 
PROCEDURE disp-wids :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if lv-Newmode then 
   assign  t-zen-blob.Blob-Filename:screen-value in frame {&frame-name} = lv-filename
           t-zen-blob.description:screen-value   = lv-desc.
/*
 cb-type:screen-value = setcombovalue(cb-type:handle,t-{&table-name}.zen-blobtableid).
*/
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI window-maint  _DEFAULT-ENABLE
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
  DISPLAY cb-type TOGGLE-1 
      WITH FRAME a-frame-maint.
  IF AVAILABLE t-zen-blob THEN 
    DISPLAY t-zen-blob.description t-zen-blob.Blob-Filename 
      WITH FRAME a-frame-maint.
  ENABLE br-maint cb-type TOGGLE-1 btn-load 
      WITH FRAME a-frame-maint.
  {&OPEN-BROWSERS-IN-QUERY-a-frame-maint}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-ChildReturn window-maint 
PROCEDURE Local-ChildReturn :
/*------------------------------------------------------------------------------
  Purpose:  allow processing after a child screen exits.   
  Parameters:  <none>
  Notes:   pv-from child which has exited    
------------------------------------------------------------------------------*/
def input param pv-from as char no-undo.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise window-maint 
PROCEDURE local-initialise :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

BuildCombo(cb-type:handle in frame {&frame-name},
                  "zen-classcode",
                  "code",
                  "description",
                  'where zen-classcode.class = "blobtype"',
                  'by zen-classcode.class',
                  no,
                  no).
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Local-update-child-procedures window-maint 
PROCEDURE Local-update-child-procedures :
/*------------------------------------------------------------------------------
  Purpose:  refresh any child procedures
  Parameters:  pv-to handle of child requesting refresh
  Notes:       called via published event in child
------------------------------------------------------------------------------*/
def input param pv-to as handle no-undo.

case pv-to:private-data:
/*     when 'order-maint.w' then run refresh in  pv-to (t-{&table-name.cust-num).  */
/*     when 'sr-maint.w'    then run refresh in  pv-to.                            */
end case.        


end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pre-Save window-maint 
PROCEDURE Pre-Save :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if lv-newmode then
assign
   t-{&table-name}.ParentTableName = lv-parentname
   t-{&table-name}.ParentTableId   = lv-parentid
   /*
   t-{&table-name}.blob-type = getcombokey(cb-type:handle in frame {&frame-name})
   */
.

   return "passed".
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh window-maint 
PROCEDURE Refresh :
/*------------------------------------------------------------------------------
  Purpose:   refresh the screen  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-parentname as char no-undo.
def input param pv-parentid as dec no-undo.
def input param pv-desc as char no-undo.
def input param pv-filename as char no-undo.

assign
   lv-parentname = pv-parentname
   lv-parentid = pv-parentid
   lv-desc = pv-desc
   lv-filename = pv-filename.

  run openquery.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen window-maint 
PROCEDURE validate-screen :
/* -----------------------------------------------------------
  Purpose:   validate screen entries prior to save  
  Parameters:  <none>
  Notes:      return 'passed' if ok or string of failed widget-handle 
-------------------------------------------------------------*/
  def var v-recid as recid no-undo. 
  def buffer b-{&table-name} for t-{&table-name}.
/*
  IF lv-new THEN
      IF CAN-FIND(FIRST b-{&Table-name} WHERE b-{&Table-name}.keyfield = t-{&Table-name}.keyfield) 
      THEN DO:
          MESSAGE msg (120,"","","","") VIEW-AS ALERT-BOX.
          RETURN STRING(t-{&Table-name}.keyfield:HANDLE in frame one).
      end.
  */

  return 'passed'.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateScreen window-maint 
PROCEDURE ValidateScreen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if t-zen-blob.blob-data = ? then do:
   message 'No Binary Data Loaded' skip
           'Please LOAD File '
   view-as alert-box error.
   return string(btn-load:handle in frame {&frame-name}).
end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

