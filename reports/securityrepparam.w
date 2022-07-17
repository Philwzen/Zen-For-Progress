&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME win-main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS win-main 
/******************************************************************************
 PROGRAM ID.     : {&core}reports/securityrepparam.w
 PROGRAM TITLE   : Menu Security Report
 CREATE DATE     : 01/01/2009
 COMPANY NAME    : Great Valley Technologies, Inc.
*******************************************************************************
 Date     Ini. DESCRIPTION
 ------------------------------------------------------------------------------
 01/01/09 PW,  Initial Release
 03/11/09 EKS, Changed code to be more uniform.
******************************************************************************/

/* Create an unnamed pool to store all the widgets created by this procedure.
   This is a good default which assures that this procedure's triggers and
   internal procedures will execute in this procedure's storage, and that
   proper cleanup will occur on deletion of the procedure. */
create widget-pool.

/* ***************************  Preprocessors  ************************** */
{app-paths.i}
&glob title-text Menu Security Report     /* message box title */

&glob NoNew             /* no new button */
&glob NoEdit            /* no edit button */
&glob NoSave            /* no save button */
&glob NoDelete          /* no delete button */
&glob NoExport          /* no export button */
&glob NoUndo            /* no undo button */
&glob NoQuery           /* no query button */
&glob NoAudit           /* no audit button */
&glob NoPrint           /* no print button */
&glob NoChangedCheck    /* disable changed onleave check */
&glob NoExitCheck       /* disble exit question */

/*
&glob bug               /* turn on debug mesasges */
&glob tree              /* turn on procedure tree listing */
&glob NoButtons         /* no buttons displayed */
&glob NoExit            /* no exit button */
&glob NoHelp            /* no help button */
&glob NoImmediateQuery  /* do not openquery */
&Glob NoImmediateQuery  /* force open query */
&glob NotFoundMessage   /* no record not found message */
*/

/* ***************************  Definitions  ************************** */
define temp-table t-menuSecurity no-undo
   field menuFullName as char
   field accessInfo   as char
   field extraDetails as char.
define stream securityFile.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-logonid cb-userGroup rs-outputMenuOptions ~
btn-OK 
&Scoped-Define DISPLAYED-OBJECTS v-logonid cb-userGroup rs-outputMenuOptions 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 v-logonid cb-userGroup rs-outputMenuOptions 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR win-main AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-OK 
     LABEL "OK" 
     SIZE 15 BY 1.14.

def var cb-userGroup as char FORMAT "X(256)":U 
     LABEL "User Group" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "None","x"
     DROP-DOWN-LIST
     SIZE 41 BY 1 TOOLTIP "Select a user group from the drop down list."
     BGCOLOR 21  NO-UNDO.

def var v-logonid as char FORMAT "X(25)":U 
     LABEL "User" 
     VIEW-AS FILL-IN 
     SIZE 36.4 BY 1 TOOLTIP "Enter a valid user name, or leave blank if you select a user group."
     BGCOLOR 21  NO-UNDO.

def var rs-outputMenuOptions as char 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Permitted Menu Options", "Can",
"All Menu Options", "All"
     SIZE 35.6 BY 2 TOOLTIP "Choose which option you wish to output to securitylsiting.csv."
     BGCOLOR 21  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     v-logonid AT ROW 1.48 COL 23.8 COLON-ALIGNED
     cb-userGroup AT ROW 3.24 COL 23.8 COLON-ALIGNED WIDGET-ID 8
     rs-outputMenuOptions AT ROW 5.71 COL 25.8 NO-LABEL
     btn-OK AT ROW 8.76 COL 28.2
     "Include:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 5.71 COL 16 WIDGET-ID 4
     "OR" VIEW-AS TEXT
          SIZE 5 BY .86 AT ROW 2.57 COL 19.6 WIDGET-ID 10
          FONT 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70.2 BY 10.95
         
         TITLE "Menu Security Report".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW win-main ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         COLUMN             = 37.4
         ROW                = 14.43
         HEIGHT             = 10.95
         WIDTH              = 70.2
         MAX-HEIGHT         = 12.1
         MAX-WIDTH          = 112
         VIRTUAL-HEIGHT     = 12.1
         VIRTUAL-WIDTH      = 112
         RESIZE             = yes
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
ASSIGN win-main = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME f-main
   FRAME-NAME                                                           */
ASSIGN 
       FRAME f-main:HIDDEN           = TRUE
       FRAME f-main:PRIVATE-DATA     = 
                "1".

/* SETTINGS FOR COMBO-BOX cb-userGroup IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN v-logonid IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR RADIO-SET rs-outputMenuOptions IN FRAME f-main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-OK win-main
ON CHOOSE OF btn-OK IN FRAME f-main /* OK */
DO:
   def var hFailedWidget as handle     no-undo.
   def var lv-userGroup  as char  no-undo.


   /* validate the screen before continuing */
   run validate-screen in this-procedure.
   if return-value ne "passed" then do:
      hFailedWidget = widget-handle(return-value).
      if valid-handle(hFailedWidget) then apply "entry" to hFailedWidget.
      return no-apply.
   end. /* error - put user's cursor on problem field */

   /* assign the field values */
   assign frame {&frame-name} {&list-1}.

   /* if we have selected a user, then we want the group to be the user's
      group (server-side will check through the user to see if access to a
      menu is allowed or denied, and if we send it a group, then it will also
      check through the groups after first checking the user.  This must be
      done in case a user is not mentioned at all on the server side. */
   lv-userGroup = if v-logonid ne "" then getField( /* zenlibrary.p */
      "zen-duser",
      "duser",
      v-logonid,
      "u-group") else cb-userGroup.

   /* make sure we have nothing in the temp-table */
   empty temp-table t-menuSecurity.

   /* find all menu security information requested by the user */
   {{&core}run.i
      &program   = "Securityrep.p"
      &path      = "{&core}{&reports}{&srv}"
      &Appsrv    = "System"
      &noper     = true
      &procedure = "get-menuSecurityInfo"
      &params    = "(input  v-logonid,
                     input  lv-userGroup,
                     input  rs-outputMenuOptions,
                     output table t-menuSecurity)"}

   /* output our results to a file */
   output stream securityFile to securitylisting.csv.

   /* put the columns into the file */
   put stream securityFile unformatted
      "Users,Groups,Access,Menu" skip.

   /* loop through records, putting each field into its appropriate column */
   for each t-menuSecurity:
      put stream securityFile unformatted
         v-logonid                                   "," /* selected user */
         lv-userGroup                                "," /* user group */
         t-menuSecurity.accessInfo                   "," /* accessible */
         replace(t-menuSecurity.menuFullName,"&","") "," /* menu name & path */
         t-menuSecurity.extraDetails                     /* shouldn't be any */
         skip.
   end. /* each t-menuSecurity */

   /* close the output */
   output stream securityFile close.

   /* tell user where the report is */
   message "The completed report was output to securitylisting.csv."
      view-as alert-box info buttons ok.
   run exit-trigger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK win-main 


/* ***************************  Main Block  *************************** */
/* Set current-window: this will parent dialog-boxes and frames.        */
assign
   current-window                = {&window-name} 
   this-procedure:current-window = {&window-name}.

/* basic logic included by almost all programs */
{{&core}commonmaint.i
   &path = "{&sys}{&srv}"}

/* The "close" event can be used from inside or outside the procedure to
   terminate it. */
on close of this-procedure do:
   if not lv-exited then run exit-trigger in this-procedure no-error.
   run disable_ui.
end. /* procedure closed */

/* if window is closed, then exit */
on window-close of {&window-name} do:
   run exit-trigger in this-procedure no-error.
end. /* window closed */

/* special end key was pressed or error occurred, then exit */
on endkey, end-error of {&window-name} anywhere do:
   run exit-trigger in this-procedure no-error.
end. /* error occurred */

/* show the window */
{&window-name}:hidden = false.

/* Best default for GUI applications is . . . */
pause 0 before-hide.

/* Now enable the interface and wait for the exit condition.  (NOTE: handle
   error and end-key so cleanup code will always fire. */
main-block: do
on error   undo main-block, leave main-block
on end-key undo main-block, leave main-block:
   run enable_UI.

   /* starts procedure initialization process */
   run initialise.

   if not this-procedure:persistent or
      session:display-type ne "GUI" then wait-for close of this-procedure.
end. /* main-block */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI win-main  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI win-main  _DEFAULT-ENABLE
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
  DISPLAY v-logonid cb-userGroup rs-outputMenuOptions 
      WITH FRAME f-main.
  ENABLE v-logonid cb-userGroup rs-outputMenuOptions btn-OK 
      WITH FRAME f-main.
  VIEW FRAME f-main.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW win-main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialise win-main 
PROCEDURE local-initialise :
/*-----------------------------------------------------------------------------
  Purpose: Allow customising of initialisation.
  Notes:   Return "override" to stop default processing.
-----------------------------------------------------------------------------*/
   /* the labels and their corresponding keys for the user group combo-box */
   def var userGroupLabels as char  no-undo.
   def var userGroupKeys   as char  no-undo.
   def var counter         as int    no-undo.


   /* get a list of user group codes and labels from the class code records */
   userGroupKeys = ClassCodes(
      input  "{&usergrp}",
      output userGroupLabels).

   /* add the user groups to the user group combo-box */
   if num-entries(cb-userGroup:list-item-pairs in frame {&frame-name},cb-userGroup:delimiter) <
      num-entries(userGroupKeys,"{&comboDelim}") then
   do counter = 1 to num-entries(userGroupKeys,"{&comboDelim}"):
      cb-userGroup:add-last(
         entry(counter,userGroupLabels,"{&comboDelim}"),
         entry(counter,userGroupKeys,"{&comboDelim}")) in frame {&frame-name}.
   end.

   /* make lookup buttons appear, since we aren't doing the normal Edit */
   SetAllLkBut(frame {&frame-name}:handle).  /* in generallibrary.p */

   /* stop the default initialization processing */
   return "override".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh win-main 
PROCEDURE refresh :
/*-----------------------------------------------------------------------------
  Purpose: Typically, the procedure run by a parent program.
  Notes:   Inatialization will occur after this procedure is run, so we do not
            want to run a procedure that will be run in that process anyway.
-----------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-screen win-main 
PROCEDURE validate-screen :
/*-----------------------------------------------------------------------------
  Purpose: Validate screen entries prior to save.
  Notes:   Return "passed" if ok or string of failed widget-handle.
-----------------------------------------------------------------------------*/
   /* check if the user tried enterring both a user and a user group */
   if input frame {&frame-name} v-logonid    ne "" and
      input                     cb-userGroup ne ""
   then do:
      message "Enter either a user or a user group, not both."
         view-as alert-box info buttons OK.
      return string(v-logonid:handle).
   end. /* cannot enter both a user and user group */

   return "passed".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

