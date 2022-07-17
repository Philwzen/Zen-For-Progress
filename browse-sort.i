&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

  Purpose:   allow column-sorting  
  Parameters:  
  Notes:  in start-search trigger     
      SORTBROWSE({&self-NAME}:HANDLE,"{&QUERY-STRING-{&self-name}}").
    also        
    setinitcolumn (browsehandle). 
     before initialisng scrren to set white background for first column.
    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/*
moved to commonmaint.i so can be used in appsrvbrowse.i
def var lv-scol as handle no-undo.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 43.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

{&browsename}:allow-column-searching = true.


on start-search of {&browsename}
  run proc-SORTBROWSE ({&browseNAME}:HANDLE,"{&QUERY-STRING-{&browsename}}").

on "MOUSE-MENU-CLICK" of {&browsename}
  run proc-findrow ({&browseNAME}:HANDLE).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


