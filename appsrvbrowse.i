&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF VAR lv-key-{&browsename} AS CHAR   no-undo.

&if defined(usemoreimage) ne 0 &then
   def var h-{&browsename}mored as handle  no-undo.
   def var holdRow              as dec no-undo.
    
   if {&browsename}:column - 1 > 0 then do:
      /* all this because some browses have one row for
         column headers (payor) and some have two (DX codes for example).
         Have also tried this:
         ((1 / font-table:get-text-height-chars({&browsename}:font) - 0.1)))
      */
      holdRow = {&browsename}:row +
                {&browsename}:height-chars -
                ({&browsename}:down * 1.075).
      create IMAGE h-{&browsename}mored
         assign
         name           = '{&browsename}mored'
         frame          = frame {&frame-name}:handle
         sensitive      = false
         visible        = false
         tooltip        = "More records to come ..."
         row            = if holdrow ge 1 then holdRow else 1
         col            = {&browsename}:column - 2.4.
      h-{&browsename}mored:load-image("{&core}{&bmp}mored.ico").  
   end. /* room exists in which to display image */
&endif

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
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */
ON RETURN OF {&browsename} APPLY "MOUSE-SELECT-DBLCLICK" TO {&browsename}.
       
ON CURSOR-UP,CURSOR-DOWN,PAGE-UP,page-down,end,home OF {&browsename} do:
    lv-key-{&browsename} = KEYFUNCTION(LASTKEY).
/*     if lv-key-{&browsename} = '' then lv-key-{&browsename} = 'end'.  */
    if lv-key-{&browsename} = 'end' 
        then apply 'Off-home' to {&browsename}.
    if lv-key-{&browsename} = 'home' 
        then apply 'Off-end' to {&browsename}.
end.

ON SCROLL-NOTIFY OF {&browsename} lv-key-{&browsename} = "MOUSE-SCROLL".
  
ON OFF-END OF {&browsename} DO:
    if lv-key-{&browsename} = '' then return no-apply.
    run local-browse-off-end in this-procedure no-error.
    if not error-status:error and
         return-value = 'override' then return.
    run browseoffend('down',{&browsename}:handle).
END.


&if defined(AllowUpScrolling) &then
ON Off-home OF {&browsename} DO:
    if lv-key-{&browsename} = '' then return no-apply.
    run local-browse-off-home in this-procedure no-error.
    if not error-status:error and
         return-value = 'override' then return.
    run browseoffend('up',{&browsename}:handle).
end.
&endif

{&browsename}:SET-REPOSITIONED-ROW({&browsename}:DOWN,"CONDITIONAL").

/*********** browse triggers *****************/

&if defined(ENABLED-tables-IN-QUERY-{&browsename}) &then
on row-leave of {&browsename} do:
   if 'ENABLED-FIELDS-IN-QUERY-{&browsename}' = '' 
      or not focus:modified 
      or donotfire('btn-exit') 
   then return.
   
   &if defined(table-name) ne 0 &then
      empty temp-table tb-{&table-name}.
      create tb-{&table-name}.
      buffer-copy t-{&table-name} to tb-{&table-name}.
   &endif

   run local-assign-extratables no-error.
   assign browse {&browsename} {&ENABLED-FIELDS-IN-QUERY-{&browsename}} 
        no-error.
   run br-changed-trigger in this-procedure no-error.
   run save-record in this-procedure no-error.
   If return-value = 'passed' 
   Then apply key-function(lastkey).
   else do:
     focus:modified = true.
     return no-apply.
   end.
end.
&endif

ON MOUSE-SELECT-DBLCLICK OF {&browsename} IN FRAME {&frame-name}
DO:
    def var lv-hedit as handle no-undo.
    lv-hedit = getwidhandle(frame {&frame-name}:handle,'btn-edit').
    &if defined(UseBrDblClick) = 0 &then
        if not valid-handle(lv-hedit) then return no-apply.
    &endif
    if valid-handle(lv-hedit) and not lv-hedit:sensitive then return no-apply.
    run br-msdblclick-trigger in this-procedure no-error.
END.

ON RETURN OF {&browsename} IN FRAME {&frame-name}
DO:
    def var lv-hedit as handle no-undo.
    lv-hedit = getwidhandle(frame {&frame-name}:handle,'btn-edit').
    if not valid-handle(lv-hedit) then return no-apply.
    if not lv-hedit:sensitive then return no-apply.
    run br-return-trigger in this-procedure no-error.
END.

ON VALUE-CHANGED OF {&browsename} IN FRAME {&frame-name}
DO:
  run br-changed-trigger in this-procedure no-error.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Browse-Row-Leave Include 
PROCEDURE Browse-Row-Leave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*    if 'ENABLED-FIELDS-IN-QUERY-{&browsename}' = ''                       */
/*       or not focus:modified                                              */
/*       or donotfire('btn-exit')                                           */
/*    then return.                                                          */
/*                                                                          */
/*    &if defined(table-name) ne 0 &then                                    */
/*       empty temp-table tb-{&table-name}.                                 */
/*       create tb-{&table-name}.                                           */
/*       buffer-copy t-{&table-name} to tb-{&table-name}.                   */
/*    &endif                                                                */
/*                                                                          */
/*    run local-assign-extratables no-error.                                */
/*    assign browse {&browsename} {&ENABLED-FIELDS-IN-QUERY-{&browsename}}  */
/*         no-error.                                                        */
/*    run br-changed-trigger in this-procedure no-error.                    */
/*    run save-record in this-procedure no-error.                           */
/*    If return-value = 'passed'                                            */
/*    Then apply key-function(lastkey).                                     */
/*    else do:                                                              */
/*      focus:modified = true.                                              */
/*      return 'failed'.                                                    */
/*    end.                                                                  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseOffEnd Include 
PROCEDURE BrowseOffEnd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-direction as char no-undo.
def input param pv-browse as handle no-undo.
def var pv-id like t-{&table-name}.{&unique-key} no-undo.
DEF VAR lv-row-{&browsename} AS ROWID  no-undo.

&if defined(usemoreimage) ne 0 &then
    if h-{&browsename}mored:hidden then return.
&endif
    if pv-direction = 'all' then do:
        run clear-tables in this-procedure no-error.
        pv-id = ?.
        lv-row-{&browsename} = ?.
    end.
    else do:    
        if pv-direction = 'up' 
            then GET first {&browsename} NO-LOCK.    
            else GET LAST {&browsename} NO-LOCK.
        
        CASE lv-key-{&browsename}:
            when 'end'  then do:
                run clear-tables in this-procedure no-error.
                pv-id = -1.
            end.
            when 'home'  then do:
                run clear-tables in this-procedure no-error.
                pv-id = ?.
            end.
            otherwise do:
               pv-id = t-{&table-name}.{&unique-key}.         
            end.
        end case.
        lv-row-{&browsename} = ROWID(t-{&table-name}).
    end.

    if "{&QryProcExtra}" = "" then do:
       &undefine QryProcExtra
    end.
    
    errorclear().
    {{&core}run.i &program   = "{&QryProg}"
                 &path      = "{&path}"
                 &Appsrv    = "System"
                 &procedure = "{&QryProc}"
                 {&QryProcExtra}  /* see payment-refund.w for example */
                 &params    = "({&extraparams}
                                pv-direction,        
                                pv-id,
                                output table t-{&table-name} append
                                {&extratables})"}
    &if defined(usemoreimage) ne 0 &then
    h-{&browsename}mored:hidden = return-value = ''. 
      &if defined(KeepRefreshButton) = 0 &THEN
       setsensitive(h-{&BROWSE-NAME}mored:visible,'inc','btn-query',frame {&frame-name}:handle).
      &endif
    &endif
    if anyerrors()
    then do:
        message msg(50,"Table","{&table-name}","","") view-as alert-box.
        return no-apply.
    end.

    if pv-direction = 'down' 
        then find last t-{&table-name} no-error. 
        else find first t-{&table-name} no-error.

    RUN BEFORE-OPEN-query IN THIS-PROCEDURE NO-ERROR.

&if defined(NoBrowseSorting) = 0 &then
   if pv-direction = 'all' then do:
        lv-scol = pv-browse:first-column.
        {&browsename}:private-data in frame {&frame-name} = lv-scol:name.
        {&OPEN-QUERY-{&browsename}}
/* put this back in if getall sorting problems */
/*         run proc-start-search ({&browsename}:handle).  */
   end.
   else do:
      {&OPEN-QUERY-{&browsename}}
   end.
&else
   {&OPEN-QUERY-{&browsename}}
&endif

    REPOSITION {&browsename} TO ROWID lv-row-{&browsename} no-error.
    CASE pv-direction:
      WHEN 'up' then do:
        GET prev {&browsename} NO-LOCK.
          IF KEYFUNCTION(LASTKEY) = "PAGE-UP"
            THEN REPOSITION {&browsename} Backwards pv-browse:Down - 2 no-error.
            ELSE REPOSITION {&browsename} Backwards 0 no-error.
      end.
      WHEN 'DOWN' THEN do:
        GET next {&browsename} NO-LOCK.
        IF KEYFUNCTION(LASTKEY) = "PAGE-DOWN"
            THEN REPOSITION {&browsename} FORWARDS pv-browse:Down - 2 no-error.
            ELSE REPOSITION {&browsename} FORWARDS 0 no-error.
      end.
      WHEN 'ALL' THEN DO:
        GET first {&browsename} NO-LOCK.
        IF KEYFUNCTION(LASTKEY) = "PAGE-DOWN"
            THEN REPOSITION {&browsename} FORWARDS pv-browse:Down - 2 no-error.
            ELSE REPOSITION {&browsename} FORWARDS 0 no-error.
        {&browsename}:select-row(1) in frame {&frame-name} no-error. 
      end.
    end case.

    run br-changed-trigger IN THIS-PROCEDURE NO-ERROR.

    lv-key-{&browsename} = ''.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

