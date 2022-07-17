&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
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
/* DO I NEED THESE ??? no */
def var v-widnames   as char   no-undo.  
def var v-save-rec   as recid  no-undo.
def var lv-widlist   as char   no-undo.
def var lv-queryrun  as log    no-undo init no.
def var lv-NewMode   as log    no-undo.
def var lv-Editmode  as log    no-undo. 
def var lv-progmode  as char   no-undo.
def var lv-scrmode   as char   no-undo.
def var lv-lastfocus as handle no-undo.
def var lv-scol      as handle no-undo.
def var lv-printtable as handle no-undo.
def var lv-parent as handle no-undo.
def var lv-exitafterprint as log.
def var lv-currentframe as handle no-undo.
def var lv-currentwid as handle no-undo.

lv-exitafterprint = 
        &if defined(exitAfterPrint) ne 0 and
            defined(ReportReturnTable) = 0
            &then true.
            &else false.
        &endif

&if defined(table-name) ne 0 &then
    def var lv-CurrentTableid as dec no-undo.
    define temp-table ts-{&table-name} no-undo like t-{&table-name}.
    define temp-table tb-{&table-name} no-undo like t-{&table-name}.
&endif

/*
def var descendingbrowse as log no-undo.
*/

&if defined(tabs) ne 0 &then
    &glob tabs true
    def var h-tab as com-handle no-undo.
&endif

/* default in the query,save and delete api's */
&if defined(QryProg) = 0 &then
    &glob QryProg {&table-name}.p
&endif
&if defined(QryProc) = 0 &then
    &glob QryProc get-records
&endif
&if defined(DelProg) = 0 &then
    &glob DelProg {&table-name}.p
&endif
&if defined(DelProc) = 0 &then
    &glob DelProc Delete-Record
&endif
&if defined(SaveProg) = 0 &then
    &glob SaveProg {&table-name}.p
&endif
&if defined(SaveProc) = 0 &then
    &glob SaveProc Save-Record
&endif
&if defined(AuditTable) = 0 &then
    &glob AuditTable {&table-name}
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClearScreen Include 
FUNCTION ClearScreen returns logical
  ( lv-wid-handle as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EnabledBgColour Include 
FUNCTION EnabledBgColour returns logical
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GotAll Include 
FUNCTION GotAll returns logical
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetFocusTo Include 
FUNCTION SetFocusTo returns logical
  ( lv-hand  as handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetInitColumn Include 
FUNCTION SetInitColumn returns logical
  ( pv-bhand as handle)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ShowFrame Include 
FUNCTION ShowFrame returns logical
  (pv-num as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SortBrowse Include 
FUNCTION SortBrowse returns logical
  (pv-browse as handle,
   pv-query  as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
         HEIGHT             = 23.38
         WIDTH              = 50.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

{{&core}pgm-hdr.i}

&if defined(tabs) ne 0 &then 
    procedure CtrlFrame.TabStrip.Click :
        run local-tabchoose in this-procedure no-error.
        ShowFrame(string(h-tab:SelectedItem:Index)).
    end procedure.
&endif

&if defined(table-name) ne 0 &then
    &if (defined(browse-name) ne 0 
        and defined(nobrowse) eq 0) &then
        {{&core}appsrvbrowse.i &path         = {&path}
                               &browsename   = {&BROWSE-name}
                               &extraparams  = " {&extraparams}"
                               &extratables  = " {&extratables}"
                               &QryProcExtra = {&qryProcExtra}}
        {{&core}browse-sort.i &browsename  = {&BROWSE-name}}
    &endif
&endif

on close of this-procedure do:
    if not lv-exited then do:
        run exit-trigger in this-procedure no-error.
        if return-value = 'notabandon' then return no-apply.
    end.
    run disable_ui in this-procedure no-error.            
end.

on 'leave' anywhere do:
if can-set(self,'screen-value') then lv-currentwid = self.
end.

&if defined(window-name) ne 0 &THEN
   on WINDOW-CLOSE of {&WINDOW-NAME} do:
       if not lv-exited then do:
           run exit-trigger in this-procedure no-error.
           if return-value = 'notabandon' then return no-apply.
       end.
   end.
   
   on endkey, END-ERROR of {&WINDOW-NAME} anywhere do:
       if not lv-exited then do:
           run exit-trigger in this-procedure no-error.
           if return-value = 'notabandon' then return no-apply.
       end.
   end.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignFrames Include 
PROCEDURE AssignFrames :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
&if defined(tabs) = 0 &then
    if lv-Newmode 
        then &if Defined(add-list) ne 0 &then assign frame {&frame-name} {&add-list} {&disp-list} no-error &endif .
    else &if Defined(edit-list) ne 0 &then assign frame {&frame-name} {&edit-list} {&disp-list} NO-ERROR &endif . 
&else
   assign
        &if Defined(list-0) ne 0 &then frame {&frame-name} {&list-0} {&disp-list} &endif
        &if Defined(list-1) ne 0 &then frame one   {&list-1} &endif
        &if Defined(list-2) ne 0 &then frame two   {&list-2} &endif
        &if Defined(list-3) ne 0 &then frame three {&list-3} &endif 
        &if Defined(list-4) ne 0 &then frame four  {&list-4} &endif 
        &if Defined(list-5) ne 0 &then frame five  {&list-5} &endif       
        &if Defined(list-6) ne 0 &then frame six   {&list-6} &endif
        no-error.
&endif
if error-status:error 
then do:
   message error-status:get-message(error-status:num-messages)
   view-as alert-box error title 'Cannot Save Changes'.
   return string(getwidhandle(frame {&frame-name}:handle,'*character')).
end.
else return 'passed'.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Audit-Trigger Include 
PROCEDURE Audit-Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
  run local-audit-trigger in this-procedure no-error.

  if not error-status:error and
         return-value = 'override' then return.

&if defined(table-name) ne 0 &then
  if not avail t-{&table-name} then return.

  runchild("{&aud}singleaudview.w",this-procedure).
&endif.
       
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankFields Include 
PROCEDURE BlankFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-frame as handle no-undo.
{{&core}bug.i}
def var lv-wid as handle no-undo.
assign lv-wid = pv-frame:first-child
       lv-wid = lv-wid:next-sibling.

do while valid-handle(lv-wid):
    if lv-wid:type = 'frame' 
        then run BlankFields in this-procedure (lv-wid).
    if can-set(lv-wid,'screen-value') then lv-wid:screen-value = ''.
    lv-wid = lv-wid:next-sibling.
end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Br-Changed-Trigger Include 
PROCEDURE Br-Changed-Trigger :
{{&core}bug.i "'brtrigger begin'"}
/*    message "in br-changed-trigger" skip      */
/*       "Program-name(1)" program-name(1) skip */
/*       "Program-name(2)" program-name(2) skip */
/*       "Program-name(3)" program-name(3) skip */
/*       view-as alert-box info buttons OK.     */


   run local-br-changed-trigger in this-procedure no-error.
   if not error-status:error and return-value = 'override' 
      then return.

   run Clear-related-tables in this-procedure no-error.

   run pop-related-tables   in this-procedure no-error.

   run Br-U1-Trigger        in this-procedure no-error.

   publish 'ChildBrowseChanged'.

    run local-after-br-changed-trigger in this-procedure no-error.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Br-Msdblclick-Trigger Include 
PROCEDURE Br-Msdblclick-Trigger :
{{&core}bug.i}

if last-event:row < 2 then return.

run local-br-msdblclick-trigger in this-procedure no-error.

if not error-status:error and
     return-value = 'override' 
then return.

run br-changed-trigger in this-procedure no-error. 
    
run edit-trigger in this-procedure no-error.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Br-Return-Trigger Include 
PROCEDURE Br-Return-Trigger :
{{&core}bug.i}
run local-br-return-trigger in this-procedure no-error.

if not error-status:error and
    return-value = 'override' 
then return.

run edit-trigger in this-procedure no-error.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Br-U1-Trigger Include 
PROCEDURE Br-U1-Trigger :
{{&core}bug.i}
run local-br-u1-trigger in this-procedure no-error.
   
   if not error-status:error and return-value = 'override' 
      then return.
  
   run Display-Fields in this-procedure no-error.
   &if defined(table-name) ne 0 &then
       setsensitive(AVAILABLE(t-{&table-name}),'inc','btn-audit',frame {&frame-name}:handle).

       if AVAILABLE(t-{&table-name}) then do: 
         setsensitive(lv-editmode,'inc','btn-save',frame {&frame-name}:handle).

       end.
   else &endif do:
      setsensitive(false,'inc','btn-edit,btn-delete',frame {&frame-name}:handle).

      SetNotModified(frame {&frame-name}:handle).

      assign    
         lv-EditMode              = false
         lv-NewMode               = false.
   end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChildBrowseChanged Include 
PROCEDURE ChildBrowseChanged :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
publish 'ParentSendRefresh' (this-procedure).
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChildExit Include 
PROCEDURE ChildExit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
   run Exit-trigger.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChildHide Include 
PROCEDURE ChildHide :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
&if defined(suppresswindow) ne 0 
&then frame {&frame-name}:hidden = true.
&else {&window-name}:hidden = true.
&endif

publish 'childhide'.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChildTabChoose Include 
PROCEDURE ChildTabChoose :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-in as handle no-undo.
def input param pv-view as log no-undo.
{{&core}bug.i}

if this-procedure = pv-in or pv-view
then do:

   &IF defined(about) = 0 &THEN 
   /* better to use string(this-procedure) */
   current-window:private-data = string(this-procedure).
   /* this-procedure:file-name. */
   &endif    
   frame {&frame-name}:hidden = false.
 /* get the new system and practice here */   
     run local-set-sysvars   in this-procedure  no-error.

   &if defined(table-name) ne 0 &then   
      if lv-CurrentTableid ne 0 then find t-{&table-name} where 
         t-{&table-name}.{&table-name}tableid = lv-CurrentTableid no-error.
   &endif
     
   run local-childview in this-procedure no-error. 
   
   publish 'childview'.
      /* fix for eriks problem */
   publish 'childtabchoose' (?,yes).

   if valid-handle(lv-currentwid) 
    then apply "entry" to lv-currentwid. 

end.
else do:
   &if defined(table-name) ne 0 &then
   
   if avail t-{&table-name} then
      lv-CurrentTableid = t-{&table-name}.{&table-name}tableid.
   &endif
/*       message */
/*            '# ' focus:name focus:frame:name skip */
/*            '# ' self:name  self:frame:name skip */
/*            '# ' frame-field */
/*            {&dbt}. */

   frame {&frame-name}:hidden = true.
   run local-childhide in this-procedure no-error. 
   publish 'childhide'.
      /* fix for eriks problem */
   publish 'childtabchoose' (?,no).
end.

 

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChildView Include 
PROCEDURE ChildView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {{&core}bug.i}
    &if defined(suppresswindow) ne 0 &then 
        frame {&frame-name}:hidden = false.
    
 /*    &if defined(table-name) ne 0 &then */
/*        if lv-CurrentTableid ne 0 then find t-{&table-name} where */
/*            t-{&table-name}.{&table-name}tableid = lv-CurrentTableid no-error. */
/*     &endif */
    &else 
       {&window-name}:hidden = false.
    &endif
    
    run local-childview in this-procedure no-error.
    publish 'childview'.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Clear-Tables Include 
PROCEDURE Clear-Tables :
{{&core}bug.i}
run local-clear-tables in this-procedure no-error.
    if not error-status:error and
         return-value = 'override' 
    then return.

&if defined(table-name) ne 0 &then
    empty temp-table t-{&table-name}.
    run Clear-related-tables in this-procedure no-error.
&endif
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crystal-Trigger Include 
PROCEDURE Crystal-Trigger :
{{&core}bug.i}
def var lv-dpgmparams as char no-undo.
def var lv-params as char no-undo.
def var lv-paramtitles as char no-undo.
def var lv-h as handle no-undo.
def var lv-currjobs as int no-undo.
def var lv-maxjobs as int no-undo.
if lv-asyncreports then do:
    lv-maxjobs = max(int(GetCtrl('{&AsyncMaxJobs}')),0).
    lv-currjobs = max(int(GetCtrl('{&AsyncCurrentJobs}')),0).
    if (lv-currjobs + 1 > lv-maxjobs)
    then do:
        message msg(55,this-procedure:filename,'','','').
        return 'cancelled'.
    end.
end.


  run local-crystal-trigger in this-procedure no-error.

  if not error-status:error and
         return-value = 'override' then return.
   else do:
      if not error-status:error then
         if return-value ne '' then do:
            lv-params = entry(1,return-value,'{&combodelim}').
            lv-paramtitles = if num-entries(return-value,'{&combodelim}') > 1
                              then entry(2,return-value,'{&combodelim}')
                              else ''.
            lv-dpgmparams = if num-entries(return-value,'{&combodelim}') > 2
                              then entry(3,return-value,'{&combodelim}')
                              else ''.
         end.

&if defined(crystaldebug) ne 0 &then
   message this-procedure:filename skip
           lv-params skip
           lv-paramtitles skip
           lv-dpgmparams skip
           lv-asyncreports
view-as alert-box title 'Crystal report Info'.
&endif

   run {&core}{&rep}submitcrystal.p persist set lv-h.
    
    if valid-handle(lv-h) 
    then do:
           run runjob in lv-h (lv-asyncreports,
                               this-procedure,
                               lv-params,
                               lv-paramtitles,
                               lv-dpgmparams).
           if return-value = 'cancelled' 
           then return 'cancelled'.
    end.
    else do:
        message msg(21,'Program ','submitcrystal.p','','')
        view-as alert-box.                                
        return 'cancelled'.                                
    end.                                         

      run local-after-crystal-trigger in this-procedure no-error.
      if not error-status:error and
         return-value = 'override' then return.  

      if lv-exitAfterPrint or lv-asyncreports and return-value ne 'failed'
      then run exit-trigger.
     
   end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Defaults-Trigger Include 
PROCEDURE Defaults-Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
/* have to do this as a run not a function call 
as we block for input and thats not allowed in a function */
if valid-handle(lh-general) 
then run call-SetWidgetDefaults in lh-general (this-procedure). /* in general library*/   
else run call-SetWidgetDefaults (this-procedure). /* in general library*/
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete-Record Include 
PROCEDURE Delete-Record :
{{&core}bug.i}
def var choice    as log   no-undo init true.
    def var lv-i      as int   no-undo.
    
    run local-delete-record in this-procedure no-error.
    if not error-status:error and
         return-value = 'override' then return.
 
    run delete-validate in this-procedure no-error.

    if return-value ne "passed" then 
        do:
/*         Message msg(8,Return-value,'','','')  */
/*             View-as Alert-box Information.    */
        return.
    end.
        
    message 'Delete this record?'    
/* msg(6,'Record','','','') */
        view-as alert-box question buttons yes-no
        title "{&title-text}" update Choice .
  
    if choice then do:
       run before-delete-related-tables in this-procedure no-error.
       &if (defined(browse-name) ne 0 
            and defined(nobrowse) eq 0) &then
          lv-i = {&browse-name}:FOCUSED-ROW in frame {&frame-name}.
       &endif
       &if defined(table-name) ne 0 &then
           /* delete related tables here 
              Be carefull NO Backout is possible !!!! */
           run delete-related-tables in this-procedure no-error.
           errorclear().
/*            {{&core}run.i &api    = "delete"                            */
/*                          &params = "(t-{&table-name}.{&Unique-key})"}  */
           {{&core}run.i &program   = "{&DelProg}"
                        &path      = "{&path}"
                        &Appsrv    = "System"
                        &procedure = "{&DelProc}"
                        &params    = "(t-{&table-name}.{&Unique-key})"}
           if anyerrors() then return error.

           delete t-{&table-name} no-error.
       &endif
       run post-delete in this-procedure no-error.

       &if (defined(browse-name) ne 0 
            and defined(nobrowse) eq 0) &then
            get prev {&browse-name}.
            &if defined(table-name) ne 0 &then
                if not avail t-{&table-name}
                then get first {&browse-name}.

                if avail t-{&table-name} then
                  reposition {&browse-name} to rowid rowid(t-{&table-name}).
                else clearscreen(frame {&frame-name}:handle).

                setsensitive(avail t-{&table-name},'inc','btn-audit,btn-delete,btn-edit,btn-print,btn-notes,btn-export',frame {&frame-name}:handle).    
            &else setsensitive(true,'inc','btn-delete,btn-edit,btn-print,btn-notes',frame {&frame-name}:handle).    
            &endif

            {&browse-name}:refresh().   
            {&browse-name}:SET-REPOSITIONED-ROW(lv-i,"ALWAYS").
       &endif
    end.
    else return.

    setsensitive(false,'inc','btn-save',frame {&frame-name}:handle).
    SetNotModified(frame {&frame-name}:handle).
    run Br-Changed-Trigger.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete-Trigger Include 
PROCEDURE Delete-Trigger :
{{&core}bug.i}
if valid-handle(focus) 
then apply "LEAVE" to focus.

run local-delete-trigger in this-procedure no-error.

if not error-status:error and
       return-value = 'override' 
then return.

run Delete-record in this-procedure no-error.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Input Include 
PROCEDURE Disable-Input :
{{&core}bug.i "'disable-input, before set-sen'"}

run set-sen (false).

run local-disable in this-procedure no-error.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Display-Fields Include 
PROCEDURE Display-Fields :
{{&core}bug.i}
if not (lv-Newmode or 
        lv-editmode) 
then run Disable-Input in this-procedure no-error. 

&if defined(table-name) ne 0 &then
    if not AVAIL(t-{&table-name}) then do:
        {{&core}bug.i "'display-fields, before blankFields'"}
        run BlankFields in this-procedure (frame {&frame-name}:handle) no-error.
        {{&core}bug.i "'display-fields, after blankFields'"}       
        return.
    end.
&endif

&if defined(tabs) = 0 
    &then
        &if Defined(add-list) ne 0 &then disp {&add-list} {&disp-list} with frame {&frame-name}. &endif
    &else
        &if Defined(list-0) ne 0 &then disp {&list-0} {&disp-list} with frame {&frame-name}.   &endif
        &if Defined(list-1) ne 0 &then disp {&list-1} with frame one.   &endif
        &if Defined(list-2) ne 0 &then disp {&list-2} with frame two.   &endif
        &if Defined(list-3) ne 0 &then disp {&list-3} with frame three. &endif
        &if Defined(list-4) ne 0 &then disp {&list-4} with frame four.  &endif
        &if Defined(list-5) ne 0 &then disp {&list-5} with frame five.  &endif
        &if Defined(list-6) ne 0 &then disp {&list-6} with frame six.   &endif  
    &endif
 
run Display-External-tables in this-procedure no-error.

run disp-wids in this-procedure no-error.

/* &if defined(tabs) ne 0 &then                                                           */
/*     case int(h-tab:SelectedItem:INDEX):                                                */
/*         &if Defined(list-1) ne 0 &then when 1 then frame one:hidden    = false. &endif */
/*         &if Defined(list-2) ne 0 &then when 2 then frame two:hidden    = false. &endif */
/*         &if Defined(list-3) ne 0 &then when 3 then frame three:hidden  = false. &endif */
/*         &if Defined(list-4) ne 0 &then when 4 then frame four:hidden   = false. &endif */
/*         &if Defined(list-5) ne 0 &then when 5 then frame five:hidden   = false. &endif */
/*         &if Defined(list-6) ne 0 &then when 6 then frame six:hidden    = false. &endif */
/*     end case.                                                                          */
/* &endif                                                                                 */


/* &if defined(table-name) ne 0 &then       */
/*     v-save-rec = recid(t-{&table-name})  */
/* &endif                                   */
/*     lv-editmode    = false.  */
SetNotModified(frame {&frame-name}:handle).
/* FreezeWindow(current-window,0).  /* in zenlibrary */  */
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Edit-Trigger Include 
PROCEDURE Edit-Trigger :
{{&core}bug.i}
if valid-handle(focus) 
then apply "LEAVE" to focus.
  
lv-editmode = true. 
    
run local-edit-trigger in this-procedure no-error.

if not error-status:error and
     return-value = 'override' 
then do:
   lv-editmode = false.
   return.
end.

&if defined(table-name) ne 0 &then
    empty temp-table tb-{&table-name}.
    create tb-{&table-name}.
    buffer-copy t-{&table-name} to tb-{&table-name}.
&endif


run Enable-Input in this-procedure no-error.

run local-after-enable in this-procedure no-error.
if not error-status:error and
     return-value = 'override' 
then return.

publish 'ChildBrowseChanged'.

{{&core}wid-chk.i}
/*    &if defined(tabs) ne 0 &then
       RUN CONTROL_load.
       {{&core}internat-ocx.i}
       showframe("1").
    &endif  */
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Input Include 
PROCEDURE Enable-Input :
{{&core}bug.i}
run set-sen (true).

run local-enable in this-procedure no-error.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Exit-Trigger Include 
PROCEDURE Exit-Trigger :
{{&core}bug.i}
def var v-ok as log no-undo init true.

   lv-exited = true.
   run local-exit-trigger in this-procedure no-error.
   
   if (not error-status:error) and return-value = 'override' then do:
      lv-exited = no.
      return.
   end.
   
   &if defined(NoChangedCheck) eq 0 &then
       if frameChanged(frame {&frame-name}:handle) and
       (lv-newmode or lv-editmode) then do:   
           v-ok = false.
           message msg(9,"","","","") /* zenlibrary */
               view-as alert-box question buttons yes-no
               title '{&title-text}' update v-ok.
       end.
       else v-ok = yes.
   &endif

   if v-ok then do:
       if lv-Newmode or lv-editmode 
           then run undo-trigger in this-procedure no-error.
      run ExitCheck in this-procedure no-error.
    end.
    else do:
      SetFocusTo(lv-lastfocus).
      return 'notabandon'.
    end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExitCheck Include 
PROCEDURE ExitCheck :
{{&core}bug.i}


   &if defined(NoExitCheck) eq 0 &then
   if getctrl("{&exitcheck}") = 'yes' then do:
       message msg(34,'Exit?','','','')
       view-as alert-box question buttons yes-no
           update lv-exited.
       if not lv-exited then do:
         SetFocusTo(lv-lastfocus).
         return.
       end.
   end.
   &endif
   
   run local-exitcheck in this-procedure no-error.

   publish 'ChildExit'.
   publish 'ParentChildReturn' (string(this-procedure)).

   unsubscribe to all.
   


   if valid-handle(focus) then LogAction(program-name(1),focus:name,'Exit').


   if frame {&frame-name}:type ne 'dialog-box' 
      then apply "CLOSE":U to this-procedure.
   else apply 'go' to frame {&frame-name}.
     
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Export-Trigger Include 
PROCEDURE Export-Trigger :
{{&core}bug.i}
/* for example run exportbrowse({&browse-NAME}:query). 
to extract all data in tables */
  def var lv-id as dec no-undo.
  def var lv-ok as log no-undo.
  
  run local-Export-Trigger in this-procedure no-error.


  if not error-status:error and
         return-value = 'override' then return.
  /* just extract columns in browse */
&if
   defined(browse-name) ne 0 and
   defined(noBrowse)    =  0 and
   defined(table-name)  ne 0
&then
   lv-id = t-{&table-name}.{&Unique-key}.
    if not gotall() 
    then do:
        message 'Only Some Records are available!' skip
                'Do You want Get All Before Exporting?'
                view-as alert-box question buttons yes-no update lv-ok.
        if lv-ok 
        then run browseoffend in this-procedure ('all',{&browse-name}:handle in frame {&frame-name})
              no-error.
    end.
    /* in generallibrary */
   exportbrowse({&browse-NAME}:handle in frame {&frame-name}). 

   find t-{&table-name} where t-{&table-name}.{&Unique-key} = lv-id no-error.

   {&browse-name}:REFRESH() no-error.
   run br-changed-trigger in this-procedure no-error.
&endif
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCalledFrom Include 
PROCEDURE GetCalledFrom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param pv-from as char no-undo.
{{&core}bug.i}
def var lv-from as char  no-undo.
def var lv-wid as handle no-undo.

run SendRanFrom in lv-parent (this-procedure,output lv-from) no-error.
   
if trim(lv-from) ne ''
then do:
   do while num-entries(lv-from,'{&delim4}') < 2:
      lv-from = lv-from + '{&delim4}'.
   end.
   if entry(2,lv-from,'{&delim4}') = 'menu-item'
   then pv-from = '{&delim4}' + lv-from.
   else do: 
      lv-wid = widget-handle(entry(1,lv-from,'{&delim4}')) no-error.
      if valid-handle(lv-wid) 
      then pv-from = '{&delim4}' + entry(1,lv-from,'{&delim4}') + '{&delim4}' + lv-wid:type.
      else pv-from = '{&delim4}' + lv-from.
   end.
end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Help-trigger Include 
PROCEDURE Help-trigger :
{{&core}bug.i}
/* if valid-handle(focus) then APPLY "LEAVE" TO FOCUS. */

run local-help-trigger in this-procedure no-error.
   if not error-status:error and return-value = 'override' then return.
btnhelp(this-procedure,yes). /* zenlibrary.p */
   if valid-handle(lv-lastFocus) then apply "entry" to lv-lastFocus.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialise Include 
PROCEDURE Initialise :
def var lv-buttonparams as char no-undo. 
{{&core}bug.i "'initialise begin'"}
      lv-logfile  = GetLogFilename(this-procedure).

    publish 'ParentSendHandle' (this-procedure).

    &if defined(UseTranslations) ne 0 &then
        &if (defined(browse-name) ne 0 
            and defined(nobrowse) eq 0) &then
            {{&core}internat-browse.i}
        &endif

        &if defined(tabs) ne 0 &then
            {{&core}internat-ocx.i}
        &endif
    &endif
    
/*     &if (defined(browse-name) ne 0 */
/*         and defined(nobrowse) eq 0) &then */
/*         DescendingBrowse  = index("{&QUERY-STRING-{&browse-name}}",' descending') > 0 . */
/*     &endif */

   /* in zenlibrary.p */
        {{&core}bug.i "'makelookupbuttons begin'"}
    makelookupbuttons(this-procedure,frame {&frame-name}:handle).
        {{&core}bug.i "'makelookupbuttons end'"}
   /*     enabledbgcolour().  */
/*     publish 'ParentSendRefresh' (this-procedure). */
 

   &if DEFINED(NoButtons) = 0 &then
      &if defined(UseDBButtons) ne 0 &then
        lv-buttonparams = PgmProperty(this-procedure:file-name,'defbuttons').
      &endif
        if lv-buttonparams = '' then lv-buttonparams = "{&defbutlist}".


        lv-buttonparams = lv-buttonparams +
                          string(this-procedure) + "," +
                          string({&window-name}:handle) + "," +
                          string(frame {&frame-name}:handle) + "," + 
                          "{&btnhorizontal},{&btnflat},{&btnstartcol},{&btnstartrow},{&btnheight},{&btnwidth},{&btncenter}".                       
        if not PgmUseDefaults(this-procedure) 
         then lv-buttonparams = replace(lv-buttonparams,"defaults^","^").
        if getfieldwhere('zen-auditdetail','tablename = "{&table-name}"','tablename') = '' 
         then lv-buttonparams = replace(lv-buttonparams,"audit^","^").
        if not stringtolog(pgmproperty(this-procedure:file-name,'usecrystal')) /* zenlibrary.p */
         then lv-buttonparams = replace(lv-buttonparams,"crystal^","^").

        &if defined(NoCrystal)  ne 0 &then lv-buttonparams = replace(lv-buttonparams,"crystal^","^"). &endif
        &if defined(NoDefaults) ne 0 &then lv-buttonparams = replace(lv-buttonparams,"defaults^","^"). &endif  
        &if defined(NoNew)      ne 0 &then lv-buttonparams = replace(lv-buttonparams,"new^","^").    &endif 
        &if defined(NoSaveNew)  ne 0 &then lv-buttonparams = replace(lv-buttonparams,"savenew^","^").    &endif 
        &if defined(NoEdit)     ne 0 &then lv-buttonparams = replace(lv-buttonparams,"edit^","^").   &endif
        &if defined(NoSave)     ne 0 &then lv-buttonparams = replace(lv-buttonparams,"save^","^").   &endif
        &if defined(NoDelete)   ne 0 &then lv-buttonparams = replace(lv-buttonparams,"delete^","^"). &endif
        &if defined(NoExport)   ne 0 &then lv-buttonparams = replace(lv-buttonparams,"Export^","^"). &endif
        &if defined(NoUndo)     ne 0 &then lv-buttonparams = replace(lv-buttonparams,"undo^","^").   &endif
        &if defined(NoQuery)    ne 0 &then lv-buttonparams = replace(lv-buttonparams,"query^","^").  &endif
        &if defined(NoAudit)    ne 0 &then lv-buttonparams = replace(lv-buttonparams,"audit^","^").  &endif
        &if defined(NoExit)     ne 0 &then lv-buttonparams = replace(lv-buttonparams,"exit^","^").   &endif
        &if defined(NoHelp)     ne 0 &then lv-buttonparams = replace(lv-buttonparams,"help^","^").   &endif
        &if defined(NoPrint)    ne 0 &then lv-buttonparams = replace(lv-buttonparams,"print^","^").  &endif  

        {{&core}bug.i "'buttons begin'"}
        CreateButs(lv-buttonparams).  /* zenlibrary.p */  
        {{&core}bug.i "'buttons end'"}
    &endif
    {{&core}bug.i "'parentsendrefresh begin'"}
    publish 'ParentSendRefresh' (this-procedure). 
    {{&core}bug.i "'parentsendrefresh end localinit begin'"}
    run local-initialise in this-procedure no-error.
    {{&core}bug.i "'localinit end'"}
    if not error-status:error and
         return-value = 'override' 
    then do:
        {{&core}bug.i "'setnotmodified begin'"}
        SetNotModified(frame {&frame-name}:handle).
        {{&core}bug.i "'setnotmodified end'"}
        return.
    end.

&if defined(ChildProgram) = 0 &then
/* if not lv-queryrun then do:  */
   &if DEFINED(NoImmediateQuery) = 0 &then
      run openquery in this-procedure no-error.
   &else
      &if defined(ImmediateQuery) ne 0 &then
         run openquery in this-procedure no-error.
      &else 
         if PgmUseDefaults(this-procedure) then do:
            LoadFieldDefaults(this-procedure,frame {&frame-name}:handle). /* zenlibrary.p */
         end.
      &endif
   &endif
    
&else
    &if defined(ImmediateQuery) = 0 &then
       if PgmUseDefaults(this-procedure) then
          LoadFieldDefaults(this-procedure,frame {&frame-name}:handle).
    &endif
   run Disable-Input in this-procedure no-error.
   &if defined(table-name) ne 0 &then
      setsensitive(avail t-{&table-name},'inc','btn-audit,btn-delete,btn-edit,btn-print,btn-notes,btn-export',frame {&frame-name}:handle).    
   &else 
      setsensitive(true,'inc','btn-delete,btn-edit,btn-print,btn-notes',frame {&frame-name}:handle).    
   &endif
&endif

&IF defined(justLoadDefaults) &THEN 
   if not PgmUseDefaults(this-procedure) /* which means we already did this */ then
      LoadFieldDefaults(this-procedure,frame {&frame-name}:handle).
&ENDIF

   &if defined(tabs) ne 0 &then 
        ShowFrame('1').
    &endif

    run local-afterinitialise in this-procedure no-error.
/*         run br-changed-trigger in this-procedure no-error.  */

    publish 'ParentSetTabInfo' (this-procedure).
    
    SetNotModified(frame {&frame-name}:handle).
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE New-Trigger Include 
PROCEDURE New-Trigger :
{{&core}bug.i}
if valid-handle(focus) then
      apply "LEAVE" to focus.
assign
    lv-editmode = true  
    lv-Newmode  = true. 

run local-new-trigger in this-procedure no-error.

if not error-status:error and
     return-value = 'override' then return.

&if defined(NoChangedCheck) eq 0 &then
if frameChanged(frame {&frame-name}:handle) 
then do:
  message msg(9,'Record','saved','Save Now ?','')
  view-as alert-box
           question buttons yes-no
           title '{&title-text}'
           update Choice as log.
  if Choice then run Save-RECORD in this-procedure no-error.
            else do:
              run display-fields in this-procedure no-error.
              return no-apply.
            end.
end.    
&endif

&if defined(table-name) ne 0 &then
    create t-{&table-name}.
    create tb-{&table-name}.
&endif

run CreateExtraFields in this-procedure no-error.

run create-extra-tables in this-procedure no-error.

run clear-related-tables in this-procedure no-error.

&if defined(tabs) ne 0 &then
    run CONTROL_load.
    {{&core}internat-ocx.i}
    ShowFrame("1").
&endif

run Enable-Input in this-procedure no-error.

run local-after-enable in this-procedure no-error. /* amd, 06/21/05 */

run DisplayLists in this-procedure no-error.
run display-fields in this-procedure no-error.

if PgmUseDefaults(this-procedure) 
then LoadFieldDefaults(this-procedure,frame {&frame-name}:handle). /* zenlibrary.p */
   
&IF defined(justLoadDefaults) &THEN 
   if not PgmUseDefaults(this-procedure) /* which means we already did this */ 
   then LoadFieldDefaults(this-procedure,frame {&frame-name}:handle).
&ENDIF
run local-afterloaddefaults in this-procedure no-error.
if return-value = "override" then return.

{{&core}wid-chk.i}.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery Include 
PROCEDURE OpenQuery :
{{&core}bug.i "'openquery begin'"}

run local-openquery in this-procedure no-error.
if not error-status:error and
     return-value = 'override' then return.
     
&if (defined(browse-name) ne 0 
     and defined(nobrowse) eq 0) &then
    def var v-numrows as int no-undo.
    v-numrows = {&browse-name}:focused-row in frame {&frame-name}.
    if v-numrows = ? then v-numrows = 0.    
&endif

run Clear-tables in this-procedure no-error.

&if defined(table-name) ne 0 &then
/*         {{&core}run.i &api    = "getrecords"                                              */
/*                       &params = "({&extraparams}'down',?,output table t-{&table-name})"}  */

errorclear().  
   {{&core}run.i &program   = "{&QryProg}"
                 &path      = "{&path}"
                 &Appsrv    = "System"
                 &procedure = "{&QryProc}"
                 {&QryProcExtra}  /* see payment-refund.w for example */
                 &params    = "({&extraparams}'down',?,output table t-{&table-name}{&extratables})"}  
                 
   &if defined(browse-name) ne 0 
       and defined(nobrowse) eq 0 
       and defined(usemoreimage) ne 0 &then
       if valid-handle(h-{&BROWSE-NAME}mored) 
           then h-{&BROWSE-NAME}mored:hidden = return-value = ''.
      &if defined(KeepRefreshButton) = 0 
         &THEN setsensitive(h-{&BROWSE-NAME}mored:visible,'inc','btn-query',frame {&frame-name}:handle).
         &else setsensitive(true,'inc','btn-query',frame {&frame-name}:handle).
      &endif
   &endif
   if anyerrors() then return error.  
&endif

run BEFORE-OPEN-query in this-procedure no-error.

&if (defined(browse-name) ne 0 
     and defined(nobrowse) eq 0) &then

    {&OPEN-QUERY-{&BROWSE-NAME}}

    run ResetBrowseSort in this-procedure ({&browse-name}:handle) no-error.  
    if num-results("{&browse-name}") > 0 then do:
        if v-numrows > num-results("{&browse-name}") 
            then {&browse-name}:select-row({&browse-name}:num-iterations in frame {&frame-name}).
        else {&browse-name}:select-row(1) no-error.
/*         {&browse-name}:fetch-selected-row(1).  */
    &if defined(table-name) ne 0 &then
        find first t-{&table-name} no-lock no-error.
    &endif
    end.
    run br-changed-trigger in this-procedure no-error. 

&else 
    &if defined(table-name) ne 0 &then
        find first t-{&table-name} no-lock no-error.
    &endif

    run local-open-query in this-procedure no-error.

    run display-fields in this-procedure no-error.
&endif

lv-queryrun = true.

&if defined(NotFoundMessage) ne 0 &then
    &if defined(table-name) ne 0 &then
        if not can-find(first t-{&table-name})
        then message 'No records found.'
         view-as alert-box.
    &endif
&endif

run Clear in this-procedure no-error.  

run Disable-Input in this-procedure no-error.

&if defined(table-name) ne 0 &then
    setsensitive(avail t-{&table-name},'inc','btn-audit,btn-delete,btn-edit,btn-print,btn-notes,btn-export',frame {&frame-name}:handle).    
&else setsensitive(true,'inc','btn-delete,btn-edit,btn-print,btn-notes',frame {&frame-name}:handle).    
&endif

/* escheleen - 05/14/09 - need to hide some of the buttons that were just
   made sensitive */
run local-after-openQuery in this-procedure no-error.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ParentChildReturn Include 
PROCEDURE ParentChildReturn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
def input param pv-from as char no-undo.
frame {&frame-name}:handle:move-to-top() no-error.
frame {&frame-name}:sensitive = true.

/* current-window:title = gethdr(this-procedure).  */

current-window:private-data = string(this-procedure) /* this-procedure:file-name */.

run local-childreturn in this-procedure (pv-from) no-error.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ParentSendHandle Include 
PROCEDURE ParentSendHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param pv-to as handle no-undo.
{{&core}bug.i}
run SetChildsParent in pv-to (this-procedure).

/* now make parent insensitive */
def var lvarpgmname as char no-undo.

if stringtolog(GetCtrl('{&DisableParents}')) = true 
then do:
    lvarpgmname = unixpath(this-procedure:name).
    lvarpgmname = entry(num-entries(lvarpgmname,'/'),lvarpgmname,'/').

    if lvarpgmname ne '{&mainmenu}'
    then frame {&frame-name}:sensitive = false. 
    else do:
    
    end.
end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ParentSendRefresh Include 
PROCEDURE ParentSendRefresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-to as handle no-undo.
{{&core}bug.i}
run Update-Child-Procedures in this-procedure (pv-to).

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Print-Trigger Include 
PROCEDURE Print-Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
def var lv-params as longchar no-undo.
def var lv-repinfo as char no-undo.
def var lv-dlgparam as char no-undo init 'ShowPrtDlg=true,SetSessionPrinter=True'. /* copies=5,orientation=landscape etc */
run local-print-trigger in this-procedure no-error.


if not error-status:error and
   return-value = "override" then return.
else do:
      lv-params = return-value.
   if lv-params = '' then lv-params = ','.

   run local-print-trigger-longchar in this-procedure (output lv-params) no-error.
   /*
   if not stringtolog(pgmproperty(this-procedure:file-name,'usecrystal'))
      then 
   */
   lv-repinfo = pgmrepinfo(this-procedure:file-name). 

   if PgmProperty(this-procedure:file-name,'ReportRunType') = 'Browse'
   then do: /* default just print browse */
      &if defined(browse-name) ne 0 and
          defined(nobrowse) = 0 &then
          
          run proc-SetOpDest in lh-general (input-output lv-dlgparam).
           if not lv-dlgparam begins '**Failed'
          then do: /*  printer to use is now session:printer-name */
             PrintBrowse({&browse-NAME}:handle in frame {&frame-name},"{&title-text}").
             message 'Print Complete' view-as alert-box title "{&title-text}".
          end.
      &endif
   end.
   /*  else if lv-repinfo = "batch" then run make-backgroundRequest(lv-params).  */
   else do: /* chop up lv-repinfo into path,prog,procedure,title,params */
       {{&core}{&prt}prtque.i &repinfo = lv-repinfo
                              &params  = lv-params}
   end.

   run local-after-print-trigger in this-procedure no-error.
   if not error-status:error and
      return-value = 'override' then return.

   if lv-exitAfterPrint or lv-asyncreports and return-value ne 'failed'
   then run exit-trigger.
end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-FindRow Include 
PROCEDURE Proc-FindRow :
def input param pv-browse as handle no-undo.
{{&core}bug.i}
if not gotall() then return .

run local-Proc-FindRow in this-procedure (pv-browse) no-error.

if not error-status:error and
     return-value = 'override' then return.

def var v-hqry    as handle no-undo.
def var lv-f      as char no-undo.
def var v-hcolumn as handle no-undo.
def var lv-value  as char   no-undo format 'x(10)'.
def var lv-qry    as char   no-undo.
def var h-db      as handle no-undo.
def var h-qry     as handle no-undo.
def var h-qry2    as handle no-undo.
def var lv-q as log no-undo.
def var lv-descending    as log   no-undo.
def var lv-gtlt as char no-undo init " >= '".
def var lv-by     as char   no-undo.

def var cntr     as i no-undo.
do cntr = pv-browse:num-columns  to 1 by -1:
   v-hColumn = pv-browse:get-browse-column(cntr) no-error.
   if last-event:column > v-hColumn:column then leave.
end.
lv-f = entry(cntr,'{&FIELDS-IN-QUERY-{&BROWSE-NAME}}',' '). 

/* v-hcolumn = pv-browse:CURRENT-COLUMN.  */

if v-hcolumn = ? then return.

if Valid-handle(h-QrY2) then h-QrY2:QUERY-CLOSE().
if Valid-handle(h-QrY2) then delete object h-QrY2 no-error.
h-qry = pv-browse:query.

if h-qry:num-buffers > 1 then return.


lv-by = h-qry:prepare-string.
if lv-by = ?
then lv-by = "{&QUERY-STRING-{&BROWSE-name}}".
cntr = index(lv-by,' by').
if cntr ne 0
then lv-by = substring(lv-by,cntr) no-error.
else lv-by = ''.

if index(lv-by,lv-f) = 0 
then lv-by = ' by ' + lv-f.

lv-descending = index(lv-by,'descending') > 0.
/* if descendingbrowse then lv-descending = not lv-descending. */

create buffer h-db for table h-qry:get-buffer-handle(v-hcolumn:table).
/* i know i know only one active wait-for 
but update is quiclkest and easiest method */
do on endkey undo,return:
update lv-value label "Search For" at 6 skip
  " Enter the search value or press Esc to cancel." at 5
   go-on (tab)
   with frame upd side-labels three-d
   view-as dialog-box  title "Quick Search".
end.
create query h-QrY2.
h-QrY2:Add-buffer(h-db).

lv-qry = "For EACH " + h-db:name +
         " where " + lv-f + lv-gtlt + lv-value + "'" +
         " no-lock " + lv-by.        
         
/* message h-qry:prepare-string skip */
/*         "{&QUERY-STRING-{&BROWSE-name}}" skip */
/*         h-db:name skip */
/*         lv-f skip */
/*         lv-gtlt skip */
/*         lv-value skip */
/*         lv-by skip */
/*         lv-qry. */

lv-q = h-QRY2:QUERY-PREPARE(lv-qry) no-error.      
if not lv-q
then do:
    message "Your request contains characters which can't be used in a search," skip
            "such as quotation marks: " lv-value
            view-as alert-box error.
    return.         
end.

h-QRY2:QUERY-OPEN.
if lv-descending
    then h-qry2:get-last.
    else h-qry2:get-first.
if h-qry2:num-results = 0 
then do:
    message 'No Entry found For >= ' lv-value
    view-as alert-box information.
    return.
end.


h-qry:reposition-to-rowid(h-db:rowid). 

if pv-browse:name = "{&browse-name}" then
   run br-changed-trigger in this-procedure no-error.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-SetInitColumn Include 
PROCEDURE Proc-SetInitColumn :
def input param pv-bhand as handle no-undo.
{{&core}bug.i}
    def var v-brhand as handle no-undo.
    v-brhand = pv-bhand:first-column.
    v-brhand:LABEL-BGCOLOR = 15.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-SortBrowse Include 
PROCEDURE Proc-SortBrowse :
def input param pv-browse as handle no-undo.
def input param pv-query  as char   no-undo.
{{&core}bug.i}

&if defined(NoBrowseSorting) = 0 &then

def var v-hqry    as handle no-undo.
def var v-hcolumn as handle no-undo.
def var v-sortby  as char   no-undo.
def var v-desc    as char   no-undo.
def var lv-f as char no-undo.
def var lv-ok as log no-undo.
def var lv-origq as char no-undo.
def var lv-colnum as int no-undo.
def var x as int no-undo init 1.
def var lv-origby as char no-undo.

if pv-browse:CURRENT-COLUMN ne ?
    then lv-scol = pv-browse:CURRENT-COLUMN.
    else if lv-scol = ?
         then return. /* lv-scol = pv-browse:first-column */
         else pv-browse:private-data = if pv-browse:private-data = lv-scol:name
                                       then ""
                                       else lv-scol:name.
if not gotall() then return .

v-hcolumn = pv-browse:first-COLUMN.
do while VALID-HANDLE(v-hcolumn):
    if v-hcolumn = lv-scol then lv-colnum = x.
    assign v-hcolumn:LABEL-BGCOLOR = 8
           x = x + 1
           v-hcolumn  = v-hcolumn:NEXT-COLUMN no-error.
end.
lv-f = entry(lv-colnum,'{&FIELDS-IN-QUERY-{&BROWSE-NAME}}',' '). 

assign
    v-hcolumn = lv-scol
    v-desc    = if pv-browse:private-data = lv-f then ' descending' else ' '
    v-hqry    = pv-browse:query
    v-sortby  = ' by ' + lv-f 
    v-hcolumn:LABEL-BGCOLOR = 15
    pv-browse:private-data  = if pv-browse:private-data = lv-f 
                                then ""
                                else lv-f
    pv-query  = if v-hqry:prepare-string = ? 
                then pv-query 
                else v-hqry:prepare-string
    lv-origq = pv-query no-error.

        pv-query = replace(pv-query,v-sortby + ' descending','').
        pv-query = replace(pv-query,v-sortby,'').
        v-sortby = v-sortby  + v-desc + ''.

/* message pv-query skip v-sortby {&dbt}. */

if index(pv-query, ' by') ne 0
    then do:
/*         lv-origby = substring(pv-query,index(pv-query,' by')). */
        pv-query = substring(pv-query,1,index(pv-query,' by') - 1).
/*         v-sortby = v-sortby + lv-origby. */
    end.
    
if index(pv-query,' indexed') ne 0 
    then pv-query = substring(pv-query,1,index(pv-query,' indexed') - 1).
    
lv-ok = v-hqry:query-prepare(pv-query + v-sortby)  no-error.
/* message pv-query skip v-sortby {&dbt}. */

if not lv-ok 
then do:
    message 'Cannot sort on this field.' /*  skip
      Error-Status:Get-Message(Error-Status:Num-Messages) skip
      lv-origq */
    view-as alert-box information.
    v-hcolumn:LABEL-BGCOLOR = 8.
    lv-ok = v-hqry:query-prepare(lv-origq) no-error.
    if not lv-ok then return. 
 /*   return.   */
end.

    pv-browse:CLEAR-SORT-ARROWS().
    pv-browse:set-sort-arrow(lv-colnum,(v-desc ne '')).

v-hqry:query-open().
pv-browse:query = v-hqry.
/* v-hqry:get-first. */
if pv-browse:name = "{&browse-name}" then
   run br-changed-trigger in this-procedure no-error.

&endif
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc-Start-Search Include 
PROCEDURE Proc-Start-Search :
def input param pv-browse as handle no-undo.
{{&core}bug.i}
/* have to do it via trigger as static browse does not populate
   prepare-string attribute of browse query 
   otherwise we would say 
   run proc-SORTBROWSE(pv-browse,pv-browse:prepare-string). */

apply 'start-search' to pv-browse.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProgramInfo Include 
PROCEDURE ProgramInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* maybe replace with global trigger */
{{&core}bug.i}
def var lv-desc as char no-undo.
lv-desc = ProgramDescription(this-procedure:file-name).
if lv-desc ne '' then
    message lv-desc view-as alert-box title 'Program Description'.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Query-Trigger Include 
PROCEDURE Query-Trigger :
{{&core}bug.i}
run local-query-trigger in this-procedure no-error.

if not error-status:error and
     return-value = 'override' then return.

&if defined(browse-name) ne 0 
    and defined(nobrowse) eq 0 
    and defined(getallonquery) ne 0
&then
    sysmsg('Retrieving Data: please wait.  If there is a lot of data, this can take a few minutes.').
    run clear-tables in this-procedure no-error.
    run browseoffend in this-procedure ('all',{&browse-name}:handle in frame {&frame-name})
      no-error. 
    sysmsg('off').
&else 
    run OpenQuery in this-procedure no-error.
&endif

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReportTableReturn Include 
PROCEDURE ReportTableReturn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-printtable as handle no-undo.
{{&core}bug.i}
&if defined(ReportReturnTable) ne 0 &then

   def var h-qry as handle no-undo.
   def var h-buf as handle no-undo.
   def var h-db  as handle no-undo.

   h-buf = pv-printtable:default-buffer-handle.  
   create buffer h-db for table 't-{&ReportReturnTable}'. 

   if Valid-handle(h-QrY) then delete object h-QrY no-error.

   create query h-qry.
   h-QrY:Add-buffer(h-buf).
   h-qry:query-prepare('for each ' + h-buf:table + ' no-lock').
   h-qry:query-open.

   h-qry:get-first.

   do while not h-qry:query-off-end:
      h-db:buffer-create.
      h-db:buffer-copy(h-buf).
      h-qry:get-next.
   end.
&endif

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ResetBrowseSort Include 
PROCEDURE ResetBrowseSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-browse as handle no-undo.
{{&core}bug.i}
def var v-hcolumn as handle no-undo.

v-hcolumn = pv-browse:first-COLUMN.
do while VALID-HANDLE(v-hcolumn):
    assign v-hcolumn:LABEL-BGCOLOR = 8
           v-hcolumn               = v-hcolumn:NEXT-COLUMN no-error.
end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Save-Record Include 
PROCEDURE Save-Record :
{{&core}bug.i}
def var lv-rowid as rowid  no-undo.
def var lv-recid as recid  no-undo.
def var lv-i     as int    no-undo.
def var lv-before as handle no-undo.
def var v-hcolumn as handle no-undo.
def var lv-allok as log no-undo.

&if (defined(browse-name) ne 0 
     and defined(nobrowse) eq 0) &then
    lv-i = {&browse-name}:FOCUSED-ROW in frame {&frame-name}.
&endif

run local-save-record in this-procedure no-error.

if not error-status:error and
     return-value = 'override' then return 'passed'.

run validate-screen in this-procedure no-error.
if not error-status:error and
   return-value ne 'passed' 
then do:
    if return-value ne 'override' then 
      /* put cursor on the problem field */
      SetFocusTo(widget-handle(return-value)).
    return 'failed'.
end.

run assignframes in this-procedure no-error.
if not error-status:error and
   return-value ne 'passed' 
then do:
    SetFocusTo(widget-handle(return-value)).
    return 'failed'.
end.


run ExtraAssign in this-procedure no-error.
if not error-status:error and
   return-value ne 'passed' 
then do:
    SetFocusTo(widget-handle(return-value)).
    return 'failed'.
end.


run pre-save in this-procedure no-error.
if not error-status:error and
   return-value ne 'passed' 
then do:
    if return-value ne "override" then SetFocusTo(widget-handle(return-value)).
    return 'failed'.
end.

run save-children in this-procedure no-error.
if not error-status:error and
   return-value ne 'passed' 
then do:
/*     SetFocusTo(widget-handle(return-value)). */
    return 'failed'.
end.

&if defined(table-name) ne 0 &then
    find first tb-{&table-name} no-error.  /* record before any changes */
    empty temp-table ts-{&table-name}. /* copy to save */   
    create ts-{&table-name}.
    buffer-copy t-{&table-name} to ts-{&table-name}.

    errorclear().
    ServerMessagesClear().
    
    {{&core}run.i 
      &program   = "{&SaveProg}"
      &path      = "{&path}"
      &Appsrv    = "System"
      &procedure = "{&SaveProc}"
      &params    = "({&ExtraSave}ts-{&table-name}.{&unique-key},
                   input-output table ts-{&table-name},
                   input table tb-{&table-name}{&ExtraTables})"}


   find first ts-{&table-name} no-error.

    if avail ts-{&table-name} then do:
       buffer-copy ts-{&table-name} to t-{&table-name}.
       find t-{&table-name} where t-{&table-name}.{&Unique-key} = ts-{&table-name}.{&Unique-key} no-error.
        /* if extratable then go get it */
       run pop-related-tables   in this-procedure no-error.
       run save-blob-data       in this-procedure no-error.
    end.
   if anyerrors() then do: /* error occurered on save reset disp data */
        buffer-copy t-{&table-name} to tb-{&table-name}.
        &if (defined(browse-name) ne 0
             and defined(nobrowse) eq 0) &then
            if t-{&table-name}.{&unique-key} = ?
               then run display-fields in this-procedure no-error. 
            else run br-changed-trigger in this-procedure no-error. 
        &else
            run display-fields in this-procedure no-error.
        &endif
         /* so that if user saves again with no changes, we avoid errors */
        lv-lastfocus:modified = true. 
        SetFocusTo(lv-lastfocus).
        return 'failed'.
    end.


   if anyServerMessages() then do: 
      /* maybe return something depending on the severity of message ?? */
   end.

   /* 08/02/05 AMD, added noBrowse logic */
   &if defined(browse-name) ne 0 and 
       defined(noBrowse) eq 0 &then
/*         IF AVAIL t-{&table-name} AND NOT lv-Newmode */
/*            THEN {&browse-name}:REFRESH().           */
      /*  ELSE */ do: 

            run BEFORE-OPEN-query in this-procedure no-error.
            {&OPEN-QUERY-{&BROWSE-NAME}}
.
            &if defined(nosortonsave) = 0 &then
               run proc-start-Search ({&browse-NAME}:HANDLE) no-error.
            &endif

            find t-{&table-name} where 
               t-{&table-name}.{&Unique-key} = ts-{&table-name}.{&Unique-key} no-error.        
            {&browse-name}:SET-REPOSITIONED-ROW(lv-i,"ALWAYS").

            reposition {&browse-name} to rowid rowid(t-{&table-name}) no-error. 
            {&browse-name}:select-focused-row().

            &if defined(leaveColumnColors) = 0 &then
               if lv-scol = ? then do:
                   v-hcolumn = {&browse-name}:first-COLUMN.
                   do while VALID-HANDLE(v-hcolumn):
                       assign v-hcolumn:LABEL-BGCOLOR = 8
                              v-hcolumn               = v-hcolumn:NEXT-COLUMN.
                   end.
               end.
            &endif
        end.
/*         run br-changed-trigger in this-procedure no-error.  */

    /* should this be display-fields? */
    &else run display-fields in this-procedure no-error.
    &endif 


    empty temp-table tb-{&table-name}.

    setsensitive(avail t-{&table-name},'inc','btn-audit,btn-delete,btn-edit,btn-print,btn-notes,btn-export',frame {&frame-name}:handle).    
&else setsensitive(true,'inc','btn-delete,btn-edit,btn-print,btn-notes',frame {&frame-name}:handle).    
&endif

/* this should really happen PW  who removed it?
Run save-children in this-procedure No-error. 
*/

/* so we can get screen mode in post save */
lv-scrmode = 'display'.
if lv-newmode then lv-scrmode = 'new'.
if lv-editmode then lv-scrmode = 'edit'.
if lv-newmode and lv-editmode then lv-scrmode = 'new-edit'.

assign
    lv-editmode = false
    lv-Newmode  = false.

&if (defined(browse-name) ne 0 
     and defined(nobrowse) eq 0) &then
     /* PUT BACK IN!! */
    apply 'entry' to {&BROWSE-NAME} in frame {&frame-name}.

    apply 'value-changed' to {&BROWSE-NAME} in frame {&frame-name}.
&endif
&if defined(noBrowse) ne 0 &then 
   run display-fields in this-procedure no-error. /* 08/02/05 AMD */
&endif

/* causing a PROBLEM when an extra table is defined */
&if defined(tabs) ne 0 &then   
    ShowFrame(string(h-tab:SelectedItem:Index)).
&endif


run post-save in this-procedure no-error.  /* added 06/27/05 AMD */

return 'passed'.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Save-Trigger Include 
PROCEDURE Save-Trigger :
{{&core}bug.i}
if valid-handle(focus) then do:
   if focus:type ne 'button'
    then apply "LEAVE" to focus.
end.

   def var h-wid as handle no-undo.

   run local-before-save in this-procedure no-error.
   if not error-status:error and 
      return-value = 'override' then return.

   if not screenchanged(frame {&frame-name}:handle) then do: /* in zenlibrary.p */
      run undo-trigger.
      return 'no changes so failed'.
   end.

   run local-save-trigger in this-procedure no-error.

   if not error-status:error and
       return-value = 'override' then return.
   run Save-record in this-procedure no-error.

    if not error-status:error and
       return-value = 'passed' then
            assign
                lv-EditMode = false
                lv-NewMode  = false.

   return return-value.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveNew-Trigger Include 
PROCEDURE SaveNew-Trigger :
{{&core}bug.i}
def var lv-wid as handle no-undo.

   lv-wid = GetWidHAndle(frame {&frame-name}:handle,'btn-savenew').
   if not valid-handle(lv-wid) then return.
   apply 'entry' to lv-wid.
   focus = lv-wid.

   run local-savenew-trigger in this-procedure no-error.

   if not error-status:error and 
      return-value = 'override' then return.

   run save-trigger in this-procedure no-error.

   if return-value ne 'failed' then 
      run new-trigger in this-procedure no-error.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFrameHandle Include 
PROCEDURE SendFrameHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param pv-frame as handle.
{{&core}bug.i}
pv-frame = frame {&frame-name}:handle.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMode Include 
PROCEDURE SendMode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def output param pv-mode as char no-undo.
{{&core}bug.i}
    /* dont check db if we want fastes net performance
       will be ok as read-only is check on server in save/delete anyway */
    if stringtolog(GetProperty('Program',this-procedure:file-name,"{&checkreadonly}")) ne true
    then do:
        pv-mode = if not canedit(this-procedure:file-name) 
                    then 'Read-Only'
                    else lv-progmode.
        return.
    end.
    /* if not worried about slow screen draw or need correct screen look
       then check db for readonly set */                               
    if getctrl("{&read-only}") = 'yes' then do:
       if not Systemmanager(getsysvar("{&clv}user")) then do:
           pv-mode = 'read-only'.    
       end.
    end.
    else pv-mode = if not canedit(this-procedure:file-name) then 'Read-Only'
                                   else lv-progmode.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendRanFrom Include 
PROCEDURE SendRanFrom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  param pv-from as handle no-undo.
def output param pv-item as char no-undo.
{{&core}bug.i}
def var x as int no-undo.

&if defined(IsMainMenu) ne 0 &then
    do x = 1 to ch-tab:tabs:count:
        if lv-childlist[x] = pv-from then do:
            if can-query(lv-ranfromlist[x],'type')
            then do:
                if lv-ranfromlist[x]:type = 'menu-item' or 
                   lv-ranfromlist[x]:type = 'sub-menu'
                 then pv-item = entry(1,lv-ranfromlist[x]:private-data,'|').
                 else pv-item = entry(2,lv-ranfromlist[x]:private-data,':').
                leave.
            end.
        end.
    end.
    if can-query(lv-ranfromlist[x],'type')
    then pv-item = pv-item + ':' + lv-ranfromlist[x]:type.
    else pv-item = ''.
&else 
    &if defined(AutoStart) = 0 
        &then pv-item = self:name + ':' + self:type.
        &else pv-item = ''.
    &endif
&endif

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendTabInfo Include 
PROCEDURE SendTabInfo :
def output param pv-info as char no-undo.
{{&core}bug.i}
pv-info = {&TabExtraInfo}.


end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sen Include 
PROCEDURE Set-Sen :
def input param pv-on-off as log format 'true/false' no-undo.
{{&core}bug.i}
def var lv-wl as char no-undo.
/* modify this list to set buttons insensitive while in editing mode */
def var lv-offwhilediting as char no-undo init 
   'btn-new,btn-edit,btn-delete,btn-print,btn-export,btn-query,btn-crystal,btn-export'.

/* available buttons are
New^ttf,
Edit^ftf,
Save^ftf,
Undo^ftf,
Delete^ftf,
Audit^ftf,
Query^ttf^Get All,
Print^ttf,
Crystal^ttf^Crystal Report,
Export^ttf,
Defaults^ttf,
Help^ttf,
Exit
*/

run extra-sensitive in THIS-PROCEDURE (pv-on-off)  no-error.

&if defined(tabs) = 0 &then
    if pv-on-off then do:
        if lv-Newmode then do: 
             lv-wl = replace("{&add-list}",' ',',').
             &if Defined(add-list)  ne 0 &then
                enable {&add-list}  with frame {&frame-name}. 
             &endif 
        end. 
        else do: 
           lv-wl = replace("{&edit-list}",' ',',').
           &if Defined(edit-list) ne 0 &then
                enable {&edit-list} with frame {&frame-name}. 
           &endif 
        end.
        setbgcolour(frame {&frame-name}:handle,lv-wl,'{&InputField}').
    end.
    else do:
        lv-wl = replace("{&add-list}",' ',',').
        setbgcolour(frame {&frame-name}:handle,lv-wl,'{&editablefield}').
        &if Defined(add-list) ne 0 &then 
            disable {&add-list} with frame {&frame-name}.
        &endif
    end.
&else
    assign lv-wl = replace("{&list-0}",' ',',')
           lv-wl = lv-wl + ',' + replace("{&list-1}",' ',',')
           lv-wl = lv-wl + ',' + replace("{&list-2}",' ',',')
           lv-wl = lv-wl + ',' + replace("{&list-3}",' ',',')
           lv-wl = lv-wl + ',' + replace("{&list-4}",' ',',')
           lv-wl = lv-wl + ',' + replace("{&list-5}",' ',',')
           lv-wl = lv-wl + ',' + replace("{&list-6}",' ',',').


    if pv-on-off then do:
        setbgcolour(frame {&frame-name}:handle,lv-wl,'{&InputField}').
        &if Defined(list-0) ne 0 &then enable {&list-0} with frame {&frame-name}.   &endif        
        &if Defined(list-1) ne 0 &then enable {&list-1} with frame one.   &endif        
        &if Defined(list-2) ne 0 &then enable {&list-2} with frame two.   &endif                
        &if Defined(list-3) ne 0 &then enable {&list-3} with frame three. &endif                
        &if Defined(list-4) ne 0 &then enable {&list-4} with frame four.  &endif                
        &if Defined(list-5) ne 0 &then enable {&list-5} with frame five.  &endif                
        &if Defined(list-6) ne 0 &then enable {&list-6} with frame six.   &endif                
    end.
    else do:
        setbgcolour(frame {&frame-name}:handle,lv-wl,'{&editablefield}').
        &if Defined(list-0) ne 0 &then disable {&list-0} with frame {&frame-name}.   &endif        
        &if Defined(list-1) ne 0 &then disable {&list-1} with frame one.   &endif        
        &if Defined(list-2) ne 0 &then disable {&list-2} with frame two.   &endif                
        &if Defined(list-3) ne 0 &then disable {&list-3} with frame three. &endif                
        &if Defined(list-4) ne 0 &then disable {&list-4} with frame four.  &endif                
        &if Defined(list-5) ne 0 &then disable {&list-5} with frame five.  &endif                
        &if Defined(list-6) ne 0 &then disable {&list-6} with frame six.   &endif                
    end.

&endif


SetAllLkBut(frame {&frame-name}:handle).

setsensitive(pv-on-off,'inc','btn-save,btn-savenew,btn-undo',frame {&frame-name}:handle).

&if defined(browse-name) ne 0 and defined(nobrowse) = 0 &then
   if query {&browse-name}:num-results ne 0 
      then setsensitive(not pv-on-off,'inc',lv-offwhilediting,frame {&frame-name}:handle).
      else setsensitive(not pv-on-off,'inc','btn-new',frame {&frame-name}:handle).
   &else
      setsensitive(not pv-on-off,'inc',lv-offwhilediting,frame {&frame-name}:handle).
&endif

&if (defined(browse-name) ne 0 
     and defined(nobrowse) eq 0) &then
    {&browse-name}:sensitive in frame {&frame-name} = not pv-on-off.
&endif
&if defined(browse-name) ne 0 
       and defined(nobrowse) eq 0 
       and defined(usemoreimage) ne 0 
       and defined(KeepRefreshButton) = 0 
   &THEN setsensitive(h-{&BROWSE-NAME}mored:visible,'inc','btn-query',frame {&frame-name}:handle).
&endif
/* do widget level security if any needed */
if GetFieldWhere('zen-dwidget','pgm = "' + this-procedure:file-name + '"','PGM')
                    = this-procedure:file-name
then WidSecCheck(frame {&frame-name}:handle,this-procedure:file-name).
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetChildsParent Include 
PROCEDURE SetChildsParent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-parent as handle no-undo.
{{&core}bug.i}
lv-parent = pv-parent.
run SubScribeToAll(pv-parent,'child').

&IF defined(suppresswindow) ne 0 &THEN
    run createtab in pv-parent(this-procedure) no-error.
&ENDIF

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetLastFocus Include 
PROCEDURE SetLastFocus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&core}bug.i}
   
   /* if last-event:widget-enter:type ne 'button' then   01/18/10 */ 
   
   if last-event:widget-enter:type ne "button" or
      (can-query(last-event:widget-enter,"name") and
       lookup(last-event:widget-enter:name,"btn-help,btn-exit") ne 0)
   then lv-lastfocus = last-event:widget-leave.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SubscribeToAll Include 
PROCEDURE SubscribeToAll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   def input param pv-proc as handle no-undo.  /* id of parent procedure */
   def input param pv-type as char   no-undo.  /* can be "child" or ?? */
   {{&core}bug.i}
   def var x as int no-undo.
   
   
   do x = 1 to num-entries(pv-proc:published-events):
       if entry(x,pv-proc:published-events) begins pv-type
          then subscribe to entry(x,pv-proc:published-events) in pv-proc.
   end.
   
   /* PHIL: LOOK */
/*    subscribe "childTabChoose" anywhere. */
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Undo-Trigger Include 
PROCEDURE Undo-Trigger :
{{&core}bug.i}

if valid-handle(focus) then
        apply "LEAVE" to focus.

run local-undo-trigger in this-procedure no-error.
assign lv-Newmode  = false 
      lv-editmode = false.
if not error-status:error and
      return-value = 'override' then return.

&if defined(table-name) ne 0 &then
  if avail t-{&table-name} and t-{&table-name}.{&Unique-key} = ? then do:
    Clearscreen(frame {&frame-name}:handle).
    run local-delete-extra-tables in this-procedure no-error.
    delete t-{&table-name}.
  end.
  delete tb-{&table-name}.
&endif  


run Disable-Input in this-procedure no-error. 

  

 &if (defined(browse-name) ne 0 
        and defined(nobrowse) eq 0) &then 
    {&browse-name}:sensitive in frame {&frame-name} = true.   
    apply 'entry' to {&browse-name}.    
  &endif 
  run br-changed-trigger in this-procedure no-error.



&if defined(table-name) ne 0 &then
  setsensitive(avail t-{&table-name},'inc','btn-edit,btn-delete,btn-print',frame {&frame-name}:handle).
&else setsensitive(true,'inc','btn-edit,btn-delete,btn-print',frame {&frame-name}:handle).
&endif
  setsensitive(false,'inc','btn-undo,btn-save',frame {&frame-name}:handle).
  setsensitive(true,'inc','btn-new',frame {&frame-name}:handle).


  {{&core}wid-chk.i}
run post-undo in this-procedure no-error.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update-Child-Procedures Include 
PROCEDURE Update-Child-Procedures :
def input param pv-to as handle no-undo.
{{&core}bug.i}
run local-update-child-procedures in this-procedure (pv-to) no-error.
    if not error-status:error and
         return-value = 'override' then return.

/*     if not error-status:error then do: */
/*         {{&core}wid-chk.i}             */
/*     end.                               */

case pv-to:private-data:
   &if defined(table-name) ne 0 &then
    when '{&core}audit/singleaudview.w'
         then if return-value ne 'NoSingleAud' 
              then run refresh in pv-to (string(t-{&AuditTable}.{&AuditTable}tableid),
                                         "{&AuditTable}").
   &endif
    when '{&core}about.w'
         then run refresh in pv-to (this-procedure).
end case.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClearScreen Include 
FUNCTION ClearScreen returns logical
  ( lv-wid-handle as handle ) :
{{&core}bug.i}
    assign   
        lv-wid-handle = lv-wid-handle:first-child
        lv-wid-handle = lv-wid-handle:first-child.

    do while valid-handle(lv-wid-handle):
        if lv-wid-handle:type = 'frame' and lv-wid-handle:private-data ne 
            "ignore" 
            then clearscreen(lv-wid-handle).
        if can-set(lv-wid-handle,'screen-value') 
           and lv-wid-handle:type ne 'literal'
           and lv-wid-handle:name ne 'edinfobox'
           and not(lv-wid-handle:type = 'fill-in' and 
                   lv-wid-handle:subtype = 'native')
           and lv-wid-handle:private-data ne "ignore" /* 10/02/08 AMD */
        then do:  
           if lv-wid-handle:type = "radio-set" then
                lv-wid-handle:screen-value = 
                entry(2,lv-wid-handle:radio-buttons,"^").
           else if lv-wid-handle:type = "toggle-box" or
              lv-wid-handle:data-type = "logical"  then 
              lv-wid-handle:screen-value = entry(2,lv-wid-handle:format,"/").
           else lv-wid-handle:screen-value = ''.
        end.
        lv-wid-handle = lv-wid-handle:next-sibling.
    end.

    return true.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EnabledBgColour Include 
FUNCTION EnabledBgColour returns logical
  ( /* parameter-definitions */ ) :
  {{&core}bug.i}
def var lv-wl as char no-undo.
if lv-wl = '' then do:
&if defined(tabs) ne 0 &then
    lv-wl = replace("{&list-1}",' ',',').
    lv-wl = lv-wl + ',' + replace("{&list-2}",' ',',').
    lv-wl = lv-wl + ',' + replace("{&list-3}",' ',',').
    lv-wl = lv-wl + ',' + replace("{&list-4}",' ',',').
    lv-wl = lv-wl + ',' + replace("{&list-5}",' ',',').
    lv-wl = lv-wl + ',' + replace("{&list-6}",' ',',').
&else
    lv-wl = replace("{&add-list}",' ',',').
&endif
end.
setbgcolour(frame {&frame-name}:handle,lv-wl,'{&editablefield}').
  return false.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GotAll Include 
FUNCTION GotAll returns logical
  ( /* parameter-definitions */ ) :
  {{&core}bug.i}
&if defined(usemoreimage) ne 0 and defined(nobrowse) eq 0 and 
defined(browse-name) ne 0 &then
    if not h-{&browse-name}mored:hidden 
    then do:
        if lv-scol ne ? then
         message 'Please get all records before sorting.' view-as alert-box.
        return false.
    end.
&endif

  return true.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetFocusTo Include 
FUNCTION SetFocusTo returns logical
  ( lv-hand  as handle ) :
  {{&core}bug.i}
    def var lv-f     as handle no-undo.
    if valid-handle(lv-hand) then do:
        lv-f = lv-hand:Frame no-error.
        if valid-handle(lv-f) 
        then showframe(lv-f:private-data).
        apply 'entry' to lv-hand.
    end.

  return true.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetInitColumn Include 
FUNCTION SetInitColumn returns logical
  ( pv-bhand as handle) :
{{&core}bug.i}
  run proc-SetInitColumn (pv-bhand).

  return true.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ShowFrame Include 
FUNCTION ShowFrame returns logical
  (pv-num as char ) :
{{&core}bug.i}
   def var lv-wid-handle as handle no-undo.
   def var lv-first-widget as handle no-undo. 
   assign         
/*         lv-wid-handle = current-window  */
      lv-wid-handle = frame {&frame-name}:handle
      lv-wid-handle = lv-wid-handle:first-child
      lv-wid-handle = lv-wid-handle:first-child
      lv-first-widget = lv-wid-handle.
   if not lv-editmode and not lv-newmode then do:
      run pop-related-tables in this-procedure no-error.
      run display-fields     in this-procedure no-error.
   end.

   do while valid-handle(lv-wid-handle):
      if lv-wid-handle:type = 'frame' then do:
         if lv-wid-handle:private-data ne 'ignore' then
            lv-wid-handle:hidden = not pv-num = lv-wid-handle:private-data.
         if not lv-wid-handle:hidden then lv-currentframe = lv-wid-handle.
      end.
      lv-wid-handle = lv-wid-handle:next-sibling.
      if lv-wid-handle = lv-first-widget then leave.
   end.
   &if defined(tabs) ne 0 &then 
      if h-tab:selecteditem:index ne int(pv-num) then
         h-tab:selecteditem = h-tab:tabs(int(pv-num)) no-error.
   &endif
   return true.  

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SortBrowse Include 
FUNCTION SortBrowse returns logical
  (pv-browse as handle,
   pv-query  as char ) :
    {{&core}bug.i}
    run Proc-SortBrowse (pv-browse,pv-query).
    
  return true.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

