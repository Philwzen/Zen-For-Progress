&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
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

{app-paths.i}

def var lv-prog as handle no-undo.
def var lv-pparams      as char no-undo.     
def var lv-pparamtitles as char no-undo.
def var lv-paramtitles as char no-undo.
def var lv-dpgmparams as char no-undo.
def var lv-asyncreports as log no-undo.
def var lv-program-name as char no-undo.
def var lv-repdest     as char no-undo init 'window'.
def var lv-opfile      as char no-undo.
def var lv-msghand as handle no-undo.
def var h-asyncserver as handle no-undo.
def var lv-asynchandle as handle no-undo.
def var lv-reptitle as char no-undo.
{{&core}{&rep}reportdefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Return-xmltype) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Return-xmltype Procedure 
PROCEDURE Return-xmltype :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input parameter pv-mreport   as memptr no-undo.
def input parameter pv-mschema   as memptr no-undo.
def input param table-handle pv-ReturnTable. 
{{&base}releaseapsrv.i &Appsrv    = "System"}

if return-value ne 'completeok' then do:

end.
if valid-handle(lv-msghand) then do:
    menumsg('off').
    Message 'Your' lv-reptitle  'report is complete' 
       view-as alert-box title 'Complete'.
end.
else Sysmsg('off').

   if get-size(pv-mreport) = 0
   then do:
      message 'No data returned for this report (' + lv-cryfile + ').'
      view-as alert-box.
      return 'failed'.
   end.

   lv-datfile = entry(1,lv-extractdir + getsysvar('{&clv}user') + lv-datfile,'.').
   OutputToFile(lv-datfile + '.xsd',pv-mschema,'local').
   lv-datfile = OutputToFile(lv-datfile + '.xml',pv-mreport,'local').
   set-size(pv-mreport) = 0.
   set-size(pv-mschema) = 0.

   ASSIGN file-info:file-name = lv-cryfile
          lv-cryfile = file-info:FULL-PATHNAME NO-ERROR. 
   if search(lv-cryfile) = ? then do:
      message 'No Rpt file Found but data created' skip
               lv-datfile.
      return.
   end.
/* rpt name , data file naem, save data file */
if can-do('*dash-doc.rpt,*dash-frontdesk.rpt',lv-cryfile)
then  run dispop  in lv-prog (lv-cryfile,lv-datfile,no).
/* run {&core}{&rep}crystalop.p (lv-cryfile,lv-datfile,no,'file',pv-prog). */
else 
 run {&core}{&rep}crystalviewer.w (lv-cryfile,lv-datfile,no).


   OS-DELETE VALUE(lv-datfile).
Sysmsg('off').
delete procedure this-procedure.

apply 'close' to this-procedure.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RunJob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunJob Procedure 
PROCEDURE RunJob :
def input param pv-asyncreports as log no-undo.
def input param pv-prog as handle no-undo.
def input param pv-params      as char no-undo.     
def input param pv-paramtitles as char no-undo.
def input param pv-dpgmparams as char no-undo.

assign  lv-asyncreports = pv-asyncreports
        lv-prog = pv-prog
        lv-pparams = pv-params
        lv-pparamtitles = pv-paramtitles
        lv-dpgmparams = pv-dpgmparams
        lv-program-name = pv-prog:file-name
        lv-reptitle = PgmProperty(lv-program-name,'ReportTitle').
            
case PgmProperty(lv-program-name,'ReportRunType'):
    when 'table' then do:
                    run TableType.
                    Sysmsg('off').
                    apply 'close' to this-procedure.
                 end.
    when 'xml'   then if pv-asyncreports then run XmlTypeAsync.
                                         else run XmlType.
    otherwise do:
        message 'Invalid Crystal Option'.
        Sysmsg('off').
        apply 'close' to this-procedure.
    end.
end case.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TableType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TableType Procedure 
PROCEDURE TableType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{{&core}{&rep}getrepdefaults.i &program-name = lv-program-name}
if lv-pparams ne '' then lv-params = lv-pparams.

if lv-pparamtitles ne '' 
   then lv-paramtitles = lv-pparamtitles.
   else do:
      if num-entries(lv-params,'{&delim2}') > 1 
         then lv-paramtitles = entry(2,lv-params,'{&delim2}').
      else do:
         do x = 1 to num-entries(lv-params):
            lv-paramtitles = ',' + 'Param' + string(x).
         end.
         lv-paramtitles = substring(lv-paramtitles,2).
      end.
   end.

{{&core}{&rep}doreport.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-XmlType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE XmlType Procedure 
PROCEDURE XmlType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-printtable as handle no-undo.
def var lv-mreport     as memptr no-undo.
def var lv-mschema     as memptr no-undo.
def var lv-printerparams as char no-undo.


/* format of lv-printerparams
assign lv-deviceid    = dec(entry(1,pv-printerparams,'{&Delim2}'))
       lv-devicename  = entry(2,pv-printerparams,'{&Delim2}')
       lv-numcopies   = int(entry(3,pv-printerparams,'{&Delim2}'))
       lv-tray        = entry(4,pv-printerparams,'{&Delim2}')
       lv-endfilename = entry(5,pv-printerparams,'{&Delim2}').
*/

   {{&core}{&rep}getrepdefaults.i &program-name = lv-program-name
                                  &dpgmparams   = lv-dpgmparams}

  if lv-pparams ne '' then lv-params = lv-pparams.
     
   lv-printerparams = "0{&delim2}" + PgmProperty(lv-program-name,'ReportRunType') +
                      "{&delim2}1{&delim2}1{&delim2}" + lv-datfile.

sysmsg('Report is being prepared... Please wait.').
   set-size(lv-mreport) = 0.
   set-size(lv-mschema) = 0.
   errorclear().

 {{&base}run.i &programc   = lv-extrprog
                &procedurec = lv-extrproc
                &pathc      = lv-extrpath
                &noper      = true
                &Appsrv    = "System"
                &params    = "('no',lv-printerparams,
                               OUTPUT lv-mreport,
                               output lv-mschema,
                               lv-value,
                               lv-params,
                               output table-handle lv-printtable)"}
                               
run return-XmlType(lv-mreport,lv-mschema,table-handle lv-printtable).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-XmlTypeAsync) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE XmlTypeAsync Procedure 
PROCEDURE XmlTypeAsync :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lv-printtable as handle no-undo.
def var lv-mreport     as memptr no-undo.
def var lv-mschema     as memptr no-undo.
def var lv-printerparams as char no-undo.

   lv-msghand = MenuMsg('Your ' + lv-reptitle + ' report is being prepared...').

   {{&core}{&rep}getrepdefaults.i &program-name = lv-program-name
                                  &dpgmparams   = lv-dpgmparams}
   if lv-pparams ne '' then lv-params = lv-pparams.
   
   lv-printerparams = "0{&delim2}" + PgmProperty(lv-program-name,'ReportRunType') +
                      "{&delim2}1{&delim2}1{&delim2}" + lv-datfile.

   set-size(lv-mreport) = 0.
   set-size(lv-mschema) = 0.
   errorclear().

 {{&base}run.i &programc   = lv-extrprog
                &procedurec = lv-extrproc
                &pathc      = lv-extrpath
     &async      = " asynchronous set lv-asynchandle event-procedure 'Return-XmlType' in this-procedure" 
                &Appsrv    = "System"
                &params    = "('yes',lv-printerparams,
                               OUTPUT lv-mreport,
                               output lv-mschema,
                               lv-value,
                               lv-params,
                               output table-handle lv-printtable)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

