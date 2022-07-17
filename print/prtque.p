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
/* {{&core}libraries/winapi-functions.i} */

def stream op.
def var lv-mReport as memptr no-undo.
def var lv-mschema as memptr no-undo.
def var lv-calledfrom as handle no-undo.
def var lv-printtable as handle no-undo.
def var lv-printerparams as char no-undo.
def var lv-device  as char   no-undo.
def var lv-scratch as char   no-undo.
def var lv-asynchandle as handle no-undo.
def var h-asyncserver as handle no-undo.
def var lv-msghand as handle no-undo.
def var lv-title as char no-undo.
def var lv-copies as int no-undo init 1.

lv-scratch = getctrl("{&scratchpath}").

this-procedure:private-data = this-procedure:file-name.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-PrintFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PrintFile Procedure 
FUNCTION PrintFile RETURNS LOGICAL
  ( pv-flags    as int,
    pv-filename as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintMemptr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PrintMemptr Procedure 
FUNCTION PrintMemptr RETURNS LOGICAL
  ( pv-flags as int,
    pv-uniq as char,
    pv-mrep  as memptr )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

&IF DEFINED(EXCLUDE-GetData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetData Procedure 
PROCEDURE GetData :
def input param pv-title as char no-undo.
def input  param pv-runasync      as log  no-undo.
def input  param pv-path as char no-undo.
def input  param pv-prog as char no-undo.
def input  param pv-proc as char no-undo.
def input  param pv-prtparams as char no-undo.
def input  param pv-dpparams  as char no-undo.
def input  param pv-params    as longchar   no-undo.
def output param pv-mreportholder   as memptr no-undo.
def output param pv-mschemaholder   as memptr no-undo.

set-size(pv-mreportholder) = 0.
errorclear().
sysmsg('Report is being prepared... Please wait.').
   
  {{&base}run.i 
     &programc   = pv-prog
     &procedurec = pv-proc
     &pathc      = pv-path
     &Appsrv     = "System"
     &params     = "(pv-runasync,
                     pv-prtparams,
                     output pv-mreportholder,
                     output pv-mschemaholder,
                     pv-dpparams,
                     pv-params,
                     output table-handle lv-printtable)"}                                                             

run return-GetDataWithTable(pv-mreportholder,pv-mschemaholder,table-handle lv-printtable).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetDataAsync) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDataAsync Procedure 
PROCEDURE GetDataAsync :
def input param pv-title as char no-undo.
def input  param pv-runasync      as log  no-undo.
def input  param pv-path as char no-undo.
def input  param pv-prog as char no-undo.
def input  param pv-proc as char no-undo.
def input  param pv-prtparams as char no-undo.
def input  param pv-dpparams  as char no-undo.
def input  param pv-params    as longchar   no-undo.
def output param pv-mreportholder   as memptr no-undo.
def output param pv-mschemaholder   as memptr no-undo.

set-size(pv-mreportholder) = 0.
errorclear().
if not pv-runasync then sysmsg('Report is being prepared... Please wait.').
else lv-msghand = MenuMsg('Your ' + pv-title + ' report is being prepared...').    

/* dont really need the set lv-asynchandle or the
   in this-procedure just included for completeness */
   
  {{&base}run.i 
     &programc   = pv-prog
     &procedurec = pv-proc
     &pathc      = pv-path
     &async      = " asynchronous set lv-asynchandle event-procedure 'Return-GetDataWithTable' in this-procedure" 
     &Appsrv     = "System"
     &params     = "(pv-runasync,
                     pv-prtparams,
                     output pv-mreportholder,
                     output pv-mschemaholder,
                     pv-dpparams,
                     pv-params,
                     output table-handle lv-printtable)"}                                                             

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OutputToDevice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OutputToDevice Procedure 
PROCEDURE OutputToDevice :
def input-output param pv-device    as char   no-undo.
def input param pv-mrep         as memptr no-undo.
def input param pv-prtparams    as char   no-undo.

def var lv-h as handle no-undo.
def var lv-numcopies   as int  no-undo.
def var lv-endfilename as char no-undo.
def var lv-prtflags    as int  no-undo init 0.
def var lv-ret         as int  no-undo.
def var lv-origprinter as char no-undo.
def var lv-result      as char no-undo.
def var lv-usr as char no-undo.
def var lv-uniq as char no-undo.
def var notUsed as char no-undo.

assign lv-numcopies   = int(entry(3,pv-prtparams,'{&Delim2}'))
       lv-endfilename = entry(5,pv-prtparams,'{&Delim2}')
       lv-origprinter = session:printer-name
       lv-usr = getsysvar('{&clv}user')
       lv-uniq = lv-scratch + lv-usr + string(today,'99-99-9999') + string(time).

def var lv-prtlog as log no-undo.
def var lv-prtlogfile as char no-undo.
def var lv-msgstr as char no-undo.
lv-msgstr = string(replace(program-name(5),' ',',')) + ',' + 
            string(pv-device) + ',' + 
            '' + ',' + 
            '' + ',' + 
            string(lv-uniq) + ',' + 
            string(lv-origprinter).

lv-prtlog = stringtolog(getctrl('UsePrintLog')).
if session:remote 
then lv-prtlogfile = "{&logs}" + string(today,'99999999') + 'serverprinter.log'.
else lv-prtlogfile = "{&logs}" + string(today,'99999999') + 'clientprinter.log'.   
if lv-prtlog then logmessage('Begin-ClientSide,' + lv-msgstr,'','Begin-ClientSide').
     
case pv-device:
    when 'file'   then pv-device = OutputToFile(lv-endfilename,pv-mrep,'local').
    when 'screen' then OutputToScreen(OutputToFile(lv-uniq + 'temp.rep',pv-mrep,'local')).
    when 'fax'    then do:
       OutputToFile(lv-uniq + 'faxtemp.pcl',pv-mrep,'local').
       lv-endfilename = ConvPcl(lv-uniq + 'faxtemp.pcl',GetCtrl('FaxFormat')).
       If lv-endfilename = ?
       then do:
            lv-result =  'failed'.
            message 'There was a problem with the file Conversion' skip
                    'Please inform Technical Support'
            View-as alert-box error.
       end.
       /* find out to whom user wishes to fax */
       else
       run {&client}{&general}faxdialogue.w (
          input  no,                       /* silent faxing - no user input */
          input  "",                       /* account number */
          input  "",                       /* document number */
          input  lv-endfilename,           /* file to fax */
          input  "",                       /* cover sheet */
          input  "",                       /* referring provider 1 */
          input  "",                       /* referring provider 2 */
          input  "",                       /* other provider */
          input  "",                       /* lawyer */
          input  "",                       /* general fax */
          input  "",                       /* general name */
          output notUsed).
    end.
    when 'email'    then do:
       lv-endfilename = OutputToFile(lv-uniq + 'temp.rep',pv-mrep,'local').
       If lv-endfilename = ?
       then do:
            lv-result =  'failed'.
            message 'There was a problem with the file Output' skip
                    'Please inform Technical Support'
            View-as alert-box error.
       end.
       /* find out to whom user wishes to fax */
       else
       run {&core}email.w ('','','','Report Output','Your Report is Attached',lv-endfilename).
    end.

    otherwise do:
        session:printer-name = pv-device no-error.
        if error-status:error or 
           Error-Status:Num-Messages ne 0 
        then do:
            message 'Invalid Printer ' + pv-device skip
                    'Please Reselect'
            view-as alert-box error buttons ok-cancel update lv-ok as log.
            if not lv-ok then return 'cancelled'.
            lv-prtflags = 1.
        end.
        run setdefaultprintera(pv-device,output lv-ret).
        lv-prtflags = 1. /* pop up systemdialog for copies filename etc */
        case printmemptr(lv-prtflags,lv-uniq,pv-mrep):
            when false then do:
                message pv-device ' Print Failed' view-as alert-box.
                lv-result =  'failed'.
            end.
            when ? then lv-result = 'cancelled'.
            when true 
            then do:
                lv-result = ''.
                pv-device = session:printer-name.
            end.
        end case.
        session:printer-name = lv-origprinter.
        run setdefaultprintera(lv-origprinter,output lv-ret).
    end.
end case.
if lv-prtlog then logmessage('End-ClientSide,' + lv-msgstr,'','End-ClientSide').

set-size(pv-mrep) = 0.

return lv-result.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Return-GetDataWithTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Return-GetDataWithTable Procedure 
PROCEDURE Return-GetDataWithTable :
def input param pv-mreport   as memptr no-undo.
def input param pv-mschema   as memptr no-undo.
def input param table-handle pv-ReturnTable. 
{{&base}releaseapsrv.i &Appsrv    = "System"}

if return-value ne 'completeok' then do:

end.

lv-device = entry(2,lv-printerparams,'{&Delim2}').

if valid-handle(lv-msghand) then do:
    menumsg('off').
    Message 'Your' lv-title  'report is complete and has been sent to' 
       (if lv-device = 'screen' or lv-device = '' then 
          'Notepad (see your taskbar below).' else lv-device + ".") 
       view-as alert-box title 'Complete'.
end.
else Sysmsg('off').
  
if stringtolog(getfield('printer','printertableid',
                         entry(1,lv-printerparams,'{&Delim2}'),
                        'local-printer'))
  
then do:
    run OutputToDevice (input-output lv-device,
                         pv-mreport,
                         lv-printerparams).
    if return-value = '' 
    then .
    /* message */
    /*    'Report' pv-program pv-procedure skip */
    /*    'Complete on Device' lv-device */
    /*    view-as alert-box title 'Complete'. */
    else do:
       if return-value ne 'cancelled' 
       then message return-value view-as alert-box title 'Output Return Value'.
       delete procedure this-procedure.
    end.
end.
else do:
    def var lv-prtlog as log no-undo.
    lv-prtlog = stringtolog(getctrl('UsePrintLog')).

    def var lv-prtlogfile as char no-undo.
    def var lv-msgstr as char no-undo.
    lv-msgstr = replace(program-name(5),' ',',') + ',' + 
                lv-device + ',' + 
                '' + ',' + 
                '' + ',' + 
                'serverside' + ',' + 
                session:printer-name.
    if session:remote 
    then lv-prtlogfile = "{&logs}" + string(today,'99999999') + 'serverprinter.log'.
    else lv-prtlogfile = "{&logs}" + string(today,'99999999') + 'clientprinter.log'.   
    if lv-prtlog then logmessage('ClientSide ,' + lv-msgstr,lv-prtlogfile,'ServerSidePrintReturn').
end.

if valid-handle(lv-calledfrom) 
then run ReportTableReturn in lv-calledfrom (pv-ReturnTable).

lv-printtable = pv-returntable.

delete procedure this-procedure.

eND PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RunJob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunJob Procedure 
PROCEDURE RunJob :
def input param pv-title as char no-undo.
def input  param pv-calledfrom    as handle no-undo.
def input  param pv-runasync      as log  no-undo.
def input  param pv-path          as char no-undo.
def input  param pv-program       as char no-undo.
def input  param pv-procedure     as char no-undo.
def input  param pv-printerparams as char no-undo.
def input  param pv-dpgmparams    as char no-undo.
def output param table-handle pv-printtable.
def input  param pv-progparams    as longchar no-undo.
def var lv-currjobs as int no-undo.
def var lv-maxjobs as int no-undo.

/* this is the form of lv-printerparams
lv-printerparams = string(t-printer.printertableid) + '{&Delim2}' +
                         t-printer.printer-name + '{&Delim2}' +
                         string(lv-copies) + '{&Delim2}' +
                         string(lv-tray) + '{&Delim2}' +
                         lv-filename + '{&Delim2}' + 
                         string(lv-batch) + '{&Delim2}' + 
                         lv-taskserver + '{&Delim2}'.
*/


set-size(lv-mreport) = 0.

assign
    lv-printerparams = pv-printerparams
    lv-calledfrom = pv-calledfrom
    lv-title = pv-title
    lv-copies = int(entry(3,pv-printerparams,'{&Delim2}')).
if pv-runasync 
then do:
lv-maxjobs = max(int(GetCtrl('{&AsyncMaxJobs}')),0).
lv-currjobs = max(int(GetCtrl('{&AsyncCurrentJobs}')),0).

    if (lv-currjobs + 1 > lv-maxjobs)
    then do:
        message msg(55,pv-title,'','','').
        delete procedure this-procedure.
        return 'cancelled'.
    end.
    run GetDataAsync(pv-title,pv-runasync,pv-path,
                          pv-program,
                          pv-procedure,
                          pv-printerparams,
                          pv-dpgmparams,
                          pv-progparams,
                          output lv-mreport,
                          output lv-mschema).
end.
else run GetData(pv-title,pv-runasync,pv-path,
                          pv-program,
                          pv-procedure,
                          pv-printerparams,
                          pv-dpgmparams,
                          pv-progparams,
                          output lv-mreport,
                          output lv-mschema).                          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-PrintFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PrintFile Procedure 
FUNCTION PrintFile RETURNS LOGICAL
  ( pv-flags    as int,
    pv-filename as char ) :

    def var lv-prtresult as int  no-undo.
    def var lv-fontnum   as char no-undo.
    def var lv-dlgparam as char no-undo.
    def var lv-err as char no-undo.
    assign
        file-info:file-name = pv-filename
        pv-filename         = file-info:full-pathname.

    /* uses proprint.dll (which will do font, but not control characters) */
    if stringtolog(getctrl('{&UseProPrint}')) 
    then do:
        lv-fontnum    = getctrl('{&PrinterFontNumber}').
        if lv-fontnum = '' then lv-fontnum = '11'.
        run proprintfile (session:printer-control-handle,
                          pv-flags,
                          current-window:hwnd,
                          int(lv-fontnum),
                          pv-filename,
                          0,
                          output lv-prtresult).
    end.
    else do:  /* instead do it longhand - will do control chars */
if stringtolog(getctrl("Dbgwapirawprint"))
then message 'Passed in ' skip 
             'Params : ' lv-dlgparam skip 
             'Copies : ' lv-copies {&dbt}.

        lv-dlgparam = SetNamedValue('copies',string(lv-copies),lv-dlgparam).      
        if pv-flags = 1   /* prompt for printer etc */
        then do:
            run proc-setopdest in lh-general (input-output lv-dlgparam). 
            if lv-dlgparam begins '**Failed' then return ?.
        end.             
        lv-copies = int(GetNamedValue('copies',lv-dlgparam)).
if stringtolog(getctrl("Dbgwapirawprint"))
then message 'About to Print Using' skip 
             'Params : ' lv-dlgparam skip 
             'Copies : ' lv-copies {&dbt}.
             
        lv-err = WapiRawPrint (session:printer-name, pv-filename,lv-copies).     
        if lv-err ne ''
        then message lv-err {&dbt}.
    end. 
    
    return true.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintMemptr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PrintMemptr Procedure 
FUNCTION PrintMemptr RETURNS LOGICAL
  ( pv-flags as int,
    pv-uniq as char,
    pv-mrep  as memptr ) :

   def var lv-file as char no-undo.

    lv-file = pv-uniq + 'mptr.rep'.

    lv-file = OutputToFile(lv-file,pv-mrep,'local').

   if lv-file begins '**' then do:
      message lv-file view-as alert-box error.
      return ?.
   end.

    return PrintFile(pv-flags,lv-file).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

