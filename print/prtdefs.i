/* prtdefs.i
receives common params for reports
of the form

1: printer control param as | delimited string
   Of the form    printername|copies|tray|filename|
2: io mempttr handle for extracted data
3: params from dpgm record as | delimited string
	any form you like .
4: params from local-print-trigger as | delimited string
 	any form you like

08/21/08 EKS, added logic to update the printer last activity date if it has
               been more than a week.
phil: fixed bad scoping and refind of last activity
*/

def input  param pv-asynch as log no-undo.
def input  param pv-printerparams as char   no-undo.
def output param pv-mreport   as memptr no-undo.
def output param pv-mschema   as memptr no-undo.
def input  param pv-dpgmparams    as char   no-undo.
def input  param pv-params        as longchar   no-undo.
def output param table-handle pv-ReturnTable. 

def var lv-currjobs as int no-undo.
def var lv-maxjobs as int no-undo.
lv-maxjobs = max(int(GetCtrl('{&AsyncMaxJobs}')),0).
if lv-maxjobs = 0 then pv-asynch = no.

set-size(pv-mreport) = 0.
set-size(pv-mschema) = 0.

/* vars for stuff we will need in all reports */

def var lv-pagelength as int no-undo.
def var lv-devicename  as char no-undo.
def var lv-deviceid    as dec  no-undo.
def var lv-numcopies   as int  no-undo.
def var lv-endfilename as char no-undo.
def var lv-scratchfile as char no-undo.
def var lv-tray        as char no-undo. /* L or U */
assign lv-deviceid    = dec(entry(1,pv-printerparams,'{&Delim2}'))
       lv-devicename  = entry(2,pv-printerparams,'{&Delim2}')
       lv-numcopies   = int(entry(3,pv-printerparams,'{&Delim2}'))
       lv-tray        = entry(4,pv-printerparams,'{&Delim2}')
       lv-endfilename = entry(5,pv-printerparams,'{&Delim2}').
if lv-numcopies = 0 then lv-numcopies = 1.

lv-scratchfile = getctrl("{&scratchpath}") +
                 getuserid() + 
                 string(today,'99-99-9999') + 
                 string(time) +  
                 '.rep'.

/* 
   lv-devicename will be set to either the printer to use or
   PgmProperty(pv-program-name,'ReportRunType')
   to 'XML' if it is a crystal report output. lv-deviceid 
   will be device id or 0 for crystal. so we need to 
   adjust what happens depending on the value of lv-devicename 
*/

if lv-devicename ne 'xml'
then do:
   if not can-find(zen-printer where zen-printer.zen-printertableid = lv-deviceid)
   then do:
       ErrorCreate(50,'Printer',lv-devicename,'','').
       return . 
   end.
end.

def var prt-descr as char no-undo.
def var printer   as char no-undo.
assign
   prt-descr     = entry(1,lv-scratchfile,'.')
   printer       = lv-devicename.

/* moveto include so can be referenced later if we dont know page size */
&if defined(noop) ne 2 &then
   /* paged argument is valued */
   &if defined(paged) eq 2 &then
      &if defined(streamName) eq 2 &then
         {{&base}{&prt}prtop.i &paged="{&paged}" &StreamName="{&streamName}"}
      &else
         {{&base}{&prt}prtop.i &paged="{&paged}"}
      &endif
   /* paged argument is blank */
   &else
      &if defined(streamName) = 2 &then
         {{&base}{&prt}prtop.i &StreamName="{&streamName}"}
      &else
         {{&base}{&prt}prtop.i}
      &endif
   &endif
&endif

{{&base}{&prt}setasynchcounter.i &ctr = 1}


