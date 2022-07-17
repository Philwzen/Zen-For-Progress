
/* return temp-table stuff */

&if defined(ReportReturnTable) ne 0 &THEN
   def var H-ReturnTable   as handle no-undo.
   
   create temp-table pv-ReturnTable.
   pv-ReturnTable:create-like('{&ReportReturnTable}').
   pv-ReturnTable:temp-table-prepare("tr-{&ReportReturnTable}").
   H-ReturnTable = pv-ReturnTable:default-buffer-handle.
   H-ReturnTable:empty-temp-table().

   for each {&ReportReturnTable}:
       H-ReturnTable:buffer-create().
       H-ReturnTable:buffer-copy(buffer {&ReportReturnTable}:handle).
   end.
&endif

output {&StreamName} close.

set-size(pv-mreport) = 0.
set-size(pv-mschema) = 0.
if printer = 'xml' then do:
   pv-mreport = InputFromFile(prt-descr + '.xml','local').
   pv-mschema = InputFromFile(prt-descr + '.xsd','local').
end.
else pv-mreport = InputFromFile({&filename},'local').

def var lv-prtlog as log no-undo.
def var lv-prtlogfile as char no-undo.
def var lv-msgstr as char no-undo.
lv-msgstr = replace(program-name(1),' ',',') + ',' + 
            lv-devicename + ',' + 
            string(pv-asynch) + ',' + 
            '{&StreamName}' + ',' + 
            lv-scratchfile + ',' + 
            prt-descr.

lv-prtlog = stringtolog(getctrl('UsePrintLog')).

/* if session:remote */
/* then lv-prtlogfile = "{&logs}" + string(today,'99999999') + 'serverprinter.log'. */
/* else lv-prtlogfile = "{&logs}" + string(today,'99999999') + 'clientprinter.log'. */
       
if avail zen-Printer then do: 
    if not zen-Printer.LocalPrinter
    then do:
       if lv-prtlog then logmessage('NonLocal-ServerPrint,' + lv-msgstr,'','ServerPrint').
       output through 
          lp value("-d" + lv-devicename + " -n" + string(lv-numcopies)) 
          binary no-convert.
       export pv-mreport.
       output close.
    end. /* not a local printer */
    else if lv-prtlog then logmessage('Local-ServerReturn,' + lv-msgstr,'','ClientServerReturn').
end.
else logmessage('No Printer Record,' + lv-msgstr,'','ServerError').

&if defined(nodelscratch) = 0 &then
   os-delete value({&filename}).
   os-delete value(prt-descr + '.xml').
   os-delete value(prt-descr + '.xsd').
&endif

{{&base}{&prt}setasynchcounter.i &ctr = -1}

return 'completeok'.
