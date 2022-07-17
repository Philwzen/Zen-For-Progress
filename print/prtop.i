
/* second half of prtdefs.i 
   split so can be used later if we dont know page size */

/* has a &paged param been passsed in */
/* 1 = global define
   2 = passed param
   3 = scope define
   0 = not defined
*/
&if defined(paged) = 0 &then
lv-pagelength = if zen-Printer.PhysicalType ne 'xml' 
                then zen-PrinterForm.page-length
                else 0.
&scope paged paged page-size value(lv-pagelength)
&endif

output {&StreamName} to value(lv-scratchfile) {&paged}.
&undefine paged
if printer ne 'xml' then do:
    
   put {&StreamName} control zen-Printer.ResetFunc.
   
end.

