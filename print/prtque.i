def var lv-printerparams as char no-undo.   
def var lv-dpgmparams as char no-undo.
def var lv-prog as char no-undo.
def var lv-proc as char no-undo.
def var lv-path as char no-undo.
def var lv-h as handle no-undo.
/* form of {&repinfo} from dpgm record
  lv-value = t-Zen-Dpgm.RepTitle + '{&Delim2}' + 
             t-Zen-Dpgm.reppath + "{&Delim2}" +
             t-Zen-Dpgm.repprog + "{&Delim2}" +
             t-Zen-Dpgm.repproc + "{&Delim2}" + 
             t-zen-dpgm.ExtractFilePath + '{&Delim2}' +
             t-Zen-Dpgm.ExtractFileName + '{&Delim2}' + 
             t-zen-dpgm.CrystalPath + '{&Delim2}' +
             t-Zen-Dpgm.CryFilename + '{&Delim2}' + 
             t-Zen-Dpgm.params.
*/

assign lv-path = entry(2,{&repinfo},"{&Delim2}")
       lv-prog = entry(3,{&repinfo},"{&Delim2}")
       lv-proc = entry(4,{&repinfo},"{&Delim2}")
       lv-dpgmparams = entry(9,{&repinfo},"{&Delim2}").

/*
message lv-path skip lv-prog skip lv-proc skip
   view-as alert-box info buttons OK.
*/

if lv-path = '' or lv-prog = '' or lv-proc = ''
/*  or FileNotFound(lv-path + '/' + lv-prog)  */
then do:    
    message msg(21,'Report',lv-path,lv-prog,lv-proc)
    view-as alert-box.
    return 'cancelled'.
end.
def var lv-currjobs as int no-undo.
def var lv-maxjobs as int no-undo.
if lv-asyncreports then do:
    lv-maxjobs = max(int(GetCtrl('{&AsyncMaxJobs}')),0).
    lv-currjobs = max(int(GetCtrl('{&AsyncCurrentJobs}')),0).
    if (lv-currjobs + 1 > lv-maxjobs)
    then do:
        message msg(55,trim(entry(1,{&repinfo},"{&Delim2}")),'','','').
        return 'cancelled'.
    end.
end.

run {&core}{&prt}printerselect.w 
     (input this-procedure,output lv-printerparams).

if lv-printerparams = 'none' 
then return 'cancelled'.

/* this is the form of lv-printerparams
lv-printerparams = string(t-printer.printertableid) + '{&Delim2}' +
                         t-printer.printer-name + '{&Delim2}' +
                         string(lv-copies) + '{&Delim2}' +
                         string(lv-tray) + '{&Delim2}' +
                         lv-filename + '{&Delim2}' + 
                         string(lv-batch) + '{&Delim2}' + 
                         lv-taskserver + '{&Delim2}'.
*/

if stringtolog(entry(6,lv-printerparams,'{&delim2}'))
then do: /* its a batch job */
  run {&core}{&prt}submitprintjob.w (lv-path,
                                    lv-prog,
                                    lv-proc,
                                    lv-printerparams,
                                    lv-dpgmparams,
                                    {&params}).
      if return-value = 'failed' 
      then return return-value.
      
/*     errorclear().                                    */
/*     {{&base}run.i &program   = "printbatch.p"        */
/*                    &procedure = "Quejob"             */
/*                    &path      = "{&core}{&prt}"    */
/*                    &Appsrv    = "System"             */
/*                    &params    = "(lv-path,           */
/*                                   lv-prog,           */
/*                                   lv-proc,           */
/*                                   lv-printerparams,  */
/*                                   lv-dpgmparams,     */
/*               output table-handle lv-printtable,     */
/*                                   {&params})"}       */
/*                                                      */
/*     if anyerrors() then return 'failed'.             */
end. 
else do: /* its an online print */
/*    if filenotfound(lv-path + lv-prog)                     */
/*    then do:                                               */
/*       message msg(21,'Program ',lv-path + lv-prog,'','')  */
/*       view-as alert-box.                                  */
/*       return 'cancelled'.                                 */
/*    end.   */
/*    lv-h = getprochandle('local',unixpath('{&core}{&prt}prtque.p')). */
/*    if not valid-handle(lv-h) */
/*    then */
   run {&core}{&prt}prtque.p persist set lv-h.
    
    if valid-handle(lv-h) 
    then do:
           run runjob in lv-h (entry(1,{&repinfo},"{&Delim2}"),
                            this-procedure,
                            lv-asyncreports,
                            lv-path,
                            lv-prog,
                            lv-proc,
                            lv-printerparams,
                            lv-dpgmparams,
                            output table-handle lv-printtable,
                            {&params}).
           if return-value = 'cancelled' 
            then return 'cancelled'.
    end.
    else do:
        message msg(21,'Program ','prtque.p','','') 
        view-as alert-box.                                 
        return 'cancelled'.                                
    end.                    			      
end.                  			      
                  			      
