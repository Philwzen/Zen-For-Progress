 /* DOREPORT.I */
 
   FOR EACH t-data:
     DELETE t-data.
   END.
   ErrorClear().
   
   {{&core}run.i &programc  = lv-extrprog
                 &pathc     = lv-extrpath
                 &Appsrv    = "System"
                 &noper     = false
                 &procedure = "extractdata"
                 &params    = "(lv-print-type,
                                lv-params,
                                OUTPUT TABLE t-data)"}
  if return-value = 'passed' 
  then do:
      output stream op to value(lv-paramfile).
      put stream op unformatted lv-paramtitles skip lv-params skip.
      output stream op close.
      SetSchema (lv-extractdir,lv-datfile).
      do x = 1 to num-entries(lv-datfile):
        lv-opfile = lv-extractdir + entry(x,lv-datfile).
        output stream op to value(lv-opfile).
        for each t-data where t-data.tablename = entry(x,lv-datfile):
            put stream op unformatted t-data.data skip.
        end.
        output stream op close.
      end.
     x = 0.
     ASSIGN file-info:file-name = lv-cryfile
            lv-cryfile = file-info:FULL-PATHNAME NO-ERROR. 
      if search(lv-cryfile) = ? then return.
     
      /* #1 Complete */
      message Msg(164,"Report","","","") view-as alert-box.
      CAll-Crystal (lv-cryfile,lv-repname,"WINDOW") no-error.
      
      if error-status:error 
      then do:
         /* #1 Failed */
         message msg(165,Error-Status:Get-Message(Error-Status:Num-Messages),"","","") skip
                 Msg(165,"Crystal Report","","","")
         view-as alert-box.
         do x = 1 to num-entries(lv-datfile):
            lv-opfile = lv-extractdir + entry(x,lv-datfile).
            OS-DELETE VALUE(lv-opfile).
         end.         
         return 'failed'.
      END. /* if error */
  
      /* delete data files if we want to */ 
      if stringtolog(getctrl('AutoCleanupRepDataFiles')) then
      do x = 1 to num-entries(lv-datfile):
        lv-opfile = lv-extractdir + entry(x,lv-datfile).
     /* OS-DELETE VALUE(lv-opfile). */
      end.
      return 'passed'.
  end.
  else do:
    /* #1 Failed */
    message Msg(165,"Extract","","","") view-as alert-box.
    do x = 1 to num-entries(lv-datfile):
        lv-opfile = lv-extractdir + entry(x,lv-datfile).
     /*   OS-DELETE VALUE(lv-opfile). */
    end.
    return 'failed'.
  end.
