
if pv-asynch and session:remote
then do:
    /*  user job count */
    lv-currjobs = max(int(GetSysVar('{&AsyncCurrentJobs}')),0).
    if lv-currjobs = ? then lv-currjobs = 0.
    lv-currjobs = lv-currjobs + {&ctr}.
    SetSysVar('{&AsyncCurrentJobs}',string(max(lv-currjobs,0))).
    
    /* system job count */
/*     lv-currjobs = max(int(GetCtrl('{&AsyncCurrentJobs}')),0). */
/*     if lv-currjobs = ? then lv-currjobs = 0. */
/*     LogMessage(program-name(1) + ' SystemCount will change by {&ctr} From ' + string(lv-currjobs),'','yes'). */
/*     lv-currjobs = lv-currjobs  + {&ctr}. */
/*     SetCtrl('{&AsyncCurrentJobs}',string(max(lv-currjobs,0))). */
    
    lv-currjobs = CtrlCounter('{&AsyncCurrentJobs}',{&ctr},no).
    LogMessage(program-name(1) + ' SystemCount changed by {&ctr} to ' + string(lv-currjobs),'','SetAsynchCounter').

end.

