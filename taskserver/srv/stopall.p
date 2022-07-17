
/* close all background jobs
   and shutdown taskservers */
{app-paths.i}



touch('{&logs}vtxstop.tag').
find zen-control where zen-control.ctrl-idx = 'VtxMonitor'
                 exclusive-lock.
zen-control.ctrl-data = 'down'.

for each zen-tserver exclusive-lock:
   zen-tserver.started = no.
end.
