/* build the menus for this procedure */
{{&core}bug.i "'menu begin'"}
&if defined(nomenu) = 0 &then
AttachMenu({&WINDOW-NAME}:HANDLE,frame {&FRAME-NAME}:HANDLE,this-procedure).
&endif
{{&core}bug.i "'menu done'"}