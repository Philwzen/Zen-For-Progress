
/* load background for screen */
&if Defined(LoadBackground) ne 0 &then
LoadDefBackground("{&LoadBackground}",
                  frame {&frame-name}:handle,
                 {&window-name}:handle).
&endif

