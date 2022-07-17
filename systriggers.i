&if defined(library-program) = 0 &then
    &if defined(serverprogram) = 0 &then
       if '{&window-system}' ne 'tty' 
       then do:
         {{&core}revert.i} 
      	on ctrl-r     of frame {&frame-name} anywhere run {&core}sessioninfo.w persistent.  
      	on ctrl-f     of frame {&frame-name} anywhere WidInfo().  
      	on ctrl-alt-r of frame {&frame-name} anywhere run {&core}runner.w.
      	on ctrl-alt-c of frame {&frame-name} anywhere runchild('{&core}contextmnt.w',widget-handle(current-window:private-data)).
      	ON ctrl-s     of frame {&frame-name} anywhere SpellCheck(). 
      	on ctrl-d     of frame {&frame-name} anywhere run widget-help in lh-zen ('info','') /*WidgetInfo() */ .
      	on ctrl-a     of frame {&frame-name} anywhere DispConnections().
      	on ctrl-alt-d of frame {&frame-name} anywhere run call-SetWidgetDefaults (widget-handle(current-window:private-data)).
      	ON ctrl-shift-p of frame {&frame-name} anywhere run ProgramInfo in widget-handle(current-window:private-data) no-error. 
      	on 'mouse-select-dblclick' of frame {&frame-name} anywhere run proc-wlook in lh-zen.
       on ctrl-alt-p of frame {&frame-name} anywhere RefreshTempTables().   
      	/* map function keys to library routine so we can program them via data table */
      	on f1,f2,f3,f4,f5,f7,f8,f9,f10,f11,f12,
      	   ctrl-f1,ctrl-f2,ctrl-f3,ctrl-f4,ctrl-f5,ctrl-f6,ctrl-f7,ctrl-f8,ctrl-f9,ctrl-f10,ctrl-f11,ctrl-f12,
      	   alt-f1,alt-f2,alt-f3,alt-f5,alt-f6,alt-f7,alt-f8,alt-f9,alt-f10,alt-f11,alt-f12,
      	   shift-f1,shift-f2,shift-f3,shift-f4,shift-f5,shift-f6,shift-f7,shift-f8,shift-f9,shift-f10,shift-f11,shift-f12
      	   /* of frame {&frame-name} */ anywhere run proc-fkey in lh-zen (widget-handle(current-window:private-data)).
	   end. 
   &endif.
&endif
/*    on "MOUSE-MENU-CLICK":U    of frame {&frame-name} anywhere:handle run help-trigger in frame {&frame-name}:handle no-error.  */
