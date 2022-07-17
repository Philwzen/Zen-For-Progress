
&if Defined(bug) ne 0 &then
    def stream bug .
&endif

&if Defined(tree) ne 0 &then
    def stream tre .
    output stream tre to tree.txt append.
    put stream tre unformatted
	program-name(1) '*' skip
	'     ' program-name(2) '*' skip	
	'          ' program-name(3) '*' skip	
	'               ' program-name(4) '*' skip	
	'                    ' program-name(5) '*' skip	
	'                         ' program-name(6) skip.
    output stream tre close.
&endif
session:appl-alert-boxes = session:window-system ne 'tty'.

{{&core}winconst.i}      /* winapi constants */

{{&core}control.i}       /* preprocessor controls */

{{&core}loadlibraries.i &AsSuper = '{&LoadLibsAsSupers}'}
/* {&LoadLibsAsSupers} load up persistent libraries */

def var lv-exited    as log    no-undo.

&IF defined(library-program) = 0 &THEN
    &if defined(serverprogram) = 0 &then
      {{&core}reg.i} 
    &endif
&ENDIF
def var lv-auto as log no-undo.
def var lv-logfile as char no-undo.

