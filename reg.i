/* call the registration checker */
/*
&if defined(login) = 0 and 
    defined(library-program) = 0 
&then
def var h-tpro as handle no-undo.
	
h-tpro = session:first-procedure.
do while h-tpro <> ?:
    if h-tpro:private-data  = "tpro" then leave.
    h-tpro = h-tpro:next-sibling.
end.  
if h-tpro = ? then do:
    run {&core}tpro.w persist set h-tpro.
    h-tpro:private-data = "tpro".
end.
run initialise in h-tpro.
&else
&endif
*/