&if {&AsSuper} = 'true' &then
procedure loadsuper:
def input param pv-lib as char no-undo.
def var lh-lib as handle no-undo.
def var x as int no-undo.
    lh-lib = session:first-procedure.
    do while lh-lib <> ?:
        if lh-lib:private-data  = "LIBRARY-" + pv-lib 
        then do:
            if not can-do(session:super-procedures,string(lh-lib)) then do:
                session:ADD-SUPER-PROCEDURE(lh-lib).
                return.
            end.
        end.
        lh-lib = lh-lib:next-sibling.
    end.  

    do x = 1 to num-entries(session:super-procedures):
        lh-lib = widget-handle(entry(x,session:super-procedures)).
        if lh-lib:private-data  = "LIBRARY-" + pv-lib 
        then return.
    end.  
    run value('{&core}{&lib}' + pv-lib + 'library.p') persistent set lh-lib.  
    lh-lib:private-data = "LIBRARY-" + pv-lib.
    session:ADD-SUPER-PROCEDURE(lh-lib).
    return.
end procedure.
&endif
&if defined(library-zen) = 0 &then
    {{&core}load-library.i &LibraryName = "zen" &AsSuper = {&AsSuper}}
&endif
&if defined(library-general) = 0 &then
    {{&core}load-library.i &LibraryName = "general" &AsSuper = {&AsSuper}}
&endif
&if defined(library-cache) = 0 &then
    {{&core}load-library.i &LibraryName = "cache" &AsSuper = {&AsSuper}}
&endif 
&if defined(library-winapi) = 0 &then
    if opsys = 'win32' then do:
        {{&core}load-library.i &LibraryName = "winapi" &AsSuper = {&AsSuper}}
    end.
&endif
&if defined(library-office) = 0 &then
    if opsys = 'win32' then do:
        {{&core}load-library.i &LibraryName = "office" &AsSuper = {&AsSuper}}
    end.
&endif

&if defined(library-msoffice) = 0 &then
    if opsys = 'win32' then do:
        {{&core}load-library.i &LibraryName = "msoffice" &AsSuper = {&AsSuper}}
    end.
&endif
&if defined(library-validation) = 0 &then
    {{&core}load-library.i &LibraryName = "validation" &AsSuper = {&AsSuper}}
&endif

&if defined(library-sonic) = 0 &then               
    {{&core}load-library.i &LibraryName = "sonic" &AsSuper = {&AsSuper}}  
&endif 
/*
&if defined(sys) ne 0 &then
    {{&sys}loadlibraries.i &AsSuper = {&AsSuper}}
&endif
*/

