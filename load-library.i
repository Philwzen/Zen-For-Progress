/* Load up the passed in library */
&if {&AsSuper} = 'true' &then
    run loadsuper ("{&LibraryName}").
    {{&core}{&lib}{&LibraryName}-functions.i &lhandle = super} 
&else
    def var lh-{&LibraryName} as handle no-undo.
    lh-{&LibraryName} = session:first-procedure.
    do while lh-{&LibraryName} <> ?:
        if lh-{&LibraryName}:private-data  = "LIBRARY-{&LibraryName}"
        then leave.
        lh-{&LibraryName} = lh-{&LibraryName}:next-sibling.
    end.  
    if not valid-handle(lh-{&LibraryName})
    then run value('{&core}{&lib}{&LibraryName}' + 'library.p') persistent set lh-{&LibraryName}.  
    lh-{&LibraryName}:private-data = "LIBRARY-{&LibraryName}".

    {{&core}{&lib}{&LibraryName}-functions.i &lhandle = lh-{&LibraryName}} 
&endif
