&if defined(library-crystal) = 0 &then 
    if NOT session:remote then do:
        {{&core}load-library.i &LibraryName = "crystal" &AsSuper = 'False'}
    end.
&endif
