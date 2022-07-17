    
&if defined(bug) ne 0 &then
    if can-do('{&bug}',entry(1,program-name(1),' ')) then do:
        if entry(1,'{&bug}',',') = 'Screen' 
        then message {1} {&dbt}.
        else do:
            output stream bug to 
                value('{&logs}' + entry(num-entries(unixpath(this-procedure:name),'/'),unixpath(this-procedure:name),'/') + 'debug.log')
            append.
            put stream bug unformatted 
                string(today,'99-99-9999') string(time) '#' {1} '#'
                program-name(1) '#' program-name(2) '#' program-name(3) '#' program-name(4) '#' program-name(5) skip.
            output stream bug close.
        end.
    end.
&endif
