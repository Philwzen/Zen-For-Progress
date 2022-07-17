&if '{&unique-key}' ne 't-recid' &then
DEF BUFFER btid-{&tb} FOR {&tb}.    
    
    FIND LAST btid-{&tb} where btid-{&tb}.{&tb}tableid ne ? 
				  USE-INDEX tableid
	 		 	 NO-LOCK NO-ERROR.
    {&tb}.{&tb}tableid = IF AVAIL btid-{&tb} 
	    THEN btid-{&tb}.{&tb}tableid + 1
            else 1.
    if {&tb}.{&tb}tableid = ? or
       {&tb}.{&tb}tableid = 0
    then {&tb}.{&tb}tableid = 1.
&endif
