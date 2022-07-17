/*
* Include file    : generatethekey.i
* Purpose         : Generate the key to be written to a tables key
*/


if can-find(first zen-control where zen-control.ctrl-idx = '{&AutoKeyGen}'
                                and zen-control.ctrl-data = 'UseSequence')
then {&TableName}.{&Unique-key} = NEXT-VALUE(NEXT-{&Unique-key}).
else do:
    DEF BUFFER btid-{&tablename} FOR {&tablename}.
    FIND LAST btid-{&tablename} where btid-{&tablename}.{&Unique-key} ne ?
              USE-INDEX tableid
             NO-LOCK NO-ERROR.
    {&tablename}.{&Unique-key} = IF AVAIL btid-{&tablename}
       THEN btid-{&tablename}.{&Unique-key} + 1
            else 1.
    if {&tablename}.{&Unique-key} = ? or
       {&tablename}.{&Unique-key} = 0
    then {&tablename}.{&Unique-key} = 1.
end.


