define temp-table {&TTPrefix}t{&TTSuffix}-{&table-name} no-undo like {&table-name} 
&if '{&unique-key}' = 't-recid' &then 
field t-recid as recid init ?
&endif
 {&extra-fields}
 {&index}.
