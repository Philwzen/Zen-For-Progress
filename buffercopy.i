
&if "{&unique-key}" = "t-recid" &then 
if not can-find(first t-{&table} where t-{&table}.t-recid = recid({&table})) then do:
	create t-{&table}.
	buffer-copy {&table} to t-{&table}.
t-{&table}.t-recid = recid({&table}).
end.
&else 
if not can-find(first t-{&table} where t-{&table}.{&uniquekey} = {&table}.{&uniquekey}) then do:
	create t-{&table}.
	buffer-copy {&table} to t-{&table}.
end.
&endif
