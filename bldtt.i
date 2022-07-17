empty temp-table t-{&table}.
for each {&table} {&where} no-lock:    
    create t-{&table}.
    buffer-copy {&table} {&except} to t-{&table} {&no-lobs}.
    {&extra}
end.
