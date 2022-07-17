for each zen-auditconfig exclusive-lock:
 if not can-find(_file where zen-auditconfig.tablename = _file-name)
 then do:
    for each zen-auditfield of zen-auditconfig exclusive-lock:
        delete zen-auditfield.
    end.
    delete zen-auditconfig.
 end.   
end.
