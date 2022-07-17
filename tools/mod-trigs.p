
for each _file where _file-name begins 'zen' no-lock:
    for each _field of _file no-lock:
    end.
    for each t-trig:
        update t-trig._proc-name format 'x(70)'.
        create _file-trig.
        buffer-copy t-trig to _file-trig.
        delete t-trig.
    end.
end.
