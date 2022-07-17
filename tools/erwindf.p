def stream op.
output stream op to c:\temp\erwin.df.
def var lv-ord as int no-undo.                        
def var lv-name as char no-undo.     
for each _file where _file-num > 0 
                 and _file-num < 32000 
                 and not _file-name begins 'zen'
               no-lock:
lv-name = _file._file-name + 'TableId'.
   find last _field of _file no-lock.
   lv-ord = _field._order + 1.
    put stream op unformatted 'ADD FIELD ' lv-name ' of "' _file._file-name '" AS Decimal' skip.
    put stream op unformatted "  decimals 0" skip.
    put stream op unformatted '  INITIAL "?"' skip.
    put stream op unformatted '  ORDER ' lv-ord skip(1).
    put stream op unformatted  'ADD INDEX TableId on "' _file._file-name '"' skip.
    put stream op unformatted  '  UNIQUE' skip.
    put stream op unformatted '  INDEX-FIELD "' lv-name '" ASCENDING' skip.
    put stream op skip(1).
end.
