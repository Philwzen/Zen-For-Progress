{app-paths.i}

def stream op.

procedure procnoparam:
def var x as int no-undo.

output stream op to '{&logs}test-op.log' append.


do  x = 1 to 10:
   put unformatted string(time,'hh:mm:ss') + program-name(1) + ' this will be in screen log ' + string(x) skip.
   put stream op unformatted string(time,'hh:mm:ss') + program-name(1) + ' this will be in logs log ' + string(x) skip.
   wait(5000).
end.
end procedure.


procedure procparam:
def input param pv-char as char no-undo.
def var x as int no-undo.

output stream op to '{&logs}test-op.log' append.


do  x = 1 to 10:
   put unformatted string(time,'hh:mm:ss') + program-name(1) + ' this will be in screen log ' + pv-char + string(x) skip.
   put stream op unformatted string(time,'hh:mm:ss') + program-name(1) + ' this will be in logs log ' + pv-char + string(x) skip.
   wait(5000).
end.
end procedure.
