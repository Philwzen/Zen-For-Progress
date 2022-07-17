{app-paths.i}

def stream op.

output stream op to '{&logs}test-op.log' append.

put stream op unformatted 
      string(today,'99/99/9999') + ' ' + 
      string(time,'hh:mm:ss') + ' ' +
      program-name(1) + 
      ' Executed ' skip.
