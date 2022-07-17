
&glob db schadm

{app-paths.i}

/* for each {&db}._sequence exclusive-lock:  */
/*    delete {&db}._sequence.                */
/* end.                                      */

FOR EACH {&db}._File WHERE _FIle-num > 0 
                       and _file-num < 32000
                     NO-LOCK:

    run create-seq({&db}._file._file-name).

END.

procedure create-seq:
   def input param pv-table as char no-undo.

   Def Var h-qry   As Handle No-undo.
   Def Var b-data  As Handle No-undo.
   def var h-field as handle no-undo.
   def var y       as int    no-undo.
   If Valid-handle(h-QrY) Then h-QrY:QUERY-CLOSE().
   If Valid-handle(h-QrY) Then DELETE OBJECT h-QrY no-error.
   Create Query h-QrY.
   Create Buffer b-data For Table pv-table.
   If Valid-handle(h-QrY) Then h-QrY:Add-buffer(b-data). 
   
   h-QRY:QUERY-PREPARE('FOR EACH {&db}.' + pv-table + ' no-lock by ' + pv-table + 'tableid ').
   h-QRY:QUERY-OPEN.
   h-QRY:GET-LAST().
  
   if not h-qry:query-off-end then do:
      h-field = b-data:buffer-field(pv-table + 'tableid').
      y = h-field:buffer-value.
      if y = ? then y = 0.
   end.

  dynamic-current-VALUE('next-' + pv-table  + 'tableid','{&db}') = y + 1.

/* disp pv-table format 'x(30)' {&db}._sequence._seq-init with frame a width 132 down.  */
   If Valid-handle(h-QrY) Then    
           DELETE OBJECT h-QrY.  
end procedure.
