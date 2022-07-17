def buffer b-test for zen-lookupfld.
def var lv-id as dec no-undo.

for each b-test where b-test.zen-fldlooktableid = ? 
                exclusive-lock:
   delete b-test.
end.
find zen-fldlook where zen-fldlook.lookupname = 'accountsbyacct#' 
                 no-lock no-error.
if not avail zen-fldlook then do:
   message 'no to record' view-as alert-box.
   return.
end.
lv-id = zen-fldlook.zen-fldlooktableid.
for each b-test where b-test.zen-fldlooktableid = lv-id exclusive-lock:
   delete b-test.
end.

find zen-fldlook where zen-fldlook.lookupname = 'accountstest' 
                 no-lock no-error.
if not avail zen-fldlook then do:
   message 'no from record' view-as alert-box.
   return.
end.

for each zen-lookupfld of zen-fldlook 
         no-lock:
   create b-test.
   buffer-copy zen-lookupfld except tstamp zen-fldlooktableid zen-lookupfldtableid 
          to b-test.
   b-test.zen-fldlooktableid = lv-id.
End.

