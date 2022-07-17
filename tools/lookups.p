output to \rex\codezen\tools\op.txt.
def buffer b-test for zen-lookupfld.


for each zen-lookupfld no-lock
                       by zen-lookupfld.zen-fldlooktableid 
                       by zen-lookupfld.fieldname:
find zen-fldlook of zen-lookupfld no-lock no-error.
    for each b-test where b-test.fieldname = zen-lookupfld.fieldname 
                and b-test.zen-fldlooktableid = zen-lookupfld.zen-fldlooktableid
and b-test.extentnum = zen-lookupfld.extentnum
and b-test.zen-lookupfldtableid ne zen-lookupfld.zen-lookupfldtableid no-lock:
        disp 'Duplicate ' 
        if avail zen-fldlook then zen-fldlook.lookupname else 'orphan'
        if avail zen-fldlook then zen-fldlook.tablename  else 'record'
                  zen-lookupfld.fieldname
        zen-lookupfld.zen-fldlooktableid
        zen-lookupfld.zen-lookupfldtableid with width 255.
    End.
End.

