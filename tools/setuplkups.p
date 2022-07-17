def var x as int no-undo.
def var fld as char no-undo.
def var y as dec no-undo.
for each zen-lookupfld:
    delete zen-lookupfld.
end.
for each zen-fldlook:
        
    do x = 1 to num-entries(zen-fldlook.displayfields):
        fld = entry(x,zen-fldlook.displayfields).
        y = y + 1.
        create zen-lookupfld.
        zen-lookupfld.zen-lookupfldtableid = y.
        zen-lookupfld.zen-fldlooktableid = zen-fldlook.zen-fldlooktableid.
        zen-lookupfld.fieldname  = fld.
        if zen-fldlook.descfield = fld then zen-lookupfld.DescField = true.
        if zen-fldlook.keyfield = fld then zen-lookupfld.KeyField = true.
        if zen-fldlook.searchfield = fld then zen-lookupfld.SearchField = true.
    /*      zen-lookupfld.FieldFormat 
            zen-lookupfld.FieldLabel     
    */
    end.
end.
