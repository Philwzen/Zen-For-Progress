
/* 04/18/2009 'gvtknot' */
for each zen-auditdetail where zen-auditdetail.auditdate <= 04/18/2009
                           and zen-auditdetail.byname = 'gvtknot'
                         no-lock:
    run deleterecords (rowid(zen-auditdetail)).
end.

/* 09/03/2009 'jsulliva' 'acct,apay,arprov,browse-id,clgf01,diag-notes,prodiag,security-label' */       
for each zen-auditdetail where zen-auditdetail.auditdate <= 09/03/2009
                           and zen-auditdetail.byname = 'jsulliva'
                           and can-do('acct,apay,arprov,browse-id,clgf01,diag-notes,prodiag,security-label',zen-auditdetail.tablename)
                         no-lock:
    run deleterecords (rowid(zen-auditdetail)).
end.

/* prodiag */
etime(true).
for each zen-auditdetail where zen-auditdetail.tablename = 'prodiag'
                         no-lock:
    run deleterecords (rowid(zen-auditdetail)).
end.

procedure deleterecords:
    def input param pv-rowid as rowid no-undo.
    def buffer b-detail for zen-auditdetail.
    find b-detail where rowid(b-detail) = pv-rowid exclusive-lock.
   
    for each zen-auditline of b-detail exclusive-lock:
        delete zen-auditline.
    end. 
    delete b-detail.
end procedure.
