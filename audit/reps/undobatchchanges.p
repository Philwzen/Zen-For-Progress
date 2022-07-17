
FOR EACH Zen-AuditDetail
            WHERE Zen-AuditDetail.TableName  = 'zen-dmenu'
              AND Zen-AuditDetail.AUDitDate >= 01/21/2009
              and can-do('update',Zen-AuditDetail.auditaction) 
            NO-LOCK:
      FOR each Zen-Auditline Of Zen-AuditDetail 
            where zen-auditline.fieldname = 'run-users' 
             and zen-auditline.datachar ne '*' 
             no-lock:
        find zen-dmenu where zen-dmenutableid = int(zen-auditdetail.sourcekey)
        exclusive-lock.
          run-users = datachar.
      end.
end.      
