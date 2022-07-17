&GLOBAL-DEFINE removelist 'faxphone,dentered,dchanged,lchgdt,sys-cd,practice,acct-no,pt-num,logonid,edate,dentered,lchgct,dchanged'

DEF VAR X AS INT NO-UNDO.

FOR EACH zen-auditfield WHERE can-do({&removelist},zen-auditfield.fieldname) 
    EXCLUSIVE-LOCK:
    X = X + 1.
    DELETE zen-auditfield.
END.
MESSAGE 'DELETEd ' X.
