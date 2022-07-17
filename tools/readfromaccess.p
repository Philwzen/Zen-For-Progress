DEFINE VARIABLE DBPath AS CHARACTER  NO-UNDO.
DEFINE VARIABLE Access AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE Rs     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i      AS INTEGER    NO-UNDO.

/* database path */
DBPath = "C:\access2007\customer service.accdb".

/* Create instance of Microsoft Access */
CREATE "Access.Application" Access.
/* Open database */
Access:OpenCurrentDatabase(DBPath,TRUE,).

/* Get the table 'your table' in a recordset */ 
Rs = Access:CurrentDB:OpenRecordset("case").
/* Go to the first record of the table Test" */
Rs:MoveFirst().

/* iterate*/
DO i = 0 TO Rs:FIELDS:COUNT:
   message Rs:FIELDS(i):VALUE view-as alert-box.
   /* Get the next record of the table */ 
   Rs:MoveNext().
end.

/* Close the recordset */
Rs:CLOSE(). /* Close the database */

Access:closeCurrentDatabase(). /* Exit MS Access */
Access:QUIT(0).
/* Release COM-HANDLE */
RELEASE OBJECT Rs.
RELEASE OBJECT Access.
