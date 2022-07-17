/* findLockCulprit.p
   Finds the user who is locking a record and causing others to wait. */

DEFINE TEMP-TABLE ttLocks NO-UNDO LIKE _Lock
  INDEX byRecid IS PRIMARY
    _Lock-RecId ASCENDING
    _Lock-Table ASCENDING.

DEFINE BUFFER culprit FOR ttLocks.

/* Access to _Lock can be painfully slow, so copy
     _Lock to an indexed temp-table first. */
FOR EACH _Lock WHILE _Lock._Lock-table <> ?:
  CREATE ttLocks.
  BUFFER-COPY _Lock TO ttLocks.
END.

FOR EACH ttLocks WHERE ttLocks._Lock-Flags MATCHES "*Q*":
  /* Do the FIND's with NO-ERROR, as the lock table is just a snapshot
       of VERY volatile data... */
  FIND FIRST culprit WHERE culprit._Lock-RecId = ttLocks._Lock-RecId
                       AND culprit._Lock-Table = ttLocks._Lock-Table
                       AND NOT culprit._Lock-Flags MATCHES "*Q*"
                           NO-ERROR.
  IF AVAILABLE culprit THEN DO WITH SIDE-LABELS TITLE " Users holding other users ":
 FIND _Connect WHERE _Connect._Connect-Usr = culprit._Lock-Usr NO-ERROR.
    IF AVAILABLE _Connect AND _Connect._Connect-TransId <> 0 THEN
      FIND _Trans WHERE _Trans._Trans-Usr = _Connect._Connect-Usr NO-ERROR.
    ELSE /* Ensure no _Trans record is available. */
      RELEASE _Trans NO-ERROR.
    FIND _File WHERE _File._File-num = culprit._Lock-Table NO-LOCK NO-ERROR.

    DISPLAY culprit._Lock-Usr   COLON 17
            culprit._Lock-Name  COLON 17
            _Connect._Connect-Device WHEN AVAILABLE _Connect LABEL "On"
            culprit._Lock-Table COLON 17 LABEL "Table" FORMAT "ZZ,ZZ9"
            _File._File-Name WHEN AVAILABLE _File NO-LABEL
            culprit._Lock-RecID COLON 17
            culprit._Lock-Type  COLON 17 LABEL "Lock type"
            culprit._Lock-Flags
            .
    IF AVAILABLE _Trans THEN
      DISPLAY _Trans._Trans-State  COLON 17
              _Trans._Trans-Txtime COLON 17 "Transaction start"
            .
    IF AVAILABLE _Connect THEN
      DISPLAY _Connect._Connect-Type COLON 17 LABEL "Client type"
              _Connect._Connect-Time COLON 17
              .
  END.
END.

