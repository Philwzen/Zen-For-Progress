&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
&glob serverprogram true
/* ***************************  Definitions  ************************** */
{app-paths.i}

DEF VAR i         AS INT NO-UNDO.
DEF VAR lv-sched  AS CHAR NO-UNDO.
DEF VAR h-parser  AS HANDLE NO-UNDO.


DEF STREAM out-s.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 20.14
         WIDTH              = 50.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* {{&core}mainp.i}  */

    IF NOT VALID-HANDLE(h-parser) THEN
    RUN {&tsk}schedule-parser.p PERSISTENT SET h-parser.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-main-block) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE main-block Procedure 
PROCEDURE main-block :
DEF VAR vd-date          AS DATE NO-UNDO.
DEF VAR lv-hold-time     AS INT NO-UNDO.
DEF VAR lv-time          AS INT NO-UNDO.
DEF VAR lv-ended         AS LOG NO-UNDO.
DEF VAR lv-problem       AS LOG NO-UNDO.
DEF VAR lv-ending        AS LOG NO-UNDO.
DEF VAR lv-iterate       AS LOG NO-UNDO.
DEF VAR vd-date2         AS DATE NO-UNDO.
DEF VAR lv-time2         AS INT NO-UNDO.
DEF VAR lv-ended2        AS LOG NO-UNDO.
DEF VAR lv-problem2      AS LOG NO-UNDO.
DEF VAR lv-ending2       AS LOG NO-UNDO.
DEF VAR lv-iterate2      AS LOG NO-UNDO.
DEF VAR vd-hold-date     AS DATE NO-UNDO.
DEF VAR vd-hold-date2    AS DATE NO-UNDO.
DEF VAR done-end         AS LOG INIT NO NO-UNDO.
DEF VAR done-prob        AS LOG INIT NO NO-UNDO.
DEF VAR lv-converted     AS CHAR NO-UNDO.
DEF VAR n                AS INT NO-UNDO.
DEF VAR lv-day           AS CHAR EXTENT 7 NO-UNDO.
    
    assign lv-day[1]   = 'Sunday'
           lv-day[2]   = 'Monday'
           lv-day[3]   = 'Tuesday'
           lv-day[4]   = 'Wednesday'
           lv-day[5]   = 'Thursday'
           lv-day[6]   = 'Friday'
           lv-day[7]   = 'Saturday'.
    
    OUTPUT STREAM out-s TO 'c:\temp\schedule.txt'.

    put stream out-s unformatted 'SCHEDULE: ' '""' lv-sched '""' skip(2).
    PUT STREAM out-s 'DAY         DATE      TIME     EVER ENDING' SKIP(2).
    
    IF NOT VALID-HANDLE(h-parser) THEN
    RUN {&tsk}schedule-parser.p PERSISTENT SET h-parser.

    RUN REFRESH IN h-parser(lv-sched,
                            TODAY,
                            TIME,
                            NO,
                            OUTPUT lv-converted,
                            OUTPUT vd-date, 
                            OUTPUT lv-time, 
                            OUTPUT lv-ended, 
                            OUTPUT lv-problem, 
                            OUTPUT lv-ending,
                            OUTPUT lv-iterate).
    
    ASSIGN lv-hold-time = lv-time
           vd-hold-date = vd-date
           lv-sched     = lv-converted
           n            = 0.
    
    DO WHILE n < i:
       n = n + 1.
      /* first iteration output results date and time from second iteration */
      IF n MODULO 2 = 0 THEN DO:
         IF lv-ended2 AND NOT done-end THEN do: 
            PUT STREAM out-s skip(2) '************ Schedule Ends ***********' SKIP(2).
             done-end = YES.
         END.
         IF lv-problem2 AND NOT done-prob THEN DO:
            PUT STREAM out-s '************ Syntax Problems *********' SKIP(2).
             done-prob = YES.
         END.
    
         IF NOT done-end AND NOT done-prob THEN 
            PUT STREAM out-s UNFORMATTED 

         lv-day[WEEKDAY(vd-date2)] SPACE(10 - length(lv-day[WEEKDAY(vd-date2)]))
         vd-date2 '  ' STRING(lv-time2,'hh:mm am') '      ' lv-ending2 SKIP.
    
         IF n = i AND NOT (done-end OR done-prob) THEN 
         PUT STREAM out-s SKIP(2) '******* Schedule Forcasting Stopped After Calculating ' SKIP
                          n ' Tasks Ahead'.
      END.
      ELSE /* second iteration output results date and time from first iteration */
      DO:
         IF lv-ended AND NOT done-end THEN do: 
             PUT STREAM out-s skip(2) '************ Schedule Ends ***********' SKIP(2).
              done-end = YES.
         END.
         IF lv-problem AND NOT done-prob THEN DO:
            PUT STREAM out-s '************ Syntax Problems *********' SKIP(2).
             done-prob = YES.
         END.
    
         IF (n = 1 AND  (NOT lv-ended AND NOT lv-problem)) OR
            (n <> 1 AND (NOT done-end AND NOT done-prob)) THEN
         PUT STREAM out-s UNFORMATTED 
             lv-day[WEEKDAY(vd-date)] SPACE(10 - length(lv-day[WEEKDAY(vd-date)])) 
             vd-date '  ' STRING(lv-time,'hh:mm am') '      ' lv-ending SKIP.
    
         IF lv-sched = '' THEN  lv-ended2 = YES.
         IF n = i AND NOT (done-end OR done-prob) THEN 
         PUT STREAM out-s SKIP(2) '******* Schedule Forcasting Stopped After Calculating '
                          n ' Tasks Ahead'.
    
      END.
      
      
      /* first iteration input date and time from second iteration */
      IF n MODULO 2 = 0 THEN
      DO:
          IF lv-iterate2 THEN 
          do:
             
             IF lv-time2 = lv-hold-time THEN
              lv-time2 = lv-hold-time + 60.
    
         END.      
         ELSE  vd-date2   = vd-date2 + 1.
          
         IF NOT done-end AND NOT done-prob THEN
         DO:
            IF NOT VALID-HANDLE(h-parser) THEN
            RUN {&tsk}schedule-parser.p PERSISTENT SET h-parser.

            RUN REFRESH IN h-parser(lv-sched, vd-date2, lv-time2, yes, OUTPUT lv-converted, OUTPUT vd-date, 
                                        OUTPUT lv-time, OUTPUT lv-ended, OUTPUT lv-problem, OUTPUT lv-ending, 
                                        OUTPUT lv-iterate).
            
         END.

      END.
      ELSE
      /* second iteration input date and time from first iteration */
      DO:
          IF lv-iterate THEN 
          do:
             
             IF lv-time = lv-hold-time THEN
              lv-time = lv-hold-time + 60.
                
          END.
          ELSE  vd-date = vd-date + 1.
          
          IF NOT done-end AND NOT done-prob AND NOT lv-ended2 THEN
          DO:
             IF NOT VALID-HANDLE(h-parser) THEN
             RUN {&tsk}schedule-parser.p PERSISTENT SET h-parser.

             RUN REFRESH IN h-parser(lv-sched,vd-date, lv-time, yes, OUTPUT lv-converted, OUTPUT vd-date2, 
                                         OUTPUT lv-time2, OUTPUT lv-ended2, OUTPUT lv-problem2, OUTPUT lv-ending2, 
                                         OUTPUT lv-iterate2).
          END.
          IF vd-date2 < vd-date THEN  lv-ended2 = YES.
          /* in case they did not specify a date...
             This means that the date back out will be less than one put in
             this is the only case where this will happen 
             only need to check here because if no date then will end now... */
      END.
    END. /* 1 to n schedules */
    OUTPUT STREAM out-s CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Refresh) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh Procedure 
PROCEDURE Refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF input parameter i-i       AS INT  NO-UNDO.
  def input parameter v-c-sched as CHAR NO-UNDO.

  ASSIGN i = i-i
         lv-sched = v-c-sched.
  
  RUN main-block.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

