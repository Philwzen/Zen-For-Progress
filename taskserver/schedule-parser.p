&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/**************************************************************************************** SYNTAX
     many different keywords can be passed into this string
     valid 'keywords' must be seperated by a pipe lv-seperator and consist of (where ^ is lv-delimiter):-
        S-DATES d^d^d (as dates eg. 01/01/01 - ANY format)                               <S-DATES>
        DAYS    n^n^n                                                                   <THESE-DAYS>
        WEEKLY  MONDAY^TUESDAY^WEDNESDAY^THURSDAY^FRIDAY^SATURDAY^SUNDAY                 <WEEK-DAY>
                EVERYDAY^WEEKDAYS^WEEKENDS                                                <GROUP>
        eg. s-dates 01/01/01^02/02/02{&Delim2}days 1^2^3{&Delim2}weekly monday^wednesday^weekends
        
        FIRST                                                                           /* GROUP 2 */
        or                                                                                 <F/L>
        LAST    DAY^MONDAY^TUESDAY^WEDNESDAY^THURSDAY^FRIDAY^SATURDAY^SUNDAY       <DAY> and/or <WEEK-DAY>
        eg. first day^tuesday{&Delim2}last saturday^sunday                                         
                                                                                        /* GROUP 3 */
        S-TIME  t^        (as a time 24 hr eg. 14:04 )                                    <S-TIME>  
                MINUTES^n (where n is an integer value 1 = 1 minute)                      <MINUTES> 
        eg. s-time 12:34 or s-time 12:34^minutes^2                                        
                                                                                           AND/OR
        ENDING  DATE^                                                                <ENDING> <E-DATE> 
                TIME                                                                          <E-TIME>
        eg. ending   or  ending 01/01/01    or   ending 01/01/01^12:34
        
     The logic for this string is as follows:-
            **NB: all group 1 and 2 logic except <S-DATES> will keep re-submitting tasks unless group 4 logic is 
                  specified or the task is physhically deleted. 
      **GROUP 1
        If <S-DATES> is passed in then the task will begin on those date/dates.
        If <S-DATES> is NOT passed in and no other group 1 or group 2 logic 
        is used the task is set for 'lv-hold-date'.
        If <THESE-DAYS> is used eg. 01^04^22 DAYS then the task will start on <THESE-DAYS> throughout the month.
        If a <WEEK-DAY> of the week is specified then the process will run EVERY <WEEK-DAY>.
        If a <GROUP> is specified the task will run for that <GROUP>.
      **GROUP 2  
        If <F/L> is used then it MUST be used in conjunction with EITHER <DAY> OR <WEEK-DAY>.
        The task will then get run on the first or last <day> and/or <weekday> of the month.
      **GROUP 3        
        If <S-TIME> is passed in the task will begin at the TIME specified.
        It may be used in conjunction with ANY group 1 or group 2 logic specified.
        If <S-TIME> is NOT passed in the task will begin at 12:00am 'lv-hold-date'.
        <S-TIME> is 24hr time with no seconds 
        If the <MINUTES> keyword is specified it MUST be followed with an integer value and MUST be used
        in conjunction with <S-TIME>.
        This represents the number of minutes to repeat the task from <S-TIME>.
      **GROUP 4
        If the <ENDING> keyword is specified then <E-DATE> and/or <E-TIME> CAN be specified afterwards,
        the task will then stop at this <E-DATE> and/or <E-TIME> that has been specified.
        If only the <E-DATE> is specified the task will end at 11:59pm on that day.
        If only the <E-TIME> is specified the task will end at that time on that day
        If neither is specified it assumes you wish to end on this day at 11:59pm.
   ** NB: THIS LIBRARY ALSO HANDLES THE ORIGINAL SYNTAX OF THE SUBMIT TIME FIELD WHICH IS
          A DATE AND OR TIME IN THE FOLLOWING FORMAT:- 99/99/99 OR 99/99/9999  AND/OR HH:MM:SS
          IF BOTH FIELDS ARE SPECIFIED THEY ARE SEPERATED BY A SPACE:- DATE TIME
          THIS 'OLD' SYNTAX CANNOT BE USED IN CONJUNCTION WITH ANY OF THE NEW SYNTAX.
          IT MUST BE USED ON ITS OWN.
*****************************************************************************************************************/
/* ***************************  Definitions  ************************** */
&glob serverprogram true
{app-paths.i}

def var lv-schedule   as char    no-undo. /* the schedule line */
def var lv-hold-date  as date    no-undo. /* date and time to look forward from */
def var lv-hold-time  as int     no-undo.
DEF var lv-screen     as log NO-UNDO. /* convert new-sched-line to screen format OR
                                             convert to record format */
DEF var lv-new-sched  AS CHAR    NO-UNDO. /* schedule line to screen or record format */
def var lv-run-date   as date    no-undo. /* next run date and time */
def var lv-run-time   as int no-undo. 
def var lv-ended      as log no-undo. /* has the schedule ended */ 
def var lv-problem    as log no-undo. /* problem interpriting schedule line */
def var lv-ending     as log no-undo. /* will this schedule end itself? */
DEF var lv-iterate    as log NO-UNDO. /* is there a time iteration ncrease input time */

/*********** NORMAL VARIABLES ******************/

DEF VAR lv-delimiter  AS CHAR INIT '^' NO-UNDO. /* delimiter for the schedule string */
DEF VAR lv-seperator  AS CHAR INIT '{&Delim2}' NO-UNDO. /* seperator for the schedule string */
def var lv-day        as char extent 7 no-undo. /* holds days of the week sun -> Sat */
DEF VAR lv-d-format   AS CHAR          NO-UNDO. /* holds current session date format */
def var lv-s-pos      as int           no-undo. /* start string pointer */
def var lv-e-pos      as int           no-undo. /* end string pointer */
def var lv-pos        as int           no-undo. /* general position pointer */
def var lv-n          as int           no-undo. /* general counter */
def var lv-counter    as int           no-undo. /* general counter */
DEF VAR lv-days       AS INT           NO-UNDO. /* earliest day in this month to run from <these-days> keyword */
DEF VAR lv-hold       AS CHAR          NO-UNDO. /* general hold character variable */
DEF VAR lv-h-var      AS CHAR          NO-UNDO. /* general hold char var */
def var vd-date       as date          no-undo. /* general date hold variable */
DEF VAR lv-section    AS INT           NO-UNDO. /* section of schedule */
DEF VAR vd-sched-date AS DATE          NO-UNDO. /* holds the next date task should be scheduled for */
DEF VAR lv-sched-time AS INT           NO-UNDO. /* holds the time of the next task to run */
DEF VAR lv-any-key    as log       NO-UNDO. /* any date logic found at all */
DEF VAR vd-end-date   AS DATE          NO-UNDO. /* hold variable for the end date */
DEF VAR lv-end-time   AS INT           NO-UNDO. /* hold variable for the end time */
DEF VAR lv-got-time   AS LOG INIT NO   NO-UNDO. /* is there a start time */
DEF VAR lv-got-date   AS LOG INIT NO   NO-UNDO. /* any dates specified in string */
DEF VAR lv-old-sched  AS LOG INIT NO   NO-UNDO. /* is it an old schedule syntax */

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


{{&core}mainp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Check-Date) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Check-Date Procedure 
PROCEDURE Check-Date :
DEFINE INPUT-OUTPUT PARAMETER lv-date    AS CHAR.    /* date read in from schedule */
       DEFINE INPUT        PARAMETER lv-screen  as log. /* format into yyyy/mm/dd for storage */
       DEFINE OUTPUT       PARAMETER lv-error   as log. /* syntax problems */
       DEFINE OUTPUT       PARAMETER vd-date    AS DATE.    /* date value */
       
       def var lv-i AS INT NO-UNDO.

       ASSIGN lv-error = NO.

       /* check the whole date is either numbers and / first
          reason being that progress never fails a date if the year is invalid!
          It takes the first valid number(s) it can find in they year pos. and guesses after that
          if there is no valid year at all 0 is assumed thus 2000 is assumed...
          possible progress bug???????????
       */

       DO lv-i = 1 TO LENGTH(lv-date):
          lv-counter = integer(SUBSTRING(lv-date,lv-i,1)) NO-ERROR.
          IF ERROR-STATUS:ERROR AND SUBSTRING(lv-date,lv-i,1) <> '/' THEN
          lv-error = YES.
       END.
       
       IF lv-error THEN RETURN.
       /* use the format dates are stored in if converting to users screen format */
       IF lv-screen THEN SESSION:DATE-FORMAT = 'ymd'.
                    ELSE SESSION:DATE-FORMAT = lv-d-format.
       
       vd-date = date(MONTH(date(lv-date)),DAY(date(lv-date)),YEAR(date(lv-date))) NO-ERROR.
   
       if error-status:ERROR OR vd-date = ? 
       THEN lv-error = YES.
                  
       IF lv-error THEN RETURN.
       
       IF lv-screen THEN DO:
          IF substring(lv-d-format,1,1) <> 'y' THEN
          ASSIGN session:DATE-FORMAT = lv-d-format
                 lv-date = string(vd-date,'99/99/9999').
       END.
       ELSE
       ASSIGN lv-date = STRING(YEAR(date(lv-date)),'9999') + '/' + 
                        STRING(MONTH(date(lv-date)),'99')  + '/' + 
                        STRING(DAY(date(lv-date)),'99').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Check-Old-Time) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Check-Old-Time Procedure 
PROCEDURE Check-Old-Time :
DEFINE INPUT-OUTPUT PARAMETER lv-sched   AS CHAR    NO-UNDO.
      DEFINE OUTPUT       PARAMETER lv-problem as log NO-UNDO.
      DEFINE OUTPUT       PARAMETER lv-date    AS CHAR    NO-UNDO.
      DEFINE OUTPUT       PARAMETER lv-time    AS INT     NO-UNDO.

      def var lv-x AS INT NO-UNDO.

      ASSIGN lv-time = -1
             lv-date = ?.

      IF INDEX(lv-sched,' ') > 0 THEN
        DO lv-x = 1 TO 2:
            IF index(ENTRY(lv-x,lv-sched,' '),'/') > 0 THEN
            DO: /* date validation */
               ASSIGN lv-h-var = ENTRY(lv-x,lv-sched,' ').
               RUN CHECK-date(INPUT-OUTPUT lv-h-var,lv-screen , OUTPUT lv-problem, OUTPUT lv-date).
               IF lv-problem THEN RETURN.
               ASSIGN ENTRY(lv-x,lv-sched,' ') = lv-h-var.
            END. 
            ELSE IF INDEX(ENTRY(lv-x,lv-sched,' '),':') > 0 THEN
            DO:
               RUN CHECK-time(ENTRY(lv-x,lv-sched,' '),YES , OUTPUT lv-problem, OUTPUT lv-time).
               IF lv-problem THEN RETURN.
            END. /* time validation */
            ELSE ASSIGN lv-problem = YES.
            IF lv-problem THEN RETURN.
         END. /* 1 to 2 entries */
      ELSE DO: /* date OR time */
          IF INDEX(lv-sched,'/') > 0 THEN DO: /* date val */
             RUN CHECK-date(INPUT-OUTPUT lv-sched,lv-screen , OUTPUT lv-problem, OUTPUT lv-date).
             IF lv-problem THEN RETURN.
          END. /* date val */
          ELSE IF INDEX(lv-sched,':') > 0 THEN DO:  /* time val */
              RUN CHECK-time(lv-sched,YES , OUTPUT lv-problem, OUTPUT lv-time).
              IF lv-problem THEN RETURN.
          END. /* time val */
          ELSE ASSIGN lv-problem = YES.
          IF lv-problem THEN RETURN.
      END. /* date OR time */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Check-Schedule-Syntax) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Check-Schedule-Syntax Procedure 
PROCEDURE Check-Schedule-Syntax :
/* This procedure not only checks the syntax - but also converts dates to yyyy/mm/dd format
            It also lets us know if the schedule will ever end, and if there is ANY keywords at all -
            if there is no keywords then we check the old syntax to see if that is correct after this proc. */

         DEFINE INPUT  PARAMETER lv-schedule AS CHAR    NO-UNDO.
         DEFINE OUTPUT PARAMETER lv-problem  as log NO-UNDO.
         DEFINE OUTPUT PARAMETER lv-ending   as log NO-UNDO.

         def var lv-end   as log NO-UNDO.
         def var lv-check AS LOG     NO-UNDO.
         
         ASSIGN lv-ending   = YES
                lv-problem  = NO
                lv-end      = NO.
                

         
         VALIDATE-BLK:
         DO:
         
            IF index(lv-schedule, 'weekly') > 0 THEN
            DO:
                
                ASSIGN lv-s-pos    = index(lv-schedule,' ',INDEX(lv-schedule,'weekly'))
                       lv-any-key  = YES
                       lv-ending   = NO
                       lv-got-date = YES.
                
                IF lv-s-pos <> 0 THEN
                ASSIGN lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 6)).
                       
                DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter): 
                
                   ASSIGN lv-h-var = entry(lv-n,lv-hold,lv-delimiter)
                          lv-check = NO.

                   IF length(lv-h-var) > 9 THEN
                   DO:
                       ASSIGN lv-problem = YES.
                       return 'error pos 1' . /* LEAVE validate-blk.*/
                   END.

                   IF lv-h-var <> 'everyday' AND
                      lv-h-var <> 'weekdays' AND
                      lv-h-var <> 'weekends' THEN
                   DO:
                       DO lv-counter = 1 TO 7:
                          IF lv-h-var = lv-day[lv-counter]THEN 
                          ASSIGN lv-check = YES.
                       END. /* check for day of week specified somewhere */
                       IF lv-check = NO THEN
                       do:
                          ASSIGN lv-problem = YES.
                          return 'error pos 2' . /* LEAVE validate-blk.*/
                       END.

                   END. /* if not everyday weekdays or weekends */
                   
                END. /* num entries weekly */
                
                IF lv-n = 1 THEN
                do:
                   ASSIGN lv-problem = YES.
                   return 'error pos 3' . /* LEAVE validate-blk.*/
                END.

            END. /* weekly validation */
            ELSE
            
            /* days in month validation  do not confuse with weekDAYS keyword */ 
            if index(lv-schedule, 'days') > 0 AND index(lv-schedule,'days') < 2 THEN
            DO:
             
               ASSIGN lv-s-pos    = index(lv-schedule,' ',INDEX(lv-schedule,'days'))
                      lv-days     = -1
                      lv-ending   = NO
                      lv-any-key  = YES
                      lv-got-date = YES.
               
               IF lv-s-pos <> 0 THEN
               ASSIGN lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 4)).
 
               DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter): 
                 
                 ASSIGN lv-h-var = entry(lv-n,lv-hold,lv-delimiter).
 
                 ASSIGN lv-days = integer(lv-h-var) no-error.
                 
                 if error-status:error OR (NOT error-status:ERROR AND (lv-days > 31 OR lv-days < 1)) 
                    OR length(lv-h-var) > 2 THEN
                 DO:
                    ASSIGN lv-problem = YES.
                    return 'error pos 4' . /* LEAVE validate-blk.*/
                 END.
                        
              END. /* all entries in days list if more than one */
 
              IF lv-n = 1 THEN
              DO:
                 ASSIGN lv-problem = YES.
                 return 'error pos 5' . /* LEAVE validate-blk.*/
              END.
 
            END. /* can find days in string */
            ELSE
 
            /* check s-dates are valid */
            if index(lv-schedule,'s-dates') > 0 then   
            do:
             
               ASSIGN lv-s-pos    = index(lv-schedule,' ',INDEX(lv-schedule,'s-dates'))
                      lv-any-key  = YES
                      lv-got-date = YES.
               
               IF lv-s-pos <> 0 THEN 
               ASSIGN lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 7)).
               
               IF INDEX(lv-hold,' ') > 0 THEN ASSIGN lv-problem = YES.
               
               IF lv-problem THEN
               return 'error pos 6' . /* LEAVE validate-blk.*/
               
               DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter): 
                  
                  ASSIGN lv-h-var = entry(lv-n,lv-hold,lv-delimiter).

                  RUN check-date(INPUT-OUTPUT lv-h-var, lv-screen,OUTPUT lv-problem, OUTPUT vd-date).
                  IF lv-problem THEN
                  return 'error pos 7' . /* LEAVE validate-blk.*/
                  
                  ASSIGN ENTRY(lv-n,lv-hold,lv-delimiter) = lv-h-var.
                  
               END. /* check multiple dates */
               
               IF lv-n = 1 THEN
               DO:
                 ASSIGN lv-problem = YES.
                 return 'error pos 8' . /* LEAVE validate-blk.*/
               END.

               /* update schedule with date format to store in yyyy/mm/dd */
               ASSIGN ENTRY(lv-section,lv-schedule,lv-seperator) = 's-dates ' + lv-hold.
 
           END. /* check s-dates are valid */
           ELSE                        
           
           /* first validation */
           IF INDEX(lv-schedule,'first') > 0 THEN
           DO:
             
             ASSIGN lv-s-pos    = index(lv-schedule,' ',INDEX(lv-schedule,'first'))
                    lv-any-key  = YES
                    lv-got-date = YES.    
  
             IF lv-s-pos <> 0 THEN 
             ASSIGN lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 5)).
                           
             DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter): 
           
               ASSIGN lv-check = NO
                      lv-h-var = ENTRY(lv-n,lv-hold,lv-delimiter).
 
               IF length(lv-h-var) > 9 THEN
               DO:
                  ASSIGN lv-problem = YES.
                  return 'error pos 9' . /* LEAVE validate-blk.*/
               END.
 
               IF lv-h-var      = 'day' THEN
               ASSIGN lv-check  = YES
                      lv-ending = NO.
                 
               /* first <weekday> logic */
               DO lv-counter = 1 TO 7:
                  
                    IF lv-h-var      = lv-day[lv-counter] THEN
                    ASSIGN lv-check  = YES
                           lv-ending = NO.
                    
               END. /* first <weekday> logic */
               
               IF NOT lv-CHECK THEN 
               do:
                  ASSIGN lv-problem = YES.
                  return 'error pos 10' . /* LEAVE validate-blk.*/
               END. /* if nothing else */
             
             END. /* num entries in first */

            IF NOT lv-CHECK THEN 
            do:
               ASSIGN lv-problem = YES.
               return 'error pos 11' . /* LEAVE validate-blk.*/
            END. /* if nothing else */
            
          END. /* first */
          ELSE

          /* last validation */
          IF INDEX(lv-schedule,'last') > 0 THEN
          DO:
            
            ASSIGN lv-s-pos    = index(lv-schedule,' ',INDEX(lv-schedule,'last'))
                   lv-any-key  = YES
                   lv-got-date = YES.
            
            IF lv-s-pos <> 0 THEN
            ASSIGN lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 4)).
            
            DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter): 

              ASSIGN lv-check = NO
                     lv-h-var = entry(lv-n,lv-hold,lv-delimiter).

              IF length(lv-h-var) > 9 THEN
              DO:
                 ASSIGN lv-problem = YES.
                 return 'error pos 12' . /* LEAVE validate-blk.*/
              END.

              IF lv-h-var      = 'day' THEN
              ASSIGN lv-check  = YES
                     lv-ending = NO.
                
              /* last <weekday> logic */
              DO lv-counter = 1 TO 7:
                 
                   IF lv-h-var      = lv-day[lv-counter] THEN
                   ASSIGN lv-check  = YES
                          lv-ending = NO.
                     
              END. /* last <weekday> logic */
              
              IF NOT lv-CHECK THEN 
              do:
                 ASSIGN lv-problem = YES.
                 return 'error pos 13' . /* LEAVE validate-blk.*/
              END. /* if nothing else */

            END. /* num entries in last */

            IF NOT lv-CHECK THEN 
            do:
               ASSIGN lv-problem = YES.
               return 'error pos 14' . /* LEAVE validate-blk.*/
            END. /* if nothing else */

          END. /* last */
          ELSE
        
          /* s-time */
          IF INDEX(lv-schedule,'s-time') > 0 THEN
          DO:
             
             ASSIGN lv-s-pos    = index(lv-schedule,' ',INDEX(lv-schedule,'s-time'))
                    lv-any-key  = YES
                    lv-ending   = NO.

             IF lv-s-pos <> 0 THEN
             ASSIGN lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 6)).
                
             DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter):

               ASSIGN lv-h-var = entry(lv-n,lv-hold,lv-delimiter).

               IF length(lv-h-var) > 7 THEN
               DO:
                  ASSIGN lv-problem = YES.
                  return 'error pos 15' . /* LEAVE validate-blk.*/
               END.
               
               IF lv-n = 1 THEN
               DO:
               
                   RUN check-time(lv-h-var, no, OUTPUT lv-problem, OUTPUT lv-counter).
                   IF lv-problem THEN do:
                     ASSIGN lv-problem = YES.
                     return 'error pos 16 ' + return-value. /* LEAVE validate-blk.*/
                   end.
               END. /* if first entry */

               IF lv-n = 2 THEN
               DO:
                  
                  IF lv-h-var <> 'minutes' THEN
                  DO:
                     ASSIGN lv-problem = YES.
                     return 'error pos 17' . /* LEAVE validate-blk.*/
                  END.
                  
               END. /* if second entry */

               IF lv-n = 3 THEN
               DO:

                  ASSIGN lv-counter = INTEGER(lv-h-var) NO-ERROR.
                  IF ERROR-STATUS:ERROR OR (NOT ERROR-STATUS:ERROR AND (lv-counter < 1 OR lv-counter > 1439)) THEN
                  DO:
                      ASSIGN lv-problem = YES.
                      return 'error pos 18' . /* LEAVE validate-blk.*/
                  END.
                  ELSE ASSIGN lv-iterate = YES.

               END. /* last entry */

             END. /* num entries in s-time */
             
             IF lv-n = 1 OR lv-n = 3 THEN /* can only have 2 or 4 entries */
             do: 
                ASSIGN lv-problem = YES.
                return 'error pos 19' . /* LEAVE validate-blk.*/
             END.

             ASSIGN lv-got-time = YES.

          END. /* s-time validation */
          ELSE

          /* ending */
          IF INDEX(lv-schedule,'ending') > 0 THEN
          DO:
           
              ASSIGN lv-s-pos    = index(lv-schedule,' ',INDEX(lv-schedule,'ending'))
                     lv-any-key  = YES.
              
              IF lv-s-pos <> 0 THEN
              ASSIGN lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 6)).
                 
              IF LENGTH(lv-h-var) > 10 THEN
              DO:
                 ASSIGN lv-problem = YES.
                 return 'error pos 20' . /* LEAVE validate-blk.*/
              END.
              
              DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter):
 
                ASSIGN lv-h-var = entry(lv-n,lv-hold,lv-delimiter).
 
                IF INDEX(lv-h-var,'/') > 0 THEN
                DO:
 
                    RUN check-date(INPUT-OUTPUT lv-h-var, lv-screen ,OUTPUT lv-problem, OUTPUT vd-date).
                    
                    IF lv-problem THEN
                    return 'error pos 21' . /* LEAVE validate-blk.*/
                    
                    ASSIGN ENTRY(lv-n,lv-hold,lv-delimiter) = lv-h-var.

                END. /* check date */
                ELSE
                IF INDEX(lv-h-var,':') > 0 THEN
                DO:
 
                    RUN check-time(lv-h-var,NO , OUTPUT lv-problem, OUTPUT lv-counter).
                    IF lv-problem THEN
                    return 'error pos 22' . /* LEAVE validate-blk.*/
 
                END. /* check-time */
              
              END. /* num-entries in ending */
          
              IF lv-n = 1 THEN do:
                  ASSIGN lv-problem = YES.
                  return 'error pos 23' . /* LEAVE validate-blk.*/
              end.
              /* update schedule with date format to store in yyyy/mm/dd */
              ASSIGN ENTRY(lv-section,lv-schedule,lv-seperator) = 'ending ' + lv-hold.

           END. /* ending is specified */
           ELSE do:
               ASSIGN lv-problem = YES.
               return 'error pos 24' . /* LEAVE validate-blk.*/
           end.
       END. /* entry n in schedule line */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Check-Time) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Check-Time Procedure 
PROCEDURE Check-Time :
DEFINE INPUT  PARAMETER pv-time  AS CHAR no-undo.
DEFINE INPUT  PARAMETER pv-secs  as log no-undo.
DEFINE OUTPUT PARAMETER pv-error AS LOG no-undo.
DEFINE OUTPUT PARAMETER pv-timeout  as int no-undo.

pv-error = NO.

IF SUBSTRING(pv-time,2,1)  = ' ' or
   SUBSTRING(pv-time,3,1) <> ':' OR
   SUBSTRING(pv-time,4,1)  = ' ' or
   substring(pv-time,5,1)  = ' ' 
THEN pv-error = YES.      

IF pv-secs AND (substring(pv-time,6,1) <> ':' or
                SUBSTRING(pv-time,7,1) = ' '  OR
                substring(pv-time,8,1) = ' ') 
THEN pv-error = YES.

IF pv-error THEN RETURN.

pv-timeout = integer(SUBSTRING(pv-time,1,INDEX(pv-time,':') - 1)) NO-ERROR.

IF ERROR-STATUS:ERROR THEN pv-error = YES.
IF pv-error THEN RETURN.

IF pv-timeout < 0 OR 
   pv-timeout > 24 THEN pv-error = YES.

IF pv-error THEN RETURN.

pv-timeout = INTEGER(SUBSTRING(pv-time,4,2)) NO-ERROR.

IF ERROR-STATUS:ERROR THEN pv-error = YES.

IF pv-error THEN RETURN.

IF pv-timeout < 0 OR 
   pv-timeout > 59 THEN pv-error = YES.

IF pv-error THEN RETURN.

IF pv-secs 
THEN DO:  /* check seconds */
  pv-timeout = INTEGER(SUBSTRING(pv-time,7,2)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN pv-error = YES.
  IF pv-error THEN RETURN.
  IF pv-timeout < 0 OR 
     pv-timeout > 59 THEN pv-error = YES.
  IF pv-error THEN RETURN.
END. /* check seconds */

pv-timeout = (int(SUBSTRING(pv-time,1,INDEX(pv-time,':') - 1)) * 60) * 60 +
              INT(SUBSTRING(pv-time,4,2)) * 60 NO-ERROR.

IF ERROR-STATUS:ERROR THEN pv-error = YES.

IF pv-secs THEN 
   pv-timeout = pv-timeout + INTEGER(SUBSTRING(pv-time,7,2)) NO-ERROR.

IF ERROR-STATUS:ERROR THEN pv-error = YES.

IF pv-error THEN RETURN.

/* if time is 'am' at high end of time then set to same time at low end of scale to
 fit in with logic */
IF pv-timeout >= 86400 THEN
   pv-timeout = pv-timeout - 86400.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Get-Next-Date-Time) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Next-Date-Time Procedure 
PROCEDURE Get-Next-Date-Time :
DEFINE INPUT PARAMETER lv-schedule   AS CHAR    NO-UNDO.

def var vd-dates-date as date    no-undo. /* the earliest date for the <s-dates> keyword if applicable */
DEF VAR vd-days-date  AS DATE    NO-UNDO. /* the current date for the <these-days> keyword if applicable */
def var vd-week-date  as date    no-undo. /* holds date to run this week if a <week-day> was used */
def var vd-group-date as date    no-undo. /* holds the date to run if a <group> was used */
DEF VAR vd-fl-date    AS DATE    NO-UNDO. /* holds the <f/l> date to run if used */
DEF VAR lv-month      AS INT     NO-UNDO. /* general month hold variable */
DEF VAR lv-year       AS INT     NO-UNDO. /* general year hold variable */
DEF VAR lv-problem    as log NO-UNDO. /* not checked - just used to take as output from procedures */

ASSIGN vd-week-date  = ?
       vd-group-date = ?
       vd-days-date  = ?
       vd-dates-date = ?
       vd-date       = ?
       vd-fl-date    = ?.

      DATE-BLK:
      DO:
         IF index(lv-schedule, 'weekly') > 0 THEN DO:
            ASSIGN lv-s-pos    = index(lv-schedule,' ',INDEX(lv-schedule,'weekly')).
                   lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 6)).
            DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter): 
               lv-h-var = entry(lv-n,lv-hold,lv-delimiter).
               IF lv-h-var = 'everyday' THEN vd-group-date = lv-hold-date.
               IF lv-h-var = 'weekdays' AND
                  (WEEKDAY(lv-hold-date) >= 2 AND 
                   WEEKDAY(lv-hold-date) <= 6) THEN vd-group-date = lv-hold-date.
               IF lv-h-var = 'weekends' AND
                  (WEEKDAY(lv-hold-date) = 1 OR 
                   WEEKDAY(lv-hold-date) = 7) THEN vd-group-date = lv-hold-date.
               IF vd-group-date = ? THEN do: /* not lv-hold-date set to next applicable date */
                 IF lv-h-var = 'weekdays' THEN /* roll forward weekday */
                 DO:
                   IF weekday(lv-hold-date) = 7 THEN vd-group-date = lv-hold-date + 2.
                                                 ELSE vd-group-date = lv-hold-date + 1.
                 END.
                 ELSE IF lv-h-var = 'weekends' THEN
                 DO: /* roll forward weekend */
                   CASE WEEKDAY(lv-hold-date):
                       WHEN 2 THEN ASSIGN vd-group-date = lv-hold-date + 5.
                       WHEN 3 THEN ASSIGN vd-group-date = lv-hold-date + 4.
                       WHEN 4 THEN ASSIGN vd-group-date = lv-hold-date + 3.
                       WHEN 5 THEN ASSIGN vd-group-date = lv-hold-date + 2.
                       WHEN 6 THEN ASSIGN vd-group-date = lv-hold-date + 1.
                   END. /* case */
                 END. /* weekend */
               END. /* vd-group-date = ? */
               RUN latest-date(INPUT-OUTPUT vd-sched-date, vd-group-date).
               /* if current date then can leave date-blk as is earliest date possible */
               IF vd-sched-date = lv-hold-date THEN LEAVE date-blk.
               /* loop through days if they exist to set the earliest date to run this week */
               DO lv-counter = 1 TO 7:
                 IF index(lv-schedule,lv-day[lv-counter]) > 0 AND
                 lv-counter >= WEEKDAY(lv-hold-date) AND vd-week-date = ? THEN
                 ASSIGN vd-week-date = lv-hold-date + (lv-counter - WEEKDAY(lv-hold-date)). 
               END. /* loop days of week */
               IF vd-week-date = ? THEN /* no days this week roll over to next week */
               DO lv-counter = 1 TO 7:
                 IF index(lv-schedule,lv-day[lv-counter]) > 0 THEN
                 vd-week-date = IF vd-week-date = ? THEN (lv-hold-date + (lv-counter - WEEKDAY(lv-hold-date)) + 7)
                                       ELSE IF vd-week-date > (lv-hold-date + (lv-counter - WEEKDAY(lv-hold-date)) + 7) 
                                            THEN (lv-hold-date + (lv-counter - WEEKDAY(lv-hold-date)) + 7)
                                            ELSE vd-week-date.
               END. /* loop days of week */
               /* get latest date */
               RUN latest-date(INPUT-OUTPUT vd-sched-date, vd-week-date).
               IF vd-sched-date = lv-hold-date THEN LEAVE date-blk.
            END. /* num entries in weekly */
         END. /* weekly */
         /* don't confuse with 'weekDAYS' */
         if index(lv-schedule, 'days') > 0 AND index(lv-schedule, 'days') < 2 
         THEN DO:
              ASSIGN lv-s-pos    = index(lv-schedule,' ',INDEX(lv-schedule,'days'))
                     lv-days     = -1.
              IF lv-s-pos <> 0 THEN lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 4)).
              DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter):
                    lv-days = integer(entry(lv-n,lv-hold,lv-delimiter)).
                    IF lv-days >= day(lv-hold-date) 
                    then vd-days-date = date(MONTH(lv-hold-date), lv-days, YEAR(lv-hold-date)) NO-ERROR.
                    IF lv-days < DAY(lv-hold-date) OR error-status:ERROR THEN
                    DO: /* roll over day into next month */
                        /* plus on one month as day has already gone past */
                        ASSIGN lv-month = MONTH(lv-hold-date) + 1
                               lv-year  = YEAR(lv-hold-date).
                        IF lv-month = 13 THEN
                        ASSIGN lv-year  = lv-year + 1
                               lv-month = 1.
                        ASSIGN  vd-date = date(lv-month, lv-days, lv-year) NO-ERROR.
                        DO WHILE ERROR-STATUS:ERROR:
                           ASSIGN  vd-date  = date(lv-month,lv-days,lv-year)
                                   lv-month = lv-month + 1 NO-ERROR.
                        END. /* while error */
                        vd-days-date = vd-date.
                    END. /* roll over day into next month */
                    RUN latest-date(INPUT-OUTPUT vd-sched-date, vd-days-date).
              end. /*  loop to get earliest day */
         end. /* days calculation */
         IF vd-sched-date = lv-hold-date THEN LEAVE date-blk.
         if index(lv-schedule, 's-dates') > 0 then do:
            ASSIGN lv-s-pos    = index(lv-schedule,' ',INDEX(lv-schedule,'s-dates')).
            IF lv-s-pos <> 0 
            THEN lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 7)).
            DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter): 
               ASSIGN lv-h-var = entry(lv-n,lv-hold,lv-delimiter).
               RUN check-date(INPUT-OUTPUT lv-h-var, NOT lv-screen, OUTPUT lv-problem, OUTPUT vd-date).
               RUN latest-date(INPUT-OUTPUT vd-sched-date,vd-date).
            END. /* check multiple dates */
         end. /* get scheduled dates if applicable */
         IF vd-sched-date = lv-hold-date THEN
         LEAVE date-blk.
         IF INDEX(lv-schedule,'first') > 0 THEN
         DO:
            ASSIGN lv-s-pos    = index(lv-schedule,' ',INDEX(lv-schedule,'first')).
            IF lv-s-pos <> 0 THEN 
            ASSIGN lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 5)).
            DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter): 
              IF entry(lv-n,lv-hold,lv-delimiter) = 'day' THEN
              DO:
                 ASSIGN lv-month    = MONTH(lv-hold-date)
                        lv-year     = YEAR(lv-hold-date).
                 IF DAY(lv-hold-date) <> 1 THEN
                 ASSIGN lv-month = lv-month + 1.
                 IF lv-month = 13 THEN
                 ASSIGN lv-month = 1
                        lv-year  = lv-year + 1.
                 ASSIGN vd-fl-date = DATE(lv-month,1,lv-year).
                 RUN latest-date(INPUT-OUTPUT vd-sched-date,vd-fl-date).
              END. /* first day logic */
              /* first <weekday> logic */
              DO lv-counter = 1 TO 7:
                 IF entry(lv-n,lv-hold,lv-delimiter) = lv-day[lv-counter] THEN
                 DO:
                    ASSIGN vd-fl-date = DATE(MONTH(lv-hold-date),1,YEAR(lv-hold-date))
                           lv-days = 1.
                    DO WHILE WEEKDAY(vd-fl-date) <> lv-counter:
                        ASSIGN lv-days = lv-days + 1
                               vd-fl-date = DATE(MONTH(lv-hold-date),lv-days,YEAR(lv-hold-date)).
                    END. /* get first <day> in the month as specified */
                    IF vd-fl-date < lv-hold-date THEN DO:
                       ASSIGN lv-days = 1
                              lv-month = month(lv-hold-date) + 1
                              lv-year  = year(lv-hold-date).
                       IF lv-month = 13 THEN 
                       ASSIGN lv-year  = lv-year + 1
                              lv-month = 1.
                       IF WEEKDAY(DATE(lv-month,lv-days,lv-year)) = lv-counter THEN
                       ASSIGN vd-fl-date = DATE(lv-month,lv-days,lv-year).
                       ELSE
                       DO WHILE WEEKDAY(DATE(lv-month,lv-days,lv-year)) <> lv-counter:
                           ASSIGN lv-days = lv-days + 1
                                  vd-fl-date = DATE(lv-month,lv-days,lv-year).
                       END. /* get first <day> in the month as specified */
                       ASSIGN vd-fl-date = DATE(lv-month,lv-days,lv-year).
                    END. /* roll over to next month */
                    RUN latest-date(INPUT-OUTPUT vd-sched-date,vd-fl-date).
                 END. /* if this is the day */
              END. /* first <weekday> logic -> 1 to 7 days of week */
            END. /* all entries in first */
         END. /* first logic */
         IF INDEX(lv-schedule,'last') > 0 THEN
         DO:
            ASSIGN lv-s-pos    = index(lv-schedule,' ',INDEX(lv-schedule,'last')).
            IF lv-s-pos <> 0 THEN
            ASSIGN lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 4)).
            DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter): 
               IF entry(lv-n,lv-hold,lv-delimiter) = 'day' THEN
               DO:
                  ASSIGN lv-month    = MONTH(lv-hold-date) + 1
                         lv-year     = YEAR(lv-hold-date).
                  IF lv-month = 13 THEN
                  ASSIGN lv-year  = lv-year + 1
                         lv-month = 1.
                  ASSIGN vd-fl-date = DATE(lv-month,1,lv-year)
                         vd-fl-date = vd-fl-date - 1. /* remove 1 day gives us the last date of our month */
                  RUN latest-date(INPUT-OUTPUT vd-sched-date,vd-fl-date).
               END. /* last day logic */
               /* last <weekday> logic */
               DO lv-counter = 1 TO 7:
                 IF entry(lv-n,lv-hold,lv-delimiter) = lv-day[lv-counter] THEN
                 DO:
                     ASSIGN lv-month = MONTH(lv-hold-date) + 1
                            lv-year  = YEAR(lv-hold-date).
                     IF lv-month = 13 THEN 
                     ASSIGN lv-month = 1
                            lv-year = lv-year + 1.
                     ASSIGN vd-fl-date = DATE(lv-month,1,lv-year)
                            vd-fl-date = vd-fl-date - 1
                            lv-days = 1.
                     DO WHILE WEEKDAY(vd-fl-date) <> lv-counter:
                          ASSIGN vd-fl-date = vd-fl-date - 1.
                     END. /* get last <day> in the month as specified */
                     IF vd-fl-date < lv-hold-date THEN
                     DO:
                        ASSIGN lv-days  = 1
                               lv-month = month(lv-hold-date) + 2
                               lv-year  = year(lv-hold-date).
                        IF lv-month = 13 OR lv-month = 14 THEN 
                        ASSIGN lv-year  = lv-year + 1
                               lv-month = IF lv-month = 13 THEN 1
                                          ELSE IF lv-month = 14 THEN 2
                                          ELSE lv-month.
                        ASSIGN vd-fl-date  = DATE(lv-month,lv-days,lv-year) - 1.
                        IF WEEKDAY(vd-fl-date) = lv-counter THEN
                        ASSIGN vd-fl-date = DATE(lv-month,lv-days,lv-year) - 1.
                        ELSE
                        DO WHILE WEEKDAY(vd-fl-date) <> lv-counter:
                          ASSIGN vd-fl-date = vd-fl-date - 1.
                        END. /* get last <day> in the month as specified */
                     END. /* loop into next month */
                     RUN latest-date(INPUT-OUTPUT vd-sched-date,vd-fl-date).
                 END. /* if this is the day */
               END. /* loop through weekdays */
            END. /* number of entries in last */
         END. /* get last date */
      end. /* DATE-BLK: */
      /* couldn't get a date greater than or equal to current date */
      IF vd-sched-date < lv-hold-date THEN
      ASSIGN lv-ended = YES.
      IF lv-got-date THEN
      ASSIGN lv-run-date = vd-sched-date.
      /* otherwise is already assigned to today */
      IF lv-got-time THEN      DO:
        TIME-BLK: 
        DO:
         /* get s-time if specified */
         IF INDEX(lv-schedule,'s-time') > 0 THEN         DO:
            ASSIGN lv-s-pos      = index(lv-schedule,' ',INDEX(lv-schedule,'s-time'))
                   lv-run-time  = 1
                   lv-counter    = 0
                   lv-sched-time = -1.
            IF lv-s-pos <> 0 THEN
            ASSIGN lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 6)).
            DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter):
               ASSIGN lv-h-var = entry(lv-n,lv-hold,lv-delimiter).
               IF lv-n = 1 THEN
               RUN check-time(lv-h-var, no, OUTPUT lv-problem, OUTPUT lv-sched-time).
               IF lv-n = 3 THEN
               ASSIGN lv-counter = INTEGER(lv-h-var) * 60.
            END. /* num entries in s-time */
            IF lv-counter <> 0 THEN /* minutes specified */
            DO:
                DO WHILE lv-sched-time <= lv-hold-time  AND lv-run-date = lv-hold-date:
                    ASSIGN lv-sched-time = lv-sched-time + lv-counter.
                    IF lv-sched-time >= 86400 THEN
                    DO:
                       ASSIGN lv-run-time = -2. /* rolled over into next day error code */
                       LEAVE time-blk.
                    END. /* time rolled over into next day */
                END. /* while still current day and time is less than current time when minutes specified */
            END. /* minutes specified */
            IF lv-run-date      = lv-hold-date AND lv-sched-time < lv-hold-time THEN
            ASSIGN lv-run-time  = -2. /* past time on current date re-run look for next date / time */
            ASSIGN lv-run-time  = IF lv-run-time <> -2 THEN lv-sched-time ELSE lv-run-time.
          END. /* get s-time */
         END. /* time-blk */
       END. /* if got time */
       ELSE       DO:
          IF lv-run-date = TODAY THEN
          ASSIGN lv-run-time = lv-hold-time + 1. /* current time */
          ELSE
          ASSIGN lv-run-time = 1. /* 12:00 am */
       END.
       ASSIGN vd-end-date = ?
              lv-end-time = 0.
       IF INDEX(lv-schedule,'ending') > 0 THEN DO:
          ASSIGN lv-s-pos    = index(lv-schedule,' ',INDEX(lv-schedule,'ending')).
          IF lv-s-pos <> 0 THEN lv-hold     = trim(SUBSTRING(lv-schedule,lv-s-pos,LENGTH(lv-schedule) - 6)).
          IF lv-s-pos = 0 THEN ASSIGN lv-n = 0.
          ELSE
          DO lv-n = 1 TO NUM-ENTRIES(lv-hold,lv-delimiter):
            lv-h-var = entry(lv-n,lv-hold,lv-delimiter).
            IF INDEX(lv-h-var,'/') > 0 THEN
            RUN check-date(INPUT-OUTPUT lv-h-var,NOT lv-screen , OUTPUT lv-problem, OUTPUT vd-end-date).
            ELSE
            IF index(lv-h-var,':') > 0 then
            RUN check-time(lv-h-var,NO , OUTPUT lv-problem, OUTPUT lv-end-time).
          END. /* num-entries in ending */
          IF lv-n = 0 THEN
          ASSIGN vd-end-date = TODAY  /* if nothing specified then today at 11:59 pm */
                 lv-end-time = 86399. 
          IF vd-end-date = ? THEN vd-end-date = TODAY.
          IF lv-end-time = 0 THEN lv-end-time = 86399.
       END. /* ending is specified */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Initialise) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialise Procedure 
PROCEDURE Initialise :
DEF OUTPUT PARAMETER pv-new-sched  AS CHAR    NO-UNDO. /* schedule line to screen or record format */
def output parameter pv-run-date   as date    no-undo. /* next run date and time */
def output parameter pv-run-time   as int no-undo. 
def output parameter pv-ended      as log no-undo. /* has the schedule ended */ 
def output parameter pv-problem    as log no-undo. /* problem interpriting schedule line */
def output parameter pv-ending     as log no-undo. /* will this schedule end itself? */
DEF OUTPUT PARAMETER pv-iterate    as log NO-UNDO. /* is there a time iteration ncrease input time */

/*************************************************** BEGIN ********************************************/
assign lv-day[1]   = 'sunday'
       lv-day[2]   = 'monday'
       lv-day[3]   = 'tuesday'
       lv-day[4]   = 'wednesday'
       lv-day[5]   = 'thursday'
       lv-day[6]   = 'friday'
       lv-day[7]   = 'saturday'
       lv-any-key  = NO
       lv-d-format = SESSION:DATE-FORMAT.

/* remove extra spaces in space delimited string */
do while index(lv-schedule,"") > 0 :
    lv-schedule = replace(lv-schedule,"  "," ").
end.

/* check syntax of scheduleline before processing */
syntax-blk:
DO lv-section = 1 TO NUM-ENTRIES(lv-schedule,lv-seperator):
   RUN check-schedule-syntax(ENTRY(lv-section,lv-schedule,lv-seperator),OUTPUT lv-problem, OUTPUT lv-ending).
   IF lv-problem THEN return return-value /* LEAVE syntax-blk */ .
   /* if cannot find any keywords but there is an entry - check for old syntax */
   IF lv-section = NUM-ENTRIES(lv-schedule,lv-seperator) AND NOT lv-any-key 
   THEN do:
       lv-old-sched = YES.
       RUN CHECK-old-time(INPUT-OUTPUT lv-schedule, OUTPUT lv-problem, 
                          OUTPUT lv-run-date, OUTPUT lv-run-time).
       IF lv-problem THEN return return-value /* LEAVE syntax-blk */ .
       assign lv-got-date = lv-run-date ne ?
              lv-got-time = lv-run-time ne -1
              lv-ending = YES.
   END. /* look for old syntax */
   IF lv-problem THEN return return-value /* LEAVE syntax-blk */ .
END. /* syntax-blk */

IF lv-section = 1 THEN lv-ending   = YES.
IF NOT lv-got-date THEN lv-run-date = TODAY.
IF NOT lv-got-time THEN lv-run-time = TIME.

IF lv-old-sched THEN DO:  /* check if has ended if used old syntax method */
    IF (lv-run-date < lv-hold-date) OR
       (lv-run-date = TODAY AND lv-run-time < lv-hold-time) 
    THEN ASSIGN lv-ended    = YES
                lv-run-time = -1
                lv-run-date = ?.
END. /* check if has ended if used old syntax method */

IF NOT lv-problem THEN lv-new-sched = lv-schedule.
 /* has been changed throughout syntax check to store yyyy/mm/dd
 date formats in the sched line instead of what they entered */

/* no syntax problems - new schedule syntax - get next date/time to run if necessary */
IF NOT lv-problem AND (lv-got-date OR lv-got-time) AND NOT lv-old-sched THEN 
DO: 
  DO lv-section = 1 TO NUM-ENTRIES(lv-schedule,lv-seperator):
     RUN get-next-date-time(entry(lv-section,lv-schedule,lv-seperator)).
  END. /* 1 to number of sections in schedule line */
  IF lv-run-date = ?           OR
     lv-run-date > vd-end-date OR                                 /* time rolled over */
     (lv-run-date  = vd-end-date AND (lv-run-time > lv-end-time OR lv-run-time = -2)) OR
     (NOT lv-got-date AND lv-run-time = -2) THEN 
  ASSIGN lv-ended    = YES
         lv-run-time = -1
         lv-run-date = ?.

  /* time rolled over to next day - try to get next date/time as that days schedule has finished */
  IF lv-run-time = -2 AND NOT lv-ended THEN DO:
    ASSIGN lv-hold-date = lv-hold-date + 1
           lv-hold-time = -1
           vd-sched-date = ?
           lv-sched-time = -1.
    DO lv-section = 1 TO NUM-ENTRIES(lv-schedule,lv-seperator):
        RUN get-next-date-time(entry(lv-section,lv-schedule,lv-seperator)).
    END. /* 1 to number of sections in schedule line */
    /* check if ended */
    IF lv-run-date = ? OR
       lv-run-date > vd-end-date OR
       (lv-run-date  = vd-end-date AND lv-run-time > lv-end-time) 
    THEN 
    ASSIGN lv-ended    = YES
           lv-run-time = -1
           lv-run-date = ?.
  END. /* time rolled over to next day and not finished schedule - try to get next date/time */
END. /* no syntax problems */

ASSIGN SESSION:DATE-FORMAT = lv-d-format
       pv-new-sched      = lv-new-sched
       pv-run-date       = lv-run-date
       pv-run-time       = lv-run-time
       pv-ended          = lv-ended
       pv-problem        = lv-problem
       pv-ending         = lv-ending
       pv-iterate        = lv-iterate.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Latest-Date) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Latest-Date Procedure 
PROCEDURE Latest-Date :
DEFINE INPUT-OUTPUT PARAMETER vd-schedule AS DATE.
      DEFINE INPUT        PARAMETER vd-new-date AS DATE.

      IF vd-schedule < lv-hold-date THEN vd-schedule = ?.

      IF vd-schedule = ? THEN
      vd-schedule = IF vd-new-date <> ? AND vd-new-date >= lv-hold-date THEN vd-new-date
                           ELSE vd-schedule.
      ELSE
      vd-schedule = IF vd-new-date < vd-schedule AND vd-new-date >= lv-hold-date THEN vd-new-date
                           ELSE vd-schedule.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Refresh) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh Procedure 
PROCEDURE Refresh :
def input  parameter pv-schedule   as char    no-undo. /* the schedule line */
def input  parameter pv-hold-date  as date    no-undo. /* date and time to look forward from */
def input  parameter pv-hold-time  as int     no-undo.
DEF INPUT  PARAMETER pv-screen     as log NO-UNDO. /* convert new-sched-line to screen format OR
                                                           convert to record format */
DEF OUTPUT PARAMETER pv-new-sched  AS CHAR    NO-UNDO. /* schedule line to screen or record format */
def output parameter pv-run-date   as date    no-undo. /* next run date and time */
def output parameter pv-run-time   as int no-undo. 
def output parameter pv-ended      as log no-undo. /* has the schedule ended */ 
def output parameter pv-problem    as log no-undo. /* problem interpriting schedule line */
def output parameter pv-ending     as log no-undo. /* will this schedule end itself? */
DEF OUTPUT PARAMETER pv-iterate    as log NO-UNDO. /* is there a time iteration increase input time */
         
         ASSIGN lv-schedule   = pv-schedule
                lv-hold-date  = pv-hold-date
                lv-hold-time  = pv-hold-time
                lv-screen     = pv-screen
                lv-new-sched  = ''
                lv-run-date   = ?
                lv-run-time   = 0
                lv-ended      = NO
                lv-problem    = NO
                lv-ending     = NO
                lv-iterate    = NO.

         RUN initialise(OUTPUT pv-new-sched,
                        OUTPUT pv-run-date,
                        OUTPUT pv-run-time,
                        OUTPUT pv-ended,
                        OUTPUT pv-problem,
                        OUTPUT pv-ending,
                        OUTPUT pv-iterate).
return return-value.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

