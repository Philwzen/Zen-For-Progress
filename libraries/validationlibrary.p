&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/******************************************************************************/
/*  PROGRAM ID.     : ????????                                                */
/*  PROGRAM TITLE   : ????????                                                */
/*                                                                            */
/*  CREATE DATE     : ??/??/??                                                */
/*                                                                            */
/*  COMPANY NAME    : Zennor Computing LTD                                    */
/*  VERSION NO.     : 1.0                                                     */
/******************************************************************************/
/******************************************************************************/
/*                          PATCH HISTORY                                     */
/*                                                                            */
/*           PATCH  USR     Job                                               */
/* DATE      NO.    ID      REF DESCRIPTION                                   */
/* -------------------------------------------------------------------------- */
/* ??/??/??  P00    Philw   00  initial release                               */
/******************************************************************************/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
create widget-pool.
/* ***************************  Definitions  ************************** */
this-procedure:private-data = "library-validation".
&glob library-validation
&glob library-program

{app-paths.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-AsEntered) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AsEntered Procedure 
FUNCTION AsEntered {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CurrentFiscalPeriod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CurrentFiscalPeriod Procedure 
FUNCTION CurrentFiscalPeriod {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfLastMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FirstDayOfLastMonth Procedure 
FUNCTION FirstDayOfLastMonth {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfLastWeek) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FirstDayOfLastWeek Procedure 
FUNCTION FirstDayOfLastWeek {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfLastYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FirstDayOfLastYear Procedure 
FUNCTION FirstDayOfLastYear {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfNextMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FirstDayOfNextMonth Procedure 
FUNCTION FirstDayOfNextMonth {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfNextWeek) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FirstDayOfNextWeek Procedure 
FUNCTION FirstDayOfNextWeek {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfNextYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FirstDayOfNextYear Procedure 
FUNCTION FirstDayOfNextYear {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfThisMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FirstDayOfThisMonth Procedure 
FUNCTION FirstDayOfThisMonth {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfThisWeek) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FirstDayOfThisWeek Procedure 
FUNCTION FirstDayOfThisWeek {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfThisYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FirstDayOfThisYear Procedure 
FUNCTION FirstDayOfThisYear {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstFiscalPeriodForCurrentYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FirstFiscalPeriodForCurrentYear Procedure 
FUNCTION FirstFiscalPeriodForCurrentYear {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstFiscalPeriodForNextYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FirstFiscalPeriodForNextYear Procedure 
FUNCTION FirstFiscalPeriodForNextYear {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstFiscalPeriodForPreviousYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FirstFiscalPeriodForPreviousYear Procedure 
FUNCTION FirstFiscalPeriodForPreviousYear {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFiscalPeriod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFiscalPeriod Procedure 
FUNCTION GetFiscalPeriod RETURNS CHARACTER
  (pv-periodInstructions as char,
   pv-yearInstructions   as char,
   pv-type as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayAsDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastDayAsDate Procedure 
FUNCTION LastDayAsDate RETURNS DATE
    (lv-date AS DATE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfLastMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastDayOfLastMonth Procedure 
FUNCTION LastDayOfLastMonth {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfLastWeek) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastDayOfLastWeek Procedure 
FUNCTION LastDayOfLastWeek {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfLastYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastDayOfLastYear Procedure 
FUNCTION LastDayOfLastYear {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfNextMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastDayOfNextMonth Procedure 
FUNCTION LastDayOfNextMonth {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfNextWeek) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastDayOfNextWeek Procedure 
FUNCTION LastDayOfNextWeek {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfNextYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastDayOfNextYear Procedure 
FUNCTION LastDayOfNextYear {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfThisMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastDayOfThisMonth Procedure 
FUNCTION LastDayOfThisMonth {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfThisWeek) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastDayOfThisWeek Procedure 
FUNCTION LastDayOfThisWeek {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfThisYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastDayOfThisYear Procedure 
FUNCTION LastDayOfThisYear {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastFiscalPeriodForCurrentYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastFiscalPeriodForCurrentYear Procedure 
FUNCTION LastFiscalPeriodForCurrentYear {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastFiscalPeriodForNextYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastFiscalPeriodForNextYear Procedure 
FUNCTION LastFiscalPeriodForNextYear {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastFiscalPeriodForPreviousYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastFiscalPeriodForPreviousYear Procedure 
FUNCTION LastFiscalPeriodForPreviousYear {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Manual) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Manual Procedure 
FUNCTION Manual {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NextFiscalPeriod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NextFiscalPeriod Procedure 
FUNCTION NextFiscalPeriod {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NoDefault) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NoDefault Procedure 
FUNCTION NoDefault {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PreviousFiscalPeriod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PreviousFiscalPeriod Procedure 
FUNCTION PreviousFiscalPeriod {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TDay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TDay Procedure 
FUNCTION TDay {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Tomorrow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Tomorrow Procedure 
FUNCTION Tomorrow {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Yesterday) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Yesterday Procedure 
FUNCTION Yesterday {{&core}{&libraries}screendefaultsparam.i} FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 32.57
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{{&core}libmain.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-AsEntered) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AsEntered Procedure 
FUNCTION AsEntered {{&core}{&libraries}screendefaultsparam.i}:

   return pv-defvalue.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CurrentFiscalPeriod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CurrentFiscalPeriod Procedure 
FUNCTION CurrentFiscalPeriod {{&core}{&libraries}screendefaultsparam.i}:
/* period,year,type */
  RETURN GetFiscalPeriod('current','current',entry(2,pv-wid:private-data,':')).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfLastMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FirstDayOfLastMonth Procedure 
FUNCTION FirstDayOfLastMonth {{&core}{&libraries}screendefaultsparam.i}:

   def var holdMonth as int  no-undo.
   def var holdYear  as int  no-undo.
   def var lv-date   as date no-undo.

   assign     
      lv-date = date(month(today),1,year(today)) + -1  
      holdMonth = month(lv-date) /* store month and year of that date */
      holdYear  = year(lv-date)
      lv-date = date(holdMonth,1,holdYear).

  RETURN string(lv-date).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfLastWeek) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FirstDayOfLastWeek Procedure 
FUNCTION FirstDayOfLastWeek {{&core}{&libraries}screendefaultsparam.i}:


  RETURN string(today - weekday(today) + 1 + -7).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfLastYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FirstDayOfLastYear Procedure 
FUNCTION FirstDayOfLastYear {{&core}{&libraries}screendefaultsparam.i}:

   def var holdMonth as int  no-undo.
   def var holdYear  as int  no-undo.
   def var lv-date   as date no-undo.

 assign holdYear = year(today) + -1
         lv-date  = date(1,1,holdYear).
 
  RETURN string(lv-date).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfNextMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FirstDayOfNextMonth Procedure 
FUNCTION FirstDayOfNextMonth {{&core}{&libraries}screendefaultsparam.i}:

   def var holdMonth as int  no-undo.
   def var holdYear  as int  no-undo.
   def var lv-date   as date no-undo.

   assign     
      lv-date = date(month(today),1,year(today)) + 45
      holdMonth = month(lv-date) 
      holdYear  = year(lv-date)
      lv-date = date(holdMonth,1,holdYear).
  RETURN string(lv-date).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfNextWeek) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FirstDayOfNextWeek Procedure 
FUNCTION FirstDayOfNextWeek {{&core}{&libraries}screendefaultsparam.i}:

  RETURN string(today - weekday(today) + 1 + 7).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfNextYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FirstDayOfNextYear Procedure 
FUNCTION FirstDayOfNextYear {{&core}{&libraries}screendefaultsparam.i}:

   def var holdMonth as int  no-undo.
   def var holdYear  as int  no-undo.
   def var lv-date   as date no-undo.

 assign holdYear = year(today) + 1
         lv-date  = date(1,1,holdYear).

  RETURN string(lv-date).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfThisMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FirstDayOfThisMonth Procedure 
FUNCTION FirstDayOfThisMonth {{&core}{&libraries}screendefaultsparam.i}:

   def var holdMonth as int  no-undo.
   def var holdYear  as int  no-undo.
   def var lv-date   as date no-undo.

   assign     
      lv-date = date(month(today),1,year(today))
      holdMonth = month(lv-date)
      holdYear  = year(lv-date)
      lv-date = date(holdMonth,1,holdYear).

  RETURN string(lv-date).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfThisWeek) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FirstDayOfThisWeek Procedure 
FUNCTION FirstDayOfThisWeek {{&core}{&libraries}screendefaultsparam.i}:

  RETURN string(today - weekday(today) + 1).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstDayOfThisYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FirstDayOfThisYear Procedure 
FUNCTION FirstDayOfThisYear {{&core}{&libraries}screendefaultsparam.i}:


   def var holdMonth as int  no-undo.
   def var holdYear  as int  no-undo.
   def var lv-date   as date no-undo.

 assign holdYear = year(today)
         lv-date  =  date(1,1,holdYear).

  RETURN string(lv-date).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstFiscalPeriodForCurrentYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FirstFiscalPeriodForCurrentYear Procedure 
FUNCTION FirstFiscalPeriodForCurrentYear {{&core}{&libraries}screendefaultsparam.i}:
/* period,year,type */
  RETURN GetFiscalPeriod('first','current',entry(2,pv-wid:private-data,':')).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstFiscalPeriodForNextYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FirstFiscalPeriodForNextYear Procedure 
FUNCTION FirstFiscalPeriodForNextYear {{&core}{&libraries}screendefaultsparam.i}:
/* period,year,type */
  RETURN GetFiscalPeriod('first','next',entry(2,pv-wid:private-data,':')).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstFiscalPeriodForPreviousYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FirstFiscalPeriodForPreviousYear Procedure 
FUNCTION FirstFiscalPeriodForPreviousYear {{&core}{&libraries}screendefaultsparam.i}:
/* period,year,type */
  RETURN GetFiscalPeriod('first','Previous',entry(2,pv-wid:private-data,':')).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetFiscalPeriod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFiscalPeriod Procedure 
FUNCTION GetFiscalPeriod RETURNS CHARACTER
  (pv-periodInstructions as char,
   pv-yearInstructions   as char,
   pv-type as char) :


def var lv-period   as char  no-undo.
def var lv-year     as char  no-undo.
def var lv-sys      as char  no-undo.
def var lv-practice as char  no-undo.


assign
   lv-sys      = getSysVar("gs-sys-cd")
   lv-practice = getSysVar("practice").

{{&core}run.i &program   = "c-fiscal.p"       
             &path      = "{&server}{&tables}"
             &Appsrv    = "System" 
             &procedure = "GetFiscalPeriod"      
             &nomess    = 'true'       
             &params    = "(lv-sys,lv-practice,
                       pv-periodInstructions,pv-yearInstructions,
                       output lv-period, output lv-year)"}
case pv-type:
   when 'period' then RETURN lv-period.
   when 'year'   then return lv-year.
   otherwise return lv-period + ':' + lv-year.
end case.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayAsDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastDayAsDate Procedure 
FUNCTION LastDayAsDate RETURNS DATE
    (lv-date AS DATE):

    def var lv-enddate as date no-undo.

    /* find last day of date returned as date  */

lv-enddate = ((DATE(MONTH(lv-date),28,YEAR(lv-date)) + 4) -
           DAY(DATE(MONTH(lv-date),28,YEAR(lv-date)) + 4)).

    RETURN lv-enddate.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfLastMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastDayOfLastMonth Procedure 
FUNCTION LastDayOfLastMonth {{&core}{&libraries}screendefaultsparam.i}:

   def var holdMonth as int  no-undo.
   def var holdYear  as int  no-undo.
   def var lv-date   as date no-undo.

   assign     
      lv-date = date(month(today),1,year(today)) + -1  
      holdMonth = month(lv-date) /* store month and year of that date */
      holdYear  = year(lv-date)
      lv-date = date(holdMonth,1,holdYear) /* set date to first day of the correct month */
      lv-date = lastDayasDate(lv-date).

  RETURN string(lv-date).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfLastWeek) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastDayOfLastWeek Procedure 
FUNCTION LastDayOfLastWeek {{&core}{&libraries}screendefaultsparam.i}:

  RETURN string(today - weekday(today) + 1 + 6 + -7).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfLastYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastDayOfLastYear Procedure 
FUNCTION LastDayOfLastYear {{&core}{&libraries}screendefaultsparam.i}:

   def var holdMonth as int  no-undo.
   def var holdYear  as int  no-undo.
   def var lv-date   as date no-undo.

 assign holdYear = year(today) +  -1 
         lv-date  =  date(12,31,holdYear).

  RETURN string(lv-date).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfNextMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastDayOfNextMonth Procedure 
FUNCTION LastDayOfNextMonth {{&core}{&libraries}screendefaultsparam.i}:


   def var holdMonth as int  no-undo.
   def var holdYear  as int  no-undo.
   def var lv-date   as date no-undo.

   assign     
      lv-date = date(month(today),1,year(today)) +  45 
      holdMonth = month(lv-date) /* store month and year of that date */
      holdYear  = year(lv-date)
      lv-date = date(holdMonth,1,holdYear) /* set date to first day of the correct month */
      lv-date = lastDayasDate(lv-date).

  RETURN string(lv-date).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfNextWeek) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastDayOfNextWeek Procedure 
FUNCTION LastDayOfNextWeek {{&core}{&libraries}screendefaultsparam.i}:


  RETURN string(today - weekday(today) + 1 + 6 + 7).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfNextYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastDayOfNextYear Procedure 
FUNCTION LastDayOfNextYear {{&core}{&libraries}screendefaultsparam.i}:


   def var holdMonth as int  no-undo.
   def var holdYear  as int  no-undo.
   def var lv-date   as date no-undo.

 assign holdYear = year(today) + 1
         lv-date  = date(12,31,holdYear).

  RETURN string(lv-date).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfThisMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastDayOfThisMonth Procedure 
FUNCTION LastDayOfThisMonth {{&core}{&libraries}screendefaultsparam.i}:

   def var holdMonth as int  no-undo.
   def var holdYear  as int  no-undo.
   def var lv-date   as date no-undo.
   assign     
      lv-date = date(month(today),1,year(today)) 
      holdMonth = month(lv-date) /* store month and year of that date */
      holdYear  = year(lv-date)
      lv-date = date(holdMonth,1,holdYear) /* set date to first day of the correct month */
      lv-date = lastDayAsDate(lv-date).
  RETURN string(lv-date).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfThisWeek) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastDayOfThisWeek Procedure 
FUNCTION LastDayOfThisWeek {{&core}{&libraries}screendefaultsparam.i}:

  RETURN string(today - weekday(today) + 1 + 6).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayOfThisYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastDayOfThisYear Procedure 
FUNCTION LastDayOfThisYear {{&core}{&libraries}screendefaultsparam.i}:

   def var holdMonth as int  no-undo.
   def var holdYear  as int  no-undo.
   def var lv-date   as date no-undo.

 assign holdYear = year(today)
         lv-date  = date(12,31,holdYear).

  RETURN string(lv-date).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastFiscalPeriodForCurrentYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastFiscalPeriodForCurrentYear Procedure 
FUNCTION LastFiscalPeriodForCurrentYear {{&core}{&libraries}screendefaultsparam.i}:
/* period,year,type */
  RETURN GetFiscalPeriod('last','current',entry(2,pv-wid:private-data,':')).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastFiscalPeriodForNextYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastFiscalPeriodForNextYear Procedure 
FUNCTION LastFiscalPeriodForNextYear {{&core}{&libraries}screendefaultsparam.i}:
/* period,year,type */
  RETURN GetFiscalPeriod('last','next',entry(2,pv-wid:private-data,':')).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastFiscalPeriodForPreviousYear) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastFiscalPeriodForPreviousYear Procedure 
FUNCTION LastFiscalPeriodForPreviousYear {{&core}{&libraries}screendefaultsparam.i}:
/* period,year,type */
  RETURN GetFiscalPeriod('last','Previous',entry(2,pv-wid:private-data,':')).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Manual) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Manual Procedure 
FUNCTION Manual {{&core}{&libraries}screendefaultsparam.i}:

return pv-defvalue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NextFiscalPeriod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NextFiscalPeriod Procedure 
FUNCTION NextFiscalPeriod {{&core}{&libraries}screendefaultsparam.i}:
/* period,year,type */
  RETURN GetFiscalPeriod('Next','current',entry(2,pv-wid:private-data,':')).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NoDefault) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NoDefault Procedure 
FUNCTION NoDefault {{&core}{&libraries}screendefaultsparam.i}:

return ''.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PreviousFiscalPeriod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PreviousFiscalPeriod Procedure 
FUNCTION PreviousFiscalPeriod {{&core}{&libraries}screendefaultsparam.i}:
/* period,year,type */
  RETURN GetFiscalPeriod('previous','current',entry(2,pv-wid:private-data,':')).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TDay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TDay Procedure 
FUNCTION TDay {{&core}{&libraries}screendefaultsparam.i}:

  RETURN string(today).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Tomorrow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Tomorrow Procedure 
FUNCTION Tomorrow {{&core}{&libraries}screendefaultsparam.i}:

  RETURN string(today + 1). 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Yesterday) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Yesterday Procedure 
FUNCTION Yesterday {{&core}{&libraries}screendefaultsparam.i}:

  RETURN string(today - 1).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

