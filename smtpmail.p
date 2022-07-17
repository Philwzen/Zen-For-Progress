&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*-------------------------------------------------------------------
Input Param : 
     mailhub     char - settable SMTP server name/IP address
                 optionally append :XX where X is a port number of the
                 SMTP server.  25 is default port.
     EmailTo     CHAR - list of email addresses separated by 
                 semicolons or commas (All semicolons will 
                 be replaced by commas so don't include stuff
                 like Smith, John)  <me@myaddress.org>"My Name"
     EmailFrom   CHAR - email address of user originating the email, 
                 the SMTP server should require that this user 
                 is real. Format looks like:
                 <user>@<host>[;<descriptive name>]
                 Example:
                 foo@bar.com[;Mr. Foo Bar]
     EmailCC     CHAR - list of email addresses separated by 
                 semicolons or commas (All semicolons will 
                 be replaced by commas so don't include stuff
                 like Smith, John) <me@myaddress.org>"My Name"
     Attachments CHAR - Comma separated list of attachment descriptions 
                 Format looks like: 
       file[:type=<mimetype>][:charset=<charset>][:filetype=<filetype>]
                 Special Filetypes are:  BINARY, B64ENCODED if you use
                 B64ENCODED make sure to call smtpmail with RemoveEncodedFiles
                 as the EmailTo param after sending all your emails
                 or the encoded files will build up in your temporary
                 directory.
     LocalFiles  CHAR - comma separated list of filenames to the files 
                 described in Attachments.  The filenames in this  
                 param must either be in the Progress path or 
                 have the entire path specified.  In contrast to 
                 filenames in the Attachments are the filenames to 
                 show for the Attachments.
     Subject     CHAR - Subject line
     Body        CHAR - Actual text of the message can be whatever you  
                 want as long as you include the correct Header 
                 information in the MIMEHeader. 
                 If you are just sending plaintext you can leave 
                 the MIMEHeader blank, this is default setting 
                 If you wanted to send HTML you might use: 
                 type=text/html:charset=us-ascii:filetype=ascii
     MIMEHeader  CHAR - [type=<mimetype>][:charset=<chrset>][:filetype=<type>]
     BodyType    char - File/text.  Determines whether a file or text 
                 goes into the message body.

---------------------------------------------------------------*/
{app-paths.i}
DEF var mailhub         as char no-undo.
DEF var EmailTo         AS CHAR NO-UNDO.
DEF var EmailFrom       AS CHAR NO-UNDO.
DEF var EmailCC         AS CHAR NO-UNDO.
DEF var Attachments     AS CHAR NO-UNDO.
DEF var LocalFiles      AS CHAR NO-UNDO.
DEF var Subject         AS CHAR NO-UNDO.
DEF var Body            AS CHAR NO-UNDO.
DEF var MIMEHeader      AS CHAR NO-UNDO.
DEF var BodyType        as char no-undo.
DEF VAR loglevel                    as int NO-UNDO.
DEF VAR LogFile                     as char NO-UNDO.
DEF VAR EncodeDirectory             AS CHAR NO-UNDO.
DEF VAR tzone                    AS CHAR NO-UNDO.

DEF VAR cLocalFile                  as char NO-UNDO.
DEF VAR cBinaryFile                 as char NO-UNDO.
DEF VAR vLocalHostName              as char NO-UNDO.

/* Used to communicate with SMTP Socket */
DEF VAR hSocket                     AS HANDLE NO-UNDO.

DEF VAR ServResponse                AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR ServerCode                  as int NO-UNDO.
DEF VAR vState                      as int  NO-UNDO.
DEF VAR crlf                        AS CHAR NO-UNDO.
def var vmessage as char no-undo.

/* SWL 07/03/2001 counter for looping */
DEF VAR icnt                        as int   NO-UNDO.
DEF VAR icnt1                       as int   NO-UNDO.
DEF VAR filcnt                      as int   NO-UNDO.
DEF VAR rcptcnt                     as int   NO-UNDO.
DEF VAR rspcnt                      as int   NO-UNDO.
DEF VAR AttachBinlist               as char NO-UNDO.
DEF VAR sending                     as log NO-UNDO.

/* SLJ 18/12/02 - Need a temp list of recipients to work through */
def var EmailToTmp          as char NO-UNDO.
def var EmailCCTmp          as char NO-UNDO.

/* SES 01/03/2003 */
def var EmailReplyTo        as char NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-NewState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewState Procedure 
FUNCTION NewState RETURNS INTEGER
  (INPUT newstate as int,
   INPUT pstring as char,
   INPUT hSocket AS HANDLE) FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* registry */
&glob HKEY_LOCAL_MACHINE -2147483646
&glob ERROR_SUCCESS                0
&glob MAX_PATH                   260
&glob REG-KEY "SYSTEM\CurrentControlSet\Control\timezoneInformation":U
&glob REG-ATT               "ActiveTimeBias":U

PROCEDURE RegOpenKeyA EXTERNAL "advapi32" :
  def input  param hkey       AS LONG.
  def input  param lpszSubKey AS CHAR.
  def output param phkResult  AS LONG.
  DEFINE RETURN param lpResult   AS LONG.
END PROCEDURE.

PROCEDURE RegCloseKey EXTERNAL "advapi32" :
  def input  param hkey     AS LONG.
  DEFINE RETURN param lpresult AS LONG.
END PROCEDURE.

PROCEDURE RegQueryValueExA EXTERNAL "advapi32" :
  def input        param hkey         AS LONG.
  def input        param lpValueName  AS CHAR.
  def input        param lpdwReserved AS LONG.
  def output       param lpdwType     AS LONG.
  def input        param lpbData      AS LONG. /* memptr */
  def input-OUTPUT param lpcbData     AS LONG.
  DEFINE RETURN       param lpresult     AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CleanUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CleanUp Procedure 
PROCEDURE CleanUp :
IF VALID-HANDLE(hSocket) THEN DO:
    IF hSocket:CONNECTED() THEN hSocket:DISCONNECT() NO-ERROR.
    DELETE OBJECT hSocket.
  END.
  apply 'close' to this-procedure.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoFiles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoFiles Procedure 
PROCEDURE DoFiles :
DEF INPUT param localfile            as char no-undo.
  DEF INPUT param cattachment           as char no-undo.
  DEF INPUT-OUTPUT param attachbinlist as char no-undo.

  DEF VAR cLocalFile                       as char no-undo.
  DEF VAR Mimetype                     as char no-undo.
  DEF VAR ccharset                      as char no-undo.
  DEF VAR cFileType                       as char NO-UNDO.

  FILE-INFO:FILE-NAME = localfile.
 
  IF INDEX(FILE-INFO:FILE-TYPE,"F") = 0 THEN DO:
    message program-name(1) + ' ' + localfile + " Not a valid file".
    RETURN localfile + " Not a valid file".
  END.

  RUN ParseParm(INPUT cAttachment,
                OUTPUT mimetype,
                OUTPUT cCHARSET,
                OUTPUT cfiletype).

/*   IF cFileType = "Binary" THEN DO: */
/*      /* 7/12/02 PK if localfile includes a file path it will cause */
/*         some problems trying to write the file in /tmp */
/*         Generate a unique file to use for the encoded version */ */
/*    */
/*      /* cBinaryFile is used in this loop to check for existence of */
/*         the new encoded file.  It must be reset to the desired value */
/*         after the loop. */ */
/*     cBinaryFile = localfile. */
/*     DO while cBinaryFile <> ?: */
/*       /* Use of ETIME in the name is to further minimize the */
/*        likelihood that the same RANDOM # could be generated by */
/*        2 processes at once, since there is a possibility that */
/*        between the time search runs and the encoded file is actually */
/*          created the same filename could be generated */ */
/*       cLocalFile = EncodeDirectory + "en" + STRING(ETIME) + */
/*                    STRING(RANDOM(1,99999),"99999"). */
/*       cBinaryFile = SEARCH(cLocalFile). /* Check for existence */ */
/*     END. */
/*    */
/*     ASSIGN */
/*     cBinaryFile = LocalFile. /* FILE-INFO:FULL-PATHNAME better? */ */
/*     /* 7/12/01 PK The delimiter in the else statement was a '<' */ */
/*     attachbinlist = IF attachbinlist = "" THEN cLocalFile */
/*                     ELSE attachbinlist + "," + cLocalFile. */
/*     &IF DEFINED(PublicVersion) > 0 &THEN */
/*       RUN b64encode.p(cBinaryFile, cLocalFile) NO-ERROR. */
/*     &ELSE */
/*       RUN utils/base64encode.p(cBinaryFile, cLocalFile) NO-ERROR. */
/*     &ENDIF */
/*     IF ERROR-STATUS:ERROR THEN message ERROR-STATUS:GET-MESSAGE(1). */
/*   END.  /* IF cFileType = "Binary" THEN  */ */
/*   ELSE IF cFileType = "B64ENCODED" THEN DO: */
/*     /* Make sure there is a base64 encoded version of the file in the */
/*        encode directory. If not then create it now for reuse. */ */
/*    */
/*     /* Make sure path delimiters are consistent with Unix */ */
/*     cLocalFile = REPLACE(localFile,"~\","/"). */
/*     IF NUM-ENTRIES(cLocalFile,"/") > 0 THEN */
/*       cLocalFile = ENTRY(NUM-ENTRIES(cLocalFile,"/"),cLocalFile,"/"). */
/*     cLocalFile = EncodeDirectory + "b64-" + cLocalFile. */
/*    */
/*     IF SEARCH(cLocalFile) = ? THEN DO: */
/*       cBinaryFile = LocalFile. */
/*       &IF DEFINED(PublicVersion) > 0 &THEN */
/*         RUN b64encode.p(cBinaryFile, cLocalFile) NO-ERROR. */
/*       &ELSE */
/*         RUN base64encode.p(cBinaryFile, cLocalFile) NO-ERROR. */
/*       &ENDIF */
/*       IF ERROR-STATUS:ERROR THEN message ERROR-STATUS:GET-MESSAGE(1). */
/*     END. */
/*     attachbinlist = IF attachbinlist = "" THEN cLocalFile */
/*                     ELSE attachbinlist + "," + cLocalFile. */
/*   END. */
/*   ELSE */ 
  
  
  attachbinlist = IF attachbinlist = "" 
                   THEN localfile
                   ELSE attachbinlist + "," + localfile.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoneWithFiles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoneWithFiles Procedure 
PROCEDURE DoneWithFiles :
/* Used to cleanup pre-encoded files after calling program is done
     sending the same attachments to multiple recipients. */
  DEF INPUT param localfile            as char no-undo.
  DEF INPUT param cattachment           as char no-undo.

  DEF VAR cLocalFile                       as char no-undo.
  DEF VAR Mimetype                     as char no-undo.
  DEF VAR ccharset                      as char no-undo.
  DEF VAR cFileType                       as char NO-UNDO.

  RUN ParseParm(INPUT cAttachment,
                OUTPUT mimetype,
                OUTPUT cCHARSET,
                OUTPUT cfiletype).

  IF cFileType = "B64ENCODED" THEN DO:
    /* Make sure path delimiters are consistent with Unix */
    cLocalFile = REPLACE(localFile,"~\","/").
    IF NUM-ENTRIES(cLocalFile,"/") > 0 THEN 
      cLocalFile = ENTRY(NUM-ENTRIES(cLocalFile,"/"),cLocalFile,"/").
    cLocalFile = EncodeDirectory + "b64-" + cLocalFile.
    IF SEARCH(cLocalFile) <> ? THEN DO:
      OS-DELETE VALUE(cLocalFile) NO-ERROR.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Get-Smtp-Date) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Smtp-Date Procedure 
PROCEDURE Get-Smtp-Date :
/*--------------------------------------------------------------------------
  Purpose:     generate an smtp date string
  Parameters:  OUTPUT CHAR Result
  Notes:       DDD "," DD MMM YYYY HH ":" MM ":" SS ( "+" / "-" ) HHMM
               CRLF
      As of 5/15/2002 This procedure is not currently used by this program.
      It was included because it generates the date in a slightly different
      format than the currently functioning code.  If someone encounters a
      problem with the date format on their smtp server - this code may 
      remedy that problem.   (Paul Keary)  Original Author: Jeff Pilant
---------------------------------------------------------------------------*/
  def output param R as char NO-UNDO.

  def var S as char NO-UNDO.
  def var M as char NO-UNDO.
  def var D as char NO-UNDO.

  /*
    The date and time-of-day SHOULD express local time. 
    The zone specifies the offset from UTC.
      "+" (east) or "-" (west).
      "+0000" indicates a time zone at Universal Time.
      "-0000" indicates a local time zone. 
      "-0400" or "-0500" -- EDT or EST
  */
  
  M = "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec":U.
  D = "Sun,Mon,Tue,Wed,Thu,Fri,Sat":U.
  RUN get-tz(OUTPUT S).
  S = ENTRY(WEEKDAY(TODAY), D)      + ", ":U
    + STRING(DAY(TODAY), "99":U)    + " ":U
    + ENTRY(MONTH(TODAY), M)        + " ":U
    + STRING(YEAR(TODAY), "9999":U) + " ":U
    + STRING(TIME, "HH:MM:SS":U)    + " ":U
    + S.
  R = S.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Get-Tz) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Tz Procedure 
PROCEDURE Get-Tz :
def output param tzResult as char NO-UNDO.
  def var tzStr      as char NO-UNDO.
  def var tzBias     as int   NO-UNDO.
  def var tzHours    as int   NO-UNDO.
  def var tzMinutes  as int   NO-UNDO.

  /* Registry read vars */
  def var hKey       as int   NO-UNDO.
  def var reslt      as int   NO-UNDO.
  def var lth        as int   NO-UNDO.
  def var Bias       AS MEMPTR    NO-UNDO.
  def var datatype   as int   NO-UNDO.
  def var hBiasKey   as int   NO-UNDO.

 
  RUN RegOpenKeyA({&HKEY_LOCAL_MACHINE},
                  {&Reg-Key},
                  OUTPUT hKey,
                  OUTPUT reslt).
  IF reslt = {&ERROR_SUCCESS} THEN
  DO:
    lth  = {&MAX_PATH} + 1.
    SET-SIZE(Bias) = lth.
    RUN RegQueryValueExA(hKey,
                         {&REG-ATT},
                         0,  /* reserved, must be 0 */
                         OUTPUT datatype,
                         GET-POINTER-VALUE(Bias),
                         INPUT-OUTPUT lth,
                         OUTPUT reslt).
    RUN RegCloseKey(hBiasKey,OUTPUT reslt).
   
    
    /* Convert value from DWORD to INTEGER */
    tzBias =                ASC(GET-BYTES(Bias, 4, 1)).
    tzBias = tzBias * 256 + ASC(GET-BYTES(Bias, 3, 1)).
    tzBias = tzBias * 256 + ASC(GET-BYTES(Bias, 2, 1)).
    tzBias = tzBias * 256 + ASC(GET-BYTES(Bias, 1, 1)).
    SET-SIZE(Bias)=0.       
    /* Convert value to +HHMM form */
    tzHours = INTEGER(- tzBias / 60).
    tzMinutes = - tzBias - 60 * tzHours.
    tzStr = TRIM(STRING(tzHours, "-99":U) + STRING(tzMinutes, "99":U)).
    IF tzHours >= 0 THEN tzStr = "+":U + tzStr.
    tzResult = tzStr.
  END.
  ELSE
    tzResult = "-0000":U. /* key not found in registry */
  RUN RegCloseKey(hKey,OUTPUT reslt).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSocket) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSocket Procedure 
PROCEDURE GetSocket :
DEF INPUT param loglevel as int NO-UNDO.
  DEF INPUT param mailhub  AS CHAR NO-UNDO.
  DEF OUTPUT param hSocket AS HANDLE NO-UNDO.

  DEF VAR iPortNum as int NO-UNDO.
  
  IF NUM-ENTRIES(mailhub,":") > 1 THEN ASSIGN iPortNum = INTEGER(ENTRY(2,mailhub,":")) NO-ERROR.
  IF iPortNum = 0 OR iPortNum = ? THEN ASSIGN iPortNum = 25.
  
  CREATE SOCKET hSocket.
  hSocket:SET-SOCKET-OPTION ("TCP-NODELAY", "FALSE").
  hSocket:SET-SOCKET-OPTION ("SO-LINGER", "FALSE").
   
  hSocket:SET-READ-RESPONSE-PROCEDURE ("ReadHandler").

  hSocket:CONNECT("-H " + entry(1,MailHub,":") + " -S " + string(iPortNum)) NO-ERROR.

  IF hSocket:CONNECTED() = FALSE THEN DO:
    RUN CleanUp.
    message "No Connection".
    RETURN.
  END.  /* Cannot make a connection to mail server */
  vstate = 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ParseParm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ParseParm Procedure 
PROCEDURE ParseParm :
DEF INPUT  PARAM cString    AS CHAR NO-UNDO.
DEF OUTPUT param cMimetype as char no-undo.
DEF OUTPUT param cCharset  as char no-undo.
DEF OUTPUT param cFiletype as char no-undo.

DEF VAR c         AS CHAR NO-UNDO.
DEF VAR i         AS INT  NO-UNDO.
DEF VAR lBoundary AS CHAR NO-UNDO.

  ASSIGN cMimeType = "text/plain"
         cCharSet  = "US-ASCII"
         cFileType = "ASCII"
         lBoundary = "".

  DO i = 1 TO NUM-ENTRIES(cString,":"):
    c = ENTRY(i,cString,":").
    CASE ENTRY(1,c,"="):
      WHEN "Type" THEN cMimeType = ENTRY(2,c,"=").
      WHEN "CharSet" THEN cCharSet = ENTRY(2,c,"=").
      WHEN "FileType" THEN cFileType = ENTRY(2,c,"=").
      WHEN "Boundary" THEN lBoundary = ENTRY(2,c,"=").
    END CASE. 
  END. 
  
  IF lBoundary <> "" THEN DO:
    IF cMimeType BEGINS "multipart"
    THEN assign cMimeType = cMimeType + ";~nboundary=" + lBoundary.
                cCharset = "".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReadHandler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadHandler Procedure 
PROCEDURE ReadHandler :
DEF VAR vlength     as int   NO-UNDO.
  DEF VAR str         as char NO-UNDO.
  DEF VAR v                              as int   NO-UNDO.
  DEF VAR idx                            as int   NO-UNDO.
  DEF VAR mData                          AS MEMPTR    NO-UNDO.
  DEF VAR vbuffer                        AS MEMPTR    NO-UNDO.
 
 /* Used to Build MIME Message Body */
  DEF VAR cTempValue                      as char NO-UNDO.
  DEF VAR cBoundary                       as char NO-UNDO.
  DEF VAR cMimeType                       as char NO-UNDO.
  DEF VAR cCharSet                        as char NO-UNDO.
  DEF VAR cFileType                       as char NO-UNDO.
  DEF VAR cFile                           as char NO-UNDO.
  DEF VAR cImportData                     as char NO-UNDO.
 
  DEF VAR smtpcmd                   AS CHAR FORMAT "x(90)" NO-UNDO.
  DEF VAR teststr                   as char NO-UNDO.

  DEF VAR cMonth                          as char NO-UNDO
         INIT "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".
  DEF VAR iLineCount                      as int   NO-UNDO.
  DEF VAR iRespLines                      as int   NO-UNDO.
  DEF VAR cExtraResp                      as char NO-UNDO.
  DEF VAR cThisLine                       as char NO-UNDO.
  DEF VAR cTextIn                         as char NO-UNDO.
  
  IF NOT VALID-HANDLE(hSocket) OR NOT hSocket:CONNECTED() THEN RETURN.

  vlength = hSocket:GET-BYTES-AVAILABLE().
  IF vlength > 0 
  THEN DO:
    SET-SIZE(vbuffer) = 0.
    SET-SIZE(vbuffer) = vlength + 1.
    hSocket:READ(vbuffer, 1, vlength, 1).
    
    ASSIGN
     str = cExtraResp + GET-STRING(vbuffer,1)
     iRespLines = num-entries(str,"~n")
     cExtraResp = "" .
     
    /* If response doesn't end with a newline, then we don't have the whole thing.    */
    /*  - In that case, stash the extra part of a line into a temporary variable that */
    /*    we can add to the beginning of the next packet we receive.  -SES            */
    
    IF SUBSTRING(str,length(str,"CHARACTER") - 1,2) NE crlf 
    THEN assign cExtraResp = ENTRY(iRespLines,str,"~n")
                ENTRY(iRespLines,str,"~n") = ""
                str = TRIM(str) + CRLF.
    v = 0.   
    DO iLineCount = 1 to iRespLines - 1: /* should be nothing after final CRLF */
        cThisLine = TRIM(ENTRY(iLineCount,str,"~n")).
        IF SUBSTRING(cThisLine,4,1,"CHARACTER") = "-" 
            THEN NEXT.
            ELSE v = INTEGER(ENTRY(1,str," ")) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            message  '0 ' Program-name(1) skip str skip Error-Status:Get-Message(Error-Status:Num-Messages). 
            LEAVE.
        END. /* not a valid response */
    END. /* Each line of response */  
    
    IF v = 0 OR v = ? THEN LEAVE.
    
    SET-SIZE(vbuffer) = 0.
    CASE vState:
      WHEN 1 THEN DO:        /********************** 1 - Build message ***************/
        IF v = 220 /* send helo */
        THEN vstate = newState(2, "HELO " + vLocalHostName + crlf,hsocket).
        ELSE message '1 ' Program-name(1) skip str.
      END. 
      WHEN 2 THEN DO:        /********************** 2 - Build From *****************/
        IF v = 250 
        THEN do:
          ASSIGN EmailTo    = REPLACE(EmailTo,";",",")
                 EmailCC    = REPLACE(EmailCC,";",",")
                 EmailToTmp = EmailTo  
                 EmailCCTmp = EmailCC.
                 vstate =  newState(3, "MAIL From: " + ENTRY(1,EmailFrom,";") + crlf,hsocket).
           rcptcnt = num-entries(emailto) + num-entries(emailcc).
        END.
        ELSE message '2 ' Program-name(1) skip str.
      END. 
      WHEN 3 THEN DO: /******************** 3 - Assign to and cc **************/
        ASSIGN icnt    = if icnt = 0 then 1 else (if icnt = ? then 1 else icnt)
               icnt1   = if icnt1 = 0 then 1 else (if icnt1 = ? then 1 else icnt1)
               smtpcmd = "".
        IF v = 250 
        THEN do:       /* loop through all to's and cc's  */
            IF NUM-ENTRIES(EmailToTmp) > 1 
            THEN DO:
                vstate = newState(3, "RCPT TO: " + ENTRY(1,EmailToTmp) + crlf,hsocket).
                emailtoTmp = SUBSTRING(emailto,INDEX(emailtoTmp,",") + 1).
            END. /* More than One TO */
            ELSE IF NUM-ENTRIES(EmailtoTmp) = 1 
                 THEN DO:
                       IF EmailCCTmp = "" /* If cc stay as state 3 */
                          THEN vstate = newState(4, "RCPT TO: " + EmailToTmp + crlf,hsocket).
                          ELSE vstate = newState(3, "RCPT TO: " + EmailToTmp + crlf,hsocket).
                    emailtoTmp = "".
                 END. /* Last Or only one Entry */
                 ELSE IF EmailCCTmp <> "" 
                 THEN DO:
                    IF NUM-ENTRIES(EmailCCTmp) > 1 
                    THEN DO:
                        vstate = newState(3, "RCPT To: " + ENTRY(1,ENTRY(1,EmailCCTmp),"^") + crlf,hsocket).
                        emailCCTmp = SUBSTRING(EmailCCTmp,INDEX(EmailCCTmp,",") + 1).
                    END. /* Multiple CC's */
                    ELSE DO:
                        vstate = newState(4, "RCPT To: " + ENTRY(1,EmailCCTmp,"^") + crlf,hsocket).
                        EmailCCTmp = "".
                    END. /* One or Last CC */
                END. /* CC'ing people */
        END. /*  IF v = 250 THEN */
        ELSE message '3 ' Program-name(1) skip str.
      end.
      /******************** 4 - Build header ********************/
      WHEN 4 THEN DO:
        IF v = 250 THEN vstate = newState(5, "DATA " + crlf,hsocket).
                   ELSE message '4' Program-name(1) skip str.
      END. /* when 4 */
      WHEN 5 THEN DO:
        IF v = 354 THEN do:
          /* Build Email Header */
          smtpcmd = "From: " + 
             (IF NUM-ENTRIES(EmailFrom,";") > 1 THEN
             ENTRY(2,emailfrom,";") + " <" + ENTRY(1,emailfrom,";") + ">"
             ELSE EmailFrom) + crlf.
             
          /**************************************/
          /* Look for Reply-To                  */
          /**************************************/
          IF EmailReplyTo <> "" THEN DO:
            smtpcmd = smtpcmd + "Reply-To: " + EmailReplyTo + "~n".
          END.  /* IF ReplyTo <> "" THEN DO */
          
          /**************************************/
          /* Loop through all To's              */
          /**************************************/
          IF EmailTo <> "" THEN DO idx = 1 TO NUM-ENTRIES(EmailTo):
            smtpcmd = smtpcmd + "To: " + ENTRY(idx,EmailTo) + "~n".
          END.  /* IF EmailTo <> "" THEN DO idx = 1 */

          /*****************************/
          /* Loop through all CC's     */
          /*****************************/
          IF EmailCC <> "" THEN DO idx = 1 TO NUM-ENTRIES(EmailCC):
              IF NUM-ENTRIES(ENTRY(idx,EmailCC),"^") = 1 
               OR ENTRY(2,ENTRY(idx,EmailCC),"^") NE "B" 
               THEN ASSIGN smtpcmd = smtpcmd + "Cc: " + ENTRY(1,ENTRY(idx,EmailCC),"^") + "~n".
          END.  /* IF EmailCC <> "" THEN  */
               
          ASSIGN
          smtpcmd = smtpcmd + "Subject: " + Subject + "~n" 
          /* Sample format    Date: 27 March 2001 10:30:00 EST */
          smtpcmd = smtpcmd + "Date: " + STRING(DAY(TODAY)) + " " +
                    entry(MONTH(TODAY),cMonth) + " " +
                    STRING(YEAR(TODAY),"9999") + " " +
                    STRING(TIME,"hh:mm:ss") + " " + tzone + "~n".

          SET-SIZE(mData) = LENGTH(smtpcmd,"RAW") + 1.
          PUT-STRING(mData,1) = smtpcmd.
          RUN WriteData(input mData, input hsocket).
          SET-SIZE(mData) = 0.

          /********************************************************/
          /* Begin sending message                                */
          /********************************************************/
          /* Done building Email Hdr, Now do body of the message */
          /** Set up a boundary value if we have file attachments **/
          /** Create multi mime header for email **/
          IF Attachments <> "" THEN DO:
            ASSIGN
            cBoundary = "MAIL_BOUNDARY"
            smtpcmd = "MIME-Version: 1.0" + "~n" +
                      'Content-type: multipart/mixed;~n' +
                      '        boundary="' + cBoundary + '"~n' + 
                      "Content-Transfer-Encoding: 7bit~n".
            smtpcmd = smtpcmd + "This is a multi-part MIME Encoded message" +
            /* *Kalmijn*: put ~r~n before boundry marker instead of ~n~n. */
                      "~r~n--" + cBoundary + "~n".
            SET-SIZE(mData) = LENGTH(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
            RUN WriteData(input mData, input hsocket).
            SET-SIZE(mData) = 0.
          END. /* IF Attachments <> "" THEN DO: */

          /** Do we have a MIME Type for this messsage **/
          IF MIMEHeader <> "" THEN DO:
            RUN ParseParm(INPUT MIMEHeader,
                          OUTPUT cMimetype,
                          OUTPUT cCharset,
                          OUTPUT cfiletype).
            smtpcmd = IF Attachments = "" THEN "Mime-Version: 1.0~n"
                                           ELSE "".
            smtpcmd = smtpcmd + "Content-Type: " + cMimeType.
            /* IF the message was multipart the charset may have
               been overridden */
            IF cCharSet <> "" THEN 
              smtpcmd = smtpcmd + "; charset=" + cCharSet.
            smtpcmd = smtpcmd + "~n" +
                      "Content-Transfer-Encoding: 7bit~n".
            SET-SIZE(mData) = LENGTH(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
            RUN WriteData(INPUT mData, INPUT hsocket).
            SET-SIZE(mData) = 0.
          END.  /* IF MIMEHeader <> "" THEN DO: */

          
          /*********************************************************/
          /* Output the Message                                    */
          /*********************************************************/
          smtpcmd = "~n".
          IF bodytype = "file" THEN DO:
            SET-SIZE(mdata) = LENGTH(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
            RUN WriteData(INPUT mData, INPUT hSocket).
            
            SET-SIZE(mData) = 0.
            mData = InputFromFile(body,'local').
            RUN WriteData(INPUT mData, INPUT hsocket).
            
            SET-SIZE(mData) = 0.
            SET-SIZE(mData) = 2.
            PUT-STRING(mData,1) = "~n".

          END. /* if bodytype = "file" */
          ELSE DO:
            smtpcmd = smtpcmd + Body + "~n".
            SET-SIZE(mData) = length(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
          END.  /*  else */
          
          RUN WriteData(INPUT mData, INPUT hsocket).
          SET-SIZE(mData) = 0.
          
          
          DO idx = 1 TO NUM-ENTRIES(LocalFiles):
            ASSIGN
            cFile = ENTRY(1,ENTRY(idx,Attachments),":")
            cLocalFile = ENTRY(idx,LocalFiles).
            
            /** set up the mime header **/
            /* Content-Type: <mimetype>; charset=<charset> */
            RUN parseParm(input entry(idx,attachments),
                          output cMimetype,
                          output cCharset,
                          output cfiletype).

            smtpcmd = "~n--" + cBoundary + "~n" +
                      "Content-type: " + cMimeType + "; ".
            IF LOOKUP(cFileType,"BINARY,B64ENCODED") = 0 THEN
              smtpcmd = smtpcmd + "charset=" + cCharSet.
            smtpcmd = smtpcmd + '~n        name="' + cFile + '"~n'.


            IF LOOKUP(cFileType,"BINARY,B64ENCODED") > 0 THEN
              smtpcmd = smtpcmd + 'Content-Transfer-Encoding: base64~n'.

            smtpcmd = smtpcmd + 'Content-Disposition: attachment;~n' +
                      '        filename="' + cFile + '"~n~n'.
            SET-SIZE(mData) = LENGTH(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
            RUN WriteData(INPUT mData, INPUT hsocket).
            SET-SIZE(mData) = 0.

            /** now do the file **/
            mData = InputFromFile(ENTRY(idx,attachbinlist),'local').
            RUN WriteData(INPUT mData, INPUT hsocket).
            SET-SIZE(mData) = 0.

            smtpcmd = "~n".
            SET-SIZE(mData) = LENGTH(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
            RUN WriteData(INPUT mData, INPUT hsocket).
            SET-SIZE(mData) = 0.

            /** if we have a "Binary" file then try to delete 
                the encoded version **/
            IF cFileType = "Binary" THEN
              OS-DELETE VALUE(entry(idx,attachbinlist)) NO-ERROR.
          END. /** process each attachment  do idx - 1 to num-entries **/

          IF Attachments <> "" THEN DO:
            smtpcmd = '~n--' + cBoundary + '--~n~n'.
            SET-SIZE(mData) = length(smtpcmd,"RAW") + 1.
            PUT-STRING(mData,1) = smtpcmd.
            RUN WriteData(input mData, input hsocket).
            SET-SIZE(mData) = 0.
          END. /* IF Attachments <> "" THEN DO: */
          vstate = newstate(6, crlf + "." + crlf, hsocket).
        END. /* if v = 354 */
        ELSE vState = -1.
      END. /* when 5 */
      WHEN 6 THEN DO:
        IF v = 250 THEN vstate = newState(7,"QUIT" + crlf,hsocket).
        ELSE vState = -1.
      END. /* when 6 */
    END CASE. /* vstate */
  END. /* IF vlength > 0 THEN DO: */
  
  IF vState = 7 THEN message '6 ' Program-name(1) skip "Email has been accepted for delivery.".
  IF vstate < 0 OR vstate = 7 THEN DO:
    RUN cleanup.
    /* If running in batch mode then tell the WHILE loop to exit */
    sending = NO.
    APPLY 'CLOSE' TO THIS-PROCEDURE.
  END.  /* IF vstate < 0 OR vstate = 7 THEN DO: */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMail Procedure 
PROCEDURE SendMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT param pv-mailhub         as char no-undo.
DEF INPUT param pv-EmailTo         AS CHAR NO-UNDO.
DEF INPUT param pv-EmailFrom       AS CHAR NO-UNDO.
DEF INPUT param pv-EmailCC         AS CHAR NO-UNDO.
DEF INPUT param pv-Attachments     AS CHAR NO-UNDO.
DEF INPUT param pv-LocalFiles      AS CHAR NO-UNDO.
DEF INPUT param pv-Subject         AS CHAR NO-UNDO.
DEF INPUT param pv-Body            AS CHAR NO-UNDO.
DEF INPUT param pv-MIMEHeader      AS CHAR NO-UNDO.
DEF INPUT param pv-BodyType        as char no-undo.

assign 
    mailhub = pv-mailhub
    emailto = pv-emailto
    emailfrom = pv-emailfrom
    emailcc = pv-emailcc
    attachments = pv-attachments
    localfiles = pv-localfiles
    subject = pv-subject
    body = pv-body
    mimeheader = pv-mimeheader
    bodytype = pv-bodytype
    crlf = CHR(13) + CHR(10)
    EncodeDirectory = session:temp-directory
    LogFile = EncodeDirectory + "socketemail.log"
    EmailFrom = trim(emailFrom," ,")
    EmailTo = trim(emailTo," ,").

IF EmailFrom = "" or EmailTo = "" 
THEN RETURN "** From or To is blank".

RUN get-tz(OUTPUT tzone).

IF NUM-ENTRIES(EmailFrom,"^") > 1 THEN DO:
    ASSIGN 
     EmailReplyTo = entry(2,EmailFrom,"^")
     EmailFrom    = entry(1,EmailFrom,"^").
END.

vLocalHostName = os-getenv('computername'). 

IF vLocalHostName = "" 
THEN vLocalHostName = "localhost". 

vLocalHostName = REPLACE(vLocalHostName," ",""). 

/* 5/13/02 PK Added to support reuse of base64encoded files over
   multiple calls to this procedure.  After the last call run this
   program again with "RemoveEncodedFiles" as the EmailTo and the
   temporary encoded files will be removed. */
IF EmailTo = "RemoveEncodedFiles" THEN DO:
  IF localfiles <> "" THEN DO filcnt = 1 TO NUM-ENTRIES(localfiles):
    RUN DoneWithFiles (INPUT ENTRY(filcnt,localfiles),
                       INPUT ENTRY(filcnt,attachments)) NO-ERROR.
  END.
  RUN Cleanup.
  RETURN.
END.

/* process attachments and generate a comma separated list of
   file names for the output of base64encode.p . This is done prior
   to opening the socket to minimize the impact on resources
   (you do not need to have a socket open doing nothing for hours */
IF localfiles <> "" THEN
DO filcnt =  1 TO NUM-ENTRIES(localfiles):
  RUN dofiles(INPUT ENTRY(filcnt,localfiles),
              INPUT ENTRY(filcnt,attachments),
              INPUT-OUTPUT attachbinlist) NO-ERROR.

  IF vMessage <> "" THEN DO:
    RUN cleanup.
    RETURN program-name(1) + crlf + vmessage.
  END.
END. 


sending = YES.

RUN getsocket(input loglevel,input mailhub,output hSocket).

 IF vMessage <> "" THEN DO:
    RUN cleanup.
    RETURN vmessage.
  END. 
  
Return 'Submitted'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WriteData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WriteData Procedure 
PROCEDURE WriteData :
DEF INPUT param mdata       AS memptr NO-UNDO.
  DEF INPUT param hsocket     AS handle NO-UNDO.
  DEF VAR DataBytesWritten        as int NO-UNDO.
  DEF VAR WriteSuccess            as log NO-UNDO.
  DEF VAR MessageSize             as int NO-UNDO.
  DEF VAR mystring                as char NO-UNDO.

  ASSIGN
  MessageSize = GET-SIZE(mdata)
  DataBytesWritten = 0.
  IF messagesize = 0 THEN RETURN.

  /* 7/10/01 PK - Chomp off the Null at the end of the memptr data */
  IF GET-BYTE(mData,messagesize) = 0 OR
     GET-BYTE(mData,messagesize) = 255 THEN DO:  /* ||| Is this I18N compatible? - SES */
    messagesize = messagesize - 1.
  END.

  /* 6/20/01 GC - Loop continuously until the number of bytes
                  written is greater or equal to the message size */
  DO WHILE DataBytesWritten < MessageSize:
    WriteSuccess = hSocket:WRITE(mdata, DataBytesWritten + 1,
                                 MessageSize - DataBytesWritten).
    /*IF NOT WriteSuccess THEN LEAVE. */
    IF WriteSuccess THEN 
      DataBytesWritten = DataBytesWritten + hSocket:BYTES-WRITTEN.
  END. /* DO WHILE */
  SET-SIZE(mData) = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-NewState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewState Procedure 
FUNCTION NewState RETURNS INTEGER
  (INPUT newstate as int,
   INPUT pstring as char,
   INPUT hSocket AS HANDLE):
  DEF VAR vState              as int NO-UNDO.
  DEF VAR vbuffer             AS MEMPTR  NO-UNDO.
  DEF VAR DataBytesWritten    as int NO-UNDO.
  DEF VAR WriteSuccess        as log NO-UNDO.
  DEF VAR MessageSize         as int NO-UNDO.
  
  ASSIGN 
    DataBytesWritten = 0
    MessageSize = LENGTH(pstring,"RAW").
        
  SET-SIZE(vbuffer) = 0.
  vState = newState.
  IF pstring = "" THEN RETURN -1.
  
  SET-SIZE(vbuffer) = LENGTH(pstring,"RAW") + 1.
  PUT-STRING(vbuffer,1) = pstring.
  
  DO WHILE DataBytesWritten < MessageSize:
    WriteSuccess = hSocket:WRITE(vbuffer, DataBytesWritten + 1, 
                                 MessageSize - DataBytesWritten).
    IF NOT WriteSuccess THEN LEAVE.
    DataBytesWritten = DataBytesWritten + hSocket:BYTES-WRITTEN.
  END. 
  
  SET-SIZE(vbuffer) = 0.
  
  RETURN vstate.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

