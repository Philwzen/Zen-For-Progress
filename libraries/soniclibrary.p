&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
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
create widget-pool.
/* ***************************  Definitions  ************************** */
this-procedure:private-data = "library-sonic".
&glob library-sonic
&glob library-program

{app-paths.i}

DEF VAR pubsubsession AS HANDLE no-undo.
DEF VAR h-messwin     AS HANDLE NO-UNDO.
def var h-processing  as handle no-undo.
DEF VAR messageH      AS HANDLE no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-PubHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PubHandle Procedure 
FUNCTION PubHandle RETURNS HANDLE FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SendMessage Procedure 
FUNCTION SendMessage RETURNS LOGICAL
  (pv-topic as char, 
   pv-message as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetMessageConnections) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetMessageConnections Procedure 
FUNCTION SetMessageConnections RETURNS LOGICAL
  ( pv-system as char,
    pv-group  as char,
    pv-user   as char)  FORWARD.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-NewMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewMessage Procedure 
PROCEDURE NewMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

if not valid-handle(h-messwin) 
      then run {&core}messwin.w persist set h-messwin.
  run refresh in h-messwin ('','').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReadHandler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadHandler Procedure 
PROCEDURE ReadHandler :
def input  param messageH     AS HANDLE no-undo.  
def input  param msgConsumerH AS HANDLE no-undo.
def output param replyH       AS HANDLE no-undo.
    
def var lv-text    as char   no-undo.
def var lv-msgtype as char   no-undo.
def var lv-extra   as char   no-undo.

assign
    lv-msgtype = DYNAMIC-FUNCTION('getMessageType':U IN messageH)
    lv-extra = DYNAMIC-FUNCTION('getdestinationName':U IN msgconsumerH) + '{&Delim2}' + 
               DYNAMIC-FUNCTION('getCharProperty' IN messageH, "FROM").
    
/* Check message type */
    case lv-msgtype:
        when "XMLMessage"           then .
        when "BytesMessage"         then .
        when "ObjectMessage"        then .
        when "MapMessage"           then .
        when "StreamMessage"        then .
        when "TextMessage"          Then do:
            lv-text = DYNAMIC-FUNCTION('getText':U IN messageH).
            run SonicDisplay in this-procedure (lv-extra,lv-text).
        end.
        when "Message"              then .
        when "Unknown message type" then .
        otherwise .
    end case.
    RUN deleteMessage IN messageH.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetConnection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetConnection Procedure 
PROCEDURE SetConnection :
def input param pv-system as char no-undo.
def input param pv-group  as char no-undo.
def input param pv-user   as char no-undo.


    def var consumerH  AS HANDLE no-undo.
    def var consumerH1 AS HANDLE no-undo.
    def var consumerH2 AS HANDLE no-undo.
    
    def var lv-sonicserver as char no-undo.
    def var lv-sonicbroker as char no-undo.
    
    assign
        lv-sonicserver = getctrl("SonicServer")  /* -H localhost -S 5162 */
        lv-sonicbroker = getctrl("SonicBroker"). /*  "{&brokerHost}:{&brokerPort}" */
        
    if lv-sonicserver begins 'none' then return.
    if lv-sonicbroker begins 'none' then return.
    
    /* first connect to name server */
    RUN jms/pubsubsession.p PERSISTENT SET pubsubsession (lv-sonicserver) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN error.
    /* now connect to sonicmq broker port */
    RUN setBrokerURL IN pubsubsession (lv-sonicbroker) NO-ERROR.    
    IF ERROR-STATUS:ERROR THEN RETURN error.
    RUN beginSession IN pubsubsession NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN error.   
     
    /* for 7.1 we might have to use 
    searching your openedge documentation for ABL?JMS API Reference or 4GL?JMS API Reference 
    &scoped-define brokerHost mylaptop.mydomain.com
    &scoped-define brokerPort 23456
    &scoped-define connectOptions -SMQConnect
    &scoped-define connectUser Administrator
    &scoped-define connectPwd Administrator
    run jms/jmssession.p persistent set hBus ( " {&connectOptions} " ).
       run setBrokerURL in hBus ( "{&brokerHost}:{&brokerPort}" ).
       run setUser      in hBus ( "{&connectUser}" ).
       run setPassword  in hBus ( "{&connectPwd}" ).
       run setClientID  in hBus ( substring ( base64-encode ( generate-uuid ), 1, 22 ) ).
       run beginSession in hBus no-error.
    */
    
    
/*     RUN setdefaulttimetolive IN pubsubsession (86400000.00) no-error.  */
    RUN createMessageConsumer IN pubsubsession (
                                  THIS-PROCEDURE,    /* This proc will handle it */
                                 "readHandler", /* name of internal procedure */
                          OUTPUT consumerH2) NO-ERROR.    
    IF ERROR-STATUS:ERROR THEN RETURN error.
    RUN createMessageConsumer IN pubsubsession (
                                  THIS-PROCEDURE,    /* This proc will handle it */
                                 "readHandler", /* name of internal procedure */
                                  OUTPUT consumerH1) NO-ERROR.
                                  
    IF ERROR-STATUS:ERROR THEN RETURN error.
    RUN createMessageConsumer IN pubsubsession (
                                  THIS-PROCEDURE,    /* This proc will handle it */
                                 "readHandler", /* name of internal procedure */
                                  OUTPUT consumerH) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN error.
    RUN subscribe IN pubsubsession (pv-user,      /* name of topic */
                                 ?,                         /* Subscription is not durable */
                                 ?,                         /* No message selector */
                                 no,                        /* Want my own messages too */
                                 consumerH2) NO-ERROR.            
    IF ERROR-STATUS:ERROR THEN RETURN error.
    RUN subscribe IN pubsubsession (pv-gROUP,     /* name of topic */
                                 ?,                         /* Subscription is not durable */
                                 ?,                         /* No message selector */
                                 no,                        /* Want my own messages too */
                                 consumerH1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN error.
    RUN subscribe IN pubsubsession (pv-system,               /* name of topic */
                                 ?,                         /* Subscription is not durable */
                                 ?,                         /* No message selector */
                                 no,                        /* Want my own messages too */
                                 consumerH) NO-ERROR.                /* Handles the incoming messages*/
    
    IF ERROR-STATUS:ERROR THEN RETURN error.
    /* Start receiving messages */
    RUN startReceiveMessages IN pubsubsession NO-ERROR.        
    IF ERROR-STATUS:ERROR THEN RETURN error.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SonicDisplay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SonicDisplay Procedure 
PROCEDURE SonicDisplay :
def input param pv-extra   as char no-undo.
def input param pv-text    as char no-undo.

def var h-main as handle no-undo.

    if not session:remote then do:
        h-main = widget-handle(GetSysVar("top-window")).
        run messagereceived in h-main (pv-extra,pv-text).
        if num-entries(pv-text,'{&Delim2}') > 1 
        then do:
            if not valid-handle(h-processing)
                then run {&core}processing.w persist set h-processing.
            run refresh in h-processing (entry(2,pv-text,'{&Delim2}')).
        end.
        else do:
            if not valid-handle(h-messwin) 
                then run {&core}messwin.w persist set h-messwin.
            run refresh in h-messwin (pv-extra,pv-text).
        end.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WriteHandler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WriteHandler Procedure 
PROCEDURE WriteHandler :
def input param pv-topic as char no-undo.
    DEF INPUT PARAM pv-msg   AS CHAR no-undo.      
    
    RUN createTextMessage IN pubsubsession (OUTPUT messageH). 
    
    RUN setText IN messageH (pv-msg).
    /* Set the "FROM:" and the "TO:" properties */
    
    RUN setStringProperty IN messageH ("FROM",GetSysVar("user")).
    RUN setStringProperty IN messageH ("TO", pv-topic).
    RUN publish IN pubsubsession (pv-topic, messageH, ?, ?, ?) no-error.
    
    RUN deleteMessage IN messageH.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-PubHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PubHandle Procedure 
FUNCTION PubHandle RETURNS HANDLE:
  RETURN pubsubsession.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SendMessage Procedure 
FUNCTION SendMessage RETURNS LOGICAL
  (pv-topic as char, 
   pv-message as char) :

    RUN writeHandler (pv-topic,pv-message).

   return true.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetMessageConnections) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetMessageConnections Procedure 
FUNCTION SetMessageConnections RETURNS LOGICAL
  ( pv-system as char,
    pv-group  as char,
    pv-user   as char) :

    RUN SetConnection (pv-system,pv-group,pv-user) no-error.
    
    if error-status:error then 
    do:
    
    return false.
    end.
    else RETURN true.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

