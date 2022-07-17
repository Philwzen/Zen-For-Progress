&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

  define var hInternetSession   as  int  no-undo.
  define var hFTPSession        as  int  no-undo.
  define var cCurrentDir        as  char no-undo.

&SCOPE  MAX_PATH 260

&SCOPE FILE_ATTRIBUTE_NORMAL  128

/* Internet constants */
&SCOPE INTERNET_OPEN_TYPE_PRECONFIG    0
/* indicates to use config information from registry */
&SCOPE INTERNET_FLAG_EXISITING_CONNECT 536870912
/* used for ftp connections */
&SCOPE INTERNET_FLAG_PASSIVE           134217728

/* Flags for FTP transfer mode */
&SCOPE  FTP_TRANSFER_TYPE_ASCII  1   /* 0x00000001 */
&SCOPE  FTP_TRANSFER_TYPE_BINARY 2   /* 0x00000002 */

&SCOPE INTERNET_DEFAULT_FTP_PORT     21
&SCOPE INTERNET_DEFAULT_GOPHER_PORT  70
&SCOPE INTERNET_DEFAULT_HTTP_PORT    80
&SCOPE INTERNET_DEFAULT_HTTPS_PORT  443
&SCOPE INTERNET_DEFAULT_SOCKS_PORT 1080

/* Type of service to access */
&SCOPE INTERNET_SERVICE_FTP    1
&SCOPE INTERNET_SERVICE_GOPHER 2
&SCOPE INTERNET_SERVICE_HTTP   3

PROCEDURE InternetConnectA EXTERNAL "wininet.dll" PERSISTENT:
  def input param  hInternetSession  as  long.
  def input param  lpszServerName    as  char.
  def input param  nServerPort       as  long.
  def input param  lpszUserName      as  char.
  def input param  lpszPassword      as  char.
  def input param  dwService         as  long.
  def input param  dwFlags           as  long.
  def input param  dwContext         as  long.
  define return param hInternetConnect  as  long.
END.

PROCEDURE InternetGetLastResponseInfoA EXTERNAL "wininet.dll" PERSISTENT:
  def output param lpdwError          as  long.
  def output param lpszBuffer         as  char.
  def input-output param lpdwBufferLength   as  long.
  define return param iResultCode       as  long.
END.

PROCEDURE InternetOpenUrlA EXTERNAL "wininet.dll" PERSISTENT:
  def input param  hInternetSession  as  long.
  def input param  lpszUrl           as  char.
  def input param  lpszHeaders       as  char.
  def input param  dwHeadersLength   as  long.
  def input param  dwFlags           as  long.
  def input param  dwContext         as  long.
  define return param iResultCode       as  long.
END.

PROCEDURE InternetOpenA EXTERNAL "wininet.dll" PERSISTENT:
  def input param  sAgent            as  char.
  def input param  lAccessType       as  long.
  def input param  sProxyName        as  char.
  def input param  sProxyBypass      as  char.
  def input param  lFlags            as  long.
  define return param iResultCode       as  long.
END.

PROCEDURE InternetReadFile EXTERNAL "wininet.dll" PERSISTENT:
  def input param  hFile            as  long.
  def output param  sBuffer          as  char.
  def input param  lNumBytesToRead  as  long.
  def output param  lNumOfBytesRead  as  long.
  define return param  iResultCode      as  long.
END.

PROCEDURE InternetCloseHandle EXTERNAL "wininet.dll" PERSISTENT:
  def input param  hInet             as  long.
  define return param iResultCode       as  long.
END.

PROCEDURE FtpFindFirstFileA EXTERNAL "wininet.dll" PERSISTENT :
    def input param  hFtpSession as  long.
    def input param  lpFileName as char.
    def input param  lpFindFileData as memptr.
    def input param  dwFlags        as long.
    def input param  dwContext      as long.
    define return param hSearch as long.
END PROCEDURE.    


PROCEDURE InternetFindNextFileA EXTERNAL "wininet.dll" PERSISTENT:
    def input param  hSearch as long.
    def input param  lpFindFileData as memptr.
    define return param found as long.
END PROCEDURE.


PROCEDURE FtpGetCurrentDirectoryA EXTERNAL "wininet.dll" PERSISTENT:
    def input param  hFtpSession as long.
    def input param  lpszCurrentDirectory as long.
    def input-output param lpdwCurrentDirectory as long.
    define return param iRetCode as long.
END PROCEDURE.

PROCEDURE FtpSetCurrentDirectoryA EXTERNAL "wininet.dll" PERSISTENT:
    def input param  hFtpSession as long.
    def input param  lpszDirectory as long.
    define return param iRetCode as long.
END PROCEDURE.

PROCEDURE FtpOpenFileA EXTERNAL "wininet.dll" PERSISTENT:
    def input param  hFtpSession  as long.
    def input param  lpszFileName as long.
    def input param  dwAccess     as long.
    def input param  dwFlags      as long.
    def input param  dwContext    as long.
    define return param iRetCode as long.
END PROCEDURE.

PROCEDURE FtpPutFileA EXTERNAL "wininet.dll" PERSISTENT:
    def input param  hFtpSession       as long.
    def input param  lpszLocalFile     as long.
    def input param  lpszNewRemoteFile as long.
    def input param  dwFlags           as long.
    def input param  dwContext         as long.
    define return param iRetCode          as long.
END PROCEDURE.

PROCEDURE FtpGetFileA EXTERNAL "wininet.dll" PERSISTENT:
    def input param  hFtpSession          as long.
    def input param  lpszRemoteFile       as long.
    def input param  lpszNewFile          as long.
    def input param  fFailIfExists        as long.
    def input param  dwFlagsAndAttributes as long.
    def input param  dwFlags              as long.
    def input param  dwContext            as long.
    define return param iRetCode             as long.
END PROCEDURE.

PROCEDURE FtpDeleteFileA EXTERNAL "wininet.dll" PERSISTENT:
    def input param  hFtpSession          as long.
    def input param  lpszRemoteFile       as long.
    define return param iRetCode             as long.
END PROCEDURE.

PROCEDURE GetLastError external "kernel32.dll" :
  define return param dwMessageID as long. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CloseInternetConnection Include 
FUNCTION CloseInternetConnection RETURNS LOGICAL
  ( input phInternetSession as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConnectWinInet Include 
FUNCTION ConnectWinInet RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FtpConnect Include 
FUNCTION FtpConnect RETURNS LOGICAL
  (pv-URL as char,
   pv-user as char,
   pv-password as char) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FtpDeleteFile Include 
FUNCTION FtpDeleteFile RETURNS CHARACTER
  ( pv-file as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FtpDisconnect Include 
FUNCTION FtpDisconnect RETURNS LOGICAL
 ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FtpGetFile Include 
FUNCTION FtpGetFile RETURNS CHARACTER
  ( pv-source as char,
    pv-target as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FtpPutFile Include 
FUNCTION FtpPutFile RETURNS CHARACTER
  ( input pv-source as char,
    input pv-target as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FtpSetCurrentDir Include 
FUNCTION FtpSetCurrentDir RETURNS LOGICAL
  ( cCurDir as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InternetGetLastResponseInfo Include 
FUNCTION InternetGetLastResponseInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/*
if FtpConnect(lv-server,lv-user,lv-password)
then if FtpSetCurrentDir(lv-dir)
     then FtpPutFile(pv-file,GetFileName(pv-file)).

/* FtpGetFile(source,Target).*/
/* FtpDeleteFile(target). */

FtpDisconnect().
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CloseInternetConnection Include 
FUNCTION CloseInternetConnection RETURNS LOGICAL
  ( input phInternetSession as int ) :
/*------------------------------------------------------------------------------
  Purpose:  Close the handle the InternetSession.  Since all other handles are 
            leafs of this handle, the will also be closed when the root is 
            closed. (i.e. hFTPSession. )
 
    Notes:  
------------------------------------------------------------------------------*/
  def var iRetCode      as int no-undo.

  run InternetCloseHandle(input  phInternetSession,
                          output iRetCode).
     

  RETURN iRetCode > 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConnectWinInet Include 
FUNCTION ConnectWinInet RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  connect to specified website and exchange information.
    Notes:  
------------------------------------------------------------------------------*/
 
  /*-------------------------------------------------------------------------- 
    Call to establish an Internet session.  The handle, hInternetSession,
    will be used when connecting to the URL. 
  ---------------------------------------------------------------------------*/
  run InternetOpenA(input  'WebBasedAgent',
                    input  {&INTERNET_OPEN_TYPE_PRECONFIG},
                    input  '',
                    input  '',
                    input  0,
                    output hInternetSession).
  
  RETURN hInternetSession <> 0. /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FtpConnect Include 
FUNCTION FtpConnect RETURNS LOGICAL
  (pv-URL as char,
   pv-user as char,
   pv-password as char):
  def var iError as int no-undo.
  if ConnectWinInet() 
    then 
  run InternetConnectA(input  hInternetSession,
                       input  pv-URL,
                       input  {&INTERNET_DEFAULT_FTP_PORT},
                       input  pv-user,
                       input  pv-password,
                       input  {&INTERNET_SERVICE_FTP},
                       input  0,
                       input  0,
                       output hFTPSession).
  

  /* FOR CERN based Firewall support 
  RUN InternetOpenUrlA(INPUT hInternetSession,
                       INPUT pv-URL,
                       INPUT '',
                       INPUT 0,
                       INPUT {&INTERNET_FLAG_PASSIVE},
                       INPUT 0,
                       OUTPUT hFTPSession).
  */

  IF hFTPSession = 0 then
  do:
    run GetLastError(output iError).
    message "InternetConnectA Failed:  " iError view-as alert-box.
    InternetGetLastResponseInfo().
    RETURN FALSE.
  end.

  RETURN TRUE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FtpDeleteFile Include 
FUNCTION FtpDeleteFile RETURNS CHARACTER
  ( pv-file as char ) :
/*------------------------------------------------------------------------------
  Purpose:  Deletes a file from the FTP Server if you have permissions.
    Notes:  
------------------------------------------------------------------------------*/

  define var lpRemoteFile   as  memptr  no-undo.
  define var iRetCode       as int no-undo.
  
  assign
  /* remove the file size from the file name */
  set-size(lpRemoteFile)     = length(pv-file) + 1
  put-string(lpRemoteFile,1) = pv-file.
  
  Session:Set-Wait-State('General').
  
  run FtpDeleteFileA(input hFtpSession,
                     input get-pointer-value(lpRemoteFile),
                     output iRetCode).
                 
  Session:Set-Wait-State('').
  
  if iRetCode = 0 then
    InternetGetLastResponseInfo().

  assign
  set-size(lpRemoteFile)     = 0.

  RETURN "".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FtpDisconnect Include 
FUNCTION FtpDisconnect RETURNS LOGICAL
 ( /* parameter-definitions */ ) :
    
    CloseInternetConnection(hInternetSession).

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FtpGetFile Include 
FUNCTION FtpGetFile RETURNS CHARACTER
  ( pv-source as char,
    pv-target as char ) :
/*------------------------------------------------------------------------------
  Purpose:  Retrieves a file from the FTP Server and stores it under the 
            specified file name, creating a new local file in the process.
    Notes:  
------------------------------------------------------------------------------*/

  define var lpRemoteFile   as  memptr  no-undo.
  define var lpNewFile      as  memptr  no-undo.
  define var fOverwirte     as  log     no-undo.
  define var iRetCode       as int no-undo.
  
  assign
  /* remove the file size from the file name */
  set-size(lpRemoteFile)     = length(pv-source) + 1
  put-string(lpRemoteFile,1) = pv-source
  set-size(lpNewFile)        = length(pv-target) + 1
  put-string(lpNewFile,1)    = pv-target.
  
  Session:Set-Wait-State('General').
  
  run FtpGetFileA(input hFtpSession,
                 input get-pointer-value(lpRemoteFile),
                 input get-pointer-value(lpNewFile),
                 input 0, /* 1 - fail if file exists, 0 - overwrite */
                 input {&FILE_ATTRIBUTE_NORMAL},
                 input {&FTP_TRANSFER_TYPE_BINARY},
                 input 0,
                 output iRetCode).
                 
  Session:Set-Wait-State('').
  
  if iRetCode = 0 then
    InternetGetLastResponseInfo().

  assign
  set-size(lpRemoteFile)     = 0
  set-size(lpNewFile)        = 0.

  RETURN "".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FtpPutFile Include 
FUNCTION FtpPutFile RETURNS CHARACTER
  ( input pv-source as char,
    input pv-target as char ) :
/*------------------------------------------------------------------------------
  Purpose:  Sends a file to the FTP Server and stores it under the 
            specified file name, creating a new remote file in the process
            if you have the appropriate permissions.  If not you will be told
            so via InternetGetLastResponse.
    Notes:  
------------------------------------------------------------------------------*/

  define var lpLocalFile        as  memptr  no-undo.
  define var lpNewRemoteFile    as  memptr  no-undo.
  define var fOverwirte         as  log     no-undo.
  define var iRetCode           as int no-undo.
  
  assign
  /* remove the file size from the file name */
  set-size(lpNewRemoteFile)     = length(pv-target) + 1
  put-string(lpNewRemoteFile,1) = pv-target
  set-size(lpLocalFile)         = length(pv-source) + 1
  put-string(lpLocalFile,1)     = pv-source.
  
  Session:Set-Wait-State('General').
  
  run FtpPutFileA(input hFtpSession,
                  input get-pointer-value(lpLocalFile),
                  input get-pointer-value(lpNewRemoteFile),
                  input {&FTP_TRANSFER_TYPE_BINARY},
                  input 0,
                  output iRetCode).
                 
  Session:Set-Wait-State('').
  
  if iRetCode = 0 then
    InternetGetLastResponseInfo().

  assign
  set-size(lpNewRemoteFile)     = 0
  set-size(lpLocalFile)         = 0.
 


  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FtpSetCurrentDir Include 
FUNCTION FtpSetCurrentDir RETURNS LOGICAL
  ( cCurDir as char ) :
   def var iRetCode              as int  no-undo. 
   def var lpDirectory           as  memptr   no-undo. 
 
   set-size(lpDirectory)        = {&MAX_PATH}.

  put-string(lpDirectory,1) = cCurDir.
                                            
  run FtpSetCurrentDirectoryA(input hFTPSession,
                              input get-pointer-value(lpDirectory),
                              output iRetCode).
 
  if iRetCode = 0 then
  do:
    RUN GetLastError(OUTPUT iRetCode).
    IF iRetCode = 12003 
    THEN InternetGetLastResponseInfo().
    ELSE message 'FtpSetCurrentDirectory failed:' iRetCode view-as alert-box.  
  end.
   
  set-size(lpDirectory) = 0.
  
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InternetGetLastResponseInfo Include 
FUNCTION InternetGetLastResponseInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  If an error is encountered then display what the last response
            was.
    Notes:  
------------------------------------------------------------------------------*/
  define var cBuffer            as  char no-undo.
  define var iBufferSz          as  int init 4096 no-undo.
  define var iResultCode        as  int no-undo.
  define var iTemp              as  int no-undo.
  
  /* allocate for the buffer */
  assign
  cBuffer = fill(' ', iBufferSz).

  run InternetGetLastResponseInfoA (output iResultCode,
                                    output cBuffer,
                                    input-output iBufferSz,
                                    output iTemp).

  message substitute('Error (&1):  &2',
                     iResultCode,
                     substr(cBuffer,1,iBufferSz)) view-as alert-box.
                       
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

