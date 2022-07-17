&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          centrec          PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE t-zen-blob NO-UNDO LIKE zen-blob.
DEFINE TEMP-TABLE t-zen-ocx NO-UNDO LIKE zen-ocx.



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
{app-paths.i}
&glob login TRUE

def var lv-windir as char no-undo.
 lv-windir = os-getenv('windir').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-RegisteredOcx) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RegisteredOcx Procedure 
FUNCTION RegisteredOcx RETURNS LOGICAL
    ( pv-ocx as char )  FORWARD.

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
   Temp-Tables and Buffers:
      TABLE: t-zen-blob T "?" NO-UNDO centrec zen-blob
      TABLE: t-zen-ocx T "?" NO-UNDO schadm zen-ocx
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 7.76
         WIDTH              = 39.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
    /* need to change this to only return where install = true 
      and also to NOT pass over blob of control we need to only
       get that if its not registered */
       
    {{&core}run.i &program   = "zen-ocx.p"
                 &path      = "{&core}{&srv}"
                 &Appsrv    = "System"  
                 &noper     = true
                 &procedure = "open-query"
                 &params    = "(output table t-zen-ocx)"}

FOR EACH t-zen-ocx WHERE t-zen-ocx.install:
    IF NOT RegisteredOCX(t-zen-ocx.ocx-name) 
    THEN do:
       run RegisterOCX(t-zen-ocx.ocx-filename).
       if return-value ne 'passed' 
       then Message msg(159,t-zen-ocx.ocx-filename,'','','')
            view-as alert-box warning.
    end.
end.
 
PROCEDURE REGISTERDLL EXTERNAL "{&core}ocx~\Regist10.dll":
    def input param Dllpath as char.
    def input param bRegister AS byte.
    DEFINE RETURN param hInstance AS byte.
END.

if not session:remote 
    then sysmsg("Off").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GetBlobs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetBlobs Procedure 
PROCEDURE GetBlobs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
for each t-zen-blob:
    delete t-zen-blob.
end.

{{&core}run.i &program   = "zen-ocx.p"
              &path      = "{&core}{&srv}"
              &Appsrv    = "System"  
              &noper     = true
              &procedure = "get-records"
              &params    = "('{&table-name}',
                             t-{&table-name}.{&table-name}tableid,
                             output table t-zen-blob)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RegisterOcx) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RegisterOcx Procedure 
PROCEDURE RegisterOcx :
def input param pv-ocx as char no-undo.

def var lv-file   as char no-undo.
def var lv-from   as char no-undo.
def var lv-to     as char no-undo.
DEF VAR rg-path   AS CHAR NO-UNDO.
DEF VAR hInstance AS INT  NO-UNDO.
def var lv-blob as memptr no-undo.
def var lv-ok     as INT  no-undo.
def var x         as int  no-undo.

assign
   pv-ocx = dospath(pv-ocx)
   x = max(1,num-entries(pv-ocx,'~\'))
   lv-file = entry(x,pv-ocx,'~\')
   lv-to   = substring(pv-ocx,1,r-index(pv-ocx,'~\') - 1).

if lv-to begins '$windir' 
then lv-to = replace(lv-to,'$windir',lv-windir).

find t-zen-blob where t-zen-blob.Blob-Filename = t-zen-ocx.ocx-filename.
copy-lob from t-zen-blob.blob-data to lv-blob.

if get-size(lv-blob) ne 0 and lv-from ne ''
then do:
  if not session:remote and
      t-zen-ocx.ocx-name ne 'unregisterable'
      then sysmsg(AltLanguage("Attempting to register OCX control " + pv-ocx + "...")).
   os-create-dir value(lv-to). /* it will fail if already exits so no problem */
   
   outputtofile(lv-to + lv-file,lv-blob,'local').
 
   if t-zen-ocx.ocx-name ne 'unregisterable'
   then do:
      execute('regsvr32.exe /s',
              lv-windir + '~\system32',
              lv-to + '~\' + lv-file,
              'normal').
   end.
   /* NEW style registration doesnot work yet */
   /*      run REGISTERDLL (lv-to + '~\' + lv-file,1,OUTPUT lv-ok).  */
   /*      if lv-ok NE 255                                     */
   /*         then return 'passed'.                            */
   /*         ELSE RETURN 'failed'.                            */
   
   lv-to   = lv-to + '~\'.

   do x = 1 to num-entries(t-zen-ocx.SurportingFiles):
     if entry(2,entry(x,t-zen-ocx.SurportingFiles),'.') ne 'exe' and
        entry(2,entry(x,t-zen-ocx.SurportingFiles),'.') ne 'msi' 
     then do:
        find t-zen-blob where t-zen-blob.Blob-Filename = entry(x,t-zen-ocx.SurportingFiles).
        copy-lob from t-zen-blob.blob-data to lv-blob.
        if get-size(lv-blob) ne 0 and lv-from ne ''
        then do:
           outputtofile(lv-to,lv-blob,'local').

     end.
     
     OS-COPY value(lv-from + entry(x,t-zen-ocx.SurportingFiles)) 
                  value(lv-to + entry(x,t-zen-ocx.SurportingFiles)).
     else execute(entry(x,t-zen-ocx.SurportingFiles),lv-from,"",'normal').
   end.
   return 'passed'.  
END.
ELSE return 'failed'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-RegisteredOcx) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RegisteredOcx Procedure 
FUNCTION RegisteredOcx RETURNS LOGICAL
    ( pv-ocx as char ) :

      if t-zen-ocx.ocx-name = 'unregisterable' 
      then return false.

      DEF VAR ch-Check AS COM-HANDLE NO-UNDO.

      CREATE VALUE(pv-ocx) ch-Check NO-ERROR.
      
      IF error-status:error and not
         ERROR-STATUS:GET-MESSAGE(1) MATCHES "*0x80040112*" 
      THEN return False.

      else RETURN true.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

