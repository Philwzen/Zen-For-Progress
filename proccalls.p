&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
*/


/* Temp-Table and Buffer definitions                                    */
define temp-table t-Zen-Dpgm NO-UNDO LIKE Zen-Dpgm.



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
&glob docdir documentation/syscalls/

def stream ip.
def stream op.
def stream idx.

def var streamopen as log no-undo.
define temp-table t-data no-undo
    field t-proc as char
    field t-intproc as char
    field t-params as char extent 100
    index order t-proc t-intproc.

define temp-table t-api no-undo like zen-apidetail
    field params as  char extent 100.

define temp-table t-dpgm no-undo 
    field author  as char
    field created as date
    field nAME    as char
    field PGM     as char
    index order name.

def var lv-runninglist    as char no-undo.
def var lv-runninghandles as char no-undo.
def var lv-intproclist as char no-undo.
def var h-appserver as handle no-undo.
    def var lv-companytitle as char no-undo.
    def var lv-sysversion   as char no-undo.
    assign
        lv-companytitle = GetCtrl("{&CompanyTitle}")
        lv-sysversion   = GetCtrl("{&SystemVersion}").

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
   Temp-Tables and Buffers:
      TABLE: t-Zen-Dpgm T "?" NO-UNDO sigmstr Zen-Dpgm
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 12.67
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

Run runall.
sysmsg('Loading Procedures').    
run loadprocs.
run printall.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CheckAPi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckAPi Procedure 
PROCEDURE CheckAPi :
def input param lv-prog   as char no-undo.
def input param lv-proc   as char no-undo.
def input param lv-path   as char no-undo.


def var x as int no-undo.
def var lv-apiname as char no-undo.

lv-apiname = substring(lv-prog,index(lv-prog,'.') + 1) + lv-proc.


    if not can-find(first t-Api where t-api.programname   = lv-prog
                                  and t-Api.ProcedureName = lv-proc)
    then do:                                          
        create t-Api.
        assign t-Api.ApiName       = lv-apiname
               t-api.programname   = lv-prog
               t-Api.ProcedureName = lv-proc
               t-Api.ProgramPath   = lv-path.
        do x = 1 to 100:               
               t-Api.params[x]     = t-data.t-params[x].
        end.
    end.
    
/*     {{&core}run.i &program   = "zen-apidetail.p"  */
/*                  &path      = "{&core}{&srv}"     */
/*                 &noper        = true             */
/*                  &Appsrv    = "System"           */
/*                  &procedure = "newapi"           */
/*                  &params    = "(lv-apiname,      */
/*                                 lv-prog,         */
/*                                 lv-proc,         */
/*                                 lv-path)"}       */


    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateHeader Procedure 
PROCEDURE CreateHeader :
def input param lv-prog as char no-undo.
def input param lv-path as char no-undo.    
    
        
    output stream op  close.
    streamopen = false.
    sysmsg('Processing ' + lv-prog).    
            
            output stream op to value(dospath("{&docdir}" + lv-prog + ".html")).
           streamopen = true. 
            run getdpgm(lv-path + lv-prog).
            
            put stream op unformatted
                '<!doctype html public "-//w3c//dtd html 4.0 transitional//en">' skip
                '<html><head><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' skip
                '<meta name="GENERATOR" content="Mozilla/4.5 [en]C-CCK-MCD   (WinNT; U) [Netscape]">' skip
                '<title>Zen Library 2001 Release Notes</title></head>' skip
                '<body bgcolor="#FFFFFF" link="#0000FF" vlink="#800080">' skip
                '<!-- ========== START OF NAVBAR ========== -->
                <A NAME="navbar_top"><!-- --></A>
                <TABLE BORDER="0" WIDTH="100%" CELLPADDING="1" CELLSPACING="0">
                <TR>
                <TD COLSPAN=2 BGCOLOR="#EEEEFF" CLASS="NavBarCell1">
                <A NAME="navbar_top_firstrow"><!-- --></A>
                <TABLE BORDER="0" CELLPADDING="0" CELLSPACING="3">
                <TR ALIGN="center" VALIGN="top">
                <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="index.html"><FONT ID="NavBarFont1"><B>Index</B></FONT></A></TD>                
                <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="apiindex.html"><FONT ID="NavBarFont1"><B>Api Index</B></FONT></A></TD>
                </TR>
                </TABLE>
                </TD>
                <TD ALIGN="right" VALIGN="top" ROWSPAN=3><EM>'
                lv-companytitle ' ' lv-sysversion '</EM>
                </TD>
                </TR>
                <!-- =========== END OF NAVBAR =========== -->' skip
                '<h1><a NAME="Top"></a>Release Notes</h1>' skip
                '<b>These release notes contain information specific to ' lv-sysversion ' release' skip
                '(referred to in these notes as "this release")</b>' skip
                '<br><H2><FONT SIZE="-1">' lv-sysversion ' Library Routine</FONT>' skip
                '<BR>' lv-prog ' </H2>' skip.
            put stream op unformatted
                '<A NAME="Procedure Summary"><!-- --></A></font>' skip
                '<TABLE BORDER="1" CELLPADDING="3" CELLSPACING="0" WIDTH="100%">' skip
                '<TR BGCOLOR="#CCCCFF" CLASS="TableHeadingColor">' skip
                '<TD COLSPAN=5><FONT SIZE="+2">' skip
                '<B>Procedure Summary</B></FONT></TD> </TR>' skip.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoIndex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoIndex Procedure 
PROCEDURE DoIndex :
def var lv-path as char no-undo.
def var lv-prog as char no-undo.
 
 sysmsg('Creating Index').
 
output stream idx to value(dospath("{&docdir}index.html")).

put stream idx unformatted
    '<!doctype html public "-//w3c//dtd html 4.0 transitional//en">' skip
    '<html><head><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' skip
    '<meta name="GENERATOR" content="Mozilla/4.5 [en]C-CCK-MCD   (WinNT; U) [Netscape]">' skip
    '<title>Zen Library 2001 Index Release Notes</title></head>' skip
    '<body bgcolor="#FFFFFF" link="#0000FF" vlink="#800080">' skip
    '<h1><a NAME="Top"></a>Zen Library 2001 Release Notes</h1>' skip
    '<b>These release notes contain information specific to ' lv-sysversion ' release' skip
        '(referred to in these notes as "this release")</b>' skip
    '<br><H2><FONT SIZE="-1"></FONT>' skip
    '<!-- ========== START OF NAVBAR ========== -->
    <A NAME="navbar_top"><!-- --></A>
    <TABLE BORDER="0" WIDTH="100%" CELLPADDING="1" CELLSPACING="0">
    <TR>
    <TD COLSPAN=2 BGCOLOR="#EEEEFF" CLASS="NavBarCell1">
    <A NAME="navbar_top_firstrow"><!-- --></A>
    <TABLE BORDER="0" CELLPADDING="0" CELLSPACING="3">
    <TR ALIGN="center" VALIGN="top">
    <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="apiindex.html"><FONT ID="NavBarFont1"><B>Api Index</B></FONT></A></TD>                
    </TR>
    </TABLE>
    </TD>
    <TD ALIGN="right" VALIGN="top" ROWSPAN=3><EM>
    Zen Library 2001</EM>
    </TD>
    </TR>
     <!-- =========== END OF NAVBAR =========== -->' skip
    '<A NAME="Procedures"><!-- --></A></font>' skip
    '<TABLE BORDER="1" CELLPADDING="3" CELLSPACING="0" WIDTH="100%">' skip
    '<TR BGCOLOR="#CCCCFF" CLASS="TableHeadingColor">' skip
    '<TD COLSPAN=5><FONT SIZE="+2"><B>Procedures</B></FONT></TD></TR>' skip.
    
    {{&core}run.i &program   = "zen-dpgm.p"
                 &path      = "{&core}{&srv}"
                 &noper     = true
                 &Appsrv    = "System"
                 &procedure = "BuildList"
                 &params    = "(input-output table t-dpgm)"}
                 

for each t-dpgm by t-dpgm.pgm:
    assign
        lv-prog = entry(num-entries(t-dpgm.pgm,'/'),t-dpgm.pgm,'/')
        lv-path = substring(t-dpgm.pgm,1,r-index(t-dpgm.pgm,'/')).
    put stream idx unformatted
      '<TR BGCOLOR="white" CLASS="TableRowColor">' skip
      '<TD ALIGN="right" VALIGN="top" WIDTH="1%"><FONT SIZE="+1"></td><B>' skip
      '<TD><A HREF="' lv-prog '.html">' lv-prog '</A></B></td>' skip.
    put stream idx unformatted
      '<TD>' t-Dpgm.NAME '</B></td>' skip
      '<TD>' lv-path '</B></td>' skip
      '<TD>' string(t-Dpgm.created,'99/99/9999') '</td>' skip
      '<TD>' t-Dpgm.author '</td></tr></FONT></B>' skip.
end.
put stream idx unformatted '</FONT>' skip.

output stream idx close.
output stream idx to value("{&docdir}APIindex.html").    
put stream idx unformatted
    '<!doctype html public "-//w3c//dtd html 4.0 transitional//en">' skip
    '<html><head><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' skip
    '<meta name="GENERATOR" content="Mozilla/4.5 [en]C-CCK-MCD   (WinNT; U) [Netscape]">' skip
    '<title>Zen Library 2001 Index Release Notes</title></head>' skip
    '<body bgcolor="#FFFFFF" link="#0000FF" vlink="#800080">' skip
    '<h1><a NAME="Top"></a>Zen Library 2001 Release Notes</h1>' skip
    '<b>These release notes contain information specific to ' lv-sysversion ' release' skip
        '(referred to in these notes as "this release")</b>' skip
    '<br><H2><FONT SIZE="-1"></FONT>' skip
    '<!-- ========== START OF NAVBAR ========== -->
    <A NAME="navbar_top"><!-- --></A>
    <TABLE BORDER="0" WIDTH="100%" CELLPADDING="1" CELLSPACING="0">
    <TR>
    <TD COLSPAN=2 BGCOLOR="#EEEEFF" CLASS="NavBarCell1">
    <A NAME="navbar_top_firstrow"><!-- --></A>
    <TABLE BORDER="0" CELLPADDING="0" CELLSPACING="3">
    <TR ALIGN="center" VALIGN="top">
    <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="index.html"><FONT ID="NavBarFont1"><B>Index</B></FONT></A></TD>                
    </TR>
    </TABLE>
    </TD>
    <TD ALIGN="right" VALIGN="top" ROWSPAN=3><EM>
    Zen Library 2001</EM>
    </TD>
    </TR>
     <!-- =========== END OF NAVBAR =========== -->' skip
    '<A NAME="Api Functions"><!-- --></A></font>' skip
    '<TABLE BORDER="1" CELLPADDING="3" CELLSPACING="0" WIDTH="100%">' skip
    '<TR BGCOLOR="#CCCCFF" CLASS="TableHeadingColor">' skip
    '<TD COLSPAN=5><FONT SIZE="+2"><B>Api Functions</B></FONT></TD></TR>' skip.
    
for each t-api by t-api.ApiName
               by t-api.ProcedureName:
               
    put stream idx unformatted
      '<TR BGCOLOR="white" CLASS="TableRowColor">' skip
      '<TD ALIGN="right" VALIGN="top" WIDTH="1%"><FONT SIZE="+1"></td><B>' skip
      '<TD>' t-api.ApiName '</B></td>' skip
      '<TD><A HREF="' t-api.programname '.html#' t-api.ProcedureName '"rel=back>' t-api.ProcedureName '</a></B></td>' skip
      '<TD><A HREF="' t-api.programname '.html"rel=back>' t-api.programname '</A></B></td>' skip
      '<TD>' t-api.ProgramPath '</td>' skip.
    
end.
put stream idx unformatted '</FONT>' skip.
output stream idx close.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetDpgm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDpgm Procedure 
PROCEDURE GetDpgm :
def input param lv-prg as char no-undo.

    create t-dpgm.
           t-Dpgm.PGM     = lv-prg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetInfo Procedure 
PROCEDURE GetInfo :
def var h as handle no-undo.
def var x as int no-undo.
def var y as int no-undo.
def var z as int no-undo.
def var lv-proclist as char no-undo.
def var lv-paramlist as char no-undo.
    
        do x = 1 to num-entries(lv-runninglist):
            h = widget-handle(entry(x,lv-runninghandles)).
            if h:internal-entries ne ? then do:
                lv-proclist = h:internal-entries.
                do y = 1 to num-entries(lv-proclist):
                    lv-paramlist = h:get-signature(entry(y,lv-proclist)).
                    
                    if entry(1,lv-paramlist) = 'EXTERN' then next.
/*                       if can-find(first t-data where t-data.t-intproc = entry(y,lv-proclist))
                       then next. */
                    
                    if can-find(first t-data where t-data.t-proc = entry(x,lv-runninglist)
                                               and t-data.t-intproc = entry(y,lv-proclist))
                    then next.
                    
                    create t-data.
                    assign t-data.t-proc = entry(x,lv-runninglist)
                           t-data.t-intproc = entry(y,lv-proclist).
                    do z = 1 to num-entries(lv-paramlist):
                        t-data.t-params[z]  = entry(z,lv-paramlist).
                    end.
                end.
            end.
        end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadProcs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadProcs Procedure 
PROCEDURE LoadProcs :
def var h as handle no-undo.
def var lv-context as char no-undo.    
    assign h = session:first-procedure
           lv-runninglist = ''.
    run walktree (h).       
    h-appserver = getappserverhandle("system") no-error.
    IF VALID-HANDLE(h-appserver) 
    THEN DO:
        h = h-appserver:FIRST-PROCEDURE.
        run walktree (h).
    END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintAll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintAll Procedure 
PROCEDURE PrintAll :
def var irec    as char no-undo.

def var lv-prog as char no-undo.
def var lv-path as char no-undo.

def var lv-done as char no-undo.
def var x as int no-undo.
def var lv-ok as log no-undo.
sysmsg('Determining Calls').    

run getinfo.

for each t-data by t-proc by t-intproc:
    if lv-prog ne entry(num-entries(t-proc,'/'),t-proc,'/')
    then do:
        if can-do(lv-done,t-proc) 
            then next.
            else lv-done = lv-done + ',' + t-proc.
        assign lv-path = substring(t-proc,1,r-index(t-proc,'/'))
               lv-prog = entry(num-entries(t-proc,'/'),t-proc,'/'). 
        if lv-prog begins '_' or
           lv-path begins 'ade'
        then next.   
/* message lv-path skip lv-prog view-as alert-box buttons yes-no update lv-ok. */
/* if not lv-ok then return. */
        run createheader (lv-prog,lv-path).
    end.
if streamopen then do:
    if t-data.t-params[1] ne 'extern' then do:
        run checkAPI(lv-prog,t-intproc,lv-path).

        put stream op unformatted
            '<TR BGCOLOR="white" CLASS="TableRowColor">' skip
            '<TD ALIGN="right" VALIGN="top" WIDTH="1%"><FONT SIZE="+2">' skip
            '<B><CODE>' t-params[1] '</CODE></TD>' skip
            '<TD><CODE><a name= ' t-intproc '>' t-intproc '</A></B>() ' t-params[2] '</FONT></CODE>' skip.
        x = 3.
        do x = 3 to 100:
            if t-params[x] = '' then leave.
            put stream op unformatted
                '<BR>' skip
                t-params[x] skip.
        end.
    end.
    put stream op unformatted '</TD></TR>' skip.
    end.

    end.

run DOIndex.

sysmsg('off').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-runall) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runall Procedure 
PROCEDURE runall :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*         Run {&core}appserverload.p ("").  */
/*     {{&core}run.i &program   = "zen-dpgm.p"                   */
/*                  &path      = "{&core}{&srv}"                 */
/*                  &noper     = true                           */
/*                  &Appsrv    = "System"                       */
/*                  &procedure = "BuildList"                    */
/*                  &params    = "(input-output table t-dpgm)"} */
/*     For Each t-dpgm:                                         */
/*         If Entry(2,t-dpgm.pgm,'.') = 'w' Then                */
/*         Run Value(t-dpgm.pgm) Persist.                       */
/*     End.                                                     */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WalkTree) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WalkTree Procedure 
PROCEDURE WalkTree :
def input param h as handle no-undo.

def var v-fname as char no-undo.
def var v-pos   as int  no-undo.

      do while valid-handle(h):   
            assign v-pos   = r-index(h:file-name, "~\")
                   v-pos   = v-pos + 1
                   v-fname = substring(h:file-name, v-pos).
            
            if lv-runninglist = '' then
                assign lv-runninglist    = v-fname
                       lv-runninghandles = string(h).
            else
                assign lv-runninglist = lv-runninglist + "," + v-fname
                       lv-runninghandles = lv-runninghandles + "," + string(h).          
            
            h = h:next-sibling.
        end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

