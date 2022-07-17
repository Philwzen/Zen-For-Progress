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
&glob serverprogram true
{app-paths.i}

{{&core}{&rep}reptemp.i}   

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

&IF DEFINED(EXCLUDE-ExtractData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtractData Procedure 
PROCEDURE ExtractData :
{{&base}{&rep}srvrepdefs.i} /* define printing parameters */
def var lv-user as char no-undo.
def var vc-desc as char no-undo.
    lv-user       = GetSysVar("user").
    
    /* custom vars for this extract */
    def var lv-author as char no-undo.
    /* end custom definitions */

    /* assign custom vars from input param string */
    assign
           lv-author = entry(1,pv-params,"{&Delim2}").
    /* end custom assign */
    OUTPUT STREAM out-s TO VALUE('./' + lv-user + '.dat').

    /* clear down any old temps */
    empty temp-table t-data.
    /* create field names - headers for crystal first */
    /* in the format and one set per table to be extracted */
    create t-data.
    assign t-data.data = "key{&delim2}pgm{&Delim2}name{&Delim2}description{&delim2}author{&delim2}created"
           t-data.type = "FieldNames"
           t-data.tablename = "dpgm.txt". /* this joins data to header fields */
    
    
    /* start dump of relevant data - we are dumping as building up a string and assigning straight to
       the temp-tables .data field has problems when stringing a log/dec/int/date with a ? as whole string
       becomes ? */

    for each zen-dpgm where zen-dpgm.author matches lv-author
             no-lock:
       PUT STREAM out-s UNFORMATTED 'dpgm.txt{&Delim2}' STRING(zen-dpgmtableid) '{&Delim2}'
                                    zen-dpgm.pgm '{&Delim2}' 
                                    zen-dpgm.name '{&Delim2}' 
                                    zen-dpgm.Description '{&Delim2}' 
                                    zen-dpgm.author '{&delim2}'
                                    zen-dpgm.created '{&Delim2}' SKIP.
/*        FOR EACH table2 of table1                                                                       */
/*                         no-lock:                                                                       */
/*            PUT STREAM out-s UNFORMATTED 'extractfielname2.dat{&Delim2}' STRING(keyfield1) '{&Delim2}'  */
/*                                          field2                                '{&Delim2}'             */
/*                                          etc                                   '{&Delim2}' SKIP.       */
/*        END.                                                                                            */
    end. 
    /* end of data extract routine */

    OUTPUT STREAM out-s CLOSE.
    /* read back from stream and create relevant records */
    INPUT stream out-s FROM value('./' + lv-user + '.dat') NO-ECHO.    

    REPEAT:
       vc-desc = ''.
       IMPORT STREAM out-s UNFORMATTED vc-desc.
       CREATE t-data.
       ASSIGN t-data.tablename = entry(1,vc-desc,'{&Delim2}')
              t-data.type      = 'data'
              t-data.data      = SUBSTRING(vc-desc, INDEX(vc-desc,'{&Delim2}') + 1, LENGTH(vc-desc)).
    END. /* repeat */
    INPUT STREAM out-s CLOSE.

    OS-DELETE value('./' + lv-user + '.dat').

    return "passed".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

