&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ensignia         PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-brokstat NO-UNDO LIKE brokstat
       field t-recid as recid init ?
       field order as int format '9999'
       field GrComm as dec
       index order as primary agency_id glsubtyp_id order
       .


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: Viewer to display broker statements either from archive or if older
               than two month then on the fly from gltrans.             
  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Peter Wilson

  Created: 09/03/2004

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{app-paths.i}
{{&zen}winconst.i}

DEF VAR lh-col AS HANDLE NO-UNDO extent 20.

DEF BUFFER lb-gltrans FOR gltrans.

DEF VAR lv-type_id   AS CHAR FORMAT "x(3)" NO-UNDO.
DEF VAR lv-subtype   AS CHAR FORMAT "x(3)" NO-UNDO.
DEF VAR lv-ledger_co AS CHAR FORMAT "9(2)" NO-UNDO.

def temp-table t-processed no-undo
    field agency_id as char
    field Fnames    as char.

DEF STREAM op.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Br-Statement

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-brokstat

/* Definitions for BROWSE Br-Statement                                  */
&Scoped-define FIELDS-IN-QUERY-Br-Statement tt-brokstat.agency_id ~
tt-brokstat.policy_id tt-brokstat.trans_date tt-brokstat.policyholder ~
tt-brokstat.trans_type tt-brokstat.gross_prem tt-brokstat.agcy_comm ~
tt-brokstat.lloyds_comm tt-brokstat.ipt_amt tt-brokstat.nett_amount ~
tt-brokstat.part_pay tt-brokstat.balance 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br-Statement 
&Scoped-define QUERY-STRING-Br-Statement FOR EACH tt-brokstat ~
      WHERE tt-brokstat.glsubtyp_id = "610" AND ~
     (IF lv-startdate NE ? THEN tt-brokstat.TRANS_date >= lv-startdate ELSE TRUE) AND ~
     (IF lv-enddate NE ? THEN tt-brokstat.TRANS_date <= lv-enddate ELSE TRUE) AND ~
     IF lv-funder = "" THEN tt-brokstat.fund_prov_id = tt-brokstat.fund_prov_id  ~
          ELSE IF lv-funder = "NFD" THEN tt-brokstat.fund_prov_id = ""  ~
          ELSE IF lv-funder = "000" THEN tt-brokstat.fund_prov_id > ""  ~
          ELSE tt-brokstat.fund_prov_id = lv-funder  ~
 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br-Statement OPEN QUERY Br-Statement FOR EACH tt-brokstat ~
      WHERE tt-brokstat.glsubtyp_id = "610" AND ~
     (IF lv-startdate NE ? THEN tt-brokstat.TRANS_date >= lv-startdate ELSE TRUE) AND ~
     (IF lv-enddate NE ? THEN tt-brokstat.TRANS_date <= lv-enddate ELSE TRUE) AND ~
     IF lv-funder = "" THEN tt-brokstat.fund_prov_id = tt-brokstat.fund_prov_id  ~
          ELSE IF lv-funder = "NFD" THEN tt-brokstat.fund_prov_id = ""  ~
          ELSE IF lv-funder = "000" THEN tt-brokstat.fund_prov_id > ""  ~
          ELSE tt-brokstat.fund_prov_id = lv-funder  ~
 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br-Statement tt-brokstat
&Scoped-define FIRST-TABLE-IN-QUERY-Br-Statement tt-brokstat


/* Definitions for BROWSE br-unalloc                                    */
&Scoped-define FIELDS-IN-QUERY-br-unalloc tt-brokstat.agency_id ~
tt-brokstat.trans_num tt-brokstat.trans_date tt-brokstat.trans_type ~
tt-brokstat.agcy_comm tt-brokstat.lloyds_comm tt-brokstat.balance ~
tt-brokstat.notes 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-unalloc 
&Scoped-define QUERY-STRING-br-unalloc FOR EACH tt-brokstat NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-unalloc OPEN QUERY br-unalloc FOR EACH tt-brokstat NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-unalloc tt-brokstat
&Scoped-define FIRST-TABLE-IN-QUERY-br-unalloc tt-brokstat


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-immediate lv-as_at lv-agency_id ~
cb-Currency lv-lgbroker_id lv-policy_id bt-display lv-dates lv-funder ~
Br-Statement br-unalloc lv-output bt-back bt-excel bt-forward bt-print ~
bt-total_debt bt-total_sus RECT-1 RECT-2 RECT-3 
&Scoped-Define DISPLAYED-FIELDS tt-brokstat.lgbroker_id ~
tt-brokstat.payment_date 
&Scoped-define DISPLAYED-TABLES tt-brokstat
&Scoped-define FIRST-DISPLAYED-TABLE tt-brokstat
&Scoped-Define DISPLAYED-OBJECTS lv-immediate lv-as_at lv-agency_id ~
cb-Currency lv-lgbroker_id lv-txt lv-policy_id lv-dates lv-startdate ~
lv-enddate lv-funder cb-funder lv-output 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-immediate lv-as_at lv-agency_id cb-Currency ~
lv-lgbroker_id lv-dates lv-startdate lv-enddate cb-funder lv-output 
&Scoped-define List-2 tt-brokstat.lgbroker_id tt-brokstat.trans_num ~
tt-brokstat.due_date tt-brokstat.payment_date 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CloseExcelWord C-Win 
FUNCTION CloseExcelWord RETURNS LOGICAL
  ( pv-excelappl as com-handle,
    pv-wordappl as  com-handle )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnValidDate C-Win 
FUNCTION fnValidDate RETURNS CHARACTER
  ( ip-from AS DATE,
    ip-to AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD scrmsg C-Win 
FUNCTION scrmsg RETURNS LOGICAL
  ( pv-msg as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-back  NO-FOCUS FLAT-BUTTON
     LABEL "bt-back" 
     SIZE 9 BY 1.91 TOOLTIP "Previous As At Date".

DEFINE BUTTON bt-display  NO-FOCUS FLAT-BUTTON
     LABEL "bt-display" 
     SIZE 8 BY 1.91 TOOLTIP "Display Broker Statement Details".

DEFINE BUTTON bt-excel  NO-FOCUS FLAT-BUTTON
     LABEL "bt-excel" 
     SIZE 8 BY 1.91 TOOLTIP "Export to Calc/Excel".

DEFINE BUTTON bt-forward  NO-FOCUS FLAT-BUTTON
     LABEL "bt-forward" 
     SIZE 9 BY 1.91 TOOLTIP "Next As At Date".

DEFINE BUTTON bt-print  NO-FOCUS FLAT-BUTTON
     LABEL "bt-print" 
     SIZE 8 BY 1.91 TOOLTIP "Print Broker Statements".

DEFINE BUTTON bt-total_debt  NO-FOCUS FLAT-BUTTON
     LABEL "bt-total_debt" 
     SIZE 9 BY 1.91 TOOLTIP "Display Total Debt".

DEFINE BUTTON bt-total_sus  NO-FOCUS FLAT-BUTTON
     LABEL "bt_total_sus" 
     SIZE 8 BY 1.91 TOOLTIP "Display Total Unallocated".

DEFINE VARIABLE cb-Currency AS CHARACTER FORMAT "X(256)":U 
     LABEL "Currency" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "GBP" 
     DROP-DOWN-LIST
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE cb-funder AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE lv-agency_id AS CHARACTER FORMAT "X(5)":U 
     LABEL "Agency ID" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-as_at AS DATE FORMAT "99/99/9999":U 
     LABEL "As At Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-enddate AS DATE FORMAT "99/99/9999":U 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-lgbroker_id AS CHARACTER FORMAT "X(8)":U 
     LABEL "Lloyd's Broker" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-policy_id AS CHARACTER FORMAT "X(8)":U 
     LABEL "Policy ID" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-startdate AS DATE FORMAT "99/99/9999":U 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-subagcy AS CHARACTER FORMAT "X(5)":U 
     LABEL "Sub Agent" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .95 NO-UNDO.

DEFINE VARIABLE lv-txt AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 60 BY .81 NO-UNDO.

DEFINE VARIABLE lv-funder AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Funded and Non-Funded", "",
"Non-Funded Only", "NFD",
"Funded Only", "000"
     SIZE 34 BY 2.86 NO-UNDO.

DEFINE VARIABLE lv-output AS CHARACTER INITIAL "f" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Print", "p",
"File", "f"
     SIZE 10 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 36 BY 3.33.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 37 BY 3.33.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 83 BY 3.33.

DEFINE VARIABLE lv-dates AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE lv-immediate AS LOGICAL INITIAL no 
     LABEL "Immediate Print" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br-Statement FOR 
      tt-brokstat SCROLLING.

DEFINE QUERY br-unalloc FOR 
      tt-brokstat SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br-Statement
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br-Statement C-Win _STRUCTURED
  QUERY Br-Statement NO-LOCK DISPLAY
      tt-brokstat.agency_id COLUMN-LABEL "Agency!No" FORMAT "x(5)":U
      tt-brokstat.policy_id COLUMN-LABEL "Policy!No" FORMAT "x(8)":U
            WIDTH 12
      tt-brokstat.trans_date FORMAT "99/99/99":U WIDTH 9
      tt-brokstat.policyholder FORMAT "x(20)":U WIDTH 23.2
      tt-brokstat.trans_type COLUMN-LABEL "Type" FORMAT "x(4)":U
            WIDTH 5
      tt-brokstat.gross_prem FORMAT "z,zzz,zz9.99-":U
      tt-brokstat.agcy_comm COLUMN-LABEL "Agency!Comm" FORMAT "zzz,zz9.99-":U
            WIDTH 10.8
      tt-brokstat.lloyds_comm COLUMN-LABEL "Lloyds!Comm" FORMAT "zzz,zz9.99-":U
            WIDTH 10.4
      tt-brokstat.ipt_amt COLUMN-LABEL "IPT Amount" FORMAT "zzz,zz9.99-":U
      tt-brokstat.nett_amount COLUMN-LABEL "Nett!Amount" FORMAT "zz,zzz,zz9.99-":U
            WIDTH 44.8
      tt-brokstat.part_pay FORMAT "zz,zzz,zz9.99-":U
      tt-brokstat.balance FORMAT "zz,zzz,zz9.99-":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 159 BY 10.48.

DEFINE BROWSE br-unalloc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-unalloc C-Win _STRUCTURED
  QUERY br-unalloc NO-LOCK DISPLAY
      tt-brokstat.agency_id COLUMN-LABEL "Agency!No" FORMAT "x(5)":U
      tt-brokstat.trans_num COLUMN-LABEL "Trans!No" FORMAT "999999":U
      tt-brokstat.trans_date FORMAT "99/99/99":U
      tt-brokstat.trans_type COLUMN-LABEL "Type" FORMAT "x(3)":U
            WIDTH 7
      tt-brokstat.agcy_comm COLUMN-LABEL "Agency!Comm" FORMAT "zzz,zz9.99-":U
      tt-brokstat.lloyds_comm COLUMN-LABEL "Lloyds!Comm" FORMAT "zzz,zz9.99-":U
      tt-brokstat.balance COLUMN-LABEL "Sundry" FORMAT "zz,zzz,zz9.99-":U
      tt-brokstat.notes FORMAT "x(50)":U WIDTH 82
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 159 BY 6.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     lv-immediate AT ROW 1.24 COL 74
     lv-as_at AT ROW 3.38 COL 13 COLON-ALIGNED
     lv-agency_id AT ROW 3.38 COL 42 COLON-ALIGNED
     cb-Currency AT ROW 3.38 COL 68 COLON-ALIGNED
     lv-lgbroker_id AT ROW 4.81 COL 15 COLON-ALIGNED
     lv-txt AT ROW 1.24 COL 92 COLON-ALIGNED NO-LABEL
     tt-brokstat.lgbroker_id AT ROW 17.67 COL 143 COLON-ALIGNED
          LABEL "Lloyd's Guaranteeing Broker" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1 NO-TAB-STOP 
     lv-policy_id AT ROW 4.81 COL 42 COLON-ALIGNED
     lv-subagcy AT ROW 4.81 COL 68 COLON-ALIGNED
     bt-display AT ROW 1 COL 2
     lv-dates AT ROW 3.38 COL 88
     lv-startdate AT ROW 3.38 COL 102 COLON-ALIGNED
     lv-enddate AT ROW 4.81 COL 102 COLON-ALIGNED
     lv-funder AT ROW 3.14 COL 124 NO-LABEL
     cb-funder AT ROW 5.05 COL 139 COLON-ALIGNED NO-LABEL
     Br-Statement AT ROW 6.71 COL 1
     br-unalloc AT ROW 19.57 COL 1
     lv-output AT ROW 1.24 COL 63 NO-LABEL
     tt-brokstat.trans_num AT ROW 17.67 COL 19.4 COLON-ALIGNED HELP
          ""
          LABEL "Transaction Number" FORMAT "999999"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1 NO-TAB-STOP 
     tt-brokstat.due_date AT ROW 17.67 COL 51 COLON-ALIGNED
          LABEL "Funding (Due) Date" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1 NO-TAB-STOP 
     tt-brokstat.payment_date AT ROW 17.67 COL 95 COLON-ALIGNED
          LABEL "Payment Received Date" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1 NO-TAB-STOP 
     bt-back AT ROW 1 COL 43
     bt-excel AT ROW 1 COL 18
     bt-forward AT ROW 1 COL 52
     bt-print AT ROW 1 COL 10
     bt-total_debt AT ROW 1 COL 26
     bt-total_sus AT ROW 1 COL 35
     "Transaction Date" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 2.67 COL 87
     "Unallocated Cash" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 18.81 COL 2
          FONT 6
     "Broker Statement" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 6 COL 2
          FONT 6
     RECT-1 AT ROW 2.91 COL 86
     RECT-2 AT ROW 2.91 COL 123
     RECT-3 AT ROW 2.91 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 161 BY 26.52.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-brokstat T "?" NO-UNDO ensignia brokstat
      ADDITIONAL-FIELDS:
          field t-recid as recid init ?
          field order as int format '9999'
          field GrComm as dec
          index order as primary agency_id glsubtyp_id order
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Broker Statements"
         HEIGHT             = 25.76
         WIDTH              = 160
         MAX-HEIGHT         = 46.52
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 46.52
         VIRTUAL-WIDTH      = 256
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   Custom                                                               */
/* BROWSE-TAB Br-Statement cb-funder DEFAULT-FRAME */
/* BROWSE-TAB br-unalloc Br-Statement DEFAULT-FRAME */
/* SETTINGS FOR COMBO-BOX cb-Currency IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cb-funder IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN tt-brokstat.due_date IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE 2 EXP-LABEL EXP-FORMAT                          */
/* SETTINGS FOR FILL-IN tt-brokstat.lgbroker_id IN FRAME DEFAULT-FRAME
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN lv-agency_id IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-as_at IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lv-dates IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-enddate IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX lv-immediate IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-lgbroker_id IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR RADIO-SET lv-output IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-startdate IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lv-subagcy IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-subagcy:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN lv-txt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-brokstat.payment_date IN FRAME DEFAULT-FRAME
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN tt-brokstat.trans_num IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE 2 EXP-LABEL EXP-FORMAT EXP-HELP                 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br-Statement
/* Query rebuild information for BROWSE Br-Statement
     _TblList          = "Temp-Tables.tt-brokstat"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "tt-brokstat.glsubtyp_id = ""610"" AND
     (IF lv-startdate NE ? THEN tt-brokstat.TRANS_date >= lv-startdate ELSE TRUE) AND
     (IF lv-enddate NE ? THEN tt-brokstat.TRANS_date <= lv-enddate ELSE TRUE) AND
     IF lv-funder = """" THEN tt-brokstat.fund_prov_id = tt-brokstat.fund_prov_id 
          ELSE IF lv-funder = ""NFD"" THEN tt-brokstat.fund_prov_id = """" 
          ELSE IF lv-funder = ""000"" THEN tt-brokstat.fund_prov_id > """" 
          ELSE tt-brokstat.fund_prov_id = lv-funder 
"
     _FldNameList[1]   > Temp-Tables.tt-brokstat.agency_id
"tt-brokstat.agency_id" "Agency!No" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tt-brokstat.policy_id
"tt-brokstat.policy_id" "Policy!No" ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.tt-brokstat.trans_date
"tt-brokstat.trans_date" ? "99/99/99" "date" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.tt-brokstat.policyholder
"tt-brokstat.policyholder" ? "x(20)" "character" ? ? ? ? ? ? no ? no no "23.2" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.tt-brokstat.trans_type
"tt-brokstat.trans_type" "Type" "x(4)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _FldNameList[6]   = Temp-Tables.tt-brokstat.gross_prem
     _FldNameList[7]   > Temp-Tables.tt-brokstat.agcy_comm
"tt-brokstat.agcy_comm" "Agency!Comm" ? "decimal" ? ? ? ? ? ? no ? no no "10.8" yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.tt-brokstat.lloyds_comm
"tt-brokstat.lloyds_comm" "Lloyds!Comm" ? "decimal" ? ? ? ? ? ? no ? no no "10.4" yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.tt-brokstat.ipt_amt
"tt-brokstat.ipt_amt" "IPT Amount" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.tt-brokstat.nett_amount
"tt-brokstat.nett_amount" "Nett!Amount" ? "decimal" ? ? ? ? ? ? no ? no no "44.8" yes no no "U" "" ""
     _FldNameList[11]   = Temp-Tables.tt-brokstat.part_pay
     _FldNameList[12]   = Temp-Tables.tt-brokstat.balance
     _Query            is NOT OPENED
*/  /* BROWSE Br-Statement */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-unalloc
/* Query rebuild information for BROWSE br-unalloc
     _TblList          = "Temp-Tables.tt-brokstat"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.tt-brokstat.agency_id
"tt-brokstat.agency_id" "Agency!No" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tt-brokstat.trans_num
"tt-brokstat.trans_num" "Trans!No" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.tt-brokstat.trans_date
"tt-brokstat.trans_date" ? "99/99/99" "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.tt-brokstat.trans_type
"tt-brokstat.trans_type" "Type" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.tt-brokstat.agcy_comm
"tt-brokstat.agcy_comm" "Agency!Comm" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.tt-brokstat.lloyds_comm
"tt-brokstat.lloyds_comm" "Lloyds!Comm" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.tt-brokstat.balance
"tt-brokstat.balance" "Sundry" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.tt-brokstat.notes
"tt-brokstat.notes" ? ? "character" ? ? ? ? ? ? no ? no no "82" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE br-unalloc */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Broker Statements */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Broker Statements */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br-Statement
&Scoped-define SELF-NAME Br-Statement
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br-Statement C-Win
ON ROW-DISPLAY OF Br-Statement IN FRAME DEFAULT-FRAME
DO:
  def var x as int no-undo init 1.

  DO while valid-handle(lh-col[x]):
    assign lh-col[x]:BGCOLOR = IF tt-brokstat.fund_prov_id = "" THEN ? ELSE 14
           x = x + 1.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br-Statement C-Win
ON VALUE-CHANGED OF Br-Statement IN FRAME DEFAULT-FRAME
DO:
   IF AVAILABLE tt-brokstat 
    THEN disp {&list-2} with frame {&frame-name}.
   else ASSIGN tt-brokstat.trans_num:SCREEN-VALUE = "0"
               tt-brokstat.due_date:SCREEN-VALUE = ?
               tt-brokstat.payment_date:SCREEN-VALUE = ?
               tt-brokstat.lgbroker_id:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-back C-Win
ON CHOOSE OF bt-back IN FRAME DEFAULT-FRAME /* bt-back */
DO:
  /*Get data from prevous as_at date*/
  lv-as_at = lv-as_at - 1.  
  disp lv-as_at with frame {&frame-name}.

  APPLY "CHOOSE" TO bt-display.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-display
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-display C-Win
ON CHOOSE OF bt-display IN FRAME DEFAULT-FRAME /* bt-display */
DO:
    
    DEF VAR lv-message     AS CHAR FORMAT "x(35)" NO-UNDO.

    /*Validate dates entered in trans date filter chosen*/
    etime(true).
    assign frame {&frame-name}
        {&list-1}.

    lv-message = fnValidDate(lv-startdate,lv-enddate).

    IF lv-dates AND lv-message NE "" THEN
    DO:
        MESSAGE lv-message VIEW-AS ALERT-BOX ERROR TITLE "Transaction Dates".
        RETURN NO-APPLY.
    END.

    SESSION:SET-WAIT-STATE("general":U).

    EMPTY TEMP-TABLE tt-brokstat.       /*Clear down each time*/

    /*appserver call*/

       {{&zen}run.i &program   = "brokstat.p"
                    &path      = "{&sys}{&srv}"
                    &Appsrv    = "System"
                    &noper     = true
                    &procedure = "GetData"
                    &params    = "(getcombokey(cb-currency:handle),
                                   lv-agency_id,
                                   lv-policy_id,
                                   lv-lgbroker_id,
                                   lv-as_at,
                                   OUTPUT TABLE tt-brokstat)"}
    /*Refresh debtors browse and filter by funder and trans date from screen*/
    
     {&OPEN-QUERY-br-statement}
    
    /*Refresh unallocated browse and filter trans date from screen*/
    /*Not populated when funded only chosen*/

    IF can-do(",nfd",getcombokey(cb-funder:handle)) THEN
        {&OPEN-QUERY-br-unalloc}   
    
    APPLY ("VALUE-CHANGED":U) TO BR-statement.
    ScrMsg('Complete in ' + string((etime / 1000),'zz9.99') + ' Seconds' ).
    SESSION:SET-WAIT-STATE("").

    /*Advise user if not records returned*/

    IF NUM-RESULTS("BR-statement") = 0 THEN
    MESSAGE "No Outstanding Entries for parameters chosen to display" 
    VIEW-AS ALERT-BOX INFORMATION TITLE "Broker Statement".

    IF NUM-RESULTS("br-unalloc") = 0 THEN
    MESSAGE "No Unallocated Entries for parameters chosen to display" 
    VIEW-AS ALERT-BOX INFORMATION TITLE "Broker Statement".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-forward
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-forward C-Win
ON CHOOSE OF bt-forward IN FRAME DEFAULT-FRAME /* bt-forward */
DO:
   assign frame {&frame-name} {&list-1}.

  IF lv-as_at = TODAY THEN DO:
      MESSAGE "Not able to display future records." VIEW-AS ALERT-BOX ERROR TITLE "Error".
      RETURN NO-APPLY.
  END.
  ELSE DO:
      lv-as_at = lv-as_at + 1.
      disp lv-as_at with frame {&frame-name}.
      APPLY "CHOOSE" TO bt-display.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-print C-Win
ON CHOOSE OF bt-print IN FRAME DEFAULT-FRAME /* bt-print */
DO:
  def var lv-curr_date as  date no-undo.
  def var lv-due_date as  date no-undo.
  etime(true).
  SESSION:SET-WAIT-STATE("general":U).

  assign frame {&frame-name} 
    {&List-1}  
    lv-curr_date = date(month(lv-as_at),1,YEAR(lv-as_at))      /*Set current date as 1st of current month*/
    lv-due_date = submnths(lv-curr_date,1) . /*Due date is 1 month less than current date*/

  empty temp-table t-processed.

 {{&zen}run.i 
            &program   = "brokstat.p"
            &path      = "{&sys}{&srv}"
            &Appsrv    = "System"
            &noper     = true
            &procedure = "OutputToDisk"
            &params    = "(lv-immediate,
                           lv-curr_date,
                           lv-due_date,
                           lv-as_at,
                           table tt-brokstat, 
                           output table t-processed)"}

if lv-immediate 
then for each t-processed: 
          run CallWord (t-processed.agency_id,lv-output).
          if error-stATus:error 
          then Do:
                SESSION:SET-WAIT-STATE("").
                message 'An Error Ocurred During Job, You Need To Resubmit'
                view-as alert-box error.
                return no-apply.
          End.
     End.

  ScrMsg('Print Complete In ' + string((etime / 1000),'zz9.99') + ' Seconds' ).
  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-total_debt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-total_debt C-Win
ON CHOOSE OF bt-total_debt IN FRAME DEFAULT-FRAME /* bt-total_debt */
DO:
  
run tots ('Debt').


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-total_sus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-total_sus C-Win
ON CHOOSE OF bt-total_sus IN FRAME DEFAULT-FRAME /* bt_total_sus */
DO:

run tots ('Sus').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-agency_id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-agency_id C-Win
ON RETURN OF lv-agency_id IN FRAME DEFAULT-FRAME /* Agency ID */
DO:
   apply "CHOOSE":U to bt-display.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-dates
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-dates C-Win
ON VALUE-CHANGED OF lv-dates IN FRAME DEFAULT-FRAME
DO:
  
    CASE SELF:SCREEN-VALUE:
        WHEN "yes" THEN ASSIGN         /*Enable from & to Date fields*/
                        lv-startdate:SENSITIVE = TRUE
                        lv-enddate:SENSITIVE = TRUE.
        OTHERWISE ASSIGN                /*Disable from & to dates fields and set to null*/
                        lv-startdate:SENSITIVE = FALSE
                        lv-startdate:SCREEN-VALUE = ?
                        lv-enddate:SENSITIVE = FALSE
                        lv-enddate:SCREEN-VALUE = ?.
    END CASE.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-funder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-funder C-Win
ON VALUE-CHANGED OF lv-funder IN FRAME DEFAULT-FRAME
DO:
   
    assign frame {&frame-name} lv-funder.
    
    IF lv-funder = "000" 
    THEN ASSIGN cb-funder:sensitive = TRUE
                cb-funder:SCREEN-VALUE  = "000".
    ELSE cb-funder:sensitive = false.
            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 
  RUN enable_UI.
  
  {{&zen}Focus.i}

  run initialise. 


  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CallWord C-Win 
PROCEDURE CallWord :
def input param pv-agency_id   as char no-undo.
def input param pv-destination as char no-undo.

DEF VAR lv-data      AS CHAR NO-UNDO.
DEF VAR lv-doc       AS CHAR NO-UNDO.
DEF VAR lv-tableList AS CHAR INITIAL "001,002,003,004,005,610,630" NO-UNDO.
def var lv-table     as char       no-undo.
def var lv-sel       as char       no-undo.
DEF VAR ch-wordAppl  AS COM-HANDLE NO-UNDO.
DEF VAR ch-table     AS COM-HANDLE NO-UNDO.
def var lv-template  as char no-undo.
def var lv-target    as char no-undo.
DEF VAR i            AS INT NO-UNDO.
def var lv-extractdir as char no-undo.
def var lv-backdir   as char no-undo.

ASSIGN 
    lv-extractdir = GetIniValue('Ensignia','ExtractDir')
    lv-backdir  = GetFullPath(lv-extractdir + GetIniValue('OfficeServer','OfsBack')) + '/'
    lv-doc      = GetFullPath(GetIniValue('Ensignia','ReportsDir')) + '\' + t-processed.Fnames + pv-agency_id + ".doc"
    lv-template = GetFullPath(GetIniValue('Ensignia','TemplateDir') + 'BkStatTemplate.doc')
    lv-data     = GetFullPath(lv-extractdir + t-processed.Fnames + pv-agency_id + ".txt").

ScrMsg('Loading Word').
ch-wordAppl = MSOpenApplication("Word.Application",'hidden') no-error.
if error-status:error then do:
    MSCloseApplication(ch-wordAppl).
    return error.
End.
ScrMsg('Loading Template').
if not MSOpenDocument(ch-wordAppl,lv-template) then do:
    MSCloseApplication(ch-wordAppl).
    return error.
End.
ScrMsg('Merging Header :' + lv-template + ': ' + lv-data).

lv-target = MSMailMerge(ch-wordappl,lv-data,{&WdSendToNewDocument}).
if error-status:error then do:
    MSCloseApplication(ch-wordAppl).
    return error.
End.

DO i = 1 TO num-entries(lv-tablelist):
    assign         
        lv-sel   =  ENTRY(i,lv-tableList)
        lv-table = lv-extractdir + t-processed.Fnames + pv-agency_id + lv-sel + ".txt"
        lv-sel = "&" + lv-sel + "&"
        lv-table = GetFullPath(lv-table).

    if not MSSetSelection(ch-wordAppl,lv-sel) then do:
        MSCloseApplication(ch-wordAppl).
        return error.
    End.

    if lv-table = ? then do: /* remove page if not required */
        MSDeleteSelection(ch-wordAppl,1).
        next.
    End.

    ScrMsg('Inserting Data for ' + lv-sel).
    ch-table = MSInsertTable(ch-wordappl,lv-table,'arial',8,'') no-error. 
    if error-status:error then do:
        MSCloseApplication(ch-wordAppl).
        return error.
    End.
/*     MsSetMargins(ch-table,?,?,0.5,0.51). */
        /* finance = 001,002,003,004,005 fund_prov_id glsubtyp_id = 610
           Debtors = 610                 glsubtyp_id  fund_prov_id = '' 
           unallac cash = 630            glsubtyp_id  */
    ScrMsg('Formatting Data').
    case ENTRY(i,lv-tableList):
        when '610' then do: /* debtors */
            if MsNumColumns(ch-table) = 15 then do:
                MsHighlightColumns(ch-table,'8:10:12:14',{&wdTexture20Percent}). 
                MSAlignColumns(ch-wordappl,ch-table,'4:8:9:10:11:12:13:14:15',{&wdalignparagraphright}).
                MSSizeColumns(ch-wordappl,ch-table,'2',18). 
                MSSizeColumns(ch-wordappl,ch-table,'8:13:14:15',13). 
                MSSizeColumns(ch-wordappl,ch-table,'9:10:11:12',11). 
            End.
            else do:        
                MsHighlightColumns(ch-table,'8:9:11:13',{&wdTexture20Percent}). 
                MSAlignColumns(ch-wordappl,ch-table,'4:8:9:10:11:12:13:14:',{&wdalignparagraphright}).
                MSSizeColumns(ch-wordappl,ch-table,'2',18). 
                MSSizeColumns(ch-wordappl,ch-table,'8:12:13:14',13). 
                MSSizeColumns(ch-wordappl,ch-table,'9:10:11:',11). 
            End.
        End.
        when '630' then do: /* unalloc cash */
            MsHighlightColumns(ch-table,'6:8',{&wdTexture20Percent}). 
            MSAlignColumns(ch-wordappl,ch-table,'6:7:8',{&wdalignparagraphright}).
            MSSizeColumns(ch-wordappl,ch-table,'8',13). 
            MSSizeColumns(ch-wordappl,ch-table,'6:7',11). 
        End.
        otherwise do: /* financed */
            MsHighlightColumns(ch-table,'6:8:10:12:14',{&wdTexture20Percent}).
            MSAlignColumns(ch-wordappl,ch-table,'4:7:8:9:10:11:12:13',{&wdalignparagraphright}).
            MSSizeColumns(ch-wordappl,ch-table,'7:11:12:13',13). 
            MSSizeColumns(ch-wordappl,ch-table,'8:9:10',11). 
        End.
    end case.
    backup(lv-table,lv-backdir + GetFileName(lv-table)).
    ScrMsg('Finished ' + lv-sel).
END. /*i = 1 to 7*/
MSUpdateFields(ch-wordAppl).

scrmsg('Saving as ' + lv-doc).
if not MSSaveAs(ch-wordAppl,lv-target,lv-doc) then do:
    MSCloseApplication(ch-wordAppl).
    return error.
End.

if not MSSetSaved(ch-wordAppl,lv-template,true) then do:
    MSCloseApplication(ch-wordAppl).
    return error.
End.

if pv-destination = 'p' then
    MSPrint(ch-wordAppl,'').

MSCloseApplication(ch-wordAppl).

backup(lv-data,lv-backdir + GetFileName(lv-data)).

ScrMsg('').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY lv-immediate lv-as_at lv-agency_id cb-Currency lv-lgbroker_id lv-txt 
          lv-policy_id lv-dates lv-startdate lv-enddate lv-funder cb-funder 
          lv-output 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE tt-brokstat THEN 
    DISPLAY tt-brokstat.lgbroker_id tt-brokstat.payment_date 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE lv-immediate lv-as_at lv-agency_id cb-Currency lv-lgbroker_id 
         lv-policy_id bt-display lv-dates lv-funder Br-Statement br-unalloc 
         lv-output bt-back bt-excel bt-forward bt-print bt-total_debt 
         bt-total_sus RECT-1 RECT-2 RECT-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise C-Win 
PROCEDURE initialise :
RUN pButtons.         /*Set graphics*/
RUN pCurrency.        /*Get currencies for combo-box*/
RUN pFunder.          /*Get funders for combo-box*/
def var x as int no-undo init 1.

assign
    lv-as_at:SCREEN-VALUE in frame {&frame-name} = string(TODAY)
    lh-col[x] = br-statement:FIRST-COLUMN in frame {&frame-name}.

 do while valid-handle(lh-col[x]:next-column):
    lh-col[x + 1] = lh-col[x]:next-column.
    x = x + 1.
 End.
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pButtons C-Win 
PROCEDURE pButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

    bt-display:LOAD-IMAGE("edm/images/calcmoney.ico").
    bt-display:LOAD-IMAGE-INSENSITIVE("edm/images/calcmoney_disabled.ico").
    bt-print:LOAD-IMAGE("edm/images/print.ico").
    bt-print:LOAD-IMAGE-INSENSITIVE("edm/images/print-disabled.ico").
    bt-excel:LOAD-IMAGE("edm/images/excel.ico").
    bt-excel:LOAD-IMAGE-INSENSITIVE("edm/images/excel-disabled.ico").
    bt-total_debt:LOAD-IMAGE("edm/images/totaldebt.ico").
    bt-total_sus:LOAD-IMAGE("edm/images/totalcred.ico").
    bt-back:LOAD-IMAGE("edm/images/back.ico").
    bt-forward:LOAD-IMAGE("edm/images/forward.ico").

END. /*do with frame*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCurrency C-Win 
PROCEDURE pCurrency :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

buildcombo(cb-currency:handle in frame {&frame-name},
           'compcurr,currency',
           'company_id',
           'currency_id',
           'where compcurr.company_id = "01",where currency.currency_id = compcurr.currency_id',
           ''  ,
           no ,
           no ).
cb-currency:screen-value  = 'GBP'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFunder C-Win 
PROCEDURE pFunder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

buildcombo(cb-funder:handle in frame {&frame-name},
           'fpdet',
           'fund_prov_id',
           'short_name',
           '',
           ''  ,
           no ,
           true ).

    entry(1,cb-funder:private-data) = '000'.
    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tots C-Win 
PROCEDURE Tots :
def input param pv-type as char no-undo.

DEF var lv-gross       AS DEC NO-UNDO.
DEF var lv-agcy_comm   AS DEC NO-UNDO.
DEF var lv-lloyds_comm AS DEC NO-UNDO.
DEF var lv-ipt         AS DEC NO-UNDO.
DEF var lv-nett        AS DEC NO-UNDO.
DEF var lv-part_pay    AS DEC NO-UNDO.
DEF var lv-balance     AS DEC NO-UNDO.

case pv-type :
    when 'Debt' then do: /*Total up all entries in the Debtors Browse*/
        FOR EACH tt-brokstat WHERE tt-brokstat.glsubtyp_id = "610":
            ASSIGN lv-gross       = lv-gross + tt-brokstat.gross_prem
                   lv-agcy_comm   = lv-agcy_comm + tt-brokstat.agcy_comm
                   lv-lloyds_comm = lv-lloyds_comm + tt-brokstat.lloyds_comm
                   lv-ipt         = lv-ipt + tt-brokstat.ipt_amt
                   lv-nett        = lv-nett + tt-brokstat.nett_amount
                   lv-part_pay    = lv-part_pay + tt-brokstat.part_pay
                   lv-balance     = lv-balance + tt-brokstat.balance.
         END.
    End.
    When 'sus' then do:
        FOR EACH tt-brokstat WHERE tt-brokstat.glsubtyp_id = "630":
            ASSIGN lv-agcy_comm   = lv-agcy_comm + tt-brokstat.agcy_comm
                   lv-lloyds_comm = lv-lloyds_comm + tt-brokstat.lloyds_comm
                   lv-ipt         = lv-ipt + tt-brokstat.balance.         
                    /*Move into ipt colum for dispay only*/
        END.
    End.
end case.

  /*Display totals*/
                           
RUN "broker statements/d-total.w" 
        (input pv-type,
               lv-gross,
               lv-agcy_comm,
               lv-lloyds_comm,
               lv-ipt,
               lv-nett,
               lv-part_pay,
               lv-balance).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CloseExcelWord C-Win 
FUNCTION CloseExcelWord RETURNS LOGICAL
  ( pv-excelappl as com-handle,
    pv-wordappl as  com-handle ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
MSCloseApplication(pv-excelAppl).
MSCloseApplication(pv-wordAppl).

  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnValidDate C-Win 
FUNCTION fnValidDate RETURNS CHARACTER
  ( ip-from AS DATE,
    ip-to AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN (IF ip-to < ip-from THEN "'To Date' must be after 'From Date'."  
           ELSE
           IF (ip-to > TODAY OR ip-from > TODAY) THEN "'From Date' and 'To Date' must not be after today."
           ELSE 
           IF (ip-to < 01/01/1990 OR ip-from < 01/01/1990) THEN "'From Date' and 'To Date' must be after 01/01/1990."
           ELSE
           IF (ip-to = ? OR ip-from = ?) THEN "'From Date' and 'To Date' must be input."
           ELSE "").
           
  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION scrmsg C-Win 
FUNCTION scrmsg RETURNS LOGICAL
  ( pv-msg as char ) :
/*   message pv-msg view-as alert-box.  */
  lv-txt:screen-value in frame {&frame-name} = pv-msg.
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

