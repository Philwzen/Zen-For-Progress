/*
               RUN zen/proprint.p (INPUT  pv-window,      window handle
                                   INPUT  pv-PrintFile,   file to print
                                   INPUT  pv-FontNumber,  font to use
                                   INPUT  pv-UseDialog,   display dialog (1=yes,0=no)
                                   INPUT  pv-PageSize,    page-size 
                                   INPUT  pv-PageCount,   pages to print 0=all
                                   OUTPUT pv-Printed ).   printed ok
*/
{zen/libload.i}

DEF INPUT  PARAM pv-window      AS handle        NO-UNDO.   
DEF INPUT  PARAM pv-PrintFile   AS CHAR          NO-UNDO.
DEF INPUT  PARAM pv-FontNumber  AS INT           NO-UNDO.
DEF INPUT  PARAM pv-UseDialog   AS INT           NO-UNDO.
DEF INPUT  PARAM pv-PageSize    AS INT           NO-UNDO.
DEF INPUT  PARAM pv-PageCount   AS INT           NO-UNDO.
DEF OUTPUT PARAM pv-Printed     AS LOG INIT TRUE NO-UNDO .

DEF VAR PrintResult AS INT NO-UNDO .


aSSIGN pv-window = IF VALID-HANDLE(pv-window) 
				THEN pv-window
				ELSE IF VALID-HANDLE( CURRENT-WINDOW ) 
					THEN CURRENT-WINDOW
	                        ELSE DEFAULT-WINDOW
	FILE-INFO:FILE-NAME = pv-PrintFile NO-ERROR.

IF (FILE-INFO:FULL-PATHNAME = ? ) THEN DO:
	MESSAGE "Unable to find file to print."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK IN WINDOW pv-window.
	return 'nofile'.
END.
assign pv-PrintFile = FILE-INFO:FULL-PATHNAME
       SESSION:PRINTER-CONTROL-HANDLE = ?.

IF OPSYS = "WIN32" THEN
	RUN ProPrintfile in h-library 
			 (INPUT  SESSION:PRINTER-CONTROL-HANDLE,
	                  INPUT  pv-UseDialog ,
	                  INPUT  pv-window:HWND,
	                  INPUT  pv-FontNumber,
	                  INPUT  pv-PrintFile,
	                  INPUT  pv-PageCount,
	                  OUTPUT PrintResult ). 
else
	RUN ProPrintfile16 in h-library 
			 (INPUT  SESSION:PRINTER-CONTROL-HANDLE,
	                  INPUT  pv-UseDialog ,
	                  INPUT  pv-window:HWND,
	                  INPUT  pv-FontNumber,
	                  INPUT  pv-PrintFile,
	                  INPUT  pv-PageCount,
	                  OUTPUT PrintResult ). 
/* Return value from DLL of 0 indicates a successful request to
   print the file. It does NOT indicate the file has finished
   printing. */
pv-Printed = (PrintResult = 0). 
