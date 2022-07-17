
/*------------------------------------------------------------------------
    File        : prtparams.i
    Purpose     : Include print parameters in a run statement.
    Author(s)   : escheleen
    Created     : Wed Mar 18 11:00:05 PDT 2009
    Notes       : A couple different places where we include the prtdefs.i
                   program, we must call a separate procedure to do the actual
                   printin for a program.  We must then pass the following
                   parameters in order for all variables to be set correctly.
                   This include file can then be used to update all of those
                   places without needing to find them all.
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */
/* surrounding logic for this include will look like:
run yarr in this-procedure( */
input  pv-asynch,
input  pv-printerparams,
output pv-mreport,
output pv-mschema,
input  pv-dpgmparams,
input  pv-params,
output table-handle pv-ReturnTable /* ).
The parentheses and period may look like they should be in the inlcude file,
but for clarity, I have left them out, so they will be in the code that
someone is much more likely to be reading */