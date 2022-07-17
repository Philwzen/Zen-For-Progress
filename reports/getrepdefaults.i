def var lv-params as char no-undo.
def var x         as int  no-undo. 
def var lv-ok     as log  no-undo.
def var lv-value  as char no-undo.

lv-value = PgmRepInfo(unixpath({&program-name})).
        
&if defined(dpgmparams) ne 0 &THEN
   if {&dpgmparams} ne '' 
   then lv-value = {&dpgmparams}.
&endif

assign
    lv-repname   = entry(1,lv-value,'{&Delim2}')
    lv-extrpath  = entry(2,lv-value,'{&Delim2}')
    lv-extrprog  = entry(3,lv-value,'{&Delim2}')
    lv-extrproc  = entry(4,lv-value,'{&Delim2}')
    lv-extractdir = entry(5,lv-value,'{&Delim2}')
    lv-datfile   = entry(6,lv-value,'{&Delim2}')
    lv-cryfile   = entry(7,lv-value,'{&Delim2}') + entry(8,lv-value,'{&Delim2}')
    lv-params    = entry(9,lv-value,'{&Delim2}').

if lv-extractdir = '' 
then lv-extractdir = GetCtrl("{&Data-Extract-Dir}").                                 
/*     lv-cryfile   = "{&crystal}" + string(int(getsysvar('{&clv}language')),'99') + '\'  */
/*                         + lv-cryfile                                                  */
    lv-paramfile = lv-extractdir + "params.dat".

if search(lv-extrpath + lv-extrprog) = ? or 
   search(lv-extrpath + substring(lv-extrprog,1,length(lv-extrprog) - 1) + 'p') = ?
then do:
    message Msg(50,"Extract Program",lv-extrpath + lv-extrprog,"","") view-as alert-box.
    return error.
end.

if lv-datfile = '' 
then do:
    message Msg(21,"Data File",lv-datfile,"","") view-as alert-box.
    return error.
end.

if search(lv-cryfile)= ? 
then do:    
    message 'Report file not found (' + lv-cryfile + ")." skip
            'Extract the data only?'
    view-as alert-box buttons yes-no update lv-ok.
    if not lv-ok then
	 return error.
end.
 

