/* reportdefs.i */

{{&core}load-crystal.i}
{{&core}{&rep}reptemp.i}    
&glob NoChangedCheck true
DEF VAR lv-repname   AS CHAR NO-UNDO. 
def var lv-cryfile   as char no-undo. 
def var lv-datfile   as char no-undo.
def var lv-paramfile as char no-undo.
/* def var lv-print-type as int no-undo. */ /* not used yet immediate/batch/spool */
def var lv-extrprog  as char no-undo.
def var lv-extrproc  as char no-undo.
def var lv-extrpath  as char no-undo.
def var lv-extractdir as char no-undo. 
