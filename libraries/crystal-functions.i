/* crystal reports Control */
FUNCTION SetSchema    RETURNS log      (dirname as  char,tablelist as char) in {&lhandle}.
FUNCTION Call-Crystal RETURNS LOGICAL  (pv-repname as char,pv-title   as char,pv-mode    as char)  in {&lhandle}.
