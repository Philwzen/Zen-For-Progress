&glob unxdelim |
&glob windelim !
def var lv-delim    as char no-undo.
&if "{&OPSYS}" = 'unix' 
&then lv-delim = '{&unxdelim}'.
&else lv-delim = '{&windelim}'.
&endif
lv-delim = trim(lv-delim).
