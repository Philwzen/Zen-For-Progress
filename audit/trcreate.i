/* 
*   trcreate.i
*
*   Standard database create trigger
*
*   Parameters:
*       &Table      Database table name
*       &Sequence   Database sequencename
*/
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = {&Table}
                          &Unique-key     = {&Uniquekey}}
&endif
/*
no need for this as write trigger does it
{{&aud}audit.i  &TableName    = {&Table}
                &OldBuffer    = "oldbuffer" 
                &tablenamekey = {&Uniquekey}
                &Update       = 'create'}

{{&aud}tstamp.i &TableName    = {&Table}}
*/
