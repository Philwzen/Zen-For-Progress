/* 
*   trwrite.i
*
*   Standard database write trigger
*
*   Parameters:
*       &Table      Database table name
*/
/* Write audit record if required */
{{&aud}audit.i  &TableName    = {&Table}
                &OldBuffer    = "oldbuffer" 
                &tablenamekey = {&key}
                &Update       = 'Update'}

{{&aud}tstamp.i &TableName    = {&Table}}

