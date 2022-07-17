/* 
*   trdelete.i
*
*   Standard database write trigger
*
*   Parameters:
*       &Table      Database table name
*/

/* Write audit record if required */

{{&aud}audit.i  &TableName    = {&Table}
                &OldBuffer    = {&Table}
                &TableNameKey = {&Key}
                &Update       = 'Delete'}
