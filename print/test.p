define stream op.


output stream op to printer binary paged page-size 100. /* binary no-convert */
/*
PUT stream op CONTROL NULL. /* force windows passthrough printing */
*/
put stream op unformatted "test" skip.
output stream op close.
