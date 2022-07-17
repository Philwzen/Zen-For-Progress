/* reptemp.i */

DEFINE TEMP-TABLE t-data
    FIELD data AS CHAR
    FIELD TYPE AS CHAR
    field tablename as char
    index order tablename ascending
                type descending.
