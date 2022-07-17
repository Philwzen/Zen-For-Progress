/******************************************************************************/
/*  PROGRAM ID.     : find.i                                                  */
/*  PROGRAM TITLE   :                                                         */
/* replace standard finds with open query to enable use of multiple indexs etc*/
/*  CREATE DATE     : ??/??/??                                                */
/*                                                                            */
/*  COMPANY NAME    : East India Trading LTD (Copyright 94-99)                  */
/*  VERSION NO.     : 1.0                                                     */
/******************************************************************************/
/******************************************************************************/
/*                          PATCH HISTORY                                     */
/*                                                                            */
/*           PATCH  USR     Job                                               */
/* DATE      NO.    ID      REF DESCRIPTION                                   */
/* -------------------------------------------------------------------------- */
/* ??/??/??  P00    Philw   00  initial release                               */
/******************************************************************************/
/*********
{{&core}find.i &db    = ""
              &table = ""
              &where = ""
              &type  = "first"
              &index = ""
              &lock  = "no"}
**********/
do:
    open query qf-{&table}
        for each {&db}{&table} {&where}
             		   {&lock}-lock {&index}.
    get {&type} qf-{&table}.     		
end.
