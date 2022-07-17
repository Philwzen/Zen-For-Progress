/******************************************************************************/
/*  PROGRAM ID.     : wid-chk.i                                               */
/*  PROGRAM TITLE   : widget security {&frame-name} only                      */
/*                                                                            */
/*  CREATE DATE     : 12/01/96                                                */
/*                                                                            */
/*  COMPANY NAME    : East India Trading ltd (COPYRIGHT 94,95,96)               */
/*  VERSION NO.     : 1.0                                                     */
/******************************************************************************/
/******************************************************************************/
/*                          PATCH HISTORY                                     */
/*                                                                            */
/*           PATCH  USR     Job                                               */
/* DATE      NO.    ID      REF DESCRIPTION                                   */
/* -------------------------------------------------------------------------- */
/* 12/01/96  p00    philw   00  initial release                               */
/******************************************************************************/
/* wid-chk.i disable certain widgets if security has been set up */
/*****************************************************************************/
if GetFieldWhere('zen-dwidget','pgm = "' + this-procedure:file-name + '"','PGM')
                    = this-procedure:file-name
then WidSecCheck(frame {&frame-name}:handle,this-procedure:file-name).
/* set focus to first enabled widget */
&if defined(tabs) ne 0 
&then setframefocus(frame one:handle).
&else setframefocus(frame {&frame-name}:handle).
&endif


