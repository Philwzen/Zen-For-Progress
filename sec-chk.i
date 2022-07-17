/******************************************************************************/
/*  PROGRAM ID.     : sec-chk.i                                               */
/*  PROGRAM TITLE   : security check                                          */
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

/* this will do any security checking for programs */
&if defined(IgnoreSecurity) = 0 &then       
if not CanRun(this-procedure:file-name) then do:
	apply 'close' to this-procedure.
	leave MAIN-BLOCK.
end.
&endif
&if defined(UseTranslations) ne 0 &then
{{&core}internat-screen.i}
&endif
/*
/* have to do this after frames have been parented to windows */
run window-size in h-library (frame {&frame-name}:handle,{&window-name}:handle).
*/
