/******************************************************************************/
/*  PROGRAM ID.     : shell.p                                                 */
/*  PROGRAM TITLE   : run opsys shell                                         */
/*                                                                            */
/*  CREATE DATE     : 12/01/96                                                */
/*                                                                            */
/*  COMPANY NAME    : East India Trading ltd (COPYRIGHT 94,95)                  */
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
/* shell.p */
/* East India Trading ltd (COPYRIGHT 94,95)                  */

case opsys:
	when 'msdos' or
      when 'win32' then dos.
      when 'unix'  then unix.
      when 'vms'   then vms.
      otherwise message 'Unknown Opsys: ' opsys view-as 
	alert-box error.
end case.
