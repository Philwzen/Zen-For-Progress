/* main block code for libraries */
procedure initialise:
    RUN local-initialise IN THIS-PROCEDURE NO-ERROR.
    IF NOT ERROR-STATUS:ERROR AND RETURN-VALUE = 'override' THEN RETURN.
/*         assign z-system  = GetSysVar('system')     */
/*                z-user    = GetSysVar('user')       */
/*                z-country = GetSysVar('country')    */
/*                z-lancod  = GetSysVar('language').  */
end procedure.
