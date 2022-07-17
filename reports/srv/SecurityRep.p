&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : {&core}reports/srv/SecurityRep.p
    Purpose     : Function as server side code for the security reports.
    Author(s)   : Phil White
    Created     : 01/01/09
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{app-paths.i}
define temp-table t-menuSecurity no-undo
   field menuFullName as char
   field accessInfo   as char
   field extraDetails as char.

def var lv-user          as char  no-undo.
def var lv-userGroup     as char  no-undo.
/* can be "all" records, only records a user/group "can" access, or only
   records a user/group can "not" access */
def var lv-recordsToShow as char  no-undo.
def var lv-menuFullName  as char  no-undo.
def var lv-menuPosition  as int    no-undo.
def var lv-accessInfo    as char  no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-canAccess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD canAccess Procedure 
FUNCTION canAccess RETURNS LOGICAL
  ( pv-user          as char,
    pv-group         as char,
    pv-menuNotUsers  as char,
    pv-menuNotGroups as char,
    pv-menuOkUsers   as char,
    pv-menuOkGroups  as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-matchesRecordsToShow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD matchesRecordsToShow Procedure 
FUNCTION matchesRecordsToShow RETURNS LOGICAL
   (pv-menuAccessible as log) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BuildMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildMenu Procedure 
PROCEDURE BuildMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param v-id           like zen-dmenu.menu-id no-undo.

def var h-menu-item as handle        no-undo.
def var lv-menuAccessible as log init true no-undo. 
def buffer amenu for zen-dmenu.
def buffer bmenu for zen-dmenu.
def buffer cmenu for zen-dmenu. 
def var lv-can-prog as char no-undo.
find amenu where amenu.menu-id = v-id no-lock no-error.
lv-menuPosition = lv-menuPosition + 1.
/* each menu-item of parent menu */
for each bmenu where bmenu.menu-parent = amenu.menu-action 
               /*  And can-do(bmenu.menu-grp,lv-userGroup) */
                no-lock
                  by menu-parent
                  by display-order:
   if bmenu.menu-action = "" or menu-name = "<rule>" then next.
   /* menu user security */
   lv-menuAccessible = SecurityCheck(lv-user,
                                 lv-userGroup,  
                                 bmenu.not-users,
                                 bmenu.not-group,
                                 bmenu.run-users,
                                 bmenu.run-groups).


    entry(lv-menuPosition,lv-menuFullName) = bmenu.menu-name.
    if matchesRecordsToShow(lv-menuAccessible)             
    then do:
        create t-menuSecurity.
        assign menuFullName = lv-menuFullName
               accessInfo = if lv-menuAccessible then 'Allowed' Else 'Denied'.          
    end.
    else next.
    /* check its not a sub menu itself */ 
    if can-find(first cmenu where cmenu.menu-parent = bmenu.menu-action) 
    then do:
        if not lv-menuAccessible  
        then do:
            accessInfo = "Denied to all Items And Sub Menus".
            next.
        end.
        lv-menuFullName = lv-menuFullName + ','.
        run buildmenu (bmenu.menu-id). 
        if StringToLog(return-value) 
        then do:
            for each t-menuSecurity where menuFullName begins lv-menuFullName:
                delete t-menuSecurity.
            end.
            create t-menuSecurity.
            assign menuFullName = lv-menuFullName
                   accessInfo = 'All Sub Items Allowed'.  
        end.
        entry(lv-menuPosition,lv-menuFullName) = ''.
        lv-menuPosition = lv-menuPosition - 1.
    end.
    if matchesRecordsToShow(lv-menuAccessible)             
    then do:
        find zen-dpgm where zen-dpgm.pgm = bmenu.menu-action
                      no-lock no-error.
        if avail zen-dpgm then do:
            extraDetails = if SecurityCheck(lv-user,
                                 lv-userGroup,  
                                 zen-dpgm.not-users,
                                 zen-dpgm.not-group,
                                 zen-dpgm.run-users,
                                 zen-dpgm.run-groups)
                     Then 'Allowed Run Program'
                     else 'Denied Run Program'.
        end.
        else do:
            extraDetails = 'No Program Security Found'.
        end.                  
    end.
end. 
return string(lv-menuAccessible).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-childMenuSecurityInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-childMenuSecurityInfo Procedure
PROCEDURE get-childMenuSecurityInfo :
/*-----------------------------------------------------------------------------
 Purpose: Build a temp-table with information about security for a group/user.
 Notes:   
-----------------------------------------------------------------------------*/
   define input parameter menuProgramName like zen-dmenu.menu-action no-undo.

   def var lv-menuAccessible as log    no-undo.

   def buffer childMenu          for zen-dmenu.
   def buffer childOfCurrentMenu for zen-dmenu.


   /* menu is a child of whatever came before it, so it is one farther from
      one of the main menus (menus initially seen on the screen) */
   lv-menuPosition = lv-menuPosition + 1.

	/* each menu-item of parent menu */
   for each childMenu no-lock where
   childMenu.menu-parent = menuProgramName
   by menu-parent
   by display-order:
      /* if this menu doesn't run anything, or it is a line in the menus, then
         we don't want to both checking anything about it */
      if childMenu.menu-action = "" or
         childMenu.menu-name   = "<rule>"
      then next.

      /* check to see if this user/group can access this menu */
      lv-menuAccessible = canAccess(
         lv-user,
         lv-userGroup,
         childMenu.not-users,
         childMenu.not-group,
         childMenu.run-users,
         childMenu.run-groups).

      /* this sets the full menu name (menu path and the menu itself).  We
         make the last intry in the list the menu name of this menu, so that
         we keep the menu path as all previous entries */
      entry(lv-menuPosition,lv-menuFullName) = childMenu.menu-name.

      /* this menu is one the user would like to show in the output, so create
         a temp-table record to store information about this menu and check
         to see if this menu has any child menus */
      if matchesRecordsToShow(lv-menuAccessible) then do:
         /* create a temp-table to store information about this menu */
         create t-menuSecurity.
         assign
            t-menuSecurity.menuFullName = lv-menuFullName
            t-menuSecurity.accessInfo   =
               (if lv-menuAccessible then "Allowed by " else "Denied by ") +
               lv-accessInfo.

         /* check to see if this menu record is a parent.  If it is, and we
            can access the menu, then check each of its children */
         if can-find(first childOfCurrentMenu where
            childOfCurrentMenu.menu-parent = childMenu.menu-action)
         then do:
            /* check to see if this menu is accessible */
            if not lv-menuAccessible then do:
               t-menuSecurity.accessInfo = "Denied to all items and sub-" +
                  "menus by " + lv-accessInfo.
               next.
            end. /* menu not accessible */

            /* we need to check child menus of this menu, so add a comma to
               add an extra entry (the child menu names) to the end of the menu
               path (note: commas separate columns in Calc or Excel spreadsheets) */
            lv-menuFullName = lv-menuFullName + ",".

            /* run through each of the menus that are children of this menu */
            run get-childMenuSecurityInfo in this-procedure(
               input childMenu.menu-action).

            /* make the last menu item in the menu name blank, as we have now
               returned from the children, and are back to the parent (last
               entry was name of children, so remove the name). */
            entry(lv-menuPosition,lv-menuFullName) = ''.

            /* chop off the last comma, since we are back to the parent and no
               longer need the extra entry at the end of the full menu name */
            lv-menuFullName = substring(
               lv-menuFullName,
               1,
               length(lv-menuFullName) - 1).

            /* now that we are back to the parent, we have one less set of
               children to worry about */
            lv-menuPosition = lv-menuPosition - 1.
         end. /* found a child menu of this menu */
         else do:
            /* check to make sure there are program details . . . there should
	            always be program details for any valid menus */
            find first zen-dpgm no-lock where
               zen-dpgm.pgm = childMenu.menu-action no-error.
            if not avail zen-dpgm then
               t-menuSecurity.extraDetails = "No Program Details Found".
         end. /* does not have child menus */
      end. /* user wants this menu to show on report */
   end. /* each childMenu */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExtractData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtractData Procedure 
PROCEDURE ExtractData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param pv-user as char no-undo.
def input param pv-group as char no-undo.
def input param pv-show as char no-undo.
def output parameter table for t-menuSecurity.

empty temp-table t-menuSecurity.

if pv-group ne '' and
   pv-user ne ''
then do:
    assign lv-user  = pv-user
           lv-userGroup = pv-group
           lv-recordsToShow  = pv-show.
    run getmenus.
end.
else if pv-user ne '' and 
        pv-group = '' 
then do:
    assign lv-user  = pv-user
           lv-userGroup = '*'
           lv-recordsToShow  = pv-show.
    run getmenus.
end.
else if pv-user = '' and 
        pv-group ne ''
then do:
    assign lv-user  = '*'
           lv-userGroup = pv-group
           lv-recordsToShow  = pv-show.
    run getmenus.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-menuSecurityInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-menuSecurityInfo Procedure 
PROCEDURE get-menuSecurityInfo :
/*-----------------------------------------------------------------------------
 Purpose: Build a temp-table with information about security for a group/user.
 Notes:   
-----------------------------------------------------------------------------*/
   define input  parameter pv-user          as char  no-undo.
   define input  parameter pv-userGroup     as char  no-undo.
   /* recordsToShow can be "all" records, only records a user/group "can"
      access, or only records which are "not" accessible */
   define input  parameter pv-recordsToShow as char  no-undo.
   define output parameter table for t-menuSecurity.

   def var lv-menuAccessible as log no-undo initial true.


	/* make sure there is no leftover data */
   empty temp-table t-menuSecurity.

	/* set up the local variables, group will be blank if it was on the client
	   side unless a user was entered rather than a user group, then we also
	   want to check to see if the user's group is allowed access or denied
	   access to certain menu options */
   assign
      lv-user          = pv-user
      lv-userGroup     = pv-userGroup
      lv-recordsToShow = pv-recordsToShow.

   /* loop through the main menus that one initially sees on the screen
      (those menus that have no parent) */
   for each zen-dmenu where
   zen-dmenu.menu-parent = "" no-lock
   by menu-parent
   by display-order:
      /* check to see if this user/group can access this menu */
      lv-menuAccessible = canAccess(
         lv-user,
         lv-userGroup,
         zen-dmenu.not-users,
         zen-dmenu.not-groups,
         zen-dmenu.run-users,
         zen-dmenu.run-groups).

      /* menu position keeps track of how far away from these initial records
         all child menus are (so we can correctly place a child menu in a
         column for the report output).  Naturally, these initial menus are
         going to be in the first position */
      lv-menuPosition = 1.

      /* menuFullName stores the menu path and menu name for each menu we
         check.  A comma separates each menu which will cause the data to be
         split into separate columns when we output to a Calc or Excel spreadsheet */
      lv-menuFullName = menu-name + ",".

      /* check to see if this menu's accessibility matches the records the user
         wants to see in the report output */
      if matchesRecordsToShow(lv-menuAccessible) then do:
         /* we want to see this record in the output, so create a new record
            in a temp-table and assign values to its fields.  The temp-table
            will be used to output the menu security information. */
         create t-menuSecurity.
         assign
            t-menuSecurity.menuFullName = lv-menuFullName
            t-menuSecurity.accessInfo   =
               (if lv-menuAccessible then "Allowed by " else
               "Denied to all items and sub-menus by ") +
               lv-accessInfo.

         /* we want to show this menu, so check to see if the menu is
            accessible or not.  If it is accessible then check each of the
            menu's child menus to see if we allow or deny access to them */
         if lv-menuAccessible then
            run get-childMenuSecurityInfo in this-procedure(
               input zen-dmenu.menu-action).
      end. /* show menu record in report */
   end. /* each zen-dmenu */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetMenus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetMenus Procedure 
PROCEDURE GetMenus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var lv-menuAccessible   as log init true no-undo.
    def buffer bmenu for zen-dmenu.
    def buffer cmenu for zen-dmenu.  
        
    for each zen-dmenu where zen-dmenu.menu-parent = '' 
                         no-lock
                         by menu-parent
                         by display-order:
                         
        lv-menuAccessible = SecurityCheck(lv-user,
                                 lv-userGroup,  
                                 zen-dmenu.not-users,
                                 zen-dmenu.not-group,
                                 zen-dmenu.run-users,
                                 zen-dmenu.run-groups).
        lv-menuPosition = 1.                                 
        lv-menuFullName = menu-name + ','.
        if matchesRecordsToShow(lv-menuAccessible)       
        then do:
            create t-menuSecurity.
            assign menuFullName   = lv-menuFullName
                   accessInfo = if lv-menuAccessible then 'Allowed' Else 'Denied'.
            run buildmenu (zen-dmenu.menu-id).
        end.       
   end.        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-canAccess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION canAccess Procedure 
FUNCTION canAccess RETURNS LOGICAL
   (pv-user          as char,
    pv-group         as char,
    pv-menuNotUsers  as char,
    pv-menuNotGroups as char,
    pv-menuOkUsers   as char,
    pv-menuOkGroups  as char) :
/*-----------------------------------------------------------------------------
 Purpose: Check to see if a user/group is allowed to access this menu.
 Notes:   
-----------------------------------------------------------------------------*/
   /* the return-value, holds whether a particular menu is accessible or not */
   def var lv-menuAccessible as log no-undo init no.


   /* if we can't find the user or group for a menu, then we want to tell the
      user that access to this particular menu is denied by default */
   lv-accessInfo = "default".

   /* check to see if user is denied access to this menu */
   if pv-user ne "" and can-do(pv-menuNotUsers,pv-user) then assign
      lv-accessInfo     = "user"
      lv-menuAccessible = no.
   /* check to see if user is allowed access to this menu */
   else if pv-user ne "" and can-do(pv-menuOkUsers,pv-user) then assign
      lv-accessInfo     = "user"
      lv-menuAccessible = yes.
   /* check to see if group is denied access to this menu */
   else if pv-group ne "" and can-do(pv-menuNotGroups,pv-group) then assign
      lv-accessInfo     = "group"
      lv-menuAccessible = no.
   /* check to see if group is allowed access to this menu */
   else if pv-group ne "" and can-do(pv-menuOkGroups,pv-group) then assign
      lv-accessInfo     = "group"
      lv-menuAccessible = yes.

   return lv-menuAccessible.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-matchesRecordsToShow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION matchesRecordsToShow Procedure 
FUNCTION matchesRecordsToShow RETURNS LOGICAL
   (pv-menuAccessible as log):
/*-----------------------------------------------------------------------------
 Purpose: Check to see if this menu matches the records the user selected to
           show in the report output.
 Notes:   
-----------------------------------------------------------------------------*/
   /* return whether this menu should be in the report output */
   return
      (lv-recordsToShow = "can" and pv-menuAccessible)     or
      (lv-recordsToShow = "not" and not pv-menuAccessible) or
      (lv-recordsToShow = "all").
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

