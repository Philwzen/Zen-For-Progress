{app-paths.i}
def temp-table t-result no-undo
field t-item as char
field t-user as char
field t-access as char
field t-prog as char.

def var lv-user as char no-undo.
def var lv-group as char no-undo.
def var lv-show as char no-undo.
def var lv-item as char no-undo.
def var x as int no-undo.

function ShowRecord returns logical
(pv-can-menu as log):
        return (lv-show = 'can' and pv-can-menu) or
               (lv-show = 'not' and not pv-can-menu) or
               (lv-show = 'all').
end function.

procedure extractData:
def input param pv-user as char no-undo.
def input param pv-group as char no-undo.
def input param pv-show as char no-undo.
def output parameter table for t-result.

message pv-user '#' skip pv-group '#' view-as alert-box.
empty temp-table t-result.

if pv-group ne '' and
   pv-user ne ''
then
for each zen-duser where zen-duser.duser matches pv-user 
                     and zen-duser.u-group matches pv-group
                   no-lock 
                   break by zen-duser.u-group:
    lv-user = zen-duser.duser.
    lv-group = zen-duser.u-group.
    lv-show = pv-show.
    run getmenus.
end.
else if pv-user ne '' and 
        pv-group = '' 
then
for each zen-duser where zen-duser.duser matches pv-user 
                   no-lock 
                   break by zen-duser.u-group:
    lv-user = zen-duser.duser.
    lv-group = zen-duser.u-group.
    lv-show = pv-show.
    run getmenus.
end.

else if pv-user = '' and 
        pv-group ne ''
then
for each zen-duser where zen-duser.u-group matches pv-group
                   no-lock 
                   break by zen-duser.u-group:
    lv-user = zen-duser.duser.
    lv-group = zen-duser.u-group.
    lv-show = pv-show.
    run getmenus.
end.

end procedure.

procedure getmenus:
    def var lv-can-menu   as log init true no-undo.
    def buffer bmenu for zen-dmenu.
    def buffer cmenu for zen-dmenu.  
        
    for each zen-dmenu where zen-dmenu.menu-parent = '' 
                         no-lock
                         by menu-parent
                         by display-order:
                         
        lv-can-menu = SecurityCheck(lv-user,
                                 lv-group,  
                                 zen-dmenu.not-users,
                                 zen-dmenu.not-group,
                                 zen-dmenu.run-users,
                                 zen-dmenu.run-groups).
        x = 1.                                 
        lv-item = menu-name + ','.
        if ShowRecord(lv-can-menu)       
        then do:
            create t-result.
            assign t-item = lv-item
                   t-user = lv-user
                   t-access = string(lv-can-menu).
        end.       
        run buildmenu (zen-dmenu.menu-id).
    end.        
end procedure.

procedure buildmenu:
def input param v-id           like zen-dmenu.menu-id no-undo.

def var h-menu-item as handle        no-undo.
def var lv-can-menu as log init true no-undo. 
def buffer amenu for zen-dmenu.
def buffer bmenu for zen-dmenu.
def buffer cmenu for zen-dmenu. 
def var lv-can-prog as char no-undo.
find amenu where amenu.menu-id = v-id no-lock no-error.
x = x + 1.
/* each menu-item of parent menu */
for each bmenu where bmenu.menu-parent = amenu.menu-action 
               /*  And can-do(bmenu.menu-grp,lv-group) */
                no-lock
                  by menu-parent
                  by display-order:
   if bmenu.menu-action = "" or menu-name = "<rule>" then next.
   /* menu user security */
   lv-can-menu = SecurityCheck(lv-user,
                                 lv-group,  
                                 bmenu.not-users,
                                 bmenu.not-group,
                                 bmenu.run-users,
                                 bmenu.run-groups).


    entry(x,lv-item) = bmenu.menu-name.
    if ShowRecord(lv-can-menu)             
    then do:
        create t-result.
        assign t-item = lv-item
               t-user = lv-user
               t-access = string(lv-can-menu).          
    end.
    /* check its not a sub menu itself */ 
    if can-find(first cmenu where cmenu.menu-parent = bmenu.menu-action) 
    then do:
        lv-item = lv-item + ','.
        run buildmenu (bmenu.menu-id). 
        entry(x,lv-item) = ''.
        x = x - 1.
    end.
    if ShowRecord(lv-can-menu)             
    then do:
        find zen-dpgm where zen-dpgm.pgm = bmenu.menu-action
                      no-lock no-error.
        if avail zen-dpgm then do:
            t-prog = if SecurityCheck(lv-user,
                                 lv-group,  
                                 zen-dpgm.not-users,
                                 zen-dpgm.not-group,
                                 zen-dpgm.run-users,
                                 zen-dpgm.run-groups)
                     Then 'Can Run Prog'
                     else 'Not Run Prog'.
        end.
        else do:
            t-prog = 'No program found'.
        end.                  
    end.
end. 
end procedure.
