{{&core}bug.i "'pgm header begin'"}
/* name this procedure */
assign
 this-procedure:private-data = unixpath(this-procedure:file-name)
   frame {&frame-name}:private-data = this-procedure:name.
/* fix name if uib is running */
&if DEFINED(UIB_is_Running) NE 0 &then
    if num-entries(this-procedure:file-name,'_') > 1 then
    this-procedure:private-data = entry(2,this-procedure:file-name,'_').
&endif

/* translate static menus */
/* &if defined(UseTranslations) ne 0 &then               */
/*     if {&window-name}:type = 'window' and             */
/*         can-query({&window-name},'menu-bar')          */
/*     then do:                                          */
/*         if valid-handle({&window-name}:menu-bar)      */
/*         then do:                                      */
/*             if int(GetSysVar("language")) ne 0        */
/*               then MenuLabel({&window-name}:handle).  */
/*         end.                                          */
/*     end.                                              */
/* &endif                                                */
/* assign                                              */
/*     {&window-name}:virtual-width = 255              */
/*     {&window-name}:virtual-height = 100             */
/*     {&window-name}:title = gethdr(this-procedure).  */
/* '{&title-text}'. */
/* &IF defined(suppresswindow) = 0 &THEN                             */
/*     /* set window title from db */                                */
/*     if frame {&frame-name}:type = 'frame'                         */
/*         then {&window-name}:title = gethdr(this-procedure).       */
/*         else frame {&frame-name}:title = gethdr(this-procedure).  */
/* &endif                                                            */

&IF defined(about) = 0 &THEN 
current-window:private-data = string(this-procedure) /* this-procedure:file-name */ 
   /* + '{&delim2}' + string(this-procedure) */ . 
&endif

/* load preset background */
/* {{&core}bgload.i}  */

/* position window */
def var lv-mainwindow as handle no-undo.
def var lvarpgmname as char no-undo.
lvarpgmname = entry(num-entries(this-procedure:private-data,'/'),this-procedure:private-data,'/').
def var lv-asyncreports as log no-undo init false.
if not can-do('{&loginscreens}',lvarpgmname) 
then do:
    lv-mainwindow = widget-handle(getsysvar("{&clv}top-win")) no-error.
    lv-asyncreports = int(GetCtrl('{&AsyncMaxJobs}')) > 0.
    if lv-asyncreports 
    then lv-asyncreports = stringtolog(PgmProperty(this-procedure:file-name,'AsyncReport')).
    if lv-asyncreports 
    then lv-asyncreports = entry(1,session:parameter,'^') ne "local".                    
end.
if not valid-handle(lv-mainwindow) 
then lv-mainwindow = current-window.

/* message program-name(1) skip */
/*         string(lv-mainwindow) skip */
/*         valid-handle(lv-mainwindow) skip */
/*         lv-mainwindow:type skip */
/*         lv-mainwindow:name skip */
/*         string(THIS-PROCEDURE:CURRENT-WINDOW) skip */
/*         string(current-window) skip */
/*         string({&window-name}). */

&IF defined(suppresswindow) ne 0 &THEN
    THIS-PROCEDURE:CURRENT-WINDOW = lv-mainwindow.
    def var lv-row as int no-undo.
    def var lv-roffset as int no-undo.
    def var lv-col as int no-undo.
    
/*     SetWinPosition(frame {&frame-name}:handle,this-procedure:file-name,0,88). */

    assign
        lv-roffset = 88
        lv-row = (lv-mainwindow:height-pixels - lv-roffset)
        lv-row = lv-roffset + ((lv-row / 2) - (frame {&frame-name}:height-pixels / 2))
        lv-col = ((lv-mainwindow:width-pixels / 2) -
                                           (frame {&frame-name}:width-pixels / 2))
        frame {&frame-name}:box = true
        frame {&frame-name}:y = lv-row
        frame {&frame-name}:x = lv-col no-error.

&else
    if frame {&frame-name}:type ne 'dialog-box' 
    then do:
        {&window-name}:handle:hidden = true.
        SetWinPosition({&window-name}:handle,this-procedure:file-name,0,0).
/*         {&window-name}:handle:hidden = false.  */
    end.
&endif
tooltip(frame {&frame-name}:handle,
        pgmproperty(this-procedure:file-name,'ProgGroup') ne 'zen').

{{&core}systriggers.i}   /* global system triggers */
{{&core}bug.i "'pgm header end'"}
