/* reset all button hotkeys to do nothing */

if session:display-type ne 'gui' then do:
    IF opsys = 'unix' then 
        on esc-a,esc-A,esc-b,esc-B,esc-c,esc-C,
           esc-d,esc-D,esc-e,esc-E,esc-f,esc-F,esc-g,esc-G,
           esc-h,esc-H,esc-i,esc-I,esc-j,esc-J,esc-k,esc-K,
           esc-l,esc-L,esc-m,esc-M,esc-n,esc-N,esc-o,esc-O,
           esc-p,esc-P,esc-q,esc-Q,esc-r,esc-R,esc-s,esc-S,
           esc-t,esc-T,esc-u,esc-U,esc-v,esc-V,esc-w,esc-W,
           esc-x,esc-X,esc-y,esc-Y,esc-z,esc-Z 
        anywhere bell.
    else
        on f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,
   ctrl-f1,ctrl-f2,ctrl-f3,ctrl-f4,ctrl-f5,ctrl-f6,ctrl-f7,ctrl-f8,ctrl-f9,ctrl-f10,ctrl-f11,ctrl-f12,
   alt-f1,alt-f2,alt-f3,alt-f5,alt-f6,alt-f7,alt-f8,alt-f9,alt-f10,alt-f11,alt-f12,
   shift-f1,shift-f2,shift-f3,shift-f4,shift-f5,shift-f6,shift-f7,shift-f8,shift-f9,shift-f10,shift-f11,shift-f12
        anywhere bell.
end.
