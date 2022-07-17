for each s-user:
   find first zen-duser where 
      zen-duser.duser = s-user.signonid no-lock no-error.
   
   if not avail zen-duser then do:
      create zen-duser.
      assign
         zen-duser.duser      = s-user.signonid
         zen-duser.user-name  = s-user.user-name
         zen-duser.country    = 01
         zen-duser.lan_lanid  = 0
         zen-duser.u-group = "system".
      release zen-duser.
   end.

   /* restrict practices */
   s-user.ext1[3] = "".
   for each actv where
   actv.sys-cd   = "1"    and
   actv.practice = ""     and
   actv.actv-no  = "upra" and
   actv.acct-no  = s-user.signonid:
      /* make comma-delimited list of actv.docm-no entries which represent
         practices the user is allowed to enter */
      s-user.ext1[3] = s-user.ext1[3] + 
         (if s-user.ext1[3] = "" then "" else ",") +
         actv.docm-no.
   end.

   /* restrict printers */
   s-user.ext1[4] = "".
   for each actv where
   actv.sys-cd   = "1"    and
   actv.practice = ""     and
   actv.actv-no  = "upri" and
   actv.acct-no  = s-user.signonid:
      /* make comma-delimited list of actv.docm-no entries which represent
         printers the user is allowed to enter */
      s-user.ext1[4] = s-user.ext1[4] + 
         (if s-user.ext1[4] = "" then "" else ",") +
         actv.docm-no.
   end.
end.

/* make sure not restricting practice with no practices selected */
for each s-user where pra-restrict and def-practice ne "" and ext1[3] = "":
  ext1[3] = def-practice.
end.

/* make sure not restricting printer with no printers selected */
for each s-user where pri-restrict and def-printer ne "" and ext1[4] = "":
  ext1[4] = def-printer.
end.
