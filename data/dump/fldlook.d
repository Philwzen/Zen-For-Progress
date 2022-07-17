1 38212 "dslookup.w" "" "" 0 "zen-duser" "duser" "User-Name" "" "../zen/" no "where can-do(can-be,'admin')" "" "Administrator User IDs" "" "AdminName" "user-name" no "" "" "duser,user-name" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
100364 0 "dslookup.w" "" "" 0 "acct|blacct" "" "" "" "../zen/" yes "where acct.sys-cd = #1 and acct.practice = #3, first
blacct of acct where blacct.run-no = int(#4)
" "by lname by fname by minit" "Accounts for Specified Billing Run" "" "BillAccounts" "getsysvar|gs-sys-cd,getsysvar|practice,gsopt|fn-acct,getsysvar|reprintRunNumber" yes "" "" "" "" "dyn-lookup.p" "joinlookup" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
3 38212 "dslookup.w" "" "" 0 "zen-country" "country" "description" "" "../zen/" no "where true" "by description" "Countries" "" "Country" "description" no "" "" "country,description" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
4 39985 "calendar.w" "" "Yes" 0 "*" "d" "d" "" "../zen/" yes "where true" "" "Dates" "" "datelookup" "d" yes "" "" "" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
5 59625 "getfile" "" "" 0 "a" "a" "a" "" "" yes "where true" "" "Files" "" "filelookup" "a" no "" "" "" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
6 27532 "dslookup.w" "" "" 0 "" "" "" "" "../zen/" no "where true" "" "Help" "validinvcode" "HelpButton" "" yes "" "" "" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
100387 0 "dslookup.w" "" "" 0 "PerLoc,Person" "" "" "relations=persontableid;persontableid" "../zen/" yes "where string(perloc.locationtableid) = #1,  of perloc" "" "Location Persons" "" "LocationPersons" "getsysvar|Locationtableid" yes "" "" "" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
100385 0 "lookup.w" "" "" 0 "Location" "" "" "" "../zen/" yes "where true" "" "Locations" "" "Locations" "" yes "" "" "" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
7 38212 "dslookup.w" "" "" 0 "zen-fldlook" "lookupname" "lookupname" "" "../zen/" yes "where true" "by lookupname" "Lookup Options" "" "lookups" "" no "" "" "lookupname" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
8 38212 "dslookup.w" "" "" 0 "zen-dmenu" "menu-name" "menu-name" "" "../zen/" yes "where menu-item = #1" "" "Parent Menus" "validinvcode" "MenuItem" "menu-name" no "" "" "Menu-name" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
100386 0 "dslookup.w" "" "" 0 "perloc,Location" "" "" "relations=locationtableid;locationtableid" "../zen/" yes "where string(PerLoc.persontableid) = #1,  of perloc" "" "Person Locations" "" "PersonLocations" "getsysvar|PersonTableid" yes "" "" "" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
100384 0 "dslookup.w" "" "" 0 "person" "" "" "" "../zen/" yes "where true" "" "People" "" "Persons" "" yes "" "" "" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
247 0 "dslookup.w" "" "" 0 "zen-control" "" "" "" "../zen/" yes "where true" "" "Programs By Description" "" "ProgByDesc" "ctrl-idx" yes "" "" "" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
9 36014 "dslookup.w" "" "" 0 "zen-dpgm" "pgm" "name" "" "../zen/" yes "where true" "by pgm" "Programs" "" "programpgm" "" no "" "" "pgm,name" "pgm" "dyn-lookup.p" "joinlookup" "codezen/srv/" "widinfo.w" "codezen/" "ProgByDesc" "By Description" yes
10 38212 "dslookup.w" "" "" 0 "zen-duser" "duser" "User-name" "" "../zen/" no "where can-do(can-be,'rep')" "" "Sales Representatives" "validinvcode" "RepPerson" "User-name" no "" "" "duser,user-name" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
209 69122 "dslookup.w" "" "" 0 "s-user" "" "" "" "../zen/" yes "where true" "by signonid" "User IDs" "" "UserID" "" yes "" "" "" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "UserSameClient" "Users With Same Client" yes
13 38212 "dslookup.w" "" "" 0 "zen-duser" "duser" "user-name" "" "../zen/" yes "where true" "" "User IDs" "" "UserLookup" "" no "" "" "duser,user-name" "" "dyn-lookup.p" "open-query" "codezen/srv/" "widinfo.w" "codezen/" "" "" yes
.
PSC
filename=zen-fldlook
records=0000000000017
ldbname=schadm
timestamp=2018/02/05-15:37:41
numformat=44,46
dateformat=dmy-1950
map=NO-MAP
cpstream=ISO8859-1
.
0000004343
