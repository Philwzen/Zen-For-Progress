def var hSrv    as handle no-undo.
def var hSrvAia as handle no-undo.

create server hSrvAia.

   hSrvAia:connect("-URL http://philw.ath.cx:8080/aia/Aia?AppService=crs"). 
   
if hSrvAia:connected() then
do:
  message subst("Successfully connected to AppServer (&1).",
session:client-type) view-as alert-box.
end.
else message subst("Did not connect to AppServer  (&1).",
             session:client-type) 
     view-as alert-box.
