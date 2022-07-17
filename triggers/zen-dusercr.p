TRIGGER PROCEDURE FOR CREATE OF zen-duser.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-duser"
                          &Unique-key     = "zen-dusertableid"}
&endif