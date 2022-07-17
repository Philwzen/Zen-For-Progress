TRIGGER PROCEDURE FOR CREATE OF zen-apidetail.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-apidetail"
                          &Unique-key     = "zen-apidetailtableid"}
&endif