TRIGGER PROCEDURE FOR CREATE OF zen-auditdetail.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-auditdetail"
                          &Unique-key     = "zen-auditdetailtableid"}
&endif