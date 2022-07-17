TRIGGER PROCEDURE FOR CREATE OF zen-auditconfig.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-auditconfig"
                          &Unique-key     = "zen-auditconfigtableid"}
&endif