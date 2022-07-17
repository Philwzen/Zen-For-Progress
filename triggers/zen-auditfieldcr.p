TRIGGER PROCEDURE FOR CREATE OF zen-auditfield.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-auditfield"
                          &Unique-key     = "zen-auditfieldtableid"}
&endif