TRIGGER PROCEDURE FOR CREATE OF zen-lan_field.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-lan_field"
                          &Unique-key     = "zen-lan_fieldtableid"}
&endif