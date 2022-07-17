TRIGGER PROCEDURE FOR CREATE OF zen-property.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-property"
                          &Unique-key     = "zen-propertytableid"}
&endif