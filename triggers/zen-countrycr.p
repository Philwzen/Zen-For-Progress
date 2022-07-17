TRIGGER PROCEDURE FOR CREATE OF zen-country.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-country"
                          &Unique-key     = "zen-countrytableid"}
&endif