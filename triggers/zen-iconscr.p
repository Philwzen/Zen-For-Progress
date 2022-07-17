TRIGGER PROCEDURE FOR CREATE OF zen-icons.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-icons"
                          &Unique-key     = "zen-iconstableid"}
&endif