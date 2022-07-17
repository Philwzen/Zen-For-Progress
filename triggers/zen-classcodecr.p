TRIGGER PROCEDURE FOR CREATE OF zen-classcode.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-classcode"
                          &Unique-key     = "zen-classcodetableid"}
&endif