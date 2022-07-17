TRIGGER PROCEDURE FOR CREATE OF zen-context.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-context"
                          &Unique-key     = "zen-contexttableid"}
&endif