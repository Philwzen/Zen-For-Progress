TRIGGER PROCEDURE FOR CREATE OF zen-log.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-log"
                          &Unique-key     = "zen-logtableid"}
&endif