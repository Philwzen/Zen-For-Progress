TRIGGER PROCEDURE FOR CREATE OF zen-tserver.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-tserver"
                          &Unique-key     = "zen-tservertableid"}
&endif