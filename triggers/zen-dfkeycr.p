TRIGGER PROCEDURE FOR CREATE OF zen-dfkey.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-dfkey"
                          &Unique-key     = "zen-dfkeytableid"}
&endif