TRIGGER PROCEDURE FOR CREATE OF zen-ocx.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-ocx"
                          &Unique-key     = "zen-ocxtableid"}
&endif