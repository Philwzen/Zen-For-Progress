TRIGGER PROCEDURE FOR CREATE OF zen-control.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-control"
                          &Unique-key     = "zen-controltableid"}
&endif