TRIGGER PROCEDURE FOR CREATE OF Zen-Spool.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "Zen-Spool"
                          &Unique-key     = "Zen-Spooltableid"}
&endif