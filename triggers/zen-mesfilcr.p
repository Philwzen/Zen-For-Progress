TRIGGER PROCEDURE FOR CREATE OF zen-mesfil.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-mesfil"
                          &Unique-key     = "zen-mesfiltableid"}
&endif