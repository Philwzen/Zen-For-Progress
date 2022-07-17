TRIGGER PROCEDURE FOR CREATE OF zen-f-hotkey.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-f-hotkey"
                          &Unique-key     = "zen-f-hotkeytableid"}
&endif