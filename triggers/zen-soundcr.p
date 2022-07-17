TRIGGER PROCEDURE FOR CREATE OF zen-sound.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-sound"
                          &Unique-key     = "zen-soundtableid"}
&endif