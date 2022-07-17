TRIGGER PROCEDURE FOR CREATE OF zen-dmenu.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-dmenu"
                          &Unique-key     = "zen-dmenutableid"}
&endif