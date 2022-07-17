TRIGGER PROCEDURE FOR CREATE OF zen-colours.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-colours"
                          &Unique-key     = "zen-colourstableid"}
&endif