TRIGGER PROCEDURE FOR CREATE OF zen-widlook.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-widlook"
                          &Unique-key     = "zen-widlooktableid"}
&endif