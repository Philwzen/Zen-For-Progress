TRIGGER PROCEDURE FOR CREATE OF zen-fldlook.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-fldlook"
                          &Unique-key     = "zen-fldlooktableid"}
&endif