TRIGGER PROCEDURE FOR CREATE OF zen-blob.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-blob"
                          &Unique-key     = "zen-blobtableid"}
&endif