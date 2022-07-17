TRIGGER PROCEDURE FOR CREATE OF zen-taskhist.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-taskhist"
                          &Unique-key     = "zen-taskhisttableid"}
&endif