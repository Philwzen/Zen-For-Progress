TRIGGER PROCEDURE FOR CREATE OF zen-dpgm.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-dpgm"
                          &Unique-key     = "zen-dpgmtableid"}
&endif