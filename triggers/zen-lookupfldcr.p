TRIGGER PROCEDURE FOR CREATE OF zen-lookupfld.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-lookupfld"
                          &Unique-key     = "zen-lookupfldtableid"}
&endif