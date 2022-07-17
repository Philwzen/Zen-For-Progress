TRIGGER PROCEDURE FOR CREATE OF zen-language.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-language"
                          &Unique-key     = "zen-languagetableid"}
&endif