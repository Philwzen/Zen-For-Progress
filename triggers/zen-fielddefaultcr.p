TRIGGER PROCEDURE FOR CREATE OF Zen-FieldDefault.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "Zen-FieldDefault"
                          &Unique-key     = "Zen-FieldDefaulttableid"}
&endif