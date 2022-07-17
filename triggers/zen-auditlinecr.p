TRIGGER PROCEDURE FOR CREATE OF zen-auditline.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-auditline"
                          &Unique-key     = "zen-auditlinetableid"}
&endif