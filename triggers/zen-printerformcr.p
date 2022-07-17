TRIGGER PROCEDURE FOR CREATE OF zen-PrinterForm.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-PrinterForm"
                          &Unique-key     = "zen-PrinterFormtableid"}
&endif