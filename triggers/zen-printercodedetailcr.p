TRIGGER PROCEDURE FOR CREATE OF zen-PrinterCodeDetail.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-PrinterCodeDetail"
                          &Unique-key     = "zen-PrinterCodeDetailtableid"}
&endif