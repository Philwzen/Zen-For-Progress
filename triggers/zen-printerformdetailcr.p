TRIGGER PROCEDURE FOR CREATE OF zen-PrinterFormDetail.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-PrinterFormDetail"
                          &Unique-key     = "zen-PrinterFormDetailtableid"}
&endif