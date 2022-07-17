TRIGGER PROCEDURE FOR CREATE OF zen-PrinterCode.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-PrinterCode"
                          &Unique-key     = "zen-PrinterCodetableid"}
&endif