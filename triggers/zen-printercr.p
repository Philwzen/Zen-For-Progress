TRIGGER PROCEDURE FOR CREATE OF zen-Printer.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-Printer"
                          &Unique-key     = "zen-Printertableid"}
&endif