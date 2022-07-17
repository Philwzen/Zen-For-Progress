TRIGGER PROCEDURE FOR CREATE OF zen-dwidget.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-dwidget"
                          &Unique-key     = "zen-dwidgettableid"}
&endif