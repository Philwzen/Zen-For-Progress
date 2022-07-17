TRIGGER PROCEDURE FOR CREATE OF zen-widgetproperty.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-widgetproperty"
                          &Unique-key     = "zen-widgetpropertytableid"}
&endif