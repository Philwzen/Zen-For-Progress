TRIGGER PROCEDURE FOR CREATE OF zen-task.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-task"
                          &Unique-key     = "zen-tasktableid"}
&endif