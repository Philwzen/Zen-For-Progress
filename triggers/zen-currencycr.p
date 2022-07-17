TRIGGER PROCEDURE FOR CREATE OF zen-currency.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
&if defined(KeyInTriggers) ne 0 &then
{{&aud}generatethekey.i   &TableName      = "zen-currency"
                          &Unique-key     = "zen-currencytableid"}
&endif