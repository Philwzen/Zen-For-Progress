TRIGGER PROCEDURE FOR CREATE OF zen-spool.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
{{&aud}trcreate.i &Table    = "zen-spool"
                  &Uniquekey = "zen-spooltableid"}
