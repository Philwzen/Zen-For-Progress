TRIGGER PROCEDURE FOR DELETE OF zen-spool.

&glob serverprogram
{app-paths.i justvars = true}
{{&core}control.i}
{{&aud}trdelete.i &Table = "zen-spool"
                  &key   = "zen-spool.zen-spoolTableid"}
