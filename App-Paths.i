//&glob inzen /* def this if using zen dir as home dir */

&glob core         /wddlc/zen/ /* the ONLY Absolute path!!! ?? env var ?? */

/* global pre processors for system paths */
{{&core}pathdefinitions.i}  /* All relative to startin dir */

&if defined(justpaths) eq 0 &then 
&glob KeyInTriggers     /* Use triggers to create unique key */ 

/* useful vars */
{{&core}sysvars.i}

/* system specific preprocessors overrides */
{{&sys}overrides.i}
&endif
DEF STREAM OP.
