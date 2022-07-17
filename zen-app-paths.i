/* global pre processors for system paths */
{core/pathdefinitions.i}  /* the ONLY place we hard code a path!!! */

&glob KeyInTriggers     /* Use triggers to create unique key */ 

/* useful vars */
{{&core}sysvars.i}

/* system specific preprocessors overrides */
{{&sys}overrides.i}

DEF STREAM OP.
