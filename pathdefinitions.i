/* All relative to startin directory*/
/* path to the system */
&glob sys           sys/ 
/* zen paths */
&glob ini-filename  zen
&glob ini-path      ../zen/

&glob core          ../zen/
&glob base          ../zen/
&glob srv  srv/
&glob logs	logs/
&glob aud      {&core}audit/
&glob tsk {&core}taskserver/
&glob tools tools/
&glob Addons		{&core}ocx/
&glob template		{&core}templates/
&glob triggers       triggers/
&glob reports reports/
&glob tables	     tables/
&glob libraries      libraries/
&glob prt 	      print/
&Glob Crystal        {&reports}cry/
&glob scratch     temp/
&glob Repout	     {&scratch}
&glob SrvRepout	     {&scratch}
&glob rep      reports/
&glob cry      cryrep/
&glob lib      libraries/
&glob Bmp       grafix/
&glob ico	{&core}{&bmp}unused/
&glob docman   {&core}docman/
&glob ru    rules/