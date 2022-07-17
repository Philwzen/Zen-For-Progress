/* Sonic Messaging */
Function PubHandle              RETURNS handle  ()                                      in {&lhandle}.
Function SendMessage            RETURNS Log     (char,char)                             in {&lhandle}.
Function SetMessageConnections  RETURNS Log     (char,char,char)                        in {&lhandle}.
