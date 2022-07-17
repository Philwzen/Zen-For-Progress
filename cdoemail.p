
&glob Schema http://schemas.microsoft.com/cdo/configuration/

def var ch-msg as com-handle no-undo.
def var lv-userid as char no-undo init '????'.
def var lv-password as char no-undo init '????'.
def var lv-server as char no-undo init /* "localhost" */ "mail.practice-alt.com"  .

create "cdo.message" ch-msg.

ch-msg:Subject = "a test msg".
ch-msg:From = "guidev@practice-alt.com".
ch-msg:to = "philw@metronet.co.uk".
ch-msg:textbody = "any damm ting you like".

/* ch-msg:configuration:fields:item("{&Schema}sendusing") = 2. */
/* ch-msg:configuration:fields:item("{&Schema}smtpserver") = lv-server. */
/* ch-msg:configuration:fields:item("{&Schema}smtpauthenticate") = 1. */
/* ch-msg:configuration:fields:item("{&Schema}sendusername") = lv-userid. */
/* ch-msg:configuration:fields:item("{&Schema}sendpassword") = lv-password. */
/* ch-msg:configuration:fields:item("{&Schema}smtpserverport") = 25. */
/* ch-msg:configuration:fields:item("{&Schema}smtpusessl") = false. */
/* ch-msg:configuration:fields:item("{&Schema}smtpconnectiontimeout") = 60. */
/* ch-msg:configuration:fields:update(). */

ch-msg:send.
