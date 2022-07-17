function GetSysCacheFieldWhere returns char ( pv-table as char,
    pv-where as char,
    pv-datafield as char)  in {&lhandle}.
function GetSysCacheRecordWhere RETURNS HANDLE
  ( pv-table as char,
    pv-where as char,
    pv-retname as char)   in {&lhandle}.
Function CachedCombo RETURNS CHARACTER
  ( pv-table as char,
    pv-key   as char,
    pv-field as char,
    pv-where as char,
    pv-by    as char,
    pv-none  as log,
    pv-wild  as log,
    output pv-codes  as char,
    output pv-values as char)  in {&lhandle}.

function IsCached             returns log  (pv-table as char)         in {&lhandle}.
function RefreshSysTempTables returns log  ()     in {&lhandle}.
