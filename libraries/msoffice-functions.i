/* Functions in lh-MSOffice predeclare any Functions */
Function MSCloseApplication Returns Log        (ApplHandle   as Com-Handle) in {&lhandle}.
Function MSNumColumns       Returns Int        (TableHandle  as Com-Handle) in {&lhandle}.
Function MSCopySelection    Returns Log        (ApplHandle   as Com-Handle) in {&lhandle}.
Function MSGetDocHandle     Returns com-handle (ApplHandle   as Com-Handle,
                                                document     as Char)       in {&lhandle}.
Function MSPrInt            Returns Log        (ApplHandle   as Com-Handle,
                                                ToFile       as Char)       in {&lhandle}.
Function MSPrIntMe          Returns Log        (Doc          as Char)       in {&lhandle}.
Function MSUpdateFields     Returns Log        (ApplHandle   as Com-Handle) in {&lhandle}.
Function MSSetSaved         Returns Log        (ApplHandle   as Com-Handle,
                                                Document     as Char,
                                                Mode         as Log)        in {&lhandle}.         
Function MSApplicationName  Returns Char       (ApplHandle   as Com-Handle) in {&lhandle}.         
Function MSMailMerge        Returns Char       (ApplHandle   as Com-Handle,
                                                Datafile     as Char, 
                                                Destination  as Int,
						                        output numrecs as Int)      in {&lhandle}.
Function MSApplicationRunning Returns com-handle (Appname    as Char)       in {&lhandle}.
Function MSOpenApplication  Returns Com-Handle (AppName      as Char,
                                                HideWin      as Char)       in {&lhandle}.
Function MSSetPrInter       Returns Log        (ApplHandle   as Com-Handle,
                                                PrinterName  as Char)       in {&lhandle}.
Function MsCLoseDocument returns log (Applhandle as com-handle) in {&lhandle}.
Function MSOpenDocument     Returns Log        (ApplHandle   as Com-Handle,
                                                Document     as Char,
						readonly as char)       in {&lhandle}.
Function MSOpenTemplate     Returns Log        (ApplHandle   as Com-Handle,
                                                Document     as Char)       in {&lhandle}.
Function MSInsertWorkbook   Returns Com-Handle (ApplHandle   as Com-Handle,
                                                Data         as Char,
                                                Extras       as Char)       in {&lhandle}.
Function MSSaveAs           Returns Log        (ApplHandle   as Com-Handle,
						                        FileName     as Char,
                                                document     as Char)       in {&lhandle}.
Function MSSetSelectionFont Returns Log        (ApplHandle   as Com-Handle,
                                                FontName     as Char,
                                                FontSize     as Int,
                                                extras       as Char)       in {&lhandle}.
Function MSInsertTable      Returns Com-Handle (ApplHandle   as Com-Handle,
						                        data         as Char,
                                                FontName     as Char,
                                                FontSize     as Int,
                                                Extras       as Char)       in {&lhandle}.
Function MSCenterAll        Returns Log        (ApplHandle   as Com-Handle) in {&lhandle}.
Function MSSetSelection     Returns Log        (ApplHandle   as Com-Handle,
                                                SelString    as Char)       in {&lhandle}.
Function MSDeleteSelection  Returns Log        (ApplHandle   as Com-Handle,
					                            Extra        as Int )       in {&lhandle}.
Function MSShadeSelection   Returns Log        (ApplHandle   as Com-Handle,
                                                ClrValue     as Int)        in {&lhandle}.
Function MSHighlightColumns Returns Log        (ApplHandle   as Com-Handle,
                                                ColumnsNum   as Char,
						                        ColorValue   as Int)        in {&lhandle}.
Function MSAlignSelection   Returns Log        (ApplHandle   as Com-Handle,
                                                Mode         as Int)        in {&lhandle}.
Function MSAlignColumns     Returns Log        (ApplHandle   as Com-Handle,
						                        Tablehandle  as Com-Handle,
                                                ColumnsNum   as Char,
						                        AlignValue   as Int)        in {&lhandle}.
Function MSsizeColumns      Returns Log        (ApplHandle   as Com-Handle,
 						                        Tablehandle  as Com-Handle,
                                                ColumnsNum   as Char,
						                        SizeValue    as Int)        in {&lhandle}.
Function MSSetMargins       Returns Log        (ApplHandle   as Com-Handle,
						                        TopMargin    as Dec,
						                        BottomMargin as Dec,
						                        LeftMargin   as Dec,
						                        RightMargin  as Dec)        in {&lhandle}.
Function MsMergeToWord     Returns Log         (ApplHandle   as Com-Handle,
                                		        Data         as Char,
						                        Template     as Char)       in {&lhandle}.
Function MsMergeToExcel    Returns Log         (ApplHandle   as Com-Handle,
						                        Data         as Char,
						                        Template     as Char)       in {&lhandle}.
Function MsMergeToOutlook  Returns Log         (ApplHandle   as Com-Handle,
						                        FromName     as Char,
						                        ToName       as Char,
                                                Subject      as Char,
                                                MsgText      as Char,
                                                MsgAttach    as Char,
						                        Expires      as Char)       in {&lhandle}.
