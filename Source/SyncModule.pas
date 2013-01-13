(**

  This module defines classes for handling and comparing two directories of
  files.

  @Version 1.5
  @Date    13 Jan 2013
  @Author  David Hoyle

**)
Unit SyncModule;

Interface

Uses
  SysUtils,
  Classes,
  Contnrs,
  Windows;

Type
  (** An enumerate to define an action to take with a file. **)
  TFileAction = (faNo, faYes, faAll, faCancel, faUnknown);

  (** An event signature for the start of a search. **)
  TSearchStartNotifier = Procedure(strFolder: String) Of Object;
  (** A event signature for feeding back progress during searching for files. **)
  TSearchNotifier = Procedure(strFolder, strFileName: String; iCount: Integer) Of Object;
  (** An event signature for the end of a search. **)
  TSearchEndNotifier = Procedure(iFileCount: Integer; iTotalSize: Int64) Of Object;

  (** An event signature for the start of a comparison operation. **)
  TCompareStartNotifier = Procedure(strLeftFldr, strRightFldr: String) Of Object;
  (** An event signature for feeding back progress during comparisons. **)
  TCompareNotifier = Procedure(strLeftFldr, strRightFldr, strFileName: String;
    iPosition, iMaxItems: Integer) Of Object;
  (** An event signature for the end of the comparison operation. **)
  TCompareEndNotifier = Procedure Of Object;

  (** An event signature for the start of the Match List operation. **)
  TMatchListStartNotifier = Procedure Of Object;
  (** An event signature for feeding bacl progress during Matching List Items. **)
  TMatchListNotifier = Procedure(iPosition, iMaxItems: Integer) Of Object;
  (** An event signature for the end of the Match List operation. **)
  TMatchListEndNotifier = Procedure Of Object;

  (** An event signature for the start of the deletion process. **)
  TDeleteStartNotifier = Procedure(iFileCount: Integer; iTotalSize: Int64) Of Object;
  (** An event signature for the start of the deletion of an individual file. **)
  TDeletingNotifier = Procedure(iFile : Integer; strFileName: String) Of Object;
  (** An event signature for the end of the deletion of an individual file. **)
  TDeletedNotifier = Procedure(iFile: Integer; iSize: Int64; boolSuccess: Boolean;
    strErrmsg: String) Of Object;
  (** An event signature to prompt for the deletion of a file. **)
  TDeleteQueryNotifier = Procedure(strFileName: String; Var Option: TFileAction)
    Of Object;
  (** An event signature to prompt for the deletion of a read only file. **)
  TDeleteReadOnlyQueryNotifier = Procedure(strFileName: String; Var Option: TFileAction)
    Of Object;
  (** An event sighature for the end of the deletion process. **)
  TDeleteEndNotifier = Procedure(iDeleted, iSkipped, iErrors: Integer) Of Object;

  (** An event signature for the start of the copying process. **)
  TCopyStartNotifier = Procedure(iTotalCount: Integer; iTotalSize: Int64) Of Object;
  (** An event signature for the start of the copying of an individual file. **)
  TCopyingNotifier = Procedure(iFile : Integer; strSource, strDest,
    strFileName: String) Of Object;
  (** An event signature for feeding back progress on the copying of a file. **)
  TCopyContents = Procedure(iCopiedSize, iTotalSize: Int64) Of Object;
  (** An event signature for the end of the copying of an individual file. **)
  TCopiedNotifier = Procedure(iCopiedFiles: Integer; iCopiedFileTotalSize,
    iCopiedTotalSize: Int64; boolSuccess: Boolean; strErrmsg: String) Of Object;
  (** An event signature to prompt for the overwriting of a file. **)
  TCopyQueryNotifier = Procedure(strSourceFile, strDestFile: String;
    Var Option: TFileAction) Of Object;
  (** An event signature to prompt for the overwriting of a read only file. **)
  TCopyReadOnlyQueryNotifier = Procedure(strSourceFile, strDestFile: String;
    Var Option: TFileAction) Of Object;
  (** An event signature for the end of the copying process. **)
  TCopyEndNotifier = Procedure(iCopied, iSkipped, iError: Integer) Of Object;
  (** An event signature for the start of the different size process. **)
  TDiffSizeStartNotifier = Procedure(iFileCount: Integer) Of Object;
  (** An event signature for each file in the different size process. **)
  TDiffSizeNotifier = Procedure(strLPath, strRPath, strFileName: String) Of Object;
  (** An event signature for the end of the different size process. **)
  TDiffSizeEndNotifier = Procedure() Of Object;
  (** An event signature for the start of the Nothing to do Process. **)
  TNothingToDoStartNotifier = Procedure(iFileCount: Integer) Of Object;
  (** An event signature for each file in the Nothing to do Process. **)
  TNothingToDoNotifier = Procedure(strLPath, strRPath, strFileName: String) Of Object;
  (** An event signature for the end of the Nothing to do Process. **)
  TNothingToDoEndNotifier = Procedure() Of Object;

  (** An event signature for updating the taskbar from a progress dialogue. **)
  TUpdateProgress = Procedure(iPosition, iMaxPosition: Integer) Of Object;

  (** A ercord to define the upper and lower limits of a progress section. **)
  TSectionRecord = Record
    FMin: Integer;
    FMax: Integer;
  End;

  (** A type to define the status of a file **)
  TStatus = (stNewer, stOlder, stSame, stDiffSize);

  (** A type to define whether the CheckDifference method should check for
      Older or Newer differences. **)
  TCheckDifference = (cdNewer, cdOlder);

  (** This is an enumerate for synchronisation options on a pair of folders. **)
  TSyncOption = (soEnabled, soPrimaryLeft, soPrimaryRight, soOverwriteReadOnlyFiles,
    soConfirmYes, soConfirmNo, soNoRecursion);

  (** A set of sync options. **)
  TSyncOptions = Set Of TSyncOption;

  (** A list of enumerate values for the different types of file operation that
   can be undertaken. **)
  TFileOp = (foNothing, foLeftToRight, foRightToLeft, foDelete, foSizeDiff);
  (** A set of the above TFileOp enumerates. **)
  TFileOps = Set Of TFileOp;

  (** An enumerate of Folder Sync Options from the pre-1.5 version - required for
      upgrade. **)
  TOLDFldrSyncOption = (fsoCloseIFNoFilesAfterComparisonOLD, fsoNoConfirmation,
    fsoDoNotConfirmMkDir, fsoShowSimpleProgress, fsoStartProcessingAutomaticallyOLD,
    fsoHideLongFileNames);

  (** An enumerate of NEW Folder Sync Options **)
  TFldrSyncOption = (fsoCloseIFNoFilesAfterComparison, fsoStartProcessingAutomatically);

  (** A set of folder sync options. **)
  TOLDFldrSyncOptions = Set Of TOLDFldrSyncOption;
  (** A set of folder sync options. **)
  TFldrSyncOptions = Set Of TFldrSyncOption;

  (** A variant record to translate various options for each folder using the
      data element of the string list item. **)
  TFolderOptionsAdapter = Record
    Case Integer Of
      1:
        (FRAWData: Integer); // 4 bytes
      2:
        (FOBjData: TObject); // 4 btes
      3:
        (FSyncOptions: TSyncOptions; // 1 bytes
          FReserved1: Byte;          // 1 bytes
          FReserved2: Byte;          // 1 bytes
          FReserved3: Byte;          // 1 bytes
        );
  End;

  (** A record to describe a single file. **)
  TFileRecord = Class
  Strict Private
    FFileName  : String;
    FSize      : Int64;
    FAttributes: Integer;
    FDateTime  : Integer;
    FStatus    : TStatus;
  Strict Protected
    Procedure SetStatus(Status: TStatus);
  Public
    Constructor Create(strName: String; iSize: Int64; iAttributes: Integer;
      dtDateTime: Integer; Status: TStatus); Virtual;
    (**
      A property to return the filename of the file in this class.
      @precon  None.
      @postcon Returns the filename of the file in the file record.
      @return  a String
    **)
    Property FileName: String Read FFileName;
    (**
      A property to return the size of the file in this class.
      @precon  None.
      @postcon Returns the size of the file in the file record.
      @return  an Int64
    **)
    Property Size: Int64 Read FSize;
    (**
      A property to return the attributes of the file in this class.
      @precon  None.
      @postcon Returns the attrbutes of the file in the file record.
      @return  an Integer
    **)
    Property Attributes: Integer Read FAttributes;
    (**
      A property to return the date and time of the file in this class.
      @precon  None.
      @postcon Returns the date and time as an age integer for the file record.
      @return  an Integer
    **)
    Property DateTime: Integer Read FDateTime;
    (**
      A property to return the status of the file in this class.
      @precon  None.
      @postcon Returns the status of the file record.
      @return  a TStatus
    **)
    Property Status: TStatus Read FStatus Write SetStatus;
  End;

  (** This class defines a list of files from a single directory. **)
  TFileList = Class
  Strict Private
    FFolderPath         : String;
    FFiles              : TObjectList;
    FSearchStartNotifier: TSearchStartNotifier;
    FSearchNotifier     : TSearchNotifier;
    FSearchEndNotifier  : TSearchEndNotifier;
    FExclusions         : TStringList;
    FTotalSize          : Int64;
    FFileFilters        : TStringList;
    FSyncOptions        : TSyncOptions;
  Strict Protected
    Function InExclusions(strFileName: String): Boolean;
    Function GetCount: Integer;
    Function GetFiles(iIndex: Integer): TFileRecord;
    Procedure RecurseFolder(strFolderPath: String); Virtual;
    Procedure DoSearch(strFolder, strFile: String; iCount: Integer);
    Procedure DoSearchStart(strFolder: String);
    Procedure DoSearchEnd(iFileCount: Integer; iTotalSize: Int64);
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;
    Procedure SearchFolder(strFolderPath, strFileFilter, strExclusions: String;
      SyncOps : TSyncOptions);
    Function Find(strFileName: String): Integer; Virtual;
    Procedure Delete(iIndex : Integer);
    (**
      Aa property to return the folder path for the class.
      @precon  None.
      @postcon Returns the folder path for the class.
      @return  a String
    **)
    Property FolderPath: String Read FFolderPath;
    (**
      A property to return the number of files in the list.
      @precon  None.
      @postcon Returns the current number of items in the classes collection.
      @return  an Integer
    **)
    Property Count: Integer Read GetCount;
    (**
      A property to return the relative path of the indexed filename.
      @precon  iIndex must be from 0 to FCount - 1
      @postcon Returns the relative path of the indexed filename.
      @param   iIndex as       an Integer
      @return  a TFileRecord
    **)
    Property Files[iIndex: Integer]: TFileRecord Read GetFiles; Default;
    (**
      A property to get the total size of the file list.
      @precon  None.
      @postcon Returns an integer representing the total size.
      @return  an Int64
    **)
    Property TotalSize: Int64 Read FTotalSize;
    (**
      This is an event handler for the start of the searching for files process.
      @precon  None.
      @postcon Fired before the start of a search for files in a root folder.
      @return  a TSearchStartNotifier
    **)
    Property OnSearchStart: TSearchStartNotifier Read FSearchStartNotifier
      Write FSearchStartNotifier;
    (**
      This is an event handler that is fired fro each file found.
      @precon  None.
      @postcon Event handler that is fired fro each file found.
      @return  a TSearchNotifier
    **)
    Property OnSearch: TSearchNotifier Read FSearchNotifier Write FSearchNotifier;
    (**
      This is an event handler that is fired after searching for files.
      @precon  None
      @postcon Event handler that is fired after searching for files.
      @return  a TSearchEndNotifier
    **)
    Property OnSearchEnd: TSearchEndNotifier Read FSearchEndNotifier
      Write FSearchEndNotifier;
  End;

  (** A class to compare to list of folder file. **)
  TCompareFolders = Class
  Strict Private
    FLeftFldr            : TFileList;
    FRightFldr           : TFileList;
    FSyncOptions         : TSyncOptions;
    FFldrSyncOptions     : TFldrSyncOptions;
    FSearchStartNotifier : TSearchStartNotifier;
    FSearchNotifier      : TSearchNotifier;
    FSearchEndNotifier   : TSearchEndNotifier;
    FCompareStartNotifier: TCompareStartNotifier;
    FCompareNotifier     : TCompareNotifier;
    FCompareEndNotifier  : TCompareEndNotifier;
  Strict Protected
    Procedure CompareFolders;
    Procedure DoCompareStart(strLeftFldr, strRightFldr: String);
    Procedure DoCompare(strLeftFldr, strRightFldr, strFileName: String;
      iPosition, iMaxItems: Integer);
    Procedure DoCompareEnd;
    Procedure SetSearchNotifier(SearchNotifier: TSearchNotifier);
    Procedure SetSearchStartNotifier(SearchStartNotifier: TSearchStartNotifier);
    Procedure SetSearchEndNotifier(SearchEndNotifier: TSearchEndNotifier);
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;
    Procedure SearchFolders(strLeftFldr, strRightFldr, strPatterns: String;
      strExclusions: String; iSection: Integer; SyncOps: TSyncOptions);
    Function CheckDifference(iTimeDifference: Integer; iSizeDifference: Integer;
      Check: TCheckDifference): Boolean;
    Procedure ClearUnchangedItems;
    (**
      A property to reference the Left Folder file list.
      @precon  None.
      @postcon Returns a reference to the left file folder list.
      @return  a TFileList
    **)
    Property LeftFldr: TFileList Read FLeftFldr;
    (**
      A property to reference the Right Folder file list.
      @precon  None.
      @postcon Returns a reference to the right file folder list.

      @return  a TFileList
    **)
    Property RightFldr: TFileList Read FRightFldr;
    (**
      This property returns the sync options that should be applied to the
      lists of files.
      @precon  None.
      @postcon Returns the sync options that should be applied to the
               lists of files.
      @return  a TSyncOptions
    **)
    Property SyncOptions: TSyncOptions Read FSyncOptions;
    (**
      This property returns the Folder Sync Options for the folder pair.
      @precon  None.
      @postcon Returns the Folder Sync Options for the folder pair.
      @return  a TFldrSyncOptions
    **)
    Property FldrSyncOptions: TFldrSyncOptions Read FFldrSyncOptions;
    (**
      This is an event handler for the start of the searching for files process.
      @precon  None.
      @postcon Fired before the start of a search for files in a root folder.
      @return  a TSearchStartNotifier
    **)
    Property OnSearchStart: TSearchStartNotifier Read FSearchStartNotifier
      Write SetSearchStartNotifier;
    (**
      This is an event handler that is fired fro each file found.
      @precon  None.
      @postcon Event handler that is fired fro each file found.
      @return  a TSearchNotifier
    **)
    Property OnSearch: TSearchNotifier Read FSearchNotifier Write SetSearchNotifier;
    (**
      This is an event handler that is fired after searching for files.
      @precon  None
      @postcon Event handler that is fired after searching for files.
      @return  a TSearchEndNotifier
    **)
    Property OnSearchEnd: TSearchEndNotifier Read FSearchEndNotifier
      Write SetSearchEndNotifier;
    (**
      This is an event handler that is fired before comparing folder of file information.
      @precon  None.
      @postcon Event handler that is fired before comparing folder of file information.
      @return  a TCompareStartNotifier
    **)
    Property OnCompareStart: TCompareStartNotifier Read FCompareStartNotifier
      Write FCompareStartNotifier;
    (**
      This is an event handler that is fired for each file comparison.
      @precon  None.
      @postcon Event handler that is fired for each file comparison.
      @return  a TCompareNotifier
    **)
    Property OnCompare: TCompareNotifier Read FCompareNotifier Write FCompareNotifier;
    (**
      This is an event handler that is fired after comparing folder of file information.
      @precon  None.
      @postcon Event handler that is fired after comparing folder of file information.
      @return  a TCompareEndNotifier
    **)
    Property OnCompareEnd: TCompareEndNotifier Read FCompareEndNotifier
      Write FCompareEndNotifier;
  End;

  (** A class to represent a file to be processed (deleted or copied). **)
  TProcessItem = Class
  Strict Private
    FLPath      : String;
    FRPath      : String;
    FLeftFile   : TFileRecord;
    FRightFile  : TFileRecord;
    FFileOp     : TFileOp;
    FSyncOptions: TSyncOptions;
  Strict Protected
  Public
    Constructor Create(strLPath, strRPath: String; LeftFile, RightFile: TFileRecord;
      FileOp: TFileOp; SyncOptions: TSyncOptions);
    (**
      This property returns the Left Path for the process item.
      @precon  None.
      @postcon Returns the Left Path for the process item.
      @return  a String
    **)
    Property LPath: String Read FLPath;
    (**
      This property returns the Right Path for the process item.
      @precon  None.
      @postcon Returns the Right Path for the process item.
      @return  a String
    **)
    Property RPath: String Read FRPath;
    (**
      This property returns the Left File Record for the process item (note could be nil).
      @precon  None.
      @postcon Returns the Left File Record for the process item (note could be nil).
      @return  a TFileRecord
    **)
    Property LeftFile: TFileRecord Read FLeftFile;
    (**
      This property returns the Right File Record for the process item (note could be nil).
      @precon  None.
      @postcon Returns the Right File Record for the process item (note could be nil).
      @return  a TFileRecord
    **)
    Property RightFile: TFileRecord Read FRightFile;
    (**
      This property returns the type of file operation to be performed on the files.
      @precon  None.
      @postcon Returns the type of file operation to be performed on the files.
      @return  a TFileOp
    **)
    Property FileOp: TFileOp Read FFileOp Write FFileOp;
    (**
      This property returns the sync options associated with the file.
      @precon  None.
      @postcon Returns the sync options associated with the file.
      @return  a TSyncOptions
    **)
    Property SyncOptions: TSyncOptions Read FSyncOptions;
  End;

  (** A class to represent a collection of TCompareFolders classes. **)
  TCompareFoldersCollection = Class
  Strict Private
    FCompareFolders             : TObjectList;
    FSearchStartNotifier        : TSearchStartNotifier;
    FSearchNotifier             : TSearchNotifier;
    FSearchEndNotifier          : TSearchEndNotifier;
    FCompareStartNotifier       : TCompareStartNotifier;
    FCompareNotifier            : TCompareNotifier;
    FCompareEndNotifier         : TCompareEndNotifier;
    FMatchListStartNotifier     : TMatchListStartNotifier;
    FMatchListNotifier          : TMatchListNotifier;
    FMatchListEndNotifier       : TMatchListEndNotifier;
    FDeleteStartNotifier        : TDeleteStartNotifier;
    FDeletingNotifier           : TDeletingNotifier;
    FDeletedNotifier            : TDeletedNotifier;
    FDeleteQueryNotifier        : TDeleteQueryNotifier;
    FDeleteReadOnlyQueryNotifier: TDeleteReadOnlyQueryNotifier;
    FDeleteEndNotifier          : TDeleteEndNotifier;
    FCopyStartNotifier          : TCopyStartNotifier;
    FCopyingNotifier            : TCopyingNotifier;
    FCopyContents               : TCopyContents;
    FCopiedNotifier             : TCopiedNotifier;
    FCopyQueryNotifier          : TCopyQueryNotifier;
    FCopyReadOnlyQueryNotifier  : TCopyReadOnlyQueryNotifier;
    FCopyEndNotifier            : TCopyEndNotifier;
    FDiffSizeStartNotifier      : TDiffSizeStartNotifier;
    FDiffSizeNotifier           : TDiffSizeNotifier;
    FDiffSizeEndNotifier        : TDiffSizeEndNotifier;
    FNothingToDoStartNotifier   : TNothingToDoStartNotifier;
    FNothingToDoNotifier        : TNothingToDoNotifier;
    FNothingToDoEndNotifier     : TNothingToDoEndNotifier;
    FProcessList                : TObjectList;
    FCopiedTotalSize            : Int64;
    FFiles                      : Integer;
    FSkipped                    : Integer;
    FErrors                     : Integer;
    FStatistics                 : TStringList;
  Strict Protected
    Function GetCount: Integer;
    Function GetCompareFolders(iIndex: Integer): TCompareFolders;
    Procedure BuildMatchLists;
    Procedure FindNextNonSame(Lst: TFileList; Var iIndex: Integer);
    Procedure DoMatchListStart;
    Procedure DoMatchList(iPosition, iMaxItems: Integer);
    Procedure DoMatchListEnd;
    Procedure InsertItem(strLPath, strRPath: String; LeftFile, RightFile: TFileRecord;
      SyncOptions: TSyncOptions);
    Function OperationStatus(LeftFile, RightFile: TFileRecord;
      SyncOptions: TSyncOptions): TFileOp;
    Function GetProcessCount: Integer;
    Function GetProcessItem(iIndex: Integer): TProcessItem;
    Procedure DoDeleteStart(iFileCount: Integer; iTotalSize: Int64);
    Procedure DoDeleting(iFile : Integer; strFileName: String);
    Procedure DoDeleted(iFile: Integer; iSize: Int64; boolSuccess: Boolean;
      strErrmsg: String);
    Procedure DoDeleteQuery(strFileName: String; Var Option: TFileAction;
      SyncOptions: TSyncOptions);
    Procedure DoDeleteReadOnlyQuery(strFileName: String; Var Option: TFileAction;
      SyncOptions: TSyncOptions);
    Procedure DoDeleteEnd(iDeleted, iSkipped, iErrors: Integer);
    Procedure DoCopyStart(iTotalCount: Integer; iTotalSize: Int64);
    Procedure DoCopying(iFile : Integer; strSource, strDest, strFileName: String);
    Procedure DoCopied(iCopiedFiles: Integer; iCopiedFileTotalSize,
      iCopiedTotalSize: Int64; boolSuccess: Boolean; strErrmsg: String);
    Procedure DoCopyQuery(strSourceFile, strDestFile: String; Var Option: TFileAction;
      SyncOptions: TSyncOptions);
    Procedure DoCopyReadOnlyQuery(strSourceFile, strDestFile: String;
      Var Option: TFileAction; SyncOptions: TSyncOptions);
    Procedure DoCopyEnd(iCopied, iSkipped, iError: Integer);
    Procedure DoDiffSizeStart(iFileCount: Integer);
    Procedure DoDiffSize(strLPath, strRPath, strFileName: String);
    Procedure DoDiffSizeEnd;
    Procedure DoNothingToDoStart(iFileCount: Integer);
    Procedure DoNothingToDo(strLPath, strRPath, strFileName: String);
    Procedure DoNothingToDoEnd;
    Procedure DeleteFiles;
    Procedure CopyFiles;
    Procedure DifferentSize;
    Procedure DoNothing;
    Function CopyFileContents(strSourceFile, strDestFile: String; Var iCopied: Integer;
      Var strErrmsg: String): Boolean;
    Function CountFileOps(FileOps: TFileOps; Var iSize: Int64): Integer;
    Procedure DeleteIndividualFile(strPath: String; F: TFileRecord; iFile: Integer;
      Var boolAll, boolROAll: Boolean; SyncOps: TSyncOptions);
    Function CopyIndividualFile(strSource, strDest: String; FName: TFileRecord;
      Var boolAll: Boolean; boolReadOnly: Boolean; Var strErrmsg: String;
      SyncOps: TSyncOptions): Boolean;
    Function CanByPassQuery(SyncOps: TSyncOptions; boolReadOnly : Boolean;
      Var Option: TFileAction): Boolean;
    (**
      This property returns an indexed CompareFolders class.
      @precon  The index but be between 0 and Count - 1.
      @postcon Returns the indexed CompareFolders class.
      @param   iIndex as       an Integer
      @return  a TCompareFolders
    **)
    Property CompareFolders[iIndex: Integer]: TCompareFolders Read GetCompareFolders;
    (**
      This property represents the number of CompareFolder classes in the
      collection.
      @precon  None.
      @postcon Returns the number of CompareFolder classes in the collection.
      @return  an Integer
    **)
    Property Count: Integer Read GetCount;
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;
    Function ProcessFolders(slFolders: TStringList; strExclusions: String): Boolean;
    Procedure ProcessFiles;
    Procedure Clear;
    Procedure BuildStats;
    (**
      This property returns the number of files to process in the internal process list.
      @precon  None.
      @postcon Returns the number of files to process in the internal process list.
      @return  an Integer
    **)
    Property ProcessCount: Integer Read GetProcessCount;
    (**
      This property returns the process information for the indexed process item.
      @precon  iIndex must be between 0 and ProcessCount - 1.
      @postcon Returns the process information for the indexed process item.
      @param   iIndex as an Integer
      @return  a TProcessItem
    **)
    Property Process[iIndex: Integer]: TProcessItem Read GetProcessItem;
    (**
      This is an event handler for the start of the searching for files process.
      @precon  None.
      @postcon Fired before the start of a search for files in a root folder.
      @return  a TSearchStartNotifier
    **)
    Property OnSearchStart: TSearchStartNotifier Read FSearchStartNotifier
      Write FSearchStartNotifier;
    (**
      This is an event handler that is fired fro each file found.
      @precon  None.
      @postcon Event handler that is fired fro each file found.
      @return  a TSearchNotifier
    **)
    Property OnSearch: TSearchNotifier Read FSearchNotifier Write FSearchNotifier;
    (**
      This is an event handler that is fired after searching for files.
      @precon  None
      @postcon Event handler that is fired after searching for files.
      @return  a TSearchEndNotifier
    **)
    Property OnSearchEnd: TSearchEndNotifier Read FSearchEndNotifier
      Write FSearchEndNotifier;
    (**
      This is an event handler that is fired before comparing folder of file information.
      @precon  None.
      @postcon Event handler that is fired before comparing folder of file information.
      @return  a TCompareStartNotifier
    **)
    Property OnCompareStart: TCompareStartNotifier Read FCompareStartNotifier
      Write FCompareStartNotifier;
    (**
      This is an event handler that is fired for each file comparison.
      @precon  None.
      @postcon Event handler that is fired for each file comparison.
      @return  a TCompareNotifier
    **)
    Property OnCompare: TCompareNotifier Read FCompareNotifier Write FCompareNotifier;
    (**
      This is an event handler that is fired after comparing folder of file information.
      @precon  None.
      @postcon Event handler that is fired after comparing folder of file information.
      @return  a TCompareEndNotifier
    **)
    Property OnCompareEnd: TCompareEndNotifier Read FCompareEndNotifier
      Write FCompareEndNotifier;
    (**
      This is an event handler that is fired at the start of the process of matching files
      between a pair of folders.
      @precon  None.
      @postcon Event handler that is fired at the start of the process of matching files
               between a pair of folders.
      @return  a TMatchListStartNotifier
    **)
    Property OnMatchListStart: TMatchListStartNotifier Read FMatchListStartNotifier
      Write FMatchListStartNotifier;
    (**
      This is an event handler that is fired for each file that is matched during the
      matching process.
      @precon  None.
      @postcon Event handler that is fired for each file that is matched during the
               matching process.
      @return  a TMatchListNotifier
    **)
    Property OnMatchList: TMatchListNotifier Read FMatchListNotifier
      Write FMatchListNotifier;
    (**
      This is an event handler that is fired at the end of the process of matching files
      between a pair of folders.
      @precon  None.
      @postcon Event handler that is fired at the end of the process of matching files
               between a pair of folders.
      @return  a TMatchListEndNotifier
    **)
    Property OnMatchListEnd: TMatchListEndNotifier Read FMatchListEndNotifier
      Write FMatchListEndNotifier;
    (**
      This is an event handler that is fired at the start of the deletion of files
      process.
      @precon  None.
      @postcon Event handler that is fired at the start of the deletion of files
               process.
      @return  a TDeleteStartNotifier
    **)
    Property OnDeleteStart: TDeleteStartNotifier Read FDeleteStartNotifier
      Write FDeleteStartNotifier;
    (**
      This is an event handler that is fired before deleting a file.
      @precon  None.
      @postcon Event handler that is fired before deleting a file.
      @return  a TDeletingNotifier
    **)
    Property OnDeleting: TDeletingNotifier Read FDeletingNotifier Write FDeletingNotifier;
    (**
      This is an event handler that is fired after a file is deleted.
      @precon  None.
      @postcon Event handler that is fired after a file is deleted.
      @return  a TDeletedNotifier
    **)
    Property OnDeleted: TDeletedNotifier Read FDeletedNotifier Write FDeletedNotifier;
    (**
      This event handler is fired for each file that is deleted to confirm that it should
      be deleted.
      @precon  None.
      @postcon Event handler is fired for each file that is deleted to confirm that it should
               be deleted.
      @return  a TDeleteQueryNotifier
    **)
    Property OnDeleteQuery: TDeleteQueryNotifier Read FDeleteQueryNotifier
      Write FDeleteQueryNotifier;
    (**
      This event handler is fired for all readonyl file instead of OnDeleteQuery to check
      whether the file should be deleted.
      @precon  None.
      @postcon Event handler is fired for all readonyl file instead of OnDeleteQuery to
               check whether the file should be deleted.
      @return  a TDeleteReadOnlyQueryNotifier
    **)
    Property OnDeleteReadOnlyQuery: TDeleteReadOnlyQueryNotifier
      Read FDeleteReadOnlyQueryNotifier Write FDeleteReadOnlyQueryNotifier;
    (**
      This is an event handler that is fired at the end of the deletion of files
      process.
      @precon  None.
      @postcon Event handler that is fired at the end of the deletion of files
               process.
      @return  a TDeleteEndNotifier
    **)
    Property OnDeleteEnd: TDeleteEndNotifier Read FDeleteEndNotifier
      Write FDeleteEndNotifier;
    (**
      This is an event handler that is fired before starting to copy files.
      @precon  None.
      @postcon Event handler that is fired before starting to copy files.
      @return  a TCopyStartNotifier
    **)
    Property OnCopyStart: TCopyStartNotifier Read FCopyStartNotifier
      Write FCopyStartNotifier;
    (**
      This is an event handler that is fired before copying dividual files.
      @precon  None.
      @postcon Event handler that is fired before copying dividual files.
      @return  a TCopyingNotifier
    **)
    Property OnCopying: TCopyingNotifier Read FCopyingNotifier Write FCopyingNotifier;
    (**
      This is an event handler that is fired during the copying of individual files that
      allow feedback on the percentage of the file copied.
      @precon  None.
      @postcon Event handler that is fired during the copying of individual files that
               allow feedback on the percentage of the file copied.
      @return  a TCopyContents
    **)
    Property OnCopyContents: TCopyContents Read FCopyContents Write FCopyContents;
    (**
      This is an event handler that is fired after completion of the copying of individual
      files.
      @precon  None.
      @postcon Event handler that is fired after completion of the copying of individual
               files.
      @return  a TCopiedNotifier
    **)
    Property OnCopied: TCopiedNotifier Read FCopiedNotifier Write FCopiedNotifier;
    (**
      This is an event handler that is fired before the copying of an indicidual file
      over an existing file.
      @precon  None.
      @postcon Event handler that is fired before the copying of an indicidual file
               over an existing file.
      @return  a TCopyQueryNotifier
    **)
    Property OnCopyQuery: TCopyQueryNotifier Read FCopyQueryNotifier
      Write FCopyQueryNotifier;
    (**
      This is an event that is fired before copying over a read only destination file.
      @precon  None.
      @postcon Event that is fired before copying over a read only destination file.
      @return  a TCopyReadOnlyQueryNotifier
    **)
    Property OnCopyReadOnlyQuery: TCopyReadOnlyQueryNotifier
      Read FCopyReadOnlyQueryNotifier Write FCopyReadOnlyQueryNotifier;
    (**
      This is an event handler that is fired after finishing copying files.
      @precon  None.
      @postcon Event handler that is fired after finishing copying files.
      @return  a TCopyEndNotifier
    **)
    Property OnCopyEnd: TCopyEndNotifier Read FCopyEndNotifier Write FCopyEndNotifier;
    (**
      This is an event handler that is fired at the start of the different size output.
      @precon  None.
      @postcon Event handler that is fired at the start of the different size output.
      @return  a TDiffSizeStartNotifier
    **)
    Property OnDiffSizeStart: TDiffSizeStartNotifier Read FDiffSizeStartNotifier
      Write FDiffSizeStartNotifier;
    (**
      This is an event handler that is fired for each file in the different size output.
      @precon  None.
      @postcon Event handler that is fired for each file in the different size output.
      @return  a TDiffSizeNotifier
    **)
    Property OnDiffSize: TDiffSizeNotifier Read FDiffSizeNotifier Write FDiffSizeNotifier;
    (**
      This is an event handler that is fired at the end of the different size output.
      @precon  None.
      @postcon Event handler that is fired at the end of the different size output.
      @return  a TDiffSizeEndNotifier
    **)
    Property OnDiffSizeEnd: TDiffSizeEndNotifier Read FDiffSizeEndNotifier
      Write FDiffSizeEndNotifier;
    (**
      This is an event handler that is fired at the start of the nothing to do output.
      @precon  None.
      @postcon Event handler that is fired at the start of the nothing to do output.
      @return  a TNothingToDoStartNotifier
    **)
    Property OnNothingToDoStart: TNothingToDoStartNotifier Read FNothingToDoStartNotifier
      Write FNothingToDoStartNotifier;
    (**
      This is an event handler that is fired for each file in the nothing to do output.
      @precon  None.
      @postcon Event handler that is fired for each file in the nothing to do output.
      @return  a TNothingToDoNotifier
    **)
    Property OnNothingToDo: TNothingToDoNotifier Read FNothingToDoNotifier
      Write FNothingToDoNotifier;
    (**
      This is an event handler that is fired at the end of the nothing to do output.
      @precon  None.
      @postcon Event handler that is fired at the end of the nothing to do output.
      @return  a TNothingToDoEndNotifier
    **)
    Property OnNothingToDoEnd: TNothingToDoEndNotifier Read FNothingToDoEndNotifier
      Write FNothingToDoEndNotifier;
    (**
      This property provides access to the list of statistics stored in a string list.
      @precon  None.
      @postcon Get the string list of statistics.
      @return  a TStringList
    **)
    Property Statistics : TStringList Read FStatistics;
  End;

  (** A custom exception for exception raised by FldrSync. **)
  EFldrSyncException = Class(Exception);

Function Expand(strFileName: String): String;

Const
  (** A constant array to define string representation of the TFileAction enumerates. **)
  strFileOptions: Array [Low(TFileAction) .. High(TFileAction)] Of String = ('No', 'Yes',
    'All', 'Cancel', 'Unknown');

Implementation

Uses
  FileCtrl,
  DGHLibrary,
  Math;

(**

  This is a call back procedure for the Win32 API CopyFileEx() procedure.

  @precon  None.
  @postcon Calls the on copy contents event handler and feeds back progress.

  @param   TotalFileSize          as an Int64
  @param   TotalBytesTransferred  as an Int64
  @param   StreamSize             as an Int64
  @param   StreamBytesTransferred as an Int64
  @param   dwStreamNumber         as a DWORD
  @param   dwCallbackReason       as a DWORD
  @param   hSourceFile            as a THandle
  @param   hDestinationFile       as a THandle
  @param   CFC                    as a TCompareFoldersCollection
  @return  a DWORD

**)
Function CopyCallback(TotalFileSize, TotalBytesTransferred, StreamSize,
  StreamBytesTransferred: Int64; dwStreamNumber, dwCallbackReason: DWORD;
  hSourceFile, hDestinationFile: THandle; CFC: TCompareFoldersCollection): DWORD; Stdcall;

Const
  PROCESS_CONTINUE = 0;

Begin
  Result := PROCESS_CONTINUE;
  If dwCallbackReason = CALLBACK_CHUNK_FINISHED Then
    If Assigned(CFC) Then
      If Assigned(CFC.OnCopyContents) Then
        CFC.OnCopyContents(TotalBytesTransferred, TotalFileSize);
End;

(**

  This method expands the passed files name to a fuilly qualitied filename and adds an
  appropriate Unicode modifier to workaround long file names with the Unicode versions of
  the windows API.

  @precon  None.
  @postcon Returns an expanded fully qualitied Unicode path.

  @param   strFileName as a String
  @return  a String

**)
Function Expand(strFileName: String): String;

Begin
  Result := ExpandFileName(strFileName);
  If Length(strFileName) >= 2 Then
    Begin
      If Result[2] = ':' Then
        Result := '\\?\' + Result
      Else If Copy(Result, 1, 2) = '\\' Then
        Result := '\\?\UNC\' + Copy(Result, 3, Length(Result) - 2);
    End;
End;

{ TFileRecord }

(**

  This is the constructor method for the TFileRecord class.

  @precon  None.
  @postcon Initialises the file record with a filename, size, date and time and a status.

  @param   strName     as a String
  @param   iSize       as an Int64
  @param   iAttributes as an Integer
  @param   dtDateTime  as an Integer
  @param   Status      as a TStatus

**)
Constructor TFileRecord.Create(strName: String; iSize: Int64; iAttributes: Integer;
  dtDateTime: Integer; Status: TStatus);

Begin
  FFileName   := strName;
  FSize       := iSize;
  FAttributes := iAttributes;
  FDateTime   := dtDateTime;
  FStatus     := Status;
End;

(**

  This is a seter method for the Status property.

  @precon  None.
  @postcon Sets the status of the file record.

  @param   Status     as a TStatus

**)
Procedure TFileRecord.SetStatus(Status: TStatus);

Begin
  If FStatus <> Status Then
    FStatus := Status;
End;

{ TFileList }

(**

  This is the constructor method for the TFileList class.

  @precon  None.
  @postcon Creates the class instance.

**)
Constructor TFileList.Create;

Begin
  Inherited Create;
  FTotalSize   := 0;
  FFiles       := TObjectList.Create(True);
  FFileFilters := TStringList.Create;
End;

(**

  This method deletes the indexed item from the file list.

  @precon  None.
  @postcon The indexed item is deleted.

  @param   iIndex as an Integer

**)
Procedure TFileList.Delete(iIndex: Integer);

Begin
  FFiles.Delete(iIndex);
End;

(**

  This is the destructor method for the TFileList class.

  @precon  None.
  @postcon Deallocated the  array file records and destroys the class instance.


**)
Destructor TFileList.Destroy;

Begin
  FFileFilters.Free;
  FFiles.Free;
  FExclusions.Free;
  Inherited;
End;

(**

  This method updates the progress event method hooked with the current progress
  .

  @precon  None.
  @postcon Fires the progress event if the event handler is hooked.

  @param   strFolder  as a String
  @param   iCount     as an Integer
  @param   strFile    as a String

**)
Procedure TFileList.DoSearch(strFolder, strFile: String; iCount: Integer);

Begin
  If Assigned(FSearchNotifier) Then
    FSearchNotifier(strFolder, strFile, iCount);
End;

(**

  This method fires the SearchEnd event if the event has a handler installed.

  @precon  None.
  @postcon Fires the SearchEnd event if the event has a handler installed.

  @param   iFileCount as an Integer
  @param   iTotalSize as an Int64

**)
Procedure TFileList.DoSearchEnd(iFileCount: Integer; iTotalSize: Int64);

Begin
  If Assigned(FSearchEndNotifier) Then
    FSearchEndNotifier(iFileCount, iTotalSize);
End;

(**

  This method fires the SearchStart event if the event has a handler installed.

  @precon  None.
  @postcon Fires the SearchStart event if the event has a handler installed.

  @param   strFolder as a String

**)
Procedure TFileList.DoSearchStart(strFolder: String);

Begin
  If Assigned(FSearchStartNotifier) Then
    FSearchStartNotifier(strFolder);
End;

(**

  This method searches the give folder for files matching the file filters and excluding
  any files that match one of the exclusions somewhere in their path and adds them to the
  folder collection.

  @precon  None.
  @postcon Searches the give folder for files matching the file filters and excluding any
           files that match one of the exclusions somewhere in their path and adds them
           to the folder collection.

  @param   strFolderPath as a String
  @param   strFileFilter as a String
  @param   strExclusions as a String
  @param   SyncOps       as a TsyncOptions

**)
Procedure TFileList.SearchFolder(strFolderPath, strFileFilter, strExclusions: String;
  SyncOps : TsyncOptions);

Var
  iFilter: Integer;

Begin
  FFolderPath := strFolderPath;
  FSyncOptions := SyncOps;
  If strFileFilter = '' Then
    FFileFilters.Add('*.*')
  Else
    Begin
      For iFilter := 1 To CharCount(';', strFileFilter) + 1 Do
        FFileFilters.Add(GetField(strFileFilter, ';', iFilter));
    End;
  FExclusions      := TStringList.Create;
  FExclusions.Text := LowerCase(strExclusions);
  Assert(Length(FFolderPath) > 0, 'FolderPath is NULL');
  DoSearchStart(FFolderPath);
  RecurseFolder(FFolderPath);
  DoSearchEnd(FFiles.Count, FTotalSize);
End;

(**

  This method uses a binary search to find the given filename in the collection
  and return the index of the filename in the collection.

  @precon  None.
  @postcon Returns the index of the filename if the file is found else returns
           -1.

  @param   strFileName as a String
  @return  an Integer

**)
Function TFileList.Find(strFileName: String): Integer;

Var
  iFirst, iLast, iMid: Integer;

Begin
  Result := -1;
  iFirst := 0;
  iLast  := Count - 1;
  While iLast >= iFirst Do
    Begin
      iMid := (iFirst + iLast) Div 2;
      If AnsiCompareFileName(strFileName, Files[iMid].FileName) = 0 Then
        Begin
          Result := iMid;
          Exit;
        End;
      If AnsiCompareFileName(strFileName, Files[iMid].FileName) < 0 Then
        iLast := iMid - 1
      Else
        iFirst := iMid + 1;
    End;
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of files in the list.

  @return  an Integer

**)
Function TFileList.GetCount: Integer;

Begin
  Result := FFiles.Count;
End;

(**

  This is a getter method for the FileRecord property.

  @precon  iIndex must be a valid index between 0 and Count -1.
  @postcon Returns the indexed TFileRecord.

  @param   iIndex as an Integer
  @return  a TFileRecord

**)
Function TFileList.GetFiles(iIndex: Integer): TFileRecord;

Begin
  Result := FFiles.Items[iIndex] As TFileRecord;
End;

(**

  This method recurses the passed folder for file name and adds them to the file
  collection.

  @precon  None.
  @postcon Recurses the passed folder for file name and adds them to the file
           collection.

  @param   strFolderPath as a String

**)
Procedure TFileList.RecurseFolder(strFolderPath: String);

Var
  rec                   : TSearchRec;
  iRes                  : Integer;
  strFileName, strFCName: String;
  iFirst, iLast, iMid   : Integer;
  iFilter               : Integer;

Begin
  If Not InExclusions(strFolderPath) Then
    Begin
      // Search for files
      For iFilter := 0 To FFileFilters.Count - 1 Do
        Begin
          iRes := FindFirst(Expand(strFolderPath + FFileFilters[iFilter]),
            faAnyFile, rec);
          Try
            While iRes = 0 Do
              Begin
                If rec.Attr And faDirectory = 0 Then
                  Begin
                    strFileName := strFolderPath + rec.Name;
                    If Not InExclusions(strFileName) Then
                      Begin
                        strFCName := Copy(strFileName, Length(FFolderPath) + 1,
                          Length(strFileName));
                        iFirst := 0;
                        iLast  := Count - 1;
                        While iLast >= iFirst Do
                          Begin
                            iMid := (iFirst + iLast) Div 2;
                            If AnsiCompareFileName(strFCName,
                              Files[iMid].FileName) = 0 Then
                              Break;
                            If AnsiCompareFileName(strFCName,
                              Files[iMid].FileName) < 0 Then
                              iLast := iMid - 1
                            Else
                              iFirst := iMid + 1;
                          End;
                        {$WARN SYMBOL_DEPRECATED OFF}
                        FFiles.Insert(iFirst, TFileRecord.Create(strFCName, rec.Size,
                            rec.Attr, rec.Time, stNewer));
                        {$WARN SYMBOL_DEPRECATED ON}
                        Inc(FTotalSize, rec.Size);
                        DoSearch(FFolderPath, Files[iFirst].FileName, Count);
                      End;
                  End;
                iRes := FindNext(rec);
              End;
          Finally
            SysUtils.FindClose(rec);
          End;
        End;
      // Search directories
      If Not (soNoRecursion In FSyncOptions) Then
        Begin
          iRes := FindFirst(Expand(strFolderPath + '*.*'), faAnyFile, rec);
          Try
            While iRes = 0 Do
              Begin
                If rec.Attr And faDirectory <> 0 Then
                  If (rec.Name <> '.') And (rec.Name <> '..') Then
                    RecurseFolder(strFolderPath + rec.Name + '\');
                iRes := FindNext(rec);
              End;
          Finally
            SysUtils.FindClose(rec);
          End;
        End;
    End;
End;

(**

  This method check that a filename does not contain any of the list of
  exclusion strings.

  @precon  None.
  @postcon Returns true if the filename contains one of the exclusion strings
           else returns false.

  @param   strFileName as a String
  @return  a Boolean

**)
Function TFileList.InExclusions(strFileName: String): Boolean;

Var
  i: Integer;

Begin
  strFileName := LowerCase(strFileName);
  Result      := False;
  For i       := 0 To FExclusions.Count - 1 Do
    Result    := Result Or (Pos(FExclusions[i], strFileName) > 0);
End;

{ TCompareFolders }

(**

  This function checks the different between the file dates accounting for day light
  saving (i.e. exactly 1 hour on files of the same size).

  @precon  None.
  @postcon Checks the different between the file dates accounting for day light saving (i
           .e. exactly 1 hour on files of the same size). Returns true if different.

  @param   iTimeDifference   as an Integer
  @param   iSizeDifference   as an Integer
  @param   Check             as a TCheckDifference
  @return  a Boolean

**)
Function TCompareFolders.CheckDifference(iTimeDifference: Integer;
  iSizeDifference: Integer; Check: TCheckDifference): Boolean;

Const
  Direction: Array [cdNewer .. cdOlder] Of Integer = (1, -1);

Begin
  Result := (Direction[Check] * iTimeDifference > 0);
  Result := Result And Not((iTimeDifference = Direction[Check] * 2048)
    { And (iSizeDifference = 0)});
  Result := Result And Not((iTimeDifference = Direction[Check] * 18432)
    { And (iSizeDifference = 0)});
End;

(**

  This method removes from the left and right folders all items that are marked as the
  same. This is done to reduce the memory usage of the application.

  @precon  None.
  @postcon Left and Right items that are marked the same are deleted.

**)
Procedure TCompareFolders.ClearUnchangedItems;

Var
  i : Integer;

Begin
  For i := FLeftFldr.Count - 1 DownTo 0 Do
    If FLeftFldr.Files[i].Status = stSame Then
      FLeftFldr.Delete(i);
  For i := FRightFldr.Count - 1 DownTo 0 Do
    If FRightFldr.Files[i].Status = stSame Then
      FRightFldr.Delete(i);
End;

(**

  This method compares the two folders of information and marks the
  corresponding files as either Old, New or the Same.

  @precon  None.
  @postcon The two lists of files are correctly marked up based on matching
           filenames and comparing date and time stamps.

**)
Procedure TCompareFolders.CompareFolders;

Var
  iLeft, iRight  : Integer;
  iTimeDifference: Integer;
  iSizeDifference: Integer;

Begin
  DoCompareStart(LeftFldr.FolderPath, RightFldr.FolderPath);
  For iLeft := 0 To LeftFldr.Count - 1 Do
    Begin
      DoCompare(LeftFldr.FolderPath, RightFldr.FolderPath, LeftFldr[iLeft].FileName,
        iLeft + 1, LeftFldr.Count);
      iRight := RightFldr.Find(LeftFldr[iLeft].FileName);
      If iRight > -1 Then
        Begin
          iTimeDifference := LeftFldr[iLeft].DateTime - RightFldr[iRight].DateTime;
          iSizeDifference := LeftFldr[iLeft].Size - RightFldr[iRight].Size;
          If CheckDifference(iTimeDifference, iSizeDifference, cdNewer) Then
            Begin
              LeftFldr[iLeft].Status   := stNewer;
              RightFldr[iRight].Status := stOlder;
            End
          Else If CheckDifference(iTimeDifference, iSizeDifference, cdOlder) Then
            Begin
              LeftFldr[iLeft].Status   := stOlder;
              RightFldr[iRight].Status := stNewer;
            End
          Else If iSizeDifference = 0 Then
            Begin
              LeftFldr[iLeft].Status   := stSame;
              RightFldr[iRight].Status := stSame;
            End
          Else
            Begin
              LeftFldr[iLeft].Status   := stDiffSize;
              RightFldr[iRight].Status := stDiffSize;
            End;
        End;
    End;
  DoCompareEnd;
End;

(**

  This is the constructor method for the TCompareFolders class.

  @precon  None.
  @postcon Creates an instance of TFileList for ther left and right folder.

**)
Constructor TCompareFolders.Create;

Begin
  FLeftFldr  := TFileList.Create;
  FRightFldr := TFileList.Create;
End;

(**

  This is the destructor method for the TCompareFolders class.

  @precon  None.
  @postcon Destroys the left and right folder lists.

**)
Destructor TCompareFolders.Destroy;

Begin
  FLeftFldr.Free;
  FRightFldr.Free;
  Inherited;
End;

(**

  This method fires the CompareNotifier event if the event has a handler installed.

  @precon  None.
  @postcon Fires the CompareNotifier event if the event has a handler installed.

  @param   strLeftFldr  as a String
  @param   strRightFldr as a String
  @param   strFileName  as a String
  @param   iPosition    as an Integer
  @param   iMaxItems    as an Integer

**)
Procedure TCompareFolders.DoCompare(strLeftFldr, strRightFldr, strFileName: String;
  iPosition, iMaxItems: Integer);

Begin
  If Assigned(FCompareNotifier) Then
    FCompareNotifier(strLeftFldr, strRightFldr, strFileName, iPosition, iMaxItems);
End;

(**

  This method fires the CompareEnd event if the event has a handler installed.

  @precon  None.
  @postcon Fires the CompareEnd event if the event has a handler installed.

**)
Procedure TCompareFolders.DoCompareEnd;

Begin
  If Assigned(FCompareEndNotifier) Then
    FCompareEndNotifier;
End;

(**

  This method fires the CompareStart event if the event has a handler installed.

  @precon  None.
  @postcon Fires the CompareStart event if the event has a handler installed.

  @param   strLeftFldr  as a String
  @param   strRightFldr as a String

**)
Procedure TCompareFolders.DoCompareStart(strLeftFldr, strRightFldr: String);

Begin
  If Assigned(FCompareStartNotifier) Then
    FCompareStartNotifier(strLeftFldr, strRightFldr);
End;

(**

  This method starts the process of searching the give folders for files.

  @precon  None.
  @postcon Starts the process of searching the give folders for files.

  @param   strLeftFldr     as a String
  @param   strRightFldr    as a String
  @param   strPatterns     as a String
  @param   strExclusions   as a String
  @param   iSection        as an Integer
  @param   SyncOps         as a TSyncOptions

**)
Procedure TCompareFolders.SearchFolders(strLeftFldr, strRightFldr, strPatterns,
  strExclusions: String; iSection: Integer; SyncOps: TSyncOptions);

Begin
  FSyncOptions     := SyncOps;
  FFldrSyncOptions := FldrSyncOptions;
  If Not SysUtils.DirectoryExists(strLeftFldr) Then
    Exit;
  If Not SysUtils.DirectoryExists(strRightFldr) Then
    Exit;
  FLeftFldr.SearchFolder(strLeftFldr, strPatterns, strExclusions, SyncOps);
  FRightFldr.SearchFolder(strRightFldr, strPatterns, strExclusions, SyncOps);
  CompareFolders;
End;

(**

  A setter method for the SearchEndNotifier property.

  @precon  None.
  @postcon Hooks the Search End Notifier for the LEft and Right Folder classes.

  @param   SearchEndNotifier as a TSearchEndNotifier

**)
Procedure TCompareFolders.SetSearchEndNotifier(SearchEndNotifier: TSearchEndNotifier);

Begin
  FSearchEndNotifier     := SearchEndNotifier;
  FLeftFldr.OnSearchEnd  := FSearchEndNotifier;
  FRightFldr.OnSearchEnd := FSearchEndNotifier;
End;

(**

  A setter method for the SearchNotifier property.

  @precon  None.
  @postcon Hooks the Search Notifier for the LEft and Right Folder classes.

  @param   SearchNotifier as a TSearchNotifier

**)
Procedure TCompareFolders.SetSearchNotifier(SearchNotifier: TSearchNotifier);

Begin
  FSearchNotifier     := SearchNotifier;
  FLeftFldr.OnSearch  := FSearchNotifier;
  FRightFldr.OnSearch := FSearchNotifier;
End;

(**

  A setter method for the SearchStartNotifier property.

  @precon  None.
  @postcon Hooks the Search Start Notifier for the LEft and Right Folder classes.

  @param   SearchStartNotifier as a TSearchStartNotifier

**)
Procedure TCompareFolders.SetSearchStartNotifier(SearchStartNotifier
    : TSearchStartNotifier);

Begin
  FSearchStartNotifier     := SearchStartNotifier;
  FLeftFldr.OnSearchStart  := FSearchStartNotifier;
  FRightFldr.OnSearchStart := FSearchStartNotifier;
End;

{ TProcessItem }

(**

  This is a constructor method for the TProcessItem class.

  @precon  None.
  @postcon Initialises the class with the passed values.

  @param   strLPath        as a String
  @param   strRPath        as a String
  @param   LeftFile        as a TFileRecord
  @param   RightFile       as a TFileRecord
  @param   FileOp          as a TFileOp
  @param   SyncOptions     as a TSyncOptions

**)
Constructor TProcessItem.Create(strLPath, strRPath: String;
  LeftFile, RightFile: TFileRecord; FileOp: TFileOp; SyncOptions: TSyncOptions);

Begin
  FLPath       := strLPath;
  FRPath       := strRPath;
  FLeftFile    := LeftFile;
  FRightFile   := RightFile;
  FFileOp      := FileOp;
  FSyncOptions := SyncOptions;
End;

{ TCompareFolderCollection }

(**

  This method builds a list (ProcessItems) based on the information in the Left and Right
  folders and the SyncOptions producing a list of file actions.

  @precon  None.
  @postcon Builds a list (ProcessItems) based on the information in the Left and Right
           folders and the SyncOptions producing a list of file actions.

**)
Procedure TCompareFoldersCollection.BuildMatchLists;

Var
  iCollection  : Integer;
  iRight, iLeft: Integer;
  CP           : TCompareFolders;

Begin
  DoMatchListStart;
  For iCollection := 0 To Count - 1 Do
    Begin
      CP := CompareFolders[iCollection];
      Assert((CP.LeftFldr <> Nil) And (CP.RightFldr <> Nil), 'A folder is NULL!');
      iRight := -1;
      iLeft  := -1;
      FindNextNonSame(CP.LeftFldr, iLeft);
      FindNextNonSame(CP.RightFldr, iRight);
      While (iLeft < CP.LeftFldr.Count) Or (iRight < CP.RightFldr.Count) Do
        Begin
          DoMatchList(Max(iLeft, iRight) + 1, Max(CP.LeftFldr.Count, CP.RightFldr.Count));
          If (CP.LeftFldr.Count > iLeft) And (CP.RightFldr.Count > iRight) Then
            Begin
              If AnsiCompareFileName(CP.LeftFldr[iLeft].FileName,
                CP.RightFldr[iRight].FileName) = 0 Then
                Begin
                  InsertItem(CP.LeftFldr.FolderPath, CP.RightFldr.FolderPath,
                    CP.LeftFldr[iLeft], CP.RightFldr[iRight], CP.SyncOptions);
                  FindNextNonSame(CP.LeftFldr, iLeft);
                  FindNextNonSame(CP.RightFldr, iRight);
                End
              Else If AnsiCompareFileName(CP.LeftFldr[iLeft].FileName,
                CP.RightFldr[iRight].FileName) < 0 Then
                Begin
                  InsertItem(CP.LeftFldr.FolderPath, CP.RightFldr.FolderPath,
                    CP.LeftFldr[iLeft], Nil, CP.SyncOptions);
                  FindNextNonSame(CP.LeftFldr, iLeft);
                End
              Else If AnsiCompareFileName(CP.LeftFldr[iLeft].FileName,
                CP.RightFldr[iRight].FileName) > 0 Then
                Begin
                  InsertItem(CP.LeftFldr.FolderPath, CP.RightFldr.FolderPath, Nil,
                    CP.RightFldr[iRight], CP.SyncOptions);
                  FindNextNonSame(CP.RightFldr, iRight);
                End;
            End
          Else If (CP.LeftFldr.Count > iLeft) Then
            Begin
              InsertItem(CP.LeftFldr.FolderPath, CP.RightFldr.FolderPath,
                CP.LeftFldr[iLeft], Nil, CP.SyncOptions);
              FindNextNonSame(CP.LeftFldr, iLeft);
            End
          Else If (CP.RightFldr.Count > iRight) Then
            Begin
              InsertItem(CP.LeftFldr.FolderPath, CP.RightFldr.FolderPath, Nil,
                CP.RightFldr[iRight], CP.SyncOptions);
              FindNextNonSame(CP.RightFldr, iRight);
            End;
        End;
      CP.ClearUnchangedItems;
    End;
  DoMatchListEnd;
End;

(**

  This metohd builds a set of statistics for the comparisons made.

  @precon  None.
  @postcon Adds statistics to the string list.

**)
Procedure TCompareFoldersCollection.BuildStats;

Const
  strTemplate = '%s: %1.0n files in %1.1n kbytes';

Var
  iCount : Integer;
  iSize  : Int64;
  iLeft, iRight : Int64;
  i : Integer;

Begin
  FStatistics.Clear;
  iSize := 0;
  iCount := CountFileOps([foDelete], iSize);
  FStatistics.Add(Format(strTemplate, ['Delete', Int(iCount), Int(iSize) / 1024.0]));
  iSize := 0;
  iCount := CountFileOps([foLeftToRight, foRightToLeft], iSize);
  FStatistics.Add(Format(strTemplate, ['Copy', Int(iCount), Int(iSize) / 1024.0]));
  iSize := 0;
  iCount := CountFileOps([foSizeDiff], iSize);
  FStatistics.Add(Format(strTemplate, ['Size Diff', Int(iCount), Int(iSize) / 1024.0]));
  iSize := 0;
  iCount := CountFileOps([foNothing], iSize);
  FStatistics.Add(Format(strTemplate, ['Do Nothing', Int(iCount), Int(iSize) / 1024.0]));
  iLeft := 0;
  iRight := 0;
  For i := 0 To FCompareFolders.Count - 1 Do
    Begin
      Inc(iLeft, CompareFolders[i].LeftFldr.TotalSize);
      Inc(iRight, CompareFolders[i].RightFldr.TotalSize);
    End;
  FStatistics.Add(Format('Total Left: %1.1n kbytes', [Int(iLeft) / 1024.0]));
  FStatistics.Add(Format('Total Right: %1.1n kbytes', [Int(iRight) / 1024.0]));
  FStatistics.Add(Format('Difference (L-R): %1.1n kbytes', [Int(iLEft - iRight) / 1024.0]));
End;

(**

  This method determines whether the querying call back procedures can be by passed based
  on the options provided in the SyncOps parameter. If these are either soConfirmYes or so
  ConfirmNo then the function returns TRUE and the file action options is set
  appropriately.

  @precon  None.
  @postcon Returns whether the querying process snhould be bypassed and modifies the file
           action appropriately.

  @param   SyncOps      as a TSyncOptions
  @param   boolReadOnly as a Boolean
  @param   Option       as a TFileAction as a reference
  @return  a Boolean

**)
Function TCompareFoldersCollection.CanByPassQuery(SyncOps: TSyncOptions;
  boolReadOnly : Boolean; Var Option: TFileAction): Boolean;

Var
  Op: TFileAction;

Begin
  Op := Option;
  Result := SyncOps * [soConfirmNo, soConfirmYes] <> [];
  If Result Then
    If soConfirmNo In SyncOps Then
      Option := faNo
    Else If soConfirmYes In SyncOps Then
      Option := faYes;
  If boolReadOnly Then
    Begin
      Result := Result And (soOverwriteReadOnlyFiles In SyncOps);
      If Not Result Then
        Option := Op;
    End;
End;

(**

  This method clears the collection of folder information and process information.

  @precon  None.
  @postcon The collection is made empty for receiving new information.

**)
Procedure TCompareFoldersCollection.Clear;

Begin
  FCompareFolders.Clear;
  FProcessList.Clear;
End;

(**

  This method copies the contents of a files from the source to the destination.

  @precon  None.
  @postcon Coppies the sourcefile to the destination file and increments the numver of
           files copied.

  @param   strSourceFile as a String
  @param   strDestFile   as a String
  @param   iCopied       as an Integer as a reference
  @param   strErrMsg     as a String as a reference
  @return  a Boolean

**)
Function TCompareFoldersCollection.CopyFileContents(strSourceFile, strDestFile: String;
  Var iCopied: Integer; Var strErrmsg: String): Boolean;

  (**

    This function returns the size of the given file if found else returns 0.

    @precon  None.
    @postcon Returns the size of the given file if found else returns 0.

    @param   strFileName as a String
    @return  an Int64

  **)
  Function GetFileSize(strFileName : String): Int64;

  Var
    recSearch: TSearchRec;
    iResult: Integer;
  
  Begin
    Result := 0;
    iResult := FindFirst(strFileName, faAnyFile, recSearch);
    Try
      If iResult = 0 Then
        Result := recSearch.Size;
    Finally
      SysUtils.FindClose(recSearch);
    End;
  End;

Var
  iSrcSize : Int64;
  iDestSize : Int64;
  iFreeBytesAvailableToCaller: Int64;
  iTotalNumberOfBytes: Int64;
  iTotalNumberOfFreeBytes: Int64;

Begin
  Result    := False;
  strErrmsg := '';
  iSrcSize := GetFileSize(strSourceFile);
  iDestSize := GetFileSize(strDestFile);
  GetDiskFreeSpaceEx(PChar(ExtractFilePath(strDestFile)), iFreeBytesAvailableToCaller,
    iTotalNumberOfBytes, @iTotalNumberOfFreeBytes);
  If (iSrcSize - iDestSize) < iTotalNumberOfFreeBytes Then
    Begin
      If Not CopyFileEx(PChar(Expand(strSourceFile)), PChar(Expand(strDestFile)),
        @CopyCallback, Self, Nil, 0) Then
        strErrMsg := Format('Could not copy file "%s" (%s)',
          [strSourceFile, SysErrorMessage(GetLastError)])
      Else
        Begin
          Result := True;
          Inc(iCopied);
        End;
    End Else
      strErrMsg := 'There is not enough disk space at the destination to copy this file.';
End;

(**

  This method copies the files found in the process list.

  @precon  None.
  @postcon Copies the files found in the process list.

**)
Procedure TCompareFoldersCollection.CopyFiles;

Var
  iCount            : Integer;
  boolAll, boolROAll: Boolean;
  iFile             : Integer;
  i                 : Integer;
  P                 : TProcessItem;
  strSource, strDest: String;
  FName             : TFileRecord;
  iAttr             : Cardinal;
  strErrmsg         : String;
  boolSuccess       : Boolean;

Begin
  FCopiedTotalSize := 0;
  FFiles   := 0;
  FSkipped := 0;
  iCount   := CountFileOps([foLeftToRight, foRightToLeft], FCopiedTotalSize);
  DoCopyStart(iCount, FCopiedTotalSize);
  Try
    If iCount = 0 Then
      Exit;
    boolAll   := False;
    boolROAll := False;
    iFile     := 0;
    FCopiedTotalSize     := 0;
    FErrors   := 0;
    For i     := 0 To ProcessCount - 1 Do
      Begin
        P := Process[i];
        If P.FileOp In [foLeftToRight, foRightToLeft] Then
          Begin
            If P.FileOp = foLeftToRight Then
              Begin
                strSource := P.LPath;
                strDest   := P.RPath;
                FName     := P.LeftFile;
              End
            Else
              Begin
                strSource := P.RPath;
                strDest   := P.LPath;
                FName     := P.RightFile;
              End;
            DoCopying(iFile + 1, strSource, strDest, FName.FileName);
            If Not SysUtils.DirectoryExists
              (ExtractFilePath(strDest + FName.FileName)) Then
              If Not SysUtils.ForceDirectories
                (ExtractFilePath(Expand(strDest + FName.FileName))) Then
                Raise EFldrSyncException.CreateFmt('Can not create folder "%s".',
                  [ExtractFilePath(strDest + FName.FileName)]);
            If Not FileExists(strDest + FName.FileName) Then
              boolSuccess := CopyFileContents(strSource + FName.FileName,
                strDest + FName.FileName, FFiles, strErrmsg)
            Else
              Begin
                iAttr := GetFileAttributes(PChar(Expand(strDest + FName.FileName)));
                If iAttr And FILE_ATTRIBUTE_READONLY = 0 Then
                  boolSuccess := CopyIndividualFile(strSource, strDest, FName, boolAll,
                    False, strErrmsg, P.SyncOptions)
                Else
                  boolSuccess := CopyIndividualFile(strSource, strDest, FName, boolROAll,
                    True, strErrmsg, P.SyncOptions);
              End;
            Inc(FCopiedTotalSize, FName.Size);
            If Not boolSuccess Then
              Inc(FErrors)
            Else
              Inc(iFile);
            DoCopied(iFile, FName.Size, FCopiedTotalSize, boolSuccess, strErrmsg);
          End;
      End;
  Finally
    DoCopyEnd(FFiles, FSkipped, FErrors);
  End;
End;

(**

  This method copies an individual file after querying the user for a response.

  @precon  FName must be a valid instance.
  @postcon Copies an individual file after querying the user for a response.

  @param   strSource    as a String
  @param   strDest      as a String
  @param   FName        as a TFileRecord
  @param   boolAll      as a Boolean as a reference
  @param   boolReadOnly as a Boolean
  @param   strErrMsg    as a String as a reference
  @param   SyncOps      as a TSyncOptions
  @return  a Boolean

**)
Function TCompareFoldersCollection.CopyIndividualFile(strSource, strDest: String;
  FName: TFileRecord; Var boolAll: Boolean; boolReadOnly: Boolean; Var strErrmsg: String;
  SyncOps: TSyncOptions): Boolean;

Var
  FA   : TFileAction;
  iAttr: Cardinal;

Begin
  Result := False;
  If Not boolAll Then
    FA := faUnknown
  Else
    FA := faAll;
  If Not boolReadOnly Then
    DoCopyQuery(strSource + FName.FileName, strDest + FName.FileName, FA, SyncOps)
  Else
    DoCopyReadOnlyQuery(strSource + FName.FileName, strDest + FName.FileName, FA,
      SyncOps);
  If FA = faAll Then
    Begin
      boolAll := True;
      FA      := faYes;
    End;
  Case FA Of
    faNo:
      Begin
        Inc(FSkipped);
        Result := True;
      End;
    faYes:
      Begin
        iAttr := GetFileAttributes(PChar(Expand(strDest + FName.FileName)));
        If iAttr And faReadOnly > 0 Then
          Begin
            iAttr := iAttr Xor FILE_ATTRIBUTE_READONLY;
            SetFileAttributes(PChar(Expand(strDest + FName.FileName)), iAttr);
          End;
        Result := CopyFileContents(strSource + FName.FileName, strDest + FName.FileName,
          FFiles, strErrmsg);
      End;
    faCancel:
      Abort;
  End;
End;

(**

  This method counts the number of files to operated on depending on the file operation
  and the files available. Also returns the size if the files.

  @precon  None.
  @postcon Counts the number of files to operated on depending on the file operation
           and the files available. Also returns the size if the files.

  @param   FileOps as a TFileOps
  @param   iSize   as an Int64 as a reference
  @return  an Integer

**)
Function TCompareFoldersCollection.CountFileOps(FileOps: TFileOps;
  Var iSize: Int64): Integer;

Var
  i: Integer;

Begin
  Result := 0;
  For i  := 0 To ProcessCount - 1 Do
    If Process[i].FileOp In FileOps Then
      Begin
        If foDelete In FileOps Then
          Begin // Count both Deleted Files
            If Process[i].LeftFile <> Nil Then
              Begin
                Inc(iSize, Process[i].LeftFile.Size);
                Inc(Result);
              End;
            If Process[i].RightFile <> Nil Then
              Begin
                Inc(iSize, Process[i].RightFile.Size);
                Inc(Result);
              End;
          End
        Else
          Begin // Count only one Copy file
            Inc(Result);
            If Process[i].LeftFile <> Nil Then
              Inc(iSize, Process[i].LeftFile.Size)
            Else
              Inc(iSize, Process[i].RightFile.Size);
          End;
      End;
End;

(**

  This is the constructor method for the TCompareFolderCollection class.

  @precon  None.
  @postcon Creates a collection for the folder lists.

**)
Constructor TCompareFoldersCollection.Create();

Begin
  FCompareFolders := TObjectList.Create(True);
  FProcessList    := TObjectList.Create(True);
  FStatistics     := TStringList.Create;
End;

(**

  This method deletes the files the are marked for deletion in the process items list.

  @precon  None.
  @postcon Deletes the files the are marked for deletion in the process items list.

**)
Procedure TCompareFoldersCollection.DeleteFiles;

Var
  boolAll  : Boolean;
  boolROAll: Boolean;
  iCount   : Integer;
  i        : Integer;
  P        : TProcessItem;
  iFile    : Integer;

Begin
  FCopiedTotalSize    := 0;
  FFiles   := 0;
  FSkipped := 0;
  FErrors  := 0;
  iCount   := CountFileOps([foDelete], FCopiedTotalSize);
  DoDeleteStart(iCount, FCopiedTotalSize);
  Try
    FCopiedTotalSize := 0;
    If iCount = 0 Then
      Exit;
    boolAll   := False;
    boolROAll := False;
    iFile     := 1;
    For i     := 0 To ProcessCount - 1 Do
      Begin
        P := Process[i];
        If P.FileOp = foDelete Then
          Begin
            If P.LeftFile <> Nil Then
              DeleteIndividualFile(P.LPath, P.LeftFile, iFile, boolAll, boolROAll,
                P.SyncOptions);
            If P.RightFile <> Nil Then
              DeleteIndividualFile(P.RPath, P.RightFile, iFile, boolAll, boolROAll,
                P.SyncOptions);
            Inc(iFile);
          End;
      End;
  Finally
    DoDeleteEnd(FFiles, FSkipped, FErrors);
  End;
End;

(**

  This method deletes the given files from the drive structure.

  @precon  None.
  @postcon Deletes the given files from the drive structure.

  @param   strPath     as a String
  @param   F           as a TFileRecord
  @param   iFile       as an Integer
  @param   boolAll     as a Boolean as a reference
  @param   boolROAll   as a Boolean as a reference
  @param   SyncOps     as a TSyncOptions

**)
Procedure TCompareFoldersCollection.DeleteIndividualFile(strPath: String; F: TFileRecord;
  iFile: Integer; Var boolAll, boolROAll: Boolean; SyncOps: TSyncOptions);

Const
  strMsg = 'Deletion of file "%s" failed with message "%s".';

Var
  FA        : TFileAction;
  iAttr     : Cardinal;
  boolResult: Boolean;
  strErrmsg : String;

Begin
  boolResult := False;
  DoDeleting(iFile, strPath + F.FileName);
  If Not boolAll Then
    FA := faUnknown
  Else
    FA := faAll;
  If F.Attributes And faReadOnly > 0 Then
    Begin
      If Not boolROAll Then
        FA := faUnknown;
      DoDeleteReadOnlyQuery(strPath + F.FileName, FA, SyncOps);
      If FA = faAll Then
        boolROAll := True;
    End
  Else
    DoDeleteQuery(strPath + F.FileName, FA, SyncOps);
  If FA = faAll Then
    Begin
      boolAll := True;
      FA      := faYes;
    End;
  Case FA Of
    faCancel:
      Abort;
    faYes:
      Begin
        If F.Attributes And faReadOnly > 0 Then
          Begin
            iAttr := GetFileAttributes(PChar(Expand(strPath + F.FileName)));
            iAttr := iAttr Xor FILE_ATTRIBUTE_READONLY;
            SetFileAttributes(PChar(Expand(strPath + F.FileName)), iAttr);
          End;
        Inc(FCopiedTotalSize, F.Size);
        If Not DeleteFile(PChar(Expand(strPath + F.FileName))) Then
          strErrmsg := Format(strMsg, [strPath + F.FileName,
              SysErrorMessage(GetLastError)])
        Else
          Begin
            boolResult := True;
            Inc(FFiles);
          End;
      End;
    faNo:
      Begin
        boolResult := True;
        Inc(FSkipped);
      End;
  End;
  DoDeleted(iFile, FCopiedTotalSize, boolResult, strErrmsg);
End;

(**

  This is the destructor method for the TCompareFolderCollection class.

  @precon  None.
  @postcon Destroy the class and all the CompareFolders classes it owns.

**)
Destructor TCompareFoldersCollection.Destroy;

Begin
  FStatistics.Free;
  FProcessList.Free;
  FCompareFolders.Free;
  Inherited;
End;

(**

  This method cycles throught all the files that are identified as being the same except
  for file size.

  @precon  None.
  @postcon Cycles throught all the files that are identified as being the same except
           for file size.

**)
Procedure TCompareFoldersCollection.DifferentSize;

Var
  iCount: Integer;
  iSize : Int64;
  i     : Integer;
  P     : TProcessItem;

Begin
  iSize  := 0;
  iCount := CountFileOps([foSizeDiff], iSize);
  DoDiffSizeStart(iCount);
  For i := 0 To ProcessCount - 1 Do
    Begin
      P := Process[i];
      If P.FileOp In [foSizeDiff] Then
        DoDiffSize(P.LPath, P.RPath, P.LeftFile.FileName);
    End;
  DoDiffSizeEnd();
End;

(**

  This method fires the Copied event if the event has a handler installed.

  @precon  None.
  @postcon Fires the Copied event if the event has a handler installed.

  @param   iCopiedFiles         as an Integer
  @param   iCopiedFileTotalSize as an Int64
  @param   iCopiedTotalSize     as an Int64
  @param   boolSuccess          as a Boolean
  @param   strErrmsg            as a String

**)
Procedure TCompareFoldersCollection.DoCopied(iCopiedFiles: Integer; iCopiedFileTotalSize,
  iCopiedTotalSize: Int64; boolSuccess: Boolean; strErrmsg: String);

Begin
  If Assigned(FCopiedNotifier) Then
    FCopiedNotifier(iCopiedFiles, iCopiedFileTotalSize, iCopiedTotalSize, boolSuccess,
      strErrmsg);
End;

(**

  This method fires the CopyEnd event if the event has a handler installed.

  @precon  None.
  @postcon Fires the CopyEnd event if the event has a handler installed.

  @param   iCopied  as an Integer
  @param   iSkipped as an Integer
  @param   iError   as an Integer

**)
Procedure TCompareFoldersCollection.DoCopyEnd(iCopied, iSkipped, iError: Integer);

Begin
  If Assigned(FCopyEndNotifier) Then
    FCopyEndNotifier(iCopied, iSkipped, iError);
End;

(**

  This event fires any hook DoCopying events but issuing events for each files giving the
  source, destination and filename.

  @precon  None.
  @postcon Fires any hook DoCopying events but issuing events for each files giving the
           source, destination and filename.

  @param   iFile       as an Integer
  @param   strSource   as a String
  @param   strDest     as a String
  @param   strFileName as a String

**)
Procedure TCompareFoldersCollection.DoCopying(iFile : Integer; strSource, strDest,
  strFileName: String);

Begin
  If Assigned(FCopyingNotifier) Then
    FCopyingNotifier(iFile, strSource, strDest, strFileName);
End;

(**

  This method fires the CopyQuery event if the event has a handler installed.

  @precon  None.
  @postcon Fires the CopyQuery event if the event has a handler installed.

  @param   strSourceFile as a String
  @param   strDestFile   as a String
  @param   Option        as a TFileAction as a reference
  @param   SyncOptions   as a TSyncOptions

**)
Procedure TCompareFoldersCollection.DoCopyQuery(strSourceFile, strDestFile: String;
  Var Option: TFileAction; SyncOptions: TSyncOptions);

Begin
  If Not CanByPassQuery(SyncOptions, False, Option) Then
    If Assigned(FCopyQueryNotifier) Then
      FCopyQueryNotifier(strSourceFile, strDestFile, Option);
End;

(**

  This method fires the CopyReadOnlyQuery event if the event has a handler installed.

  @precon  None.
  @postcon Fires the CopyReadOnlyQuery event if the event has a handler installed.

  @param   strSourceFile as a String
  @param   strDestFile   as a String
  @param   Option        as a TFileAction as a reference
  @param   SyncOptions   as a TSyncOptions

**)
Procedure TCompareFoldersCollection.DoCopyReadOnlyQuery(strSourceFile,
  strDestFile: String; Var Option: TFileAction; SyncOptions: TSyncOptions);

Begin
  If Not CanByPassQuery(SyncOptions, True, Option) Then
    If Assigned(FCopyReadOnlyQueryNotifier) Then
      FCopyReadOnlyQueryNotifier(strSourceFile, strDestFile, Option);
End;

(**

  This method fires the CopyStart event if the event has a handler installed.

  @precon  None.
  @postcon Fires the CopyStart event if the event has a handler installed.

  @param   iTotalCount as an Integer
  @param   iTotalSize  as an Int64

**)
Procedure TCompareFoldersCollection.DoCopyStart(iTotalCount: Integer; iTotalSize: Int64);

Begin
  If Assigned(FCopyStartNotifier) Then
    FCopyStartNotifier(iTotalCount, iTotalSize);
End;

(**

  This method fires the Deleting event if the event has a handler installed.

  @precon  None.
  @postcon Fires the Deleting event if the event has a handler installed.

  @param   iFile       as an Integer
  @param   strFileName as a String

**)
Procedure TCompareFoldersCollection.DoDeleting(iFile : Integer; strFileName: String);

Begin
  If Assigned(FDeletingNotifier) Then
    FDeletingNotifier(iFile, strFileName);
End;

(**

  This method fires an event for each file with a different size only.

  @precon  None.
  @postcon Fires an event for each file with a different size only.

  @param   strLPath    as a String
  @param   strRPath    as a String
  @param   strFileName as a String

**)
Procedure TCompareFoldersCollection.DoDiffSize(strLPath, strRPath, strFileName: String);

Begin
  If Assigned(FDiffSizeNotifier) Then
    FDiffSizeNotifier(strLPath, strRPath, strFileName);
End;

(**

  This method fires an event handler at the end of processing the files with onyl a
  different size.

  @precon  None.
  @postcon Fires an event handler at the end of processing the files with onyl a
           different size.

**)
Procedure TCompareFoldersCollection.DoDiffSizeEnd;

Begin
  If Assigned(FDiffSizeEndNotifier) Then
    FDiffSizeEndNotifier();
End;

(**

  This method fires an event before the processing of files with only sizr difference.

  @precon  None.
  @postcon Fires an event before the processing of files with only sizr difference.

  @param   iFileCount as an Integer

**)
Procedure TCompareFoldersCollection.DoDiffSizeStart(iFileCount: Integer);

Begin
  If Assigned(FDiffSizeStartNotifier) Then
    FDiffSizeStartNotifier(iFileCount);
End;

(**

  This method fires the Deleted event if the event has a handler installed.

  @precon  None.
  @postcon Fires the Deleted event if the event has a handler installed.

  @param   iFile       as an Integer
  @param   iSize       as an Int64
  @param   boolSuccess as a Boolean
  @param   strErrMsg   as a String

**)
Procedure TCompareFoldersCollection.DoDeleted(iFile: Integer; iSize: Int64;
  boolSuccess: Boolean; strErrmsg: String);

Begin
  If Assigned(FDeletedNotifier) Then
    FDeletedNotifier(iFile, iSize, boolSuccess, strErrmsg);
End;

(**

  This method fires the DeletedEnd event if the event has a handler installed.

  @precon  None.
  @postcon Fires the DeletedEnd event if the event has a handler installed.

  @param   iDeleted as an Integer
  @param   iSkipped as an Integer
  @param   iErrors  as an Integer

**)
Procedure TCompareFoldersCollection.DoDeleteEnd(iDeleted, iSkipped, iErrors: Integer);

Begin
  If Assigned(FDeleteEndNotifier) Then
    FDeleteEndNotifier(iDeleted, iSkipped, iErrors);
End;

(**

  This method fires the DeleteQuery event if the event has a handler installed.

  @precon  None.
  @postcon Fires the DeleteQuery event if the event has a handler installed.

  @param   strFileName as a String
  @param   Option      as a TFileAction as a reference
  @param   SyncOptions as a TSyncOptions

**)
Procedure TCompareFoldersCollection.DoDeleteQuery(strFileName: String;
  Var Option: TFileAction; SyncOptions: TSyncOptions);

Begin
  If Not CanByPassQuery(SyncOptions, False, Option) Then
    If Assigned(FDeleteQueryNotifier) Then
      FDeleteQueryNotifier(strFileName, Option);
End;

(**

  This method fires the DeleteReadOnlyQuery event if the event has a handler installed.

  @precon  None.
  @postcon Fires the DeleteReadOnlyQuery event if the event has a handler installed.

  @param   strFileName as a String
  @param   Option      as a TFileAction as a reference
  @param   SyncOptions as a TSyncOptions

**)
Procedure TCompareFoldersCollection.DoDeleteReadOnlyQuery(strFileName: String;
  Var Option: TFileAction; SyncOptions: TSyncOptions);

Begin
  If Not CanByPassQuery(SyncOptions, True, Option) Then
    If Assigned(FDeleteReadOnlyQueryNotifier) Then
      FDeleteReadOnlyQueryNotifier(strFileName, Option);
End;

(**

  This method fires the DeleteStart event if the event has a handler installed.

  @precon  None.
  @postcon Fires the DeleteStart event if the event has a handler installed.

  @param   iFileCount as an Integer
  @param   iTotalSize as an Int64

**)
Procedure TCompareFoldersCollection.DoDeleteStart(iFileCount: Integer; iTotalSize: Int64);

Begin
  If Assigned(FDeleteStartNotifier) Then
    FDeleteStartNotifier(iFileCount, iTotalSize);
End;

(**

  This method fires the MatchList event if the event has a handler installed.

  @precon  None.
  @postcon Fires the MatchList event if the event has a handler installed.

  @param   iPosition as an Integer
  @param   iMaxItems as an Integer

**)
Procedure TCompareFoldersCollection.DoMatchList(iPosition, iMaxItems: Integer);

Begin
  If Assigned(FMatchListNotifier) Then
    FMatchListNotifier(iPosition, iMaxItems);
End;

(**

  This method fires the MatchListEnd event if the event has a handler installed.

  @precon  None,
  @postcon Fires the MatchListEnd event if the event has a handler installed.

**)
Procedure TCompareFoldersCollection.DoMatchListEnd;

Begin
  If Assigned(FMatchListEndNotifier) Then
    FMatchListEndNotifier;
End;

(**

  This method fires the MatchListStart event if the event has a handler installed.

  @precon  None,
  @postcon Fires the MatchListStart event if the event has a handler installed.

**)
Procedure TCompareFoldersCollection.DoMatchListStart;

Begin
  If Assigned(FMatchListStartNotifier) Then
    FMatchListStartNotifier;
End;

(**

  This method processes the files which are labelled as nothing to do.

  @precon  None.
  @postcon Processes the files which are labelled as nothing to do.

**)
Procedure TCompareFoldersCollection.DoNothing;

Var
  iSize : Int64;
  iCount: Integer;
  i     : Integer;
  P     : TProcessItem;
  strFileName : String;

Begin
  iSize  := 0;
  iCount := CountFileOps([foNothing], iSize);
  DoNothingToDoStart(iCount);
  For i := 0 To ProcessCount - 1 Do
    Begin
      P := Process[i];
      If P.FileOp In [foNothing] Then
        Begin
          If P.LeftFile <> Nil   Then
            strFileName := P.LeftFile.FileName
          Else
            strFileName := P.RightFile.FileName;
          DoNothingToDo(P.LPath, P.RPath, strFileName);
        End;
    End;
  DoNothingToDoEnd();
End;

(**

  This methid fires an event for each file with nothing to do.

  @precon  None.
  @postcon Fires an event for each file with nothing to do.

  @param   strLPath    as a String
  @param   strRPath    as a String
  @param   strFileName as a String

**)
Procedure TCompareFoldersCollection.DoNothingToDo(strLPath, strRPath,
  strFileName: String);

Begin
  If Assigned(FNothingToDoNotifier) Then
    FNothingToDoNotifier(strLPath, strRPath, strFileName);
End;

(**

  This method fires an event after the processing of files with nothing to do.

  @precon  None.
  @postcon Fires an event after the processing of files with nothing to do.

**)
Procedure TCompareFoldersCollection.DoNothingToDoEnd;

Begin
  If Assigned(FNothingToDoEndNotifier) Then
    FNothingToDoEndNotifier();
End;

(**

  This method fires an event before the processing of files with nothing to do.

  @precon  None.
  @postcon Fires an event before the processing of files with nothing to do.

  @param   iFileCount as an Integer

**)
Procedure TCompareFoldersCollection.DoNothingToDoStart(iFileCount: Integer);

Begin
  If Assigned(FNothingToDoStartNotifier) Then
    FNothingToDoStartNotifier(iFileCount);
End;

(**

  This method finds the next item in the file list that has not got the status
  of stSame and moved the iIndex value to the new position else return Count.

  @precon  None.
  @postcon Finds the next item in the file list that has not got the status
           of stSame and moved the iIndex value to the new position else return
           Count.

  @param   Lst    as a TFileList
  @param   iIndex as an Integer as a reference

**)
Procedure TCompareFoldersCollection.FindNextNonSame(Lst: TFileList; Var iIndex: Integer);

Var
  i: Integer;

Begin
  Inc(iIndex);
  For i := iIndex To Lst.Count Do
    If i < Lst.Count Then
      Begin
        If Lst[i].Status <> stSame Then
          Begin
            iIndex := i;
            Break;
          End;
      End
    Else
      iIndex := Lst.Count;
End;

(**

  This is a getter method for the CompareFolders property.

  @precon  The index value must be between 0 and Count - 1 to be valid.
  @postcon Returns the indexed object cast as a TCompareFolders class.

  @param   iIndex as an Integer
  @return  a TCompareFolders

**)
Function TCompareFoldersCollection.GetCompareFolders(iIndex: Integer): TCompareFolders;

Begin
  Result := FCompareFolders[iIndex] As TCompareFolders;
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of CompareFolders classes within the collection.

  @return  an Integer

**)
Function TCompareFoldersCollection.GetCount: Integer;

Begin
  Result := FCompareFolders.Count;
End;

(**

  A getter method for the ProcessCount property.

  @precon  None.
  @postcon Returns the number of items in the process list.

  @return  an Integer

**)
Function TCompareFoldersCollection.GetProcessCount: Integer;

Begin
  Result := FProcessList.Count;
End;

(**

  A getter method for the ProcessItem property.

  @precon  iIndex must be between 0 an ProcessCount - 1.
  @postcon Returns the indexed process item.

  @param   iIndex as an Integer
  @return  a TProcessItem

**)
Function TCompareFoldersCollection.GetProcessItem(iIndex: Integer): TProcessItem;

Begin
  Result := FProcessList[iIndex] As TProcessItem;
End;

(**

  This method inserts a process item into the process item list.

  @precon  None.
  @postcon Inserts a process item into the process item list.

  @param   strLPath        as a String
  @param   strRPath        as a String
  @param   LeftFile        as a TFileRecord
  @param   RightFile       as a TFileRecord
  @param   SyncOptions     as a TSyncOptions

**)
Procedure TCompareFoldersCollection.InsertItem(strLPath, strRPath: String;
  LeftFile, RightFile: TFileRecord; SyncOptions: TSyncOptions);

Var
  FileOp: TFileOp;

Begin
  FileOp := OperationStatus(LeftFile, RightFile, SyncOptions);
  FProcessList.Add(TProcessItem.Create(strLPath, strRPath, LeftFile, RightFile, FileOp,
      SyncOptions));
End;

(**

  This method updates the operational status of the files based on their date and time and
  also whether they are reasd only or not.

  @precon  None.
  @postcon Updates the operational status of the files based on their date and time and
           also whether they are reasd only or not.

  @param   LeftFile        as a TFileRecord
  @param   RightFile       as a TFileRecord
  @param   SyncOptions     as a TSyncOptions
  @return  a TFileOp

**)
Function TCompareFoldersCollection.OperationStatus(LeftFile, RightFile: TFileRecord;
  SyncOptions: TSyncOptions): TFileOp;

Var
  boolROLeft : Boolean;
  boolRORight: Boolean;

Begin
  Result := foNothing;
  If RightFile <> Nil Then
    Case RightFile.Status Of
      stNewer:
        Begin
          Result := foRightToLeft;
          If (soPrimaryLeft In SyncOptions) And (LeftFile = Nil) Then
            Result := foDelete;
        End;
      stOlder:
        Result := foLeftToRight;
      stSame:
        Result := foNothing;
      stDiffSize:
        Result := foNothing;
    End
  Else
    Case LeftFile.Status Of
      stNewer:
        Begin
          Result := foLeftToRight;
          If (soPrimaryRight In SyncOptions) And (RightFile = Nil) Then
            Result := foDelete;
        End;
      stOlder:
        Result := foRightToLeft;
      stSame:
        Result := foNothing;
      stDiffSize:
        Result := foNothing;
    End;
  boolROLeft  := False;
  boolRORight := False;
  If LeftFile <> Nil Then
    boolROLeft := LeftFile.Attributes And faReadOnly > 0;
  If RightFile <> Nil Then
    boolRORight := RightFile.Attributes And faReadOnly > 0;
  If Not(soOverwriteReadOnlyFiles In SyncOptions) Then
    Case Result Of
      foLeftToRight:
        If Not boolROLeft And boolRORight Then
          Result := foNothing;
      foRightToLeft:
        If boolROLeft And Not boolRORight Then
          Result := foNothing;
    End;
  If soPrimaryLeft In SyncOptions Then
    If Result = foRightToLeft Then
      Result := foNothing;
  If soPrimaryRight In SyncOptions Then
    If Result = foLeftToRight Then
      Result := foNothing;
End;

(**

  This method processes the file for copying and deletion.

  @precon  None.
  @postcon Processes the file for copying and deletion.

**)
Procedure TCompareFoldersCollection.ProcessFiles;

Begin
  DeleteFiles;
  CopyFiles;
  DifferentSize;
  DoNothing;
End;

(**

  This method starts the recursion of the folders for each folder pairing.

  @precon  slFolders must contain pairs folders fld1=fld2 etc.
  @postcon Creates an instance of the compare folder classes for each pairing.

  @param   slFolders     as a TStringList
  @param   strExclusions as a String
  @return  a Boolean

**)
Function TCompareFoldersCollection.ProcessFolders(slFolders: TStringList;
  strExclusions: String): Boolean;

Var
  i  : Integer;
  CP : TCompareFolders;
  FOA: TFolderOptionsAdapter;

Begin
  FStatistics.Clear;
  For i := 0 To slFolders.Count - 1 Do
    Begin
      FOA.FOBjData := slFolders.Objects[i];
      If soEnabled In FOA.FSyncOptions Then
        Begin
          CP := TCompareFolders.Create;
          FCompareFolders.Add(CP);
          CP.OnSearchStart  := FSearchStartNotifier;
          CP.OnSearch       := FSearchNotifier;
          CP.OnSearchEnd    := FSearchEndNotifier;
          CP.OnCompareStart := FCompareStartNotifier;
          CP.OnCompare      := FCompareNotifier;
          CP.OnCompareEnd   := FCompareEndNotifier;
          CP.SearchFolders(ExtractFilePath(slFolders.Names[i]),
            ExtractFilePath(slFolders.ValueFromIndex[i]),
            ExtractFileName(slFolders.Names[i]), strExclusions, i, FOA.FSyncOptions);
        End;
    End;
  BuildMatchLists;
  BuildStats;
  Result := True;
End;

End.
