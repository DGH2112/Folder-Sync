(**

  This module defines classes for handling and comparing two directories of
  files.

  @Version 2.0
  @Date    21 Nov 2013
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
  TFileAction = (faNo, faYes, faYesToAll, faNoToAll, faYesToAllRO, faNoToAllRO, faCancel,
    faUnknown);
  (** A set of the above file actions. **)
  TFileActions = Set Of TFileAction;

  (** A type to define the status of a file **)
  TStatus = (stNewer, stOlder, stSame, stDiffSize, stTooLarge);

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

  (** An enumerate to define the type of update. **)
  TUpdateType = (utDelayed, utImmediate);
  (** An enumerate to define the type of error result. **)
  TDGHErrorResult = (derUnknown, derIgnoreOnce, derIgnoreAll, derStop);
  (** An enumerate to define the type of processing. **)
  TProcessSuccess = (psUnknown, psSuccessed, psFailed, psIgnoreOnce, psIgnoreAll);

  (** An event signature for the start of a search. **)
  TSearchStartNotifier = Procedure(strFolder: String) Of Object;
  (** A event signature for feeding back progress during searching for files. **)
  TSearchNotifier = Procedure(strFolder, strFileName: String; iCount: Integer;
    Update : TUpdateType) Of Object;
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
  TDeletedNotifier = Procedure(iFile: Integer; iSize: Int64;
    iSuccess : TProcessSuccess) Of Object;
  (** An event signature to prompt for the deletion of a file. **)
  TDeleteQueryNotifier = Procedure(strDeletePath: String; DeleteFile : TFileRecord;
    Var Option: TFileAction) Of Object;
  (** An event signature to prompt for the deletion of a read only file. **)
  TDeleteReadOnlyQueryNotifier = Procedure(strDeletePath: String;
    DeleteFile : TFileRecord; Var Option: TFileAction) Of Object;
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
    iCopiedTotalSize: Int64; iSuccess : TProcessSuccess) Of Object;
  (** An event signature to prompt for the overwriting of a file. **)
  TCopyQueryNotifier = Procedure(strSrcPath, strDestPath : String; SourceFile,
    DestFile: TFileRecord; Var Option: TFileAction) Of Object;
  (** An event signature to prompt for the overwriting of a read only file. **)
  TCopyReadOnlyQueryNotifier = Procedure(strSrcPath, strDestPath : String; SourceFile,
    DestFile: TFileRecord; Var Option: TFileAction) Of Object;
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
  (** An event signature for the start of the Size Limit Process. **)
  TExceedsSizeLimitStartNotifier = Procedure(iFileCount: Integer) Of Object;
  (** An event signature for each file in the Size Limit Process. **)
  TExceedsSizeLimitNotifier = Procedure(strLPath, strRPath, strFileName: String) Of Object;
  (** An event signature for the end of the Size Limit Process. **)
  TExceedsSizeLimitEndNotifier = Procedure() Of Object;
  (** An event signature for the start of the Error Message Process. **)
  TErrorMsgsStartNotifier = Procedure(iFileCount: Integer) Of Object;
  (** An event signature for each error in the Error Message Process. **)
  TErrorMsgsNotifier = Procedure(strErrorMsg: String) Of Object;
  (** An event signature for the end of the Error Message Process. **)
  TErrorMsgsEndNotifier = Procedure() Of Object;
  (** An event handler signature for the end of the deletion of empty folders. **)
  TDeleteFoldersEndNotifier = Procedure() Of Object;
  (** An event handler signature for the start of the deletion of empty folders. **)
  TDeleteFoldersStartNotifier = Procedure(iFolderCount: Integer) Of Object;
  (** An event handler signature for the deletion of each empty folder. **)
  TDeleteFoldersNotifier = Procedure(iFolder, iFolders : Integer;
    strFolder: String) Of Object;
  (** An event handler signature for adding folders to the empty folder list. **)
  TAddEmptyFolder = Procedure(strFolder : String) Of Object;
  (** An event handler signature for handling errors in copying. **)
  TCopyErrorNotifier = Procedure(strSource, strDest, strErrorMsg : String;
    iLastError : Cardinal; var iResult : TDGHErrorResult) Of Object;
  (** An event handler signature for handling errors in deleting. **)
  TDeleteErrorNotifier = Procedure(strSource, strErrorMsg : String; 
    iLastError : Cardinal; var iResult : TDGHErrorResult) Of Object;

  (** An event signature for updating the taskbar from a progress dialogue. **)
  TUpdateProgress = Procedure(iPosition, iMaxPosition: Integer) Of Object;

  (** A ercord to define the upper and lower limits of a progress section. **)
  TSectionRecord = Record
    FMin: Integer;
    FMax: Integer;
  End;

  (** A type to define whether the CheckDifference method should check for
      Older or Newer differences. **)
  TCheckDifference = (cdNewer, cdOlder);

  (** This is an enumerate for synchronisation options on a pair of folders. **)
  TSyncOption = (soEnabled, soPrimaryLeft, soPrimaryRight, soOverwriteReadOnlyFiles,
    soConfirmYes, soConfirmNo, soNoRecursion, soTempDisabled);

  (** A set of sync options. **)
  TSyncOptions = Set Of TSyncOption;

  (** A list of enumerate values for the different types of file operation that
   can be undertaken. **)
  TFileOp = (foNothing, foLeftToRight, foRightToLeft, foDelete, foSizeDiff,
    foExceedsSizeLimit);
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

  (** A class to hold the settings information for a pair of folders to be compared. **)
  TFolder = Class
  Strict Private
    FLeftFldr    : String;
    FRightFldr   : String;
    FPatterns    : String;
    FSyncOptions : TSyncOptions;
    FMaxFileSize : Int64;
  Strict Protected
  Public
    Constructor Create(strLeftFldr, strRightFldr, strPatterns : String;
      iSyncOptions : TSyncOptions; iMaxFileSize : Int64);
    Procedure Assign(AFolder : TFolder);
    (**
      This property defines the Left Folder in the comparison operation.
      @precon  None.
      @postcon Returns the Left Folder in the comparison operation.
      @return  a String
    **)
    Property LeftFldr : String Read FLeftFldr Write FLeftFldr;
    (**
      This property defines the Right Folder in the comparison operation.
      @precon  None.
      @postcon Returns the Right Folder in the comparison operation.
      @return  a String
    **)
    Property RightFldr : String Read FRightFldr Write FRightFldr;
    (**
      This property defines the file search wildcard patterns for the comparison.
      @precon  None.
      @postcon Returns the file search wildcard patterns for the comparison. 
      @return  a String
    **)
    Property Patterns : String Read FPatterns Write FPatterns;
    (**
      This property defines the synchronisation options for the comparison.
      @precon  None.
      @postcon Returns the synchronisation options for the comparison.
      @return  a TSyncOptions
    **)
    Property SyncOptions : TSyncOptions Read FSyncOptions Write FSyncOptions;
    (**
      This property defines the maxzimum size of file to be processed in the comparison.
      @precon  None.
      @postcon Returns the maxzimum size of file to be processed in the comparison. 
      @return  an Int64
    **)
    Property MaxFileSize : Int64 Read FMaxFileSize Write FMaxFileSize;
  End;

  (** A class to represent a collection of TFolder classes. **)
  TFolders = Class
  Strict Private
    FFolders : TObjectList;
  Strict Protected
    Function GetCount : Integer;
    Function GetFolder(iIndex : Integer) : TFolder;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function  Add(Folder : TFolder) : Integer;
    Procedure Assign(Folders : TFolders);
    Procedure Delete(iIndex : Integer);
    Procedure Exchange(iIndex1, iIndex2 : Integer);
    (**
      This property returns the indexed folder from the collection.
      @precon  iIndex must be between 0 and Count - 1.
      @postcon Returns the indexed folder from the collection.
      @param   iIndex as an Integer
      @return  a TFolder
    **)
    Property Folder[iIndex : Integer] : TFolder Read GetFolder;
    (**
      This property returns the number of folders in the collection.
      @precon  None.
      @postcon Returns the number of folders in the collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
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
    FTotalCount         : Int64;
    FFileFilters        : TStringList;
    FSyncOptions        : TSyncOptions;
    FMaxFileSize        : Int64;
    FAddEmptyFolder     : TAddEmptyFolder;
  Strict Protected
    Function InExclusions(strFileName: String): Boolean;
    Function GetCount: Integer;
    Function GetFiles(iIndex: Integer): TFileRecord;
    Procedure RecurseFolder(strFolderPath: String); Virtual;
    Procedure DoSearch(strFolder, strFile: String; iCount: Integer;
      Update : TUpdateType);
    Procedure DoSearchStart(strFolder: String);
    Procedure DoSearchEnd(iFileCount: Integer; iTotalSize: Int64);
    Procedure DoAddEmptyFolder(strFolder : String);
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;
    Procedure SearchFolder(strFolderPath, strFileFilter, strExclusions: String;
      SyncOps : TSyncOptions; iMaxFileSize : Int64);
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
      A property to get the total number of files in the list.
      @precon  None.
      @postcon Returns the total number of giles in the list processed.
      @return  an Int64
    **)
    Property TotalCount: Int64 Read FTotalCount;
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
      This is an event handler for the addition of folders to the empty folder list.
      @precon  None.
      @postcon Returns the the event handler for the addition of folders to the empty
               folders list.
      @return  a TAddEmptyFolder
    **)
    Property OnAddEmptyFolder : TAddEmptyFolder Read FAddEmptyFolder
      Write FAddEmptyFolder;
  End;

  (** A class to compare two lists of folder file. **)
  TCompareFolders = Class
  Strict Private
    FLeftFldr            : TFileList;
    FRightFldr           : TFileList;
    FSyncOptions         : TSyncOptions;
    FFldrSyncOptions     : TFldrSyncOptions;
    FMaxFileSize         : Int64;
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
    Constructor Create(AddEmptyFolderProc : TAddEmptyFolder); Virtual;
    Destructor Destroy; Override;
    Procedure SearchFolders(strLeftFldr, strRightFldr, strPatterns: String;
      strExclusions: String; iSection: Integer; SyncOps: TSyncOptions;
      iMaxFileSize : Int64);
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
      This property defines the maximum size of file to process.
      @precon  None.
      @postcon Returns the maximum size of file to process.
      @return  an Int64
    **)
    Property MaxFileSize : Int64 Read FMaxFileSize;
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

  (** A class to handle the disk space for a drive or mapping **)
  TDriveTotal = Class
  Strict Private
    FDrive        : String;
    FTotal        : Int64;
    FFreeAtStart  : Int64;
    FTotalDeletes : Int64;
    FTotalAdds    : Int64;
  Strict Protected
    Function GetFreeAtFinish : Int64;
  Public
    Constructor Create(strDrive : String);
    Procedure AddOption(iSize : Int64; FileOp : TFileOp);
    (**
      This property returns the name of the drive.
      @precon  None.
      @postcon Returns the name of the drive.
      @return  a String
    **)
    Property Drive : String Read FDrive;
    (**
      This property returns the total disk space for the drive.
      @precon  None.
      @postcon Returns the total disk space for the drive.
      @return  an Int64
    **)
    Property Total : Int64 Read FTotal;
    (**
      This property returns the free disk space before processing the files.
      @precon  None.
      @postcon Returns the free disk space before processing the files.
      @return  an Int64
    **)
    Property FreeAtStart : Int64 Read FFreeAtStart;
    (**
      This property returns the number of bytes to be deleted.
      @precon  None.
      @postcon Returns the number of bytes to be deleted.
      @return  an Int64
    **)
    Property TotalDeletes : Int64 Read FTotalDeletes;
    (**
      This property returns the number of bytes to be added.
      @precon  None.
      @postcon Returns the number of bytes to be added.
      @return  an Int64
    **)
    Property TotalAdds : Int64 Read FTotalAdds;
    (**
      This property returns the free disk space after processing the files.
      @precon  None.
      @postcon Returns the free disk space after processing the files.
      @return  an Int64
    **)
    Property FreeAtFinish : Int64 Read GetFreeAtFinish;
  End;

  (** A class to handle a collection of disk space totals. **)
  TDriveTotals = Class
  Strict Private
    FDrives : TObjectList;
  Strict Protected
    Function GetCount : Integer;
    Function GetDriveTotal(iIndex : Integer) : TDriveTotal;
    Function Find(strPath : String) : Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure ProcessOp(ProcessItem : TProcessItem);
    Procedure Sort;
    Procedure Clear;
    (**
      This property returns the number of items in the collection.
      @precon  None.
      @postcon Returns the number of items in the collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
    (**
      This property retuns the instance of the indexed drive total.
      @precon  iIndex must be between 0 and Count - 1.
      @postcon Retuns the instance of the indexed drive total.
      @param   iIndex as an Integer
      @return  a TDriveTotal
    **)
    Property Drive[iIndex : Integer] : TDriveTotal Read GetDriveTotal;
  End;

  (** A type to define an array of integers. **)
  TArrayOfInteger = Array Of Integer;

  (** This class defined a sorted collection of integer values. **)
  TSortedIntegerList = Class
  Strict Private
    FCount: Integer;
    FSortedIntegerList : TArrayOfInteger;
    Const
      (** A constant value in the class to define the growth capacity of the collection. **)
      iCAPACITY: Integer = 100;
  Strict Protected
    Function GetCount : Integer;                             
    Function GetValue(iIndex : Integer) : Integer;
    Function Find(iValue : Integer) : Integer;
  Public                         
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(iValue : Integer);
    Function  IsInList(iValue : Integer) : Boolean;
    Procedure Clear;
    (**
      This property returns the number of items in the collection.
      @precon  None.
      @postcon Returns the number of items in the collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
    (**
      This property returns the indexed integer.
      @precon  iIndex must be a valid inde between 1 and Count.
      @postcon Returns the indexed integer.
      @param   iIndex as an Integer
      @return  an Integer
    **)
    Property Value[iIndex : Integer] : Integer Read GetValue;
  End;

  (** An enumerate to define the File Operation Statistics that can be stored. **)
  TFileOpStat = (fosDelete, fosCopy, fosSizeDiff, fosDoNothing, fosTotalLeft,
    fosTotalRight, fosDifference);

  (** A set of the above file operation statistics. **)
  TFileOpStats = Set Of TFileOpStat;

  (** A record to describe the data to be stored for each file statistic. **)
  TFileOpStatRec = Record
    FName  : String;
    FCount : Int64;
    FSize  : Int64;
  End;

  (** A class to represent a collection of TCompareFolders classes. **)
  TCompareFoldersCollection = Class
  Strict Private
    FCompareFolders                : TObjectList;
    FSearchStartNotifier           : TSearchStartNotifier;
    FSearchNotifier                : TSearchNotifier;
    FSearchEndNotifier             : TSearchEndNotifier;
    FCompareStartNotifier          : TCompareStartNotifier;
    FCompareNotifier               : TCompareNotifier;
    FCompareEndNotifier            : TCompareEndNotifier;
    FMatchListStartNotifier        : TMatchListStartNotifier;
    FMatchListNotifier             : TMatchListNotifier;
    FMatchListEndNotifier          : TMatchListEndNotifier;
    FDeleteStartNotifier           : TDeleteStartNotifier;
    FDeletingNotifier              : TDeletingNotifier;
    FDeletedNotifier               : TDeletedNotifier;
    FDeleteQueryNotifier           : TDeleteQueryNotifier;
    FDeleteReadOnlyQueryNotifier   : TDeleteReadOnlyQueryNotifier;
    FDeleteEndNotifier             : TDeleteEndNotifier;
    FCopyStartNotifier             : TCopyStartNotifier;
    FCopyingNotifier               : TCopyingNotifier;
    FCopyContents                  : TCopyContents;
    FCopiedNotifier                : TCopiedNotifier;
    FCopyQueryNotifier             : TCopyQueryNotifier;
    FCopyReadOnlyQueryNotifier     : TCopyReadOnlyQueryNotifier;
    FCopyEndNotifier               : TCopyEndNotifier;
    FDiffSizeStartNotifier         : TDiffSizeStartNotifier;
    FDiffSizeNotifier              : TDiffSizeNotifier;
    FDiffSizeEndNotifier           : TDiffSizeEndNotifier;
    FNothingToDoStartNotifier      : TNothingToDoStartNotifier;
    FNothingToDoNotifier           : TNothingToDoNotifier;
    FNothingToDoEndNotifier        : TNothingToDoEndNotifier;
    FExceedsSizeLimitStartNotifier : TExceedsSizeLimitStartNotifier;
    FExceedsSizeLimitNotifier      : TExceedsSizeLimitNotifier;
    FExceedsSizeLimitEndNotifier   : TExceedsSizeLimitEndNotifier;
    FErrorMsgsStartNotifier        : TErrorMsgsStartNotifier;
    FErrorMsgsNotifier             : TErrorMsgsNotifier;
    FErrorMsgsEndNotifier          : TErrorMsgsEndNotifier;
    FDeleteFoldersStartNotifier    : TDeleteFoldersStartNotifier;
    FDeleteFoldersNotifier         : TDeleteFoldersNotifier;
    FDeleteFoldersEndNotifier      : TDeleteFoldersEndNotifier;
    FCopyErrorNotifier             : TCopyErrorNotifier;
    FDeleteErrorNotifier           : TDeleteErrorNotifier;
    FProcessList                   : TObjectList;
    FCopiedTotalSize               : Int64;
    FFiles                         : Integer;
    FSkipped                       : Integer;
    FErrors                        : Integer;
    FStatistics                    : Array[Low(TFileOpStat)..High(TFileOpStat)] Of TFileOpStatRec;
    FErrorMsgs                     : TStringList;
    FEmptyFolders                  : TStringList;
    FDrives                        : TDriveTotals;
    FCopyErrors                    : TSortedIntegerList;
    FDeleteErrors                  : TSortedIntegerList;
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
    Procedure DoDeleted(iFile: Integer; iSize: Int64; iSuccess: TProcessSuccess);
    Procedure DoDeleteQuery(strFilePath: String; DeleteFile : TFileRecord;
      Var Option: TFileAction;  SyncOptions: TSyncOptions);
    Procedure DoDeleteReadOnlyQuery(strFilePath: String; DeleteFile : TFileRecord;
      Var Option: TFileAction; SyncOptions: TSyncOptions);
    Procedure DoDeleteEnd(iDeleted, iSkipped, iErrors: Integer);
    Procedure DoCopyStart(iTotalCount: Integer; iTotalSize: Int64);
    Procedure DoCopying(iFile : Integer; strSource, strDest, strFileName: String);
    Procedure DoCopied(iCopiedFiles: Integer; iCopiedFileTotalSize,
      iCopiedTotalSize: Int64; iSuccess: TProcessSuccess);
    Procedure DoCopyQuery(strSourcePath, strDestPath: String; SourceFile,
      DestFile : TFileRecord; Var Option: TFileAction; SyncOptions: TSyncOptions);
    Procedure DoCopyReadOnlyQuery(strSourcePath, strDestPath: String; SourceFile,
      DestFile : TFileRecord; Var Option: TFileAction; SyncOptions: TSyncOptions);
    Procedure DoCopyEnd(iCopied, iSkipped, iError: Integer);
    Procedure DoDiffSizeStart(iFileCount: Integer);
    Procedure DoDiffSize(strLPath, strRPath, strFileName: String);
    Procedure DoDiffSizeEnd;
    Procedure DoNothingToDoStart(iFileCount: Integer);
    Procedure DoNothingToDo(strLPath, strRPath, strFileName: String);
    Procedure DoNothingToDoEnd;
    Procedure DoExceedsSizeLimitStart(iFileCount: Integer);
    Procedure DoExceedsSizeLimit(strLPath, strRPath, strFileName: String);
    Procedure DoExceedsSizeLimitEnd;
    Procedure DoErrorsMsgsStart(iErrorCount: Integer);
    Procedure DoErrorMsgs(strErrorMsg: String);
    Procedure DoErrorMsgsEnd;
    Procedure DoDeleteFoldersStart(iFolderCount: Integer);
    Procedure DoDeleteFolders(iFolder, iFolders : Integer; strFolder: String);
    Procedure DoDeleteFoldersEnd;
    Procedure DeleteFiles;
    Procedure CopyFiles;
    Procedure DifferentSize;
    Procedure DoNothing;
    Procedure DoSizeLimit;
    Function CopyFileContents(strSourceFile, strDestFile: String;
      Var iCopied: Integer): TProcessSuccess;
    Function CountFileOps(FileOps: TFileOps; Var iSize: Int64): Integer;
    Procedure DeleteIndividualFile(strPath: String; F: TFileRecord; iFile: Integer;
      Var FileActions : TFileActions; SyncOps: TSyncOptions);
    Function CopyIndividualFile(strSource, strDest: String; SourceFile,
      DestFile: TFileRecord; Var FileActions : TFileActions; 
      SyncOps: TSyncOptions): TProcessSuccess;
    Function CanByPassQuery(SyncOps: TSyncOptions; boolReadOnly : Boolean;
      Var Option: TFileAction): Boolean;
    Procedure AddToErrors(strErrorMsg : String);
    Procedure DoErrorMessages;
    Procedure DeleteEmptyFolders;
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
    Procedure AddEmptyFolder(strFolder : String);
    Procedure IncrementFolder(strFolder : String);
    Function  DecrementFolder(strFolder : String) : Boolean;
    Function  FileCount(strFolder : String) : NativeUInt;
    Procedure AddFileOpStat(FileOpStat : TFileOpStat; strName : String; iCount,
      iSize : Int64);
    Function GetStatistics(FileOpStat : TFileOpStat) : TFileOpStatRec;
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;
    Function ProcessFolders(Folders: TFolders; strExclusions: String): Boolean;
    Procedure ProcessFiles;
    Procedure Clear;
    Procedure BuildStats;    
    Procedure BuildTotals;
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
      This property returns the drive total collection for inspecting available disk
      space.
      @precon  None.
      @postcon Returns the drive total collection for inspecting available disk space.
      @return  a TDriveTotals
    **)
    Property Drives : TDriveTotals Read FDrives;
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
      This is an event handler that is fired at the start of the exceeds size limit output.
      @precon  None.
      @postcon Event handler that is fired at the start of the exceeds size limit output.
      @return  a TExceedsSizeLimitStartNotifier
    **)
    Property OnExceedsSizeLimitStart: TExceedsSizeLimitStartNotifier
      Read FExceedsSizeLimitStartNotifier Write FExceedsSizeLimitStartNotifier;
    (**
      This is an event handler that is fired for each file in the exceeds size limit output.
      @precon  None.
      @postcon Event handler that is fired for each file in the exceeds size limit output.
      @return  a TExceedsSizeLimitNotifier
    **)
    Property OnExceedsSizeLimit: TExceedsSizeLimitNotifier Read FExceedsSizeLimitNotifier
      Write FExceedsSizeLimitNotifier;
    (**
      This is an event handler that is fired at the end of the exceeeds size limit output.
      @precon  None.
      @postcon Event handler that is fired at the end of the exceeeds size limit output.
      @return  a TExceedsSizeLimitEndNotifier
    **)
    Property OnExceedsSizeLimitEnd: TExceedsSizeLimitEndNotifier
      Read FExceedsSizeLimitEndNotifier Write FExceedsSizeLimitEndNotifier;
    (**
      This is an event handler that is fired at the start of the error messages output.
      @precon  None.
      @postcon Event handler that is fired at the start of the error messages output.
      @return  a TErrorMsgsStartNotifier
    **)
    Property OnErrorMsgsStart: TErrorMsgsStartNotifier Read FErrorMsgsStartNotifier
      Write FErrorMsgsStartNotifier;
    (**
      This is an event handler that is fired for each file in the error message output.
      @precon  None.
      @postcon Event handler that is fired for each file in the error message output.
      @return  a TErrorMsgsNotifier
    **)
    Property OnErrorMsgs: TErrorMsgsNotifier Read FErrorMsgsNotifier
      Write FErrorMsgsNotifier;
    (**
      This is an event handler that is fired at the end of the error message output.
      @precon  None.
      @postcon Event handler that is fired at the end of the error message output.
      @return  a TErrorMsgsEndNotifier
    **)
    Property OnErrorMsgsEnd: TErrorMsgsEndNotifier Read FErrorMsgsEndNotifier
      Write FErrorMsgsEndNotifier;
    (**
      This property provides access to the list of statistics stored in a string list.
      @precon  None.
      @postcon Get the string list of statistics.
      @param   FileOpStat as a TFileOpStat
      @return  a TFileOpStatRec
    **)
    Property Statistics[FileOpStat : TFileOpStat] : TFileOpStatRec Read GetStatistics;
    (**
      This property defines an event handler for the start of the deletion of empty
      folders.
      @precon  None.
      @postcon Returns the event handler for the start of deletion of empty folders.
      @return  a TDeleteFoldersStartNotifier
    **)
    Property OnDeleteFoldersStart : TDeleteFoldersStartNotifier
      Read FDeleteFoldersStartNotifier Write FDeleteFoldersStartNotifier;
    (**
      This property defines an event for the deletion of empty folders.
      @precon  None.
      @postcon Returns the event handler for the deletion of each empty folders.
      @return  a TDeleteFoldersNotifier
    **)
    Property OnDeleteFolders : TDeleteFoldersNotifier
      Read FDeleteFoldersNotifier Write FDeleteFoldersNotifier;
    (**
      This property defines an event handler for the end of the deletion of empty
      folders.
      @precon  None.
      @postcon Returns the event handler for the end of deletion of empty folders.
      @return  a TDeleteFoldersEndNotifier
    **)
    Property OnDeleteFoldersEnd : TDeleteFoldersEndNotifier
      Read FDeleteFoldersEndNotifier Write FDeleteFoldersEndNotifier;
    (**
      This property defines the event handler for errors in copying files.
      @precon  None.
      @postcon Returns the event handler for handing errores in copying.
      @return  a TCopyErrorNotifier
    **)
    Property OnCopyError : TCopyErrorNotifier Read FCopyErrorNotifier
      Write FCopyErrorNotifier;
    (**
      This property defines the event handler for errors in deleting files.
      @precon  None.
      @postcon Returns the event handler for handing errores in deleting.
      @return  a TDeleteErrorNotifier
    **)
    Property OnDeleteError : TDeleteErrorNotifier Read FDeleteErrorNotifier
      Write FDeleteErrorNotifier;
  End;

  (** A custom exception for exception raised by FldrSync. **)
  EFldrSyncException = Class(Exception);

Function Expand(strFileName: String): String;

Const
  (** A constant array to define string representation of the TFileAction enumerates.  **)
  strFileOptions: Array [Low(TFileAction) .. High(TFileAction)] Of String = (
    '[N]o',
    '[Y]es',
    'Yes to [A]ll',
    'No to A[l]l',
    'Yes to [A]ll',
    'No to A[l]l',
    '[C]ancel',
    '[U]nknown'
  );
  (** A double constant to define the limit below which a warning should be displayed. **)
  dblDriveSpaceCheckPercentage : Double = 0.025; // 2.5%

Implementation

Uses
  CodeSiteLogging,
  FileCtrl,
  DGHLibrary,
  Math,
  ShlwAPI;

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

{ TFolder }

(**

  This method assigns the properties of the given folder to the current instance of a 
  folder.

  @precon  AFolder must be a valid instance.
  @postcon Assigns the properties of the given folder to the current instance of a 
           folder.

  @param   AFolder as a TFolder

**)
Procedure TFolder.Assign(AFolder: TFolder);

Begin
  FLeftFldr := AFolder.LeftFldr;
  FRightFldr := AFolder.RightFldr;
  FPatterns := AFolder.Patterns;
  FSyncOptions := AFolder.SyncOptions;
  FMaxFileSize := AFolder.MaxFileSize;
End;
(**

  This is a constructor for the TFolder class.

  @precon  None.
  @postcon Initialises an instance of a TFolder class.

  @param   strLeftFldr  as a String
  @param   strRightFldr as a String
  @param   strPatterns  as a String
  @param   iSyncOptions as a TSyncOptions
  @param   iMaxFileSize as an Int64

**)
Constructor TFolder.Create(strLeftFldr, strRightFldr, strPatterns: String;
  iSyncOptions: TSyncOptions; iMaxFileSize: Int64);

Begin
  FLeftFldr := strLeftFldr;
  FRightFldr := strRightFldr;
  FPatterns := strPatterns;
  FSyncOptions := iSyncOptions;
  FMaxFileSize := iMaxFileSize;
End;

{ TFolders }

(**

  This method adds the passed folder to the folders collection.

  @precon  None.
  @postcon Adds the passed folder to the folders collection.

  @param   Folder as a TFolder
  @return  an Integer

**)
Function TFolders.Add(Folder: TFolder) : Integer;

Begin
  Result := FFolders.Add(Folder);
End;

(**

  This method assigns the givens folders to a new collection of folders in the current
  instance.

  @precon  Folders must be a valid instance.
  @postcon Assigns the givens folders to a new collection of folders in the current
           instance.

  @param   Folders as a TFolders

**)
Procedure TFolders.Assign(Folders: TFolders);

Var
  i : Integer;
  AFolder: TFolder;
  
Begin
  FFolders.Clear;
  For i := 0 To Folders.Count - 1 Do
    Begin
      AFolder := TFolder.Create('', '', '', [], 0);
      AFolder.Assign(Folders.Folder[i]);
      Add(AFolder);
    End;
End;

(**

  This is a constructor for the TFolders class.

  @precon  None.
  @postcon Initialises an empty collection of Tfolder(s)

**)
Constructor TFolders.Create;

Begin
  FFolders := TObjectList.Create(True);
End;

(**

  This method deletes the indexed folder in the collection.

  @precon  iIndex must be a valid index between 0 and Count  - 1.
  @postcon Deletes the indexed folder in the collection.

  @param   iIndex as an Integer

**)
Procedure TFolders.Delete(iIndex: Integer);

Begin
  FFolders.Delete(iIndex);
End;

(**

  This is a destructor for the TFolders class.

  @precon  None.
  @postcon Fress the memory used by the collection of TFolder(s).

**)
Destructor TFolders.Destroy;

Begin
  FFolders.Free;
  Inherited Destroy;
End;

(**

  This method exchanges the two indexed items in the folder list.

  @precon  None.
  @postcon The two index items are exchanged.

  @param   iIndex1 as an Integer
  @param   iIndex2 as an Integer

**)
Procedure TFolders.Exchange(iIndex1, iIndex2 : Integer);

Begin
  FFolders.Exchange(iIndex1, iIndex2);
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of items in the Folders collection.

  @return  an Integer

**)
Function TFolders.GetCount: Integer;

Begin
  Result := FFolders.Count
End;

(**

  This is a getter method for the Folder property.

  @precon  iIndex must be a valid value between 0 and Count - 1.
  @postcon Returns a TFolder instance of the indexed folder.

  @param   iIndex as an Integer
  @return  a TFolder

**)
Function TFolders.GetFolder(iIndex: Integer): TFolder;

Begin
  Result := FFolders[iIndex] As TFolder;
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

  This method invokes the OnAddEmptyFolder event handler if it is assigned.

  @precon  None.
  @postcon the OnAddEmptyFolder event is invoked if assigned.

  @param   strFolder as a String

**)
Procedure TFileList.DoAddEmptyFolder(strFolder: String);

Begin
  If Assigned(FAddEmptyFolder) Then
    FAddEmptyFolder(strFolder);
End;

(**

  This method updates the progress event method hooked with the current progress .

  @precon  None.
  @postcon Fires the progress event if the event handler is hooked.

  @param   strFolder as a String
  @param   strFile   as a String
  @param   iCount    as an Integer
  @param   Update    as a TUpdateType

**)
Procedure TFileList.DoSearch(strFolder, strFile: String; iCount: Integer;
  Update : TUpdateType);

Begin
  If Assigned(FSearchNotifier) Then
    FSearchNotifier(strFolder, strFile, iCount, Update);
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
  @param   SyncOps       as a TSyncOptions
  @param   iMaxFileSize  as an Int64

**)
Procedure TFileList.SearchFolder(strFolderPath, strFileFilter, strExclusions: String;
  SyncOps : TSyncOptions; iMaxFileSize : Int64);

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
  DoSearch(FFolderPath, Copy(strFolderPath, Length(FFolderPath) + 1,
    Length(strFolderPath)), Count, utImmediate);
  DoAddEmptyFolder(strFolderPath);
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
                  If (FMaxFileSize = 0) Or (rec.Size <= FMaxFileSize) Then
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
                          Inc(FTotalCount);
                          DoSearch(FFolderPath, Files[iFirst].FileName, Count, utDelayed);
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
    Result    := Result Or (Like(FExclusions[i], strFileName));
    //Result    := Result Or (Pos(FExclusions[i], strFileName) > 0);
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
  strLeftFile: String;
  strRightFile: String;
  iCompare: Integer;

Begin
  DoCompareStart(LeftFldr.FolderPath, RightFldr.FolderPath);
  iLeft := 0;
  iRight := 0;
  While (iLeft < LeftFldr.Count) And (iRight < RightFldr.Count) Do
    Begin
      If iLeft < LeftFldr.Count Then
        strLeftFile := LeftFldr[iLeft].FileName
      Else
        strLeftFile := '';
      If iRight < RightFldr.Count Then
        strRightFile := RightFldr[iRight].FileName
      Else
        strRightFile := '';
      iCompare := AnsiCompareFileName(strLeftFile, strRightFile);
      Case iCompare Of
        -2147483648..-1:
          Begin
            If (FMaxFileSize > 0) And (LeftFldr[iLeft].Size > FMaxFileSize) Then
              LeftFldr[iLeft].Status   := stTooLarge
            Else
              LeftFldr[iLeft].Status := stNewer;
            Inc(iLeft);
          End;
        +1..+2147483647:
          Begin
            If (FMaxFileSize > 0) And (RightFldr[iRight].Size > FMaxFileSize) Then
              RightFldr[iLeft].Status   := stTooLarge
            Else
              RightFldr[iRight].Status := stNewer;
            Inc(iRight);
          End;
      Else  
        DoCompare(LeftFldr.FolderPath, RightFldr.FolderPath, LeftFldr[iLeft].FileName,
          Max(iLeft + 1, iRight + 1), Max(LeftFldr.Count, RightFldr.Count));
        iTimeDifference := LeftFldr[iLeft].DateTime - RightFldr[iRight].DateTime;
        iSizeDifference := LeftFldr[iLeft].Size - RightFldr[iRight].Size;
        If (FMaxFileSize > 0) And ((LeftFldr[iLeft].Size > FMaxFileSize) Or
          (RightFldr[iRight].Size > FMaxFileSize)) Then
          Begin
            LeftFldr[iLeft].Status   := stTooLarge;
            RightFldr[iRight].Status := stTooLarge;
          End
        Else If CheckDifference(iTimeDifference, iSizeDifference, cdNewer) Then
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
        Inc(iLeft);
        Inc(iRight);
      End; 
    End;        
  DoCompareEnd;
End;

(**

  This is the constructor method for the TCompareFolders class.

  @precon  None.
  @postcon Creates an instance of TFileList for ther left and right folder.

  @param   AddEmptyFolderProc as a TAddEmptyFolder

**)
Constructor TCompareFolders.Create(AddEmptyFolderProc : TAddEmptyFolder);

Begin
  FLeftFldr  := TFileList.Create;
  FLeftFldr.OnAddEmptyFolder := AddEmptyFolderProc;
  FRightFldr := TFileList.Create;
  FRightFldr.OnAddEmptyFolder := AddEmptyFolderProc;
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

  @param   strLeftFldr   as a String
  @param   strRightFldr  as a String
  @param   strPatterns   as a String
  @param   strExclusions as a String
  @param   iSection      as an Integer
  @param   SyncOps       as a TSyncOptions
  @param   iMaxFileSize  as an Int64

**)
Procedure TCompareFolders.SearchFolders(strLeftFldr, strRightFldr, strPatterns,
  strExclusions: String; iSection: Integer; SyncOps: TSyncOptions; iMaxFileSize : Int64);

Begin
  FSyncOptions     := SyncOps;
  FFldrSyncOptions := FldrSyncOptions;
  FMaxFileSize := iMaxFileSize;
  If Not SysUtils.DirectoryExists(strLeftFldr) Then
    Exit;
  If Not SysUtils.DirectoryExists(strRightFldr) Then
    Exit;
  FLeftFldr.SearchFolder(strLeftFldr, strPatterns, strExclusions, SyncOps, iMaxFileSize);
  FRightFldr.SearchFolder(strRightFldr, strPatterns, strExclusions, SyncOps, iMaxFileSize);
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

  This method adds an empty folder with its number of files and sub folders to the empty
  folder string list.

  @precon  None.
  @postcon An empty folder with its number of files and sub folders is added to the empty
           folder string list.

  @param   strFolder as a String

**)
Procedure TCompareFoldersCollection.AddEmptyFolder(strFolder: String);

Begin
  FEmptyFolders.AddObject(strFolder, TObject(FileCount(strFolder)));
End;

(**

  This method adds a set of file statistics to the given enumerate position.

  @precon  None.
  @postcon Adds a set of file statistics to the given enumerate position.

  @param   FileOpStat as a TFileOpStat
  @param   strName    as a String
  @param   iCount     as an Int64
  @param   iSize      as an Int64

**)
Procedure TCompareFoldersCollection.AddFileOpStat(FileOpStat: TFileOpStat;
  strName: String; iCount, iSize: Int64);

Begin
  FStatistics[FileOpStat].FName := strName;
  FStatistics[FileOpStat].FCount := iCount;
  FStatistics[FileOpStat].FSize := iSize;
End;

(**

  This method adds an error message to the error list.

  @precon  None.
  @postcon Adds an error message to the error list.

  @param   strErrorMsg as a String

**)
Procedure TCompareFoldersCollection.AddToErrors(strErrorMsg: String);

Begin
  FErrorMsgs.Add(strErrorMsg);
End;

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

Var
  iCount : Integer;
  iSize  : Int64;
  iLeftSize, iRightSize : Int64;
  iLeftCount, iRightCount : Int64;
  iDiffCount : int64;
  i : Integer;

Begin
  iSize := 0;
  iDiffCount := 0;
  iCount := CountFileOps([foDelete], iSize);
  AddFileOpStat(fosDelete, 'Delete', iCount, iSize);
  Inc(iDiffCount, iCount);
  iSize := 0;
  iCount := CountFileOps([foLeftToRight, foRightToLeft], iSize);
  AddFileOpStat(fosCopy, 'Copy', iCount, iSize);
  Inc(iDiffCount, iCount);
  iSize := 0;
  iCount := CountFileOps([foSizeDiff], iSize);
  AddFileOpStat(fosSizeDiff, 'Size Diff', iCount, iSize);
  iSize := 0;
  iCount := CountFileOps([foNothing], iSize);
  AddFileOpStat(fosDoNothing, 'Do Nothing', iCount, iSize);
  iLeftSize := 0;
  iLeftCount := 0;
  iRightSize := 0;
  iRightCount := 0;
  For i := 0 To FCompareFolders.Count - 1 Do
    Begin
      Inc(iLeftSize, CompareFolders[i].LeftFldr.TotalSize);
      Inc(iLeftCount, CompareFolders[i].LeftFldr.TotalCount);
      Inc(iRightSize, CompareFolders[i].RightFldr.TotalSize);
      Inc(iRightCount, CompareFolders[i].RightFldr.TotalCount);
    End;
  AddFileOpStat(fosTotalLeft, 'Total Left', iLeftCount, iLeftSize);
  AddFileOpStat(fosTotalRight, 'Total Right', iRightCount, iRightSize);
  AddFileOpStat(fosDifference, 'Difference (L-R)', iDiffCount, iLeftSize - iRightSize);
  BuildTotals;
End;

(**

  This method processes all the process items and builds a list of drive totals to
  calculate if there is enough disk space for copying.

  @precon  None.
  @postcon Processes all the process items and builds a list of drive totals to
           calculate if there is enough disk space for copying.

**)
Procedure TCompareFoldersCollection.BuildTotals;

Var
  iProcessItem : Integer;

Begin
  FDrives.Clear;
  For iProcessItem := 0 To ProcessCount - 1 Do
    FDrives.ProcessOp(Process[iProcessItem]);
  FDrives.Sort;
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
  FEmptyFolders.Clear;
End;

(**

  This method copies the contents of a files from the source to the destination.

  @precon  None.
  @postcon Coppies the sourcefile to the destination file and increments the numver of 
           files copied.

  @param   strSourceFile as a String
  @param   strDestFile   as a String
  @param   iCopied       as an Integer as a reference
  @return  a TProcessSuccess

**)
Function TCompareFoldersCollection.CopyFileContents(strSourceFile, strDestFile: String;
  Var iCopied: Integer): TProcessSuccess;

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

Const
  strExceptionMsg = 'Could not copy file "%s" to "%s" (%s)!';

Var
  iSrcSize : Int64;
  iDestSize : Int64;
  iFreeBytesAvailableToCaller: Int64;
  iTotalNumberOfBytes: Int64;
  iTotalNumberOfFreeBytes: Int64;
  iLastError : Cardinal;
  strErrorMsg : String;
  iResult : TDGHErrorResult;

Begin
  iSrcSize := GetFileSize(strSourceFile);
  iDestSize := GetFileSize(strDestFile);
  GetDiskFreeSpaceEx(PChar(ExtractFilePath(strDestFile)), iFreeBytesAvailableToCaller,
    iTotalNumberOfBytes, @iTotalNumberOfFreeBytes);
  If (iSrcSize - iDestSize) < iTotalNumberOfFreeBytes Then
    Begin
      IncrementFolder(ExtractFilePath(strDestFile));
      If Not CopyFileEx(PChar(Expand(strSourceFile)), PChar(Expand(strDestFile)),
        @CopyCallback, Self, Nil, 0) Then
        Begin
          If Assigned(FCopyErrorNotifier) Then
            Begin
              Result := psFailed;
              iLastError := GetLastError;
              strErrorMsg := SysErrorMessage(iLastError);
              If Not FCopyErrors.IsInList(iLastError) Then
                Begin
                  iResult := derUnknown;
                  FCopyErrorNotifier(strSourceFile, strDestFile, strErrorMsg, iLastError,
                    iResult);
                  Case iResult Of
                    derIgnoreAll: FCopyErrors.Add(iLastError);
                    derUnknown, derStop: Raise Exception.CreateFmt(strExceptionMsg,
                        [strSourceFile, strDestFile, strErrorMsg]);
                  End;
                  Case iResult Of
                    derIgnoreOnce: Result := psIgnoreOnce;
                    derIgnoreAll : Result := psIgnoreAll;
                  Else
                    Result := psFailed;
                  End;                  
                End;
              AddToErrors(Format(strExceptionMsg, [strSourceFile, strDestFile,
                strErrorMsg]));
            End Else
            Begin
              Raise Exception.CreateFmt(strExceptionMsg, [strSourceFile, strDestFile,
               strErrorMsg]);
            End;
        End Else
        Begin
          Result := psSuccessed;
          Inc(iCopied);
        End;
    End Else
      Raise Exception.CreateFmt('There is not enough disk space at the destination to ' +
        'copy this file "%s".', [strDestFile]);
End;

(**

  This method copies the files found in the process list.

  @precon  None.
  @postcon Copies the files found in the process list.

**)
Procedure TCompareFoldersCollection.CopyFiles;

Var
  iCount            : Integer;
  FileActions       : TFileActions;
  iFile             : Integer;
  i                 : Integer;
  P                 : TProcessItem;
  strSource, strDest: String;
  SourceFile, DestFile : TFileRecord;
  iSuccess          : TProcessSuccess;

Begin
  FCopiedTotalSize := 0;
  FFiles   := 0;
  FSkipped := 0;
  iCount   := CountFileOps([foLeftToRight, foRightToLeft], FCopiedTotalSize);
  DoCopyStart(iCount, FCopiedTotalSize);
  Try
    If iCount = 0 Then
      Exit;
    FileActions := [];
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
                strSource  := P.LPath;
                strDest    := P.RPath;
                SourceFile := P.LeftFile;
                DestFile   := P.RightFile;
              End
            Else
              Begin
                strSource  := P.RPath;
                strDest    := P.LPath;
                SourceFile := P.RightFile;
                DestFile   := P.LeftFile;
              End;
            DoCopying(iFile + 1, strSource, strDest, SourceFile.FileName);
            If Not SysUtils.DirectoryExists
              (ExtractFilePath(strDest + SourceFile.FileName)) Then
              If Not SysUtils.ForceDirectories
                (ExtractFilePath(Expand(strDest + SourceFile.FileName))) Then
                Raise EFldrSyncException.CreateFmt('Can not create folder "%s".',
                  [ExtractFilePath(strDest + SourceFile.FileName)]);
            If DestFile = Nil Then
              iSuccess := CopyFileContents(strSource + SourceFile.FileName,
                strDest + SourceFile.FileName, FFiles)
            Else
              iSuccess := CopyIndividualFile(strSource, strDest, SourceFile,
                DestFile, FileActions, P.SyncOptions);
            Inc(FCopiedTotalSize, SourceFile.Size);
            If iSuccess In [psFailed, psIgnoreOnce, psIgnoreAll] Then
              Inc(FErrors)
            Else
              Inc(iFile);
            DoCopied(iFile, SourceFile.Size, FCopiedTotalSize, iSuccess);
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

  @param   strSource   as a String
  @param   strDest     as a String
  @param   SourceFile  as a TFileRecord
  @param   DestFile    as a TFileRecord
  @param   FileActions as a TFileActions as a reference
  @param   SyncOps     as a TSyncOptions
  @return  a TProcessSuccess

**)
Function TCompareFoldersCollection.CopyIndividualFile(strSource, strDest: String;
  SourceFile, DestFile: TFileRecord; Var FileActions : TFileActions;
  SyncOps: TSyncOptions): TProcessSuccess;

Var
  FA   : TFileAction;
  iAttr: Cardinal;

Begin
  Result := psUnknown;
  FA := faUnknown;
  If (DestFile = Nil) Or (DestFile.Attributes And faReadOnly = 0) Then
    Begin
      If (faYesToAll In FileActions) Then
        FA := faYesToAll
      Else If (faNoToAll In FileActions) Then
        FA := faNoToAll;
      DoCopyQuery(strSource, strDest, SourceFile, DestFile, FA, SyncOps);
      If FA = faYesToAll Then
        Begin
          Include(FileActions, faYesToAll);
          FA := faYes;
        End
      Else If FA = faNoToAll Then
        Begin
          Include(FileActions, faNoToAll);
          FA := faNo;
        End;
    End Else
    Begin
      If (faYesToAllRO In FileActions) Then
        FA := faYesToAllRO
      Else If (faNoToAllRO In FileActions) Then
        FA := faNoToAllRO;
      DoCopyReadOnlyQuery(strSource, strDest, SourceFile, DestFile, FA, SyncOps);
      If FA = faYesToAllRO Then
        Begin
          Include(FileActions, faYesToAllRO);
          FA := faYes;
        End
      Else If FA = faNoToAllRO Then
        Begin
          Include(FileActions, faNoToAllRO);
          FA := faNo;
        End;
    End;
  Case FA Of
    faNo:
      Begin
        Inc(FSkipped);
        Result := psSuccessed;
      End;
    faYes:
      Begin
        If DestFile.Attributes And faReadOnly > 0 Then
          Begin
            iAttr := GetFileAttributes(PChar(Expand(strDest + DestFile.FileName)));
            iAttr := iAttr Xor FILE_ATTRIBUTE_READONLY;
            SetFileAttributes(PChar(Expand(strDest + DestFile.FileName)), iAttr);
          End;
        Result := CopyFileContents(strSource + SourceFile.FileName,
          strDest + DestFile.FileName, FFiles);
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
              Begin
                Inc(iSize, Process[i].LeftFile.Size);
                If Process[i].RightFile <> Nil Then
                  Dec(iSize, Process[i].RightFile.Size);
              End Else
              Begin
                Inc(iSize, Process[i].RightFile.Size);
                If Process[i].LeftFile <> Nil Then
                  Dec(iSize, Process[i].LeftFile.Size);
              End;
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
  FErrorMsgs      := TStringList.Create;
  FEmptyFolders   := TStringList.Create;
  FEmptyFolders.Sorted := True;
  FEmptyFolders.CaseSensitive := False;
  FEmptyFolders.Duplicates := dupIgnore;
  FDrives := TDriveTotals.Create;
  FCopyErrors := TSortedIntegerList.Create;
  FDeleteErrors := TSortedIntegerList.Create;
End;

(**

  This method decrements the number of files and sub-folders associated with a folder in 
  the empty folder collection.

  @precon  None.
  @postcon The number of files and sub-folders associated with the folder is decremented 
           by 1.

  @param   strFolder as a String
  @return  a Boolean

**)
Function TCompareFoldersCollection.DecrementFolder(strFolder: String) : Boolean;

Var
  iIndex : Integer;
  iFileCount: NativeUInt;
  
Begin
  Result := False;
  If FEmptyFolders.Find(strFolder, iIndex) Then
    Begin
      iFileCount := NativeUInt(FEmptyFolders.Objects[iIndex]);
      If iFileCount > 0 Then
        Begin
          Dec(iFileCount);
          FEmptyFolders.Objects[iIndex] := TObject(iFileCount);
        End;
      Result := iFileCount = 0;
    End Else
      CodeSite.SendWarning('DecFldr: %s', [strFolder]);
End;

(**

  This method cycles through the emprt folders string list backwards deleting an folder
  with zero files and sub-folders.

  @precon  None.
  @postcon Any folder with zero files and sub-folders is deleted.

**)
Procedure TCompareFoldersCollection.DeleteEmptyFolders;

Var
  i : Integer;
  iFileCount: NativeUInt;
  iFolderCount : Integer;
  iFolder : Integer;
  
Begin
  iFolderCount := 0;
  For i := 0 To FEmptyFolders.Count - 1 Do
    Begin
      iFileCount := NativeUInt(FEmptyFolders.Objects[i]);
      If iFileCount = 0 Then
        Inc(iFolderCount);
    End;
  DoDeleteFoldersStart(iFolderCount);
  iFolder := 0;
  For i := FEmptyFolders.Count - 1 DownTo 0 Do
    Begin
      iFileCount := NativeUInt(FEmptyFolders.Objects[i]);
      If iFileCount = 0 Then
        Begin
          If DecrementFolder(ExtractFilePath(Copy(FEmptyFolders[i], 1,
            Length(FEmptyFolders[i]) - 1))) Then
            Inc(iFolderCount);
          RemoveDir(FEmptyFolders[i]);
          DoDeleteFolders(iFolder, iFolderCount, FEmptyFolders[i]);
          Inc(iFolder);
          iFileCount := FileCount(FEmptyFolders[i]);
          If iFileCount > 0 Then
            CodeSite.SendWarning('%d = %s', [iFileCount, FEmptyFolders[i]]);
        End;
    End;
  DoDeleteFoldersEnd;
End;

(**

  This method deletes the files the are marked for deletion in the process items list.

  @precon  None.
  @postcon Deletes the files the are marked for deletion in the process items list.

**)
Procedure TCompareFoldersCollection.DeleteFiles;

Var
  iCount   : Integer;
  i        : Integer;
  P        : TProcessItem;
  iFile    : Integer;
  FileActions : TFileActions;

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
    FileActions := [];
    iFile     := 1;
    For i     := 0 To ProcessCount - 1 Do
      Begin
        P := Process[i];
        If P.FileOp = foDelete Then
          Begin
            If P.LeftFile <> Nil Then
              DeleteIndividualFile(P.LPath, P.LeftFile, iFile, FileActions,
                P.SyncOptions);
            If P.RightFile <> Nil Then
              DeleteIndividualFile(P.RPath, P.RightFile, iFile, FileActions,
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
  @param   FileActions as a TFileActions as a reference
  @param   SyncOps     as a TSyncOptions

**)
Procedure TCompareFoldersCollection.DeleteIndividualFile(strPath: String; F: TFileRecord;
  iFile: Integer; Var FileActions : TFileActions; SyncOps: TSyncOptions);

Const
  strExceptionMsg = 'Deletion of file "%s" failed (%s)!';

Var
  FA        : TFileAction;
  iAttr     : Cardinal;
  iSuccess  : TProcessSuccess;
  iLastError: Cardinal;
  strErrorMsg: String;
  iResult: TDGHErrorResult;

Begin
  iSuccess := psUnknown;
  DoDeleting(iFile, strPath + F.FileName);
  FA := faUnknown;
  If (F.Attributes And faReadOnly = 0) Then
    Begin
      If (faYesToAll In FileActions) Then
        FA := faYesToAll
      Else If (faNoToAll In FileActions) Then
        FA := faNoToAll;
      DoDeleteQuery(strPath, F, FA, SyncOps);
      If FA = faYesToAll Then
        Begin
          Include(FileActions, faYesToAll);
          FA := faYes;
        End
      Else If FA = faNoToAll Then
        Begin
          Include(FileActions, faNoToAll);
          FA := faNo;
        End;
    End Else
    Begin
      If (faYesToAllRO In FileActions) Then
        FA := faYesToAllRO
      Else If (faNoToAllRO In FileActions) Then
        FA := faNoToAllRO;
      DoDeleteReadOnlyQuery(strPath, F, FA, SyncOps);
      If FA = faYesToAllRO Then
        Begin
          Include(FileActions, faYesToAllRO);
          FA := faYes;
        End
      Else If FA = faNoToAllRO Then
        Begin
          Include(FileActions, faNoToAllRO);
          FA := faNo;
        End;
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
        DecrementFolder(strPath);
        If Not DeleteFile(PChar(Expand(strPath + F.FileName))) Then
          Begin
            If Assigned(FDeleteErrorNotifier) Then
              Begin
                iLastError := GetLastError;
                strErrorMsg := SysErrorMessage(iLastError);
                If Not FDeleteErrors.IsInList(iLastError) Then
                  Begin
                    iResult := derUnknown;
                    FDeleteErrorNotifier(strPath + F.FileName, strErrorMsg, iLastError,
                      iResult);
                    Case iResult Of
                      derIgnoreAll: FDeleteErrors.Add(iLastError);
                      derUnknown, derStop: Raise Exception.CreateFmt(strExceptionMsg,
                        [strPath + F.FileName, strErrorMsg]);
                    End;
                    AddToErrors(Format(strExceptionMsg, [strPath + F.FileName,
                      strErrorMsg]));
                  End;
                Case iResult Of
                  derIgnoreOnce: iSuccess := psIgnoreOnce;
                  derIgnoreAll:  iSuccess := psIgnoreAll;
                Else
                  iSuccess := psFailed;
                End;
              End Else
              Begin
                Raise Exception.CreateFmt(strExceptionMsg, [strPath + F.FileName,
                 strErrorMsg]);
              End;
          End Else
          Begin
            iSuccess := psSuccessed;
            Inc(FFiles);
          End;
      End;
    faNo:
      Begin
        iSuccess := psSuccessed;
        Inc(FSkipped);
      End;
  End;
  DoDeleted(iFile, FCopiedTotalSize, iSuccess);
End;

(**

  This is the destructor method for the TCompareFolderCollection class.

  @precon  None.
  @postcon Destroy the class and all the CompareFolders classes it owns.

**)
Destructor TCompareFoldersCollection.Destroy;

Begin
  FCopyErrors.Free;
  FDeleteErrors.Free;
  FDrives.Free;
  FEmptyFolders.Free;
  FErrorMsgs.Free;
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
  @param   iSuccess             as a TProcessSuccess

**)
Procedure TCompareFoldersCollection.DoCopied(iCopiedFiles: Integer; iCopiedFileTotalSize,
  iCopiedTotalSize: Int64; iSuccess: TProcessSuccess);

Begin
  If Assigned(FCopiedNotifier) Then
    FCopiedNotifier(iCopiedFiles, iCopiedFileTotalSize, iCopiedTotalSize, iSuccess);
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

  @param   strSourcePath as a String
  @param   strDestPath   as a String
  @param   SourceFile    as a TFileRecord
  @param   DestFile      as a TFileRecord
  @param   Option        as a TFileAction as a reference
  @param   SyncOptions   as a TSyncOptions

**)
Procedure TCompareFoldersCollection.DoCopyQuery(strSourcePath, strDestPath: String;
  SourceFile, DestFile : TFileRecord; Var Option: TFileAction; SyncOptions: TSyncOptions);

Begin
  If Not CanByPassQuery(SyncOptions, False, Option) Then
    If Assigned(FCopyQueryNotifier) Then
      FCopyQueryNotifier(strSourcePath, strDestPath, SourceFile, DestFile, Option);
End;

(**

  This method fires the CopyReadOnlyQuery event if the event has a handler installed.

  @precon  None.
  @postcon Fires the CopyReadOnlyQuery event if the event has a handler installed.

  @param   strSourcePath as a String
  @param   strDestPath   as a String
  @param   SourceFile    as a TFileRecord
  @param   DestFile      as a TFileRecord
  @param   Option        as a TFileAction as a reference
  @param   SyncOptions   as a TSyncOptions

**)
Procedure TCompareFoldersCollection.DoCopyReadOnlyQuery(strSourcePath,
  strDestPath: String; SourceFile, DestFile : TFileRecord; Var Option: TFileAction;
  SyncOptions: TSyncOptions);

Begin
  If Not CanByPassQuery(SyncOptions, True, Option) Then
    If Assigned(FCopyReadOnlyQueryNotifier) Then
      FCopyReadOnlyQueryNotifier(strSourcePath, strDestPath, SourceFile, DestFile, Option);
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

  This method outputs the error messages for the file operations using the assigned call
  back procedure.

  @precon  None.
  @postcon Outputs the error messages for the file operations

**)
Procedure TCompareFoldersCollection.DoErrorMessages;

Var
  i          : Integer;

Begin
  DoErrorsMsgsStart(FErrorMsgs.Count);
  For i := 0 To FErrorMsgs.Count - 1 Do
    DoErrorMsgs(FErrorMsgs[i]);
  DoErrorMsgsEnd();
End;

(**

  This method calls the On Error Msgs event handler if it is assigned.

  @precon  None.
  @postcon Calls the On Error Msgs event handler if it is assigned.

  @param   strErrorMsg as a String

**)
Procedure TCompareFoldersCollection.DoErrorMsgs(strErrorMsg: String);

Begin
  If Assigned(FErrorMsgsNotifier) Then
    FErrorMsgsNotifier(strErrorMsg);
End;

(**

  This method calls the On Error Msgs End event handler if it is assigned.

  @precon  None.
  @postcon Calls the On Error Msgs End event handler if it is assigned.

**)
Procedure TCompareFoldersCollection.DoErrorMsgsEnd;

Begin
  If Assigned(FErrorMsgsEndNotifier) Then
    FErrorMsgsEndNotifier;
End;

(**

  This method calls the On Error Msgs Start event handler if it is assigned.

  @precon  None.
  @postcon Calls the On Error Msgs Start event handler if it is assigned.

  @param   iErrorCount as an Integer

**)
Procedure TCompareFoldersCollection.DoErrorsMsgsStart(iErrorCount: Integer);

Begin
  If Assigned(FErrorMsgsStartNotifier) Then
    FErrorMsgsStartNotifier(iErrorCount);
End;

(**

  This method calls the FExceeedsSizeLimit is it has been assigned.

  @precon  None.
  @postcon Calls the FExceeedsSizeLimit is it has been assigned.

  @param   strLPath    as a String
  @param   strRPath    as a String
  @param   strFileName as a String

**)
Procedure TCompareFoldersCollection.DoExceedsSizeLimit(strLPath, strRPath,
  strFileName: String);

Begin
  If Assigned(FExceedsSizeLimitNotifier) Then
    FExceedsSizeLimitNotifier(strLPAth, strRPath, strFileName);
End;

(**

  This method calls the FExceeedsSizeLimitEnd is it has been assigned.

  @precon  None.
  @postcon Calls the FExceeedsSizeLimitEnd is it has been assigned.

**)
Procedure TCompareFoldersCollection.DoExceedsSizeLimitEnd;

Begin
  If Assigned(FExceedsSizeLimitEndNotifier) Then
    FExceedsSizeLimitEndNotifier();
End;

(**

  This method calls the FExceeedsSizeLimitStart is it has been assigned.

  @precon  None.
  @postcon Calls the FExceeedsSizeLimitStart is it has been assigned.

  @param   iFileCount as an Integer

**)
Procedure TCompareFoldersCollection.DoExceedsSizeLimitStart(iFileCount: Integer);

Begin
  If Assigned(FExceedsSizeLimitStartNotifier) Then
    FExceedsSizeLimitStartNotifier(iFileCount);
End;

(**

  This method fires the Deleted event if the event has a handler installed.

  @precon  None.
  @postcon Fires the Deleted event if the event has a handler installed.

  @param   iFile    as an Integer
  @param   iSize    as an Int64
  @param   iSuccess as a TProcessSuccess

**)
Procedure TCompareFoldersCollection.DoDeleted(iFile: Integer; iSize: Int64;
  iSuccess: TProcessSuccess);

Begin
  If Assigned(FDeletedNotifier) Then
    FDeletedNotifier(iFile, iSize, iSuccess);
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

  This method invokes the OnDeleteFolders event handler if its assigned.

  @precon  None.
  @postcon the OnDeleteFolders event handler is invoked if assigned.

  @param   iFolder   as an Integer
  @param   iFolders  as an Integer
  @param   strFolder as a String

**)
Procedure TCompareFoldersCollection.DoDeleteFolders(iFolder, iFolders : Integer;
  strFolder: String);

Begin
  If Assigned(FDeleteFoldersNotifier) Then
    FDeleteFoldersNotifier(iFolder, iFolders, strFolder);
End;

(**

  This method invokes the OnDeleteFoldersEnd event handler if its assigned.

  @precon  None.
  @postcon the OnDeleteFoldersEnd event handler is invoked if assigned.

**)
Procedure TCompareFoldersCollection.DoDeleteFoldersEnd;

Begin
  If Assigned(FDeleteFoldersEndNotifier) Then
    FDeleteFoldersEndNotifier;
End;

(**

  This method invokes the OnDeleteFoldersStart event handler if its assigned.

  @precon  None.
  @postcon the OnDeleteFoldersStart event handler is invoked if assigned.

  @param   iFolderCount as an Integer

**)
Procedure TCompareFoldersCollection.DoDeleteFoldersStart(iFolderCount: Integer);

Begin
  If Assigned(FDeleteFoldersStartNotifier) Then
    FDeleteFoldersStartNotifier(iFolderCount);
End;

(**

  This method fires the DeleteQuery event if the event has a handler installed.

  @precon  None.
  @postcon Fires the DeleteQuery event if the event has a handler installed.

  @param   strFilePath as a String
  @param   DeleteFile  as a TFileRecord
  @param   Option      as a TFileAction as a reference
  @param   SyncOptions as a TSyncOptions

**)
Procedure TCompareFoldersCollection.DoDeleteQuery(strFilePath: String;
  DeleteFile : TFileRecord; Var Option: TFileAction; SyncOptions: TSyncOptions);

Begin
  If Not CanByPassQuery(SyncOptions, False, Option) Then
    If Assigned(FDeleteQueryNotifier) Then
      FDeleteQueryNotifier(strFilePath, DeleteFile, Option);
End;

(**

  This method fires the DeleteReadOnlyQuery event if the event has a handler installed.

  @precon  None.
  @postcon Fires the DeleteReadOnlyQuery event if the event has a handler installed.

  @param   strFilePath as a String
  @param   DeleteFile  as a TFileRecord
  @param   Option      as a TFileAction as a reference
  @param   SyncOptions as a TSyncOptions

**)
Procedure TCompareFoldersCollection.DoDeleteReadOnlyQuery(strFilePath: String;
  DeleteFile : TFileRecord; Var Option: TFileAction; SyncOptions: TSyncOptions);

Begin
  If Not CanByPassQuery(SyncOptions, True, Option) Then
    If Assigned(FDeleteReadOnlyQueryNotifier) Then
      FDeleteReadOnlyQueryNotifier(strFilePath, DeleteFile, Option);
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

  This method processes the files which are labelled as exceeding the size limit.

  @precon  None.
  @postcon Processes the files which are labelled as exceeding the size limit.

**)
Procedure TCompareFoldersCollection.DoSizeLimit;

Var
  iSize : Int64;
  iCount: Integer;
  i     : Integer;
  P     : TProcessItem;
  strFileName : String;

Begin
  iSize  := 0;
  iCount := CountFileOps([foExceedsSizeLimit], iSize);
  DoExceedsSizeLimitStart(iCount);
  For i := 0 To ProcessCount - 1 Do
    Begin
      P := Process[i];
      If P.FileOp In [foExceedsSizeLimit] Then
        Begin
          If P.LeftFile <> Nil   Then
            strFileName := P.LeftFile.FileName
          Else
            strFileName := P.RightFile.FileName;
          DoExceedsSizeLimit(P.LPath, P.RPath, strFileName);
        End;
    End;
  DoExceedsSizeLimitEnd();
End;

(**

  This method counts the number of files and sub-folder within the given folder.

  @precon  None.
  @postcon Returns the number of files and sub-folders in the given folder.

  @param   strFolder as a String
  @return  a NativeUInt

**)
Function TCompareFoldersCollection.FileCount(strFolder: String): NativeUInt;

Var
  recSearch : TSearchRec;
  iResult: Integer;
  
Begin
  Result := 0;
  iResult := FindFirst(strFolder + '*.*', faAnyFile, recSearch);
  Try
    While iResult = 0 Do
      Begin
        If (recSearch.Name <> '.') And (recSearch.Name <> '..') Then
          Inc(Result);
        iResult := FindNext(recSearch);
      End;
  Finally
    SysUtils.FindClose(recSearch);
  End;
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

  This is a getter method for the Statistics property.

  @precon  None.
  @postcon Returns the enumerated file statistic.

  @param   FileOpStat as a TFileOpStat
  @return  a TFileOpStatRec

**)
Function TCompareFoldersCollection.GetStatistics(FileOpStat: TFileOpStat): TFileOpStatRec;

Begin
  Result := FStatistics[FileOpStat];
End;

(**

  This method increments the number of files and sub-folders associated with a folder
  in the empty folder collection.

  @precon  None.
  @postcon The number of files and sub-folders associated with the folder is incremented
           by 1.

  @param   strFolder as a String

**)
Procedure TCompareFoldersCollection.IncrementFolder(strFolder: String);

Var
  iIndex : Integer;
  iFileCount : Integer;
  
Begin
  If FEmptyFolders.Find(strFolder, iIndex) Then
    Begin
      iFileCount := NativeUInt(FEmptyFolders.Objects[iIndex]) + 1;
      FEmptyFolders.Objects[iIndex] := TObject(iFileCount);
    End Else
    Begin
      AddEmptyFolder(strFolder);
      IncrementFolder(strFolder);
    End;
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
      stSame..stDiffSize:
        Result := foNothing;
      stTooLarge:
        Result := foExceedsSizeLimit;
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
      stSame..stDiffSize:
        Result := foNothing;
      stTooLarge:
        Result := foExceedsSizeLimit;
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
  FCopyErrors.Clear;
  FDeleteErrors.Clear;
  FErrorMsgs.Clear;
  DeleteFiles;
  CopyFiles;
  DifferentSize;
  DoNothing;
  DoSizeLimit;
  DoErrorMessages;
  DeleteEmptyFolders;
End;

(**

  This method starts the recursion of the folders for each folder pairing.

  @precon  slFolders must contain pairs folders fld1=fld2 etc.
  @postcon Creates an instance of the compare folder classes for each pairing.

  @param   Folders       as a TFolders
  @param   strExclusions as a String
  @return  a Boolean

**)
Function TCompareFoldersCollection.ProcessFolders(Folders: TFolders;
  strExclusions: String): Boolean;

Var
  i  : Integer;
  CP : TCompareFolders;

Begin
  For i := 0 To Folders.Count - 1 Do
    Begin
      If soEnabled In Folders.Folder[i].SyncOptions Then
        Begin
          CP := TCompareFolders.Create(AddEmptyFolder);
          FCompareFolders.Add(CP);
          CP.OnSearchStart  := FSearchStartNotifier;
          CP.OnSearch       := FSearchNotifier;
          CP.OnSearchEnd    := FSearchEndNotifier;
          CP.OnCompareStart := FCompareStartNotifier;
          CP.OnCompare      := FCompareNotifier;
          CP.OnCompareEnd   := FCompareEndNotifier;
          CP.SearchFolders(
            ExtractFilePath(Folders.Folder[i].LeftFldr),
            ExtractFilePath(Folders.Folder[i].RightFldr),
            ExtractFileName(Folders.Folder[i].Patterns),
            strExclusions,
            i,
            Folders.Folder[i].SyncOptions,
            Folders.Folder[i].MaxFileSize
          );
        End;
    End;
  BuildMatchLists;
  BuildStats;
  Result := True;
End;

{ TDriveTotal }

(**

  This method adds a file operation to the drive totals.

  @precon  None.
  @postcon Adds a file operation to the drive totals.

  @param   iSize  as an Int64
  @param   FileOp as a TFileOp

**)
Procedure TDriveTotal.AddOption(iSize: Int64; FileOp: TFileOp);

Begin
  Case FileOp Of
    foLeftToRight: Inc(FTotalAdds, iSize);
    foRightToLeft: Inc(FTotalAdds, iSize);
    foDelete:      Inc(FTotalDeletes, iSize);
  End;
End;

(**

  A constructor for the TDriveTotal class.

  @precon  None.
  @postcon Initialises the class with a drive mapping and this initial space.

  @param   strDrive as a String

**)
Constructor TDriveTotal.Create(strDrive: String);

Var
  iTotalNumberOfFreeBytes: Int64;

Begin
  FDrive := strDrive;
  GetDiskFreeSpaceEx(PChar(FDrive), FFreeAtStart, FTotal, @iTotalNumberOfFreeBytes);
  FTotalDeletes := 0;
  FTotalAdds := 0;
End;

(**

  This is a getter method for the FreeAtFinish property.

  @precon  None.
  @postcon Calculates the free at finish value for the drive.

  @return  an Int64

**)
Function TDriveTotal.GetFreeAtFinish: Int64;

Begin
  Result := FFreeAtStart + FTotalDeletes - FTotalAdds;
End;

{ TDriveTotals }

(**

  This method clears the drive list of existing objects.

  @precon  None.
  @postcon The existing object in the list are freed.

**)
Procedure TDriveTotals.Clear;

Begin
  FDrives.Clear;
End;

(**

  A constructor for the TDriveTotals class.

  @precon  None.
  @postcon Initialises the collection to empty.

**)
Constructor TDriveTotals.Create;

Begin
  FDrives := TObjectList.Create(True);
End;

(**

  A destructor for the TDriveTotals class.

  @precon  None.
  @postcon Frees the memory used by the collection.

**)
Destructor TDriveTotals.Destroy;

Begin
  FDrives.Free;
  Inherited Destroy;
End;

(**

  This method find the drive mapping in the collection else creates a new one and returns
  its index.

  @precon  None.
  @postcon Find the drive mapping in the collection else creates a new one and returns
           its index.

  @param   strPath as a String
  @return  an Integer

**)
Function TDriveTotals.Find(strPath: String): Integer;

Var
  iDrive : Integer;
  strDrive : String;
  iPos: Integer;

Begin
  Result := -1;
  If Like('*:\*', strPath) Then
    strDrive := Copy(strPath, 1, 3)
  Else
    Begin
      iPos := PosOfNthChar(strPath, '\', 4);
      If iPos > 0 Then
        strDrive := Copy(strPath, 1, iPos)
      Else
        strDrive := strPath;
    End;
  For iDrive := 0 To Count - 1 Do
    If CompareText(Drive[iDrive].Drive, strDrive) = 0 Then
      Begin
        Result := iDrive;
        Break;
      End;
  If Result = -1 Then
    Result := FDrives.Add(TDriveTotal.Create(strDrive))
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of drives in the collection.

  @return  an Integer

**)
Function TDriveTotals.GetCount: Integer;

Begin
  Result := FDrives.Count;
End;

(**

  This is a getter method for the Drive property.

  @precon  The index must be between 0 and Count - 1.
  @postcon Returns the instance of the index drive total.

  @param   iIndex as an Integer
  @return  a TDriveTotal

**)
Function TDriveTotals.GetDriveTotal(iIndex: Integer): TDriveTotal;

Begin
  Result := FDrives[iIndex] As TDriveTotal;
End;

(**

  This method processes the given process item and adds the information to the appropriate
  drive total.

  @precon  ProcessItem must be a vaild instance.
  @postcon Processes the given process item and adds the information to the appropriate
           drive total.

  @param   ProcessItem as a TProcessItem

**)
Procedure TDriveTotals.ProcessOp(ProcessItem: TProcessItem);

Var
  iFileRight, iFileLeft : Int64;
  iDrive : Integer;

Begin
  Case ProcessItem.FileOp Of
    foLeftToRight:
      Begin
        iDrive := Find(ProcessItem.LPath);
        Drive[iDrive].AddOption(0, foLeftToRight);
        iFileLeft := ProcessItem.LeftFile.Size;
        iFileRight := 0;
        If ProcessItem.RightFile <> Nil Then
          iFileRight := ProcessItem.RightFile.Size;
        iDrive := Find(ProcessItem.RPath);
        Drive[iDrive].AddOption(iFileLeft - iFileRight, foLeftToRight);
      End;
    foRightToLeft:
      Begin
        iDrive := Find(ProcessItem.RPath);
        Drive[iDrive].AddOption(0, foRightToLeft);
        iFileRight := ProcessItem.RightFile.Size;
        iFileLeft := 0;
        If ProcessItem.LeftFile <> Nil Then
          iFileLeft := ProcessItem.LeftFile.Size;
        iDrive := Find(ProcessItem.LPath);
        Drive[iDrive].AddOption(iFileRight - iFileLeft, foRightToLeft);
      End;
    foDelete:
      Begin
        If ProcessItem.RightFile <> Nil Then
          Begin
            iDrive := Find(ProcessItem.RPath);
            Drive[iDrive].AddOption(ProcessItem.RightFile.Size, foDelete);
          End;
        If ProcessItem.LeftFile <> Nil Then
          Begin
            iDrive := Find(ProcessItem.LPath);
            Drive[iDrive].AddOption(ProcessItem.LeftFile.Size, foDelete);
          End;
      End;
  End;
End;

(**

  This function is an on compare function for sorting the drives.

  @precon  None.
  @postcon Returns the comparison between the item1 and item 2 drive names.

  @param   Item1 as a Pointer
  @param   Item2 as a Pointer
  @return  an Integer

**)
Function DriveComparisonFunction(Item1, Item2 : Pointer) : Integer;

Begin
  Result := CompareText(TDriveTotal(Item1).Drive, TDriveTotal(Item2).Drive); 
End;

(**

  This method sorts the collection by drive name.

  @precon  None.
  @postcon The collection is sorted by drive name.

**)
Procedure TDriveTotals.Sort;

Begin
  FDrives.Sort(DriveComparisonFunction);
End;

{ TSortedIntegerList }

(**

  This method adds a integer to the collection.

  @precon  None.
  @postcon The given integer is added to the collection if it does not already exist.

  @param   iValue as an Integer

**)
Procedure TSortedIntegerList.Add(iValue: Integer);

Var
  iIndex : Integer;
  i: Integer;
  tmp : TArrayOfInteger;
  
Begin
  iIndex := Find(iValue);
  Inc(FCount);
  If FCount > Succ(High(FSortedIntegerList)) Then
    Begin
      SetLength(tmp, Succ(High(FSortedIntegerList)) + iCAPACITY);
      For i := 1 To FCount - 1 Do
        tmp[i] := FSortedIntegerList[i];
      FSortedIntegerList := tmp;
      tmp := Nil;
    End;
  If iIndex < 0 Then
    Begin
      For i := Abs(iIndex) To FCount - 1 Do
        FSortedIntegerList[i] := FSortedIntegerList[i - 1];
      FSortedIntegerList[Abs(iIndex) - 1] := iValue;
    End;
End;

(**

  This method clears the list of errors.

  @precon  None.
  @postcon Sets FCount to zero.

**)
Procedure TSortedIntegerList.Clear;

Begin
  FCount := 0;
End;

(**

  A constructor for the TSortedIntegerList class.

  @precon  None.
  @postcon Initialises the class to empty.

**)
Constructor TSortedIntegerList.Create;

Begin
  SetLength(FSortedIntegerList, iCAPACITY);
  FCount := 0;
End;

(**

  A destructor for the TSortedIntegerList class.

  @precon  None.
  @postcon Frees the memory used by the collection.

**)
Destructor TSortedIntegerList.Destroy;

Begin
  FSortedIntegerList := Nil;
  Inherited Destroy;
End;

(**

  This method returns the index of the given integer in the colection if found else
  returns the location where the integer should be inserted as a negative value.

  @precon  None.
  @postcon Returns the index of the given integer in the colection if found else
           returns the location where the integer should be inserted as a negative value.

  @param   iValue as an Integer
  @return  an Integer

**)
Function TSortedIntegerList.Find(iValue: Integer): Integer;

Var
  iFirst, iMid, iLast : Integer;
  
Begin
  iFirst := 1;
  iLast := FCount;
  While iFirst <= iLast Do
    Begin
      iMid := (iFirst + iLast) Div 2;
      If iValue = FSortedIntegerList[iMid - 1] Then
        Begin
          Result := iMid;
          Exit;
        End;
      If iValue < FSortedIntegerList[iMid - 1] Then
        iLast := iMid - 1
      Else
        iFirst := iMid + 1;
    End;
  Result := -iFirst;
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of items in the collection.

  @return  an Integer

**)
Function TSortedIntegerList.GetCount: Integer;

Begin
  Result := FCount;
End;

(**

  This is a getter method for the Value property.

  @precon  iIndex must be between 1 and Count.
  @postcon Returns the integer from the collection at the index.

  @param   iIndex as an Integer
  @return  an Integer

**)
Function TSortedIntegerList.GetValue(iIndex: Integer): Integer;

Begin
  Result := FSortedIntegerList[iIndex];
End;

(**

  This method returns whether the given integer is in the collection.

  @precon  None.
  @postcon Returns whether the given integer is in the collection.

  @param   iValue as an Integer
  @return  a Boolean

**)
Function TSortedIntegerList.IsInList(iValue: Integer): Boolean;

Begin
  Result := Find(iValue) > 0;
End;

End.
