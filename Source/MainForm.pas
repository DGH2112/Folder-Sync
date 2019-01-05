(**

  This module if a class to represent the applications main form interface.
  This form provide the display of differences between two folders.

  @Version 2.0
  @Date    05 Jan 2019
  @Author  David Hoyle

  @nocheck HardCodedInteger HardCodedString HardCodedNumber

**)
Unit MainForm;

Interface

Uses
  WinAPI.Windows,
  WinAPI.Messages,
  System.SysUtils,
  System.Classes,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.Menus,
  VCL.ActnList,
  VCL.ImgList,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.ToolWin,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.AppEvnts,
  System.IniFiles,
  System.Win.Registry,
  SyncModule,
  ProgressForm,
  OptionsForm,
  PlatformDefaultStyleActnCtrls,
  VCL.ActnMan,
  VCL.ActnCtrls,
  VCL.ActnMenus,
  DGHCustomGraphicsControl,
  DGHMemoryMonitorControl,
  System.Win.ComObj,
  WinAPI.ShlObj,
  FileDeleteProgressForm,
  FileCopyProgressForm,
  DiskSpaceForm,
  System.Actions,
  System.ImageList,
  VirtualTrees;

Type
  (** This enumerate determines the type of progress to be shown in the taskbar in
      windows 7. **)
  TTaskbarProgressType = (ptNone, ptNormal, ptError);

  (** This is the class that actually describes for the form interface. **)
  TfrmMainForm = Class(TForm)
    stbrStatusBar: TStatusBar;
    ilActionImages: TImageList;
    actFileExit: TAction;
    actFileCompare: TAction;
    pmFiles: TPopupMenu;
    actToolsOptions: TAction;
    actEditCopyRightToLeft: TAction;
    actEditCopyLeftToRight: TAction;
    actEditDelete: TAction;
    actEditDoNothing: TAction;
    mmiCopyLefttoRight: TMenuItem;
    mmiCopyRighttoLeft: TMenuItem;
    mmiSep1: TMenuItem;
    mmiDeleteBoth: TMenuItem;
    mmiSep2: TMenuItem;
    mmiDoNothing: TMenuItem;
    actFileProcessFiles: TAction;
    actEditSelectAll: TAction;
    mmiSep3: TMenuItem;
    mmiSelectAll: TMenuItem;
    appEvents: TApplicationEvents;
    actHelpAbout: TAction;
    ilFileTypeIcons: TImageList;
    actToolsCompare: TAction;
    N2: TMenuItem;
    Compare1: TMenuItem;
    amActions: TActionManager;
    ambMenuBar: TActionMainMenuBar;
    atbToolbar: TActionToolBar;
    pnlTop: TPanel;
    DGHMemoryMonitor: TDGHMemoryMonitor;
    splOutputResults: TSplitter;
    pnlMainArea: TPanel;
    actEditClearLog: TAction;
    actHelpContents: TAction;
    redtOutputResults: TMemo;
    actToolsConfigMemMon: TAction;
    Splitter: TSplitter;
    vstFileList: TVirtualStringTree;
    Procedure actHelpAboutExecute(Sender: TObject);
    Procedure actFileExitExecute(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure actFileCompareExecute(Sender: TObject);
    Procedure actToolsOptionsExecute(Sender: TObject);
    Procedure actEditCopyLeftToRightExecute(Sender: TObject);
    Procedure actEditCopyRightToLeftExecute(Sender: TObject);
    Procedure actEditDeleteExecute(Sender: TObject);
    Procedure actEditDoNothingExecute(Sender: TObject);
    Procedure actFileProcessFilesExecute(Sender: TObject);
    Procedure actEditSelectAllExecute(Sender: TObject);
    Procedure appEventsException(Sender: TObject; E: Exception);
    Procedure actToolsCompareExecute(Sender: TObject);
    Procedure actToolsCompareUpdate(Sender: TObject);
    procedure actEditClearLogExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure stbrStatusBarDrawPanel(Sender: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure actEditFileOperationsUpdate(Sender: TObject);
    procedure actToolsConfigMemMonExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure vstFileListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas:
        TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode:
        TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstFileListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFileListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure vstFileListGetImageIndexEx(Sender: TBaseVirtualTree; Node:
        PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted:
        Boolean; var ImageIndex: TImageIndex; var ImageList: TCustomImageList);
    procedure vstFileListPaintText(Sender: TBaseVirtualTree; const TargetCanvas:
        TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  Strict Private
    Type
      (** A record to describe the data stored in the File List nodes of the VTV control. **)
      TFSFileListNode = Record
        FProcessIndex      : Integer;   
        FState             : TFileOp;
        FLeftFilename      : String;
        FLeftAttr          : Cardinal;
        FLeftSize          : Int64;
        FLeftDate          : TDateTime;
        FLeftFileIndex     : INteger;
        FRightFilename     : String;
        FRightAttr         : Cardinal;
        FRightSize         : Int64;
        FRightDate         : TDateTime;
        FRightFileIndex     : INteger;
        FLeftFullFileName  : String;
        FRightFullFileName : String;
      End;
      (** A poiner to be above file list node structure **)
      PFSFileListNode = ^TFSFileListNode;
  Strict Private
    FFolders         : TFolders;
    FExclusions      : String;
    FIconFiles       : TStringList;
    FProgressForm    : TfrmProgress;
    FRootKey         : String;
    FParams          : TStringList;
    FCompareEXE      : String;
    FFldrSyncOptions : TFldrSyncOptions;
    FAutoProcessing  : Boolean;
    FCloseTimer      : TTimer;
    FSyncModule      : TCompareFoldersCollection;
    FProgressSection : Integer;
    FTaskbarList     : ITaskbarList;
    FTaskbarList3    : ITaskbarList3;
    FStartTimer      : TTimer;
    FDeleteForm      : TfrmDeleteProgress;
    FCopyForm        : TfrmCopyProgress;
    FDialogueBottom  : Integer;
    FInterfaceFonts  : TInterfaceFontInfo;
    FFileOpFonts     : TFileOperationFontInfo;
    FLastFolder      : String;
    FTotalSize       : Int64;
    FFileOpStats     : TFileOpStats;
    FCopyDlgWidth   : Integer;
    FDeleteDlgWidth  : Integer;
    FConfirmDlgWidth : Integer;
    Procedure LoadSettings();
    Procedure SaveSettings();
    Procedure UpgradeINIFolderOptions(Const iniMemFile : TMemIniFile);
    Procedure ApplicationHint(Sender: TObject);
    Function GetImageIndex(Const strFileName: String): NativeInt;
    Procedure FixUpPanes;
    Procedure InsertListItem(Const iProcessItem : Integer; Const ProcessItem : TProcessItem);
    Procedure SetFileOperation(Const FileOp: TFileOp);
    Function CheckFolders: Boolean;
    Procedure ExceptionProc(Const strExceptionMsg: String);
    Procedure CloseTimerEvent(Sender: TObject);
    Procedure SearchStartProc(Const strFolder: String);
    Procedure SearchProc(Const strFolder, strFileName: String; Const iCount: Integer;
      Const Update : TUpdateType);
    Procedure SearchEndProc(Const iFileCount: Integer; Const iTotalSize: int64);
    Procedure CompareStartProc(Const strLeftFldr, strRightFldr: String);
    Procedure CompareProc(Const strLeftFldr, strRightFldr, strFileName: String;
      Const iPosition, iMaxItems: Integer);
    Procedure CompareEndProc;
    Procedure MatchListStartProc;
    Procedure MatchListProc(Const iPosition, iMaxItems: Integer);
    Procedure MatchListEndProc;
    Procedure DeleteStartProc(Const iFileCount: Integer; Const iTotalSize: int64);
    Procedure DeletingProc(Const iCurrentFileToDelete, iTotalFilesToDelete : Integer;
      Const iCumulativeFileSizeBeforeDelete, iTotalFileSizeToDelete: Int64;
      Const strDeletePath, strFileNameToDelete: String);
    Procedure DeletedProc(Const iCurrentFileToDeleted, iTotalFilesToDelete: Integer;
      Const iCumulativeFileSizeAfterDelete, iTotalFileSizeToDelete: Int64;
      Const iSuccess : TProcessSuccess);
    Procedure DeleteQueryProc(Const strFilePath: String; Const DeleteFile : TFileRecord;
      Var Option: TFileAction);
    Procedure DeleteReadOnlyQueryProc(Const strFilePath: String; Const DeleteFile : TFileRecord;
      Var Option: TFileAction);
    Procedure DeleteEndProc(Const iDeleted, iSkipped, iErrors: Integer);
    Procedure CopyStartProc(Const iTotalCount: Integer; Const iTotalSize: int64);
    Procedure CopyContentsProc(Const iCurrentFileToCopy, iTotalFilesToCopy : Integer;
      Const iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy, iCurrentFileCopiedSizeSoFar,
      iTotalCurrentFileSize: Int64);
    Procedure CopyingProc(Const iCurrentFileToCopy, iTotalFilesToCopy : Integer;
      Const iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy: Int64; Const strSource, strDest,
      strFileName: String);
    Procedure CopiedProc(Const iCurrentFileToCopy, iTotalFilesToCopy: Integer;
      Const iCumulativeFileSizeAfterCopy, iTotalFileSizeToCopy: Int64;
      Const iSuccess : TProcessSuccess);
    Procedure CopyQueryProc(Const strSourcePath, strDestPath: String; Const SourceFile,
      DestFile : TFileRecord; Var Option: TFileAction);
    Procedure CopyReadOnlyQueryProc(Const strSourcePath, strDestPath: String; Const SourceFile,
      DestFile : TFileRecord;  Var Option: TFileAction);
    Procedure CopyEndProc(Const iCopied, iSkipped, iError: Integer);
    Procedure FileQuery(Const strMsg, strConsoleMsg, strSourcePath, strDestPath: String;
      Const SourceFile, DestFile : TFileRecord; Var Option: TFileAction; Const boolReadOnly : Boolean);
    Procedure DiffSizeStart(Const iFileCount : Integer);
    Procedure DiffSize(Const iFile, iFileCount: Integer; Const strLPath, strRPath,
      strFileName: String);
    Procedure DiffSizeEnd();
    Procedure NothingToDoStart(Const iFileCount : Integer);
    Procedure NothingToDo(Const iFile, iFileCount: Integer; Const strLPath, strRPath,
      strFileName: String);
    Procedure NothingToDoEnd();
    Procedure ExceedsSizeLimitStart(Const iFileCount : Integer);
    Procedure ExceedsSizeLimit(Const iFile, iFileCount: Integer; Const strLPath, strRPath,
      strFileName: String);
    Procedure ExceedsSizeLimitEnd();
    Procedure ErrorMessageStart(Const iFileCount : Integer);
    Procedure ErrorMessage(Const strErrorMsg : String);
    Procedure ErrorMessageEnd();
    Procedure DeleteFoldersStart(Const iFolderCount : Integer);
    Procedure DeleteFolders(Const iFolder, iFolders : Integer; Const strFolder : String);
    Procedure DeleteFoldersEnd();
    Procedure CopyError(Const strSource, strDest, strErrorMsg : String;
      Const iLastError : Cardinal; var iResult : TDGHErrorResult);
    Procedure DeleteError(Const strSource, strErrorMsg : String;
      Const iLastError : Cardinal; var iResult : TDGHErrorResult);
    Procedure UpdateTaskBar(Const ProgressType: TTaskbarProgressType; Const iPosition: Integer = 0;
      Const iMaxPosition: Integer = 100);
    Procedure UpdateProgress(Const iPosition, iMaxPosition: Integer);
    Procedure StartTimerEvent(Sender: TObject);
    Procedure OutputResult(Const strMsg: String = '');
    Procedure OutputResultLn(Const strMsg: String = '');
    Procedure DisableActions;
    Procedure EnableActions(Const boolSuccess: Boolean);
    Procedure LogSize;
    Procedure OutputStats;
    Procedure MemoryPopupMenu(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    Procedure OutputFileNumber(Const iCurrentFile, iTotal : Integer);
  End;

  (** This is a custon exception for folders not found or created. **)
  TFolderNotFoundException = Class(Exception);

  (** An enumerate to define the Virtual string tree fields of data. **)
  TFSVTVFields = (
    vfState,
    vfLeftFileName,
    vfLeftAttr,
    vfLeftSize,
    vfLeftDate,
    vfRightFilename,
    vfRightAttr,
    vfRightSize,
    vfRightDate
  );

Var
    (** A global variable used by Delphis auto create form process. **)
  frmMainForm: TfrmMainForm;

Implementation

Uses
  CodeSiteLogging,
  {$IFDEF PROFILECODE}
  Profiler,
  {$ENDIF}
  FileCtrl,
  ShellAPI,
  AboutForm,
  ConfirmationDlg,
  Themes,
  MemoryMonitorOptionsForm,
  ProcessingErrorForm,
  {$IFDEF EUREKALOG_VER7}
  ExceptionLog7,
  EExceptionManager,
  {$ENDIF}
  UITypes,
  Types,
  ApplicationFunctions,
  TypInfo,
  StrUtils,
  FldrSync.Functions;

{$R *.DFM}

Type
  (** A record for convert an Int64 into Word portions for storage in the INI file
      (INI file can only store signed DWORDS) **)
  TInt64Ex = Record 
    Case Integer Of
      1: (Value : Int64);
      2: (
        iFirst  : Word;
        iSecond : Word;
        iThird  : Word;
        iFourth : Word;
      );
  End;

(**

  This method returns a string representation of a files attributes [RASH].

  @precon  None.
  @postcon Returns a string representation of a files attributes [RASH].

  @param   iAttributes as a Cardinal as a constant
  @return  a String

**)
Function GetAttributeString(Const iAttributes: Cardinal): String;

Type
  TFSFileAttr = (
    faReadOnly,
    faArchive,
    faSystem,
    faHidden,
    faDirectory,
    faDevice,
    faNormal,
    faTemporary,
    faSparseFile,
    faReparsePoint,
    faCompressed,
    faOffLine,
    faNotIndexed,
    faEncrypted,
    faVirtual
  );
  TFSAttrRec = Record
    FAttribute : Integer;
    FChar      : Char;
  End;

Const
  Attributes : Array[Low(TFSFileAttr)..High(TFSFileAttr)] Of TFSAttrRec = (
    (FAttribute: FILE_ATTRIBUTE_READONLY;              FChar: 'R'),
    (FAttribute: FILE_ATTRIBUTE_ARCHIVE;               FChar: 'A'),
    (FAttribute: FILE_ATTRIBUTE_SYSTEM;                FChar: 'S'),
    (FAttribute: FILE_ATTRIBUTE_HIDDEN;                FChar: 'H'),
    (FAttribute: FILE_ATTRIBUTE_DIRECTORY;             FChar: 'D'),
    (FAttribute: FILE_ATTRIBUTE_DEVICE;                FChar: 'd'),
    (FAttribute: FILE_ATTRIBUTE_NORMAL;                FChar: 'N'),
    (FAttribute: FILE_ATTRIBUTE_TEMPORARY;             FChar: 'T'),
    (FAttribute: FILE_ATTRIBUTE_SPARSE_FILE;           FChar: 'S'),
    (FAttribute: FILE_ATTRIBUTE_REPARSE_POINT;         FChar: 'P'),
    (FAttribute: FILE_ATTRIBUTE_COMPRESSED;            FChar: 'C'),
    (FAttribute: FILE_ATTRIBUTE_OFFLINE;               FChar: 'O'),
    (FAttribute: FILE_ATTRIBUTE_NOT_CONTENT_INDEXED;   FChar: 'I'),
    (FAttribute: FILE_ATTRIBUTE_ENCRYPTED;             FChar: 'E'),
    (FAttribute: FILE_ATTRIBUTE_VIRTUAL;               FChar: 'V')
  );

Var
  iAttr: TFSFileAttr;

Begin
  SetLength(Result, 1 + Integer(High(TFSFileAttr)) - Integer(Low(TFSFileAttr)));
  For iAttr := Low(TFSFileAttr) To High(TFSFileAttr) Do
    If Attributes[iAttr].FAttribute And iAttributes <> 0 Then
      Result[Succ(Integer(iAttr))] := Attributes[iAttr].FChar
    Else
      Result[Succ(Integer(iAttr))] := '.';
  Result := '[' + Result + ']';
End;

(**

  This is an on execute event handler for the Clear Log action.

  @precon  None.
  @postcon Clears the log.

  @param   Sender as a TObject

**)
procedure TfrmMainForm.actEditClearLogExecute(Sender: TObject);

begin
  If redtOutputResults.SelLength > 0 Then
    redtOutputResults.SelText := ''
  Else
    redtOutputResults.Clear;
end;

(**

  This is an on execute event handler for the CopyLeftToRight Action.

  @precon  None.
  @postcon Sets the selected items in the list view to be Copied from Left to
           Right.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actEditCopyLeftToRightExecute(Sender: TObject);

Begin
  SetFileOperation(foLeftToRight);
End;

(**

  This is an on execute event handler for the CopyRightToLeft action.

  @precon  None.
  @postcon Sets the selected items in the list view to be Copies from Right
           to Left.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actEditCopyRightToLeftExecute(Sender: TObject);

Begin
  SetFileOperation(foRightToLeft);
End;

(**

  This is an on execute event handler for the Delete action.

  @precon  None.
  @postcon Sets the selected items in the list view to be Deleted.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actEditDeleteExecute(Sender: TObject);

Begin
  SetFileOperation(foDelete);
End;

(**

  This is an on execute event handler for the DoNothing action.

  @precon  None.
  @postcon Sets the selected items in the listview to DoNothing.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actEditDoNothingExecute(Sender: TObject);

Begin
  SetFileOperation(foNothing);
End;

(**

  This is an on update event handler for the Fiole Operation actions.

  @precon  None.
  @postcon Enables / disables the file operations based on the opertation and the selected
           files.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actEditFileOperationsUpdate(Sender: TObject);

Var
  boolEnabled : Boolean;
  Node: PVirtualNode;
  NodeData : PFSFileListNode;
  Action: TAction;
  
Begin
  If Sender Is TAction Then
    Begin
      Action := Sender As TAction;
      boolEnabled := vstFileList.SelectedCount > 0;
      Node := vstFileList.GetFirstSelected;
      While Assigned(Node) Do
        Begin
          NodeData := vstFileList.GetNodeData(Node);
          boolEnabled := boolEnabled And (Integer(NodeData.FState) <> Action.Tag);
          Case TFileOp(Tag) Of
            foLeftToRight:
              If NodeData.FLeftFilename = '' Then
                boolEnabled := False;
            foRightToLeft:
              If NodeData.FRightFilename = '' Then
                boolEnabled := False;
          End;
          Node := vstFileList.GetNextSelected(Node);
        End; 
      Action.Enabled := boolEnabled;
    End;
End;

(**

  This is an on execute event handler for the Select All action.

  @precon  None.
  @postcon Selects all the items in the list.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actEditSelectAllExecute(Sender: TObject);

Begin
  vstFileList.SelectAll(True);
End;

(**

  This is an on execute method for the File Compare action.

  @precon  None.
  @postcon Start the process of comparing files and folders and output the
           results in the list view.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actFileCompareExecute(Sender: TObject);

Var
  iEnabledFolders: Integer;
  i              : Integer;
  boolSuccess    : Boolean;
  iCount         : Integer;
  NodeData       : PFSFileListNode;
  Node: PVirtualNode;

Begin
  OutputResultLn('Comparison started @ ' + FormatDateTime('dddd dd mmmm yyyy hh:mm:ss',
    Now()));
  boolSuccess := False;
  If Not CheckFolders Then
    Exit;
  Try
    DisableActions;
    Try
      iEnabledFolders := 0;
      For i := 0 To FFolders.Count - 1 Do
        Begin
          If [soEnabled, soTempDisabled] * FFolders.Folder[i].SyncOptions = [soEnabled] Then
            Inc(iEnabledFolders);
        End;
      FProgressForm.RegisterSections(iEnabledFolders * 3 + 2);
      FProgressSection := -1;
      vstFileList.BeginUpdate;
      Try
        vstFileList.Clear;
      Finally
        vstFileList.EndUpdate;
      End;
      Try
        FSyncModule.Clear;
        boolSuccess := FSyncModule.ProcessFolders(FFolders, FExclusions);
        FixUpPanes;
        FormResize(Sender);
        iCount := 0;
        Node := vstFileList.GetFirst;
        While Assigned(Node) Do
          Begin
            NodeData := vstFileList.GetNodeData(Node);
            If Integer(NodeData.FState) <> Integer(fofExceedsSizeLimit) Then
              Inc(iCount);
            Node := vstFileList.GetNext(Node);
          End;
        If (fsoCloseIFNoFilesAfterComparison In FFldrSyncOptions) And (iCount = 0) Then
          Begin
            FProgressForm.InitialiseSection(FProgressSection, 0, 1);
            FProgressForm.Progress(FProgressSection, 1, 'Auto-exiting application...',
              'as there are no more files to process...');
            FCloseTimer.Enabled := True;
          End;
        If (fsoStartProcessingAutomatically In FFldrSyncOptions) And
          Not FAutoProcessing Then
          Try
            FAutoProcessing := True;
            actFileProcessFilesExecute(Sender);
          Finally
            FAutoProcessing := False;
          End;
      Finally
        If Not boolSuccess Then
          Begin
            OutputResultLn;
            OutputResultLn('Comparison cancelled by user!');
          End;
        UpdateTaskBar(ptNone);
        If Not FCloseTimer.Enabled Then
          FProgressForm.Hide;
        OutputStats;
      End;
    Finally
      EnableActions(boolSuccess);
    End;
  Finally
    OutputResultLn();
  End;
End;

(**

  This method is an Action List Execute method.

  @precon  None.
  @postcon Closes the application and returns to the OS.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actFileExitExecute(Sender: TObject);

Begin
  Close();
End;

(**

  This a an on execute event handler for the Process Files action.

  @precon  None.
  @postcon Firstly, it deletes any files need to be deleted from the list, then
           copies any files that need to be copied and finally refreshes the
           information in the list view.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actFileProcessFilesExecute(Sender: TObject);

Var
  boolAbort : Boolean;
  
Begin
  If TfrmDiskSpace.Execute(FSyncModule) Then
    Begin
      OutputResultLn('Processing started @ ' + FormatDateTime('dddd dd mmmm yyyy hh:mm:ss',
        Now()));
      Try
        boolAbort := False;
        DisableActions;
        Try
          Try
            FSyncModule.ProcessFiles(FFldrSyncOptions);
          Except
            On E : EAbort Do
              boolAbort := True;
          End;
        Finally
          EnableActions(True);
        End;
        actFileProcessFiles.Enabled := Not boolAbort
      Finally
        OutputResultLn();
      End;
      If Not boolAbort Then
        actFileCompareExecute(Self);
    End;
End;

(**

  This is an on execute event hanlder for the Help About action.

  @precon  None.
  @postcon Displays the about dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actHelpAboutExecute(Sender: TObject);

Begin
  TfrmAboutDialogue.ShowAbout(Self);
End;

(**

  This is an on execute event handler for the Help Contents action.

  @precon  None.
  @postcon Displays the Contents and Welcome page of the HTML Help.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actHelpContentsExecute(Sender: TObject);

Begin
  Application.HelpShowTableOfContents;
End;

(**

  This is an on execute event handler for the Tools Compare action.

  @precon  None.
  @postcon Opens the 2 selected files in the comparison tool.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actToolsCompareExecute(Sender: TObject);

Var
  NodeData : PFSFileListNode;

Begin
  NodeData := vstFileList.GetNodeData(vstFileList.FocusedNode);
  ShellExecute(Application.Handle, 'Open', PChar(FCompareEXE), PChar(Format('"%s" "%s"', [
    NodeData.FLeftFullFileName,
    NodeData.FRightFullFileName
  ])), PChar(ExtractFilePath(FCompareEXE)), SW_SHOWNORMAL)
End;

(**

  This is an on update event handler for the Tools Compare action.

  @precon  None.
  @postcon Enables the option only if the EXE exists and the 2 files to compare
           also exist.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actToolsCompareUpdate(Sender: TObject);

Var
  boolEnabled: Boolean;
  NodeData : PFSFileListNode;

Begin
  boolEnabled := Assigned(vstFileList.FocusedNode);
  If boolEnabled Then
    Begin
      NodeData := vstFileList.GetNodeData(vstFileList.FocusedNode);
    boolEnabled := boolEnabled
      And FileExists(NodeData.FLeftFullFileName)
      And FileExists(NodeData.FRightFullFileName);
    End;
  (Sender As TAction).Enabled := boolEnabled;
End;

(**

  This is an on execute event handler for the ConfigMemMon action.

  @precon  None.
  @postcon Displays a configuration form that allows you to edit the colours, fonts and
           positions of the memory monitor information.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actToolsConfigMemMonExecute(Sender: TObject);

Begin
  TfrmMemoryMonitorOptions.Execute(DGHMemoryMonitor);
End;

(**

  This is an on execute event handler for the Tools Options action.

  @precon  None.
  @postcon Displays the options dialogue for the application.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.actToolsOptionsExecute(Sender: TObject);

Var
  strTheme : String;
  
Begin
  FInterfaceFonts[ifTableFont].FFontName := vstFileList.Font.Name;
  FInterfaceFonts[ifTableFont].FFontSize :=vstFileList.Font.Size;
  FInterfaceFonts[ifLogFont].FFontName := redtOutputResults.Font.Name;
  FInterfaceFonts[ifLogFont].FFontSize := redtOutputResults.Font.Size;
  If TfrmOptions.Execute(FFolders, FExclusions, FCompareEXE, FRootKey, FInterfaceFonts,
    FFileOpFonts, FFldrSyncOptions, strTheme, FFileOpStats) Then
    Begin
      vstFileList.Font.Name := FInterfaceFonts[ifTableFont].FFontName;
      vstFileList.Font.Size:= FInterfaceFonts[ifTableFont].FFontSize;
      redtOutputResults.Font.Name := FInterfaceFonts[ifLogFont].FFontName;
      redtOutputResults.Font.Size := FInterfaceFonts[ifLogFont].FFontSize;
      If CompareText(strTheme, TStyleManager.ActiveStyle.Name) <> 0 Then
        TStyleManager.SetStyle(strTheme);
      OutputStats;
      actFileCompareExecute(Self);
    End;
End;

(**

  This is an application on exception event handler. It displays a dialogue box
  containing the message of the exception.

  @precon  None.
  @postcon Displays the exception message.

  @param   Sender as a TObject
  @param   E      as an Exception

**)
Procedure TfrmMainForm.appEventsException(Sender: TObject; E: Exception);

Begin
  MessageDlg('Exception: ' + E.Message, mtError, [mbOK], 0);
End;

(**

  This is the applications on hint event handler.

  @precon  None.
  @postcon Updates the application status bar when menu to toolbar items are
           used.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.ApplicationHint(Sender: TObject);

Begin
  stbrStatusBar.SimplePanel := Application.Hint <> '';
  If Application.Hint <> '' Then
    stbrStatusBar.SimpleText := GetLongHint(Application.Hint);
End;

(**

  This method checks that all the folders exist and provides an option to create
  them if they don`t. If it can not find a directory and can not create the
  directory the function returns false else success is indicated by returning
  True.

  @precon  None.
  @postcon Check the existence of the directories in the folders string list.
           Returns true if they exist or are created else returns false.

  @return  a Boolean

**)
Function TfrmMainForm.CheckFolders: Boolean;

  (**

    This is a local method to check and confirm the creation of a folder. It raises an exception if the 
    folder could not be created. The drive of the folder mapping is returned in the strDrive var 
    parameter.

    @precon  None.
    @postcon Checks and confirms the creation of a folder. It raises an exception if the folder could 
             not be created.

    @param   strFolder as a String as a constant
    @param   strDrive  as a String as a reference
    @return  a Boolean

  **)
  Function CheckAndCreateFolder(Const strFolder: String;  var strDrive : String) : Boolean;

  Const
    strExcepMsg  = 'Could not create the folder "%s".';
    strCreateMsg = 'The folder "%s" does not exist. Would you like to create ' +
      'this folder?';
    strDriveMsg = 'The drive "%s" for directory "%s" does not exist. Would you like ' +
      'to ignore or cancel?';

  Var
    strPath: String;

  Begin
    Result := False;
    strDrive := ExtractFileDrive(strFolder);
    strPath := ExtractFilePath(strFolder);
    If Not System.SysUtils.DirectoryExists(strDrive + '\') Then
      Case MessageDlg(Format(strDriveMsg, [strDrive, strPath]), mtConfirmation,
        [mbIgnore, mbCancel], 0) Of
        mrIgnore:
          Begin
            Result := True;
            Exit;
          End;
        mrCancel: Abort;
      End;
    If Not System.SysUtils.DirectoryExists(strPath) Then
      Case MessageDlg(Format(strCreateMsg, [strPath]), mtConfirmation,
        [mbYes, mbNo, mbCancel], 0) Of
        mrYes:
          If Not System.SysUtils.ForceDirectories(strPath) Then
            Raise TFolderNotFoundException.CreateFmt(strExcepMsg, [strPath]);
        mrIgnore: Result := True;
        mrCancel: Abort;
      End;
  End;

  (**

    This method disabled all folder pairs with the given drive mapping.

    @precon  None.
    @postcon Disabled all folder pairs with the given drive mapping.

    @param   strDrive as a String as a constant

  **)
  Procedure DisableDrive(Const strDrive : String);

  Var
    i : Integer;
    F : TFolder;
    
  Begin
    For i := 0 To FFolders.Count - 1 Do
      Begin
        F := FFolders.Folder[i];
        If (CompareText(Copy(F.LeftFldr, 1, Length(strDrive)), strDrive) = 0) Or
           (CompareText(Copy(F.RightFldr, 1, Length(strDrive)), strDrive) = 0) Then
          F.SyncOptions := F.SyncOptions + [soTempDisabled];
      End;
  End;

Var
  i: Integer;
  strDrive: String;

Begin
  For i := 0 To FFolders.Count - 1 Do
    If [soEnabled, soTempDisabled] * FFolders.Folder[i].SyncOptions = [soEnabled] Then
      Begin
        If CheckAndCreateFolder(FFolders.Folder[i].LeftFldr, strDrive) Then
          Begin
            DisableDrive(strDrive);
            Continue;
          End;
        If CheckAndCreateFolder(FFolders.Folder[i].RightFldr, strDrive) Then
          DisableDrive(strDrive);
      End Else
      Begin
        If soTempDisabled In FFolders.Folder[i].SyncOptions Then
          Begin
            If System.SysUtils.DirectoryExists(ExtractFileDrive(FFolders.Folder[i].LeftFldr) + '\') And
             System.SysUtils.DirectoryExists(ExtractFileDrive(FFolders.Folder[i].RightFldr) + '\') Then
             FFolders.Folder[i].SyncOptions := FFolders.Folder[i].SyncOptions - [soTempDisabled];
          End;
      End;
  Result := True;
End;

(**

  This is an on timer event handler.

  @precon  None.
  @postcon Closes the application.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.CloseTimerEvent(Sender: TObject);

Begin
  Close;
End;

(**

  This is an on compare end event handler for the sync module.

  @precon  None.
  @postcon None.

**)
Procedure TfrmMainForm.CompareEndProc;

Begin
  FProgressForm.Progress(FProgressSection, 100, 'Done.', '');
  OutputResultLn('Done.');
End;

(**

  This is an on compare event handler for the sync module.

  @precon  None.
  @postcon Updates the progress dialogue with the current progress through the comparison process.

  @nohint  strLeftFldr strRightFldr

  @param   strLeftFldr  as a String as a constant
  @param   strRightFldr as a String as a constant
  @param   strFileName  as a String as a constant
  @param   iPosition    as an Integer as a constant
  @param   iMaxItems    as an Integer as a constant

**)
Procedure TfrmMainForm.CompareProc(Const strLeftFldr, strRightFldr, strFileName: String;
  Const iPosition, iMaxItems: Integer);

Begin
  If iPosition Mod 10 = 0 Then
    FProgressForm.Progress(FProgressSection, Trunc(iPosition / iMaxItems * 100.0),
      Format('Comparing... %1.0n in %1.0n', [Int(iPosition), Int(iMaxItems)]),
      strFileName);
End;

(**

  This is an on compare start event handler for the sync module.

  @precon  None.
  @postcon Increments the progress section and initialises the progress dialogue.

  @param   strLeftFldr  as a String as a constant
  @param   strRightFldr as a String as a constant

**)
Procedure TfrmMainForm.CompareStartProc(Const strLeftFldr, strRightFldr: String);

Begin
  OutputResult(Format('Comparing: ', [strLeftFldr, strRightFldr]));
  Inc(FProgressSection);
  FProgressForm.InitialiseSection(FProgressSection, 0, 100);
End;

(**

  This is an on copied event handler for the sync module.

  @precon  None.
  @postcon Updates the copy progress or if an error outputs the error message.

  @nohint  iCurrentFileToCopy iTotalFilesToCopy

  @param   iCurrentFileToCopy           as an Integer as a constant
  @param   iTotalFilesToCopy            as an Integer as a constant
  @param   iCumulativeFileSizeAfterCopy as an Int64 as a constant
  @param   iTotalFileSizeToCopy         as an Int64 as a constant
  @param   iSuccess                     as a TProcessSuccess as a constant

**)
Procedure TfrmMainForm.CopiedProc(Const iCurrentFileToCopy, iTotalFilesToCopy: Integer;
    Const iCumulativeFileSizeAfterCopy, iTotalFileSizeToCopy: Int64;
    Const iSuccess : TProcessSuccess);

Begin
  FCopyForm.IndividualProgress(0, 0 ,iCumulativeFileSizeAfterCopy, iTotalFileSizeToCopy);
  If FTotalSize = 0 Then
    Inc(FTotalSize);
  Case iSuccess Of
    psSuccessed: OutputResultLn();
    psFailed: OutputResultLn(
      Format(' Error Copying file (error type ignored [%s]).', [FSyncModule.LastError]));
    //psIgnored: OutputResultLn(' Ignored Copying file.');
  End;
End;

(**

  This is an on copy contents event handler for the sync module.

  @precon  None.
  @postcon Updates the progress of the individual file copy.

  @nohint  iCurrentFileToCopy iTotalFilesToCopy

  @param   iCurrentFileToCopy            as an Integer as a constant
  @param   iTotalFilesToCopy             as an Integer as a constant
  @param   iCumulativeFileSizeBeforeCopy as an Int64 as a constant
  @param   iTotalFileSizeToCopy          as an Int64 as a constant
  @param   iCurrentFileCopiedSizeSoFar   as an Int64 as a constant
  @param   iTotalCurrentFileSize         as an Int64 as a constant

**)
Procedure TfrmMainForm.CopyContentsProc(Const iCurrentFileToCopy, iTotalFilesToCopy : Integer;
    Const iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy, iCurrentFileCopiedSizeSoFar,
    iTotalCurrentFileSize: Int64);

Begin
  FCopyForm.IndividualProgress(iCurrentFileCopiedSizeSoFar, iTotalCurrentFileSize,
    iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy);
End;

(**

  This is an on copy end event handler for the sync Module.

  @precon  None.
  @postcon Outputs the number of files copied, skipped, etc and frees the copy dialogue.

  @param   iCopied  as an Integer as a constant
  @param   iSkipped as an Integer as a constant
  @param   iError   as an Integer as a constant

**)
Procedure TfrmMainForm.CopyEndProc(Const iCopied, iSkipped, iError: Integer);

Begin
  OutputResult(Format('  Copied %1.0n (Skipped %1.0n file(s)',
    [Int(iCopied), Int(iSkipped)]));
  If iError > 0 Then
    OutputResult(Format(', Errored %1.0n file(s)', [Int(iError)]));
  OutputResultLn(')');
  If FCopyForm <> Nil Then
    FCopyDlgWidth := FCopyForm.Width;
  FreeAndNil(FCopyForm);
End;

(**

  This is an on error copting event handler for the processing of the files.

  @precon  None.
  @postcon Displays the error message on the screen and asks the user what to do.

  @param   strSource   as a String as a constant
  @param   strDest     as a String as a constant
  @param   strErrorMsg as a String as a constant
  @param   iLastError  as a Cardinal as a constant
  @param   iResult     as a TDGHErrorResult as a reference

**)
Procedure TfrmMainForm.CopyError(Const strSource, strDest, strErrorMsg: String;
  Const iLastError: Cardinal; Var iResult: TDGHErrorResult);

Begin
  UpdateTaskBar(ptError, 0, 1);
  OutputResultLn();
  OutputResultLn('    An error has occurred during the copying of files:');
  OutputResultLn(Format('      Source     : %s', [strSource]));
  OutputResultLn(Format('      Destination: %s', [strDest]));
  OutputResultLn(Format('      OS Error   : (%d) %s', [iLastError, strErrorMsg]));
  OutputResult('    Do you want to [I]gnore Once, Ignore [A]ll the errors or [S]top processing? ');
  Case TfrmErrorDlg.Execute('An error has occurred during the copying of files', 
    strSource, strDest, Format('(%d) %s', [iLastError, strErrorMsg])) Of
    mrIgnore:
      Begin
        iResult := derIgnoreOnce;
        OutputResultLn('I');
      End;
    mrYesToAll:
      Begin
        iResult := derIgnoreAll;
        OutputResultLn('A');
      End;
    mrAbort:
      Begin
        iResult := derStop;
        OutputResultLn('S');
      End;
  End;
End;

(**

  This is an on copying event handler for the sync module.

  @precon  None.
  @postcon Updates the copy dialogue.

  @nohint  iCumulativeFileSizeBeforeCopy iTotalFileSizeToCopy

  @param   iCurrentFileToCopy            as an Integer as a constant
  @param   iTotalFilesToCopy             as an Integer as a constant
  @param   iCumulativeFileSizeBeforeCopy as an Int64 as a constant
  @param   iTotalFileSizeToCopy          as an Int64 as a constant
  @param   strSource                     as a String as a constant
  @param   strDest                       as a String as a constant
  @param   strFileName                   as a String as a constant

**)
Procedure TfrmMainForm.CopyingProc(Const iCurrentFileToCopy, iTotalFilesToCopy : Integer;
    Const iCumulativeFileSizeBeforeCopy, iTotalFileSizeToCopy: Int64; Const strSource, strDest,
    strFileName: String);

Begin
  FCopyForm.Progress(
    iCurrentFileToCopy,
    iTotalFilesToCopy,
    ExtractFilePath(strSource + strFileName),
    ExtractFilePath(strDest + strFileName),
    ExtractFileName(strFileName)
  );
  OutputFileNumber(iCurrentFileToCopy, iTotalFilesToCopy);
  OutputResult(Format('%s => %s%s', [strSource, strDest, strFilename]));
End;

(**

  This is an on copy query event handler for the sync module.

  @precon  None.
  @postcon Displays a confirmation dialogue to the user to ask whether the file should be overwritten.

  @param   strSourcePath as a String as a constant
  @param   strDestPath   as a String as a constant
  @param   SourceFile    as a TFileRecord as a constant
  @param   DestFile      as a TFileRecord as a constant
  @param   Option        as a TFileAction as a reference

**)
Procedure TfrmMainForm.CopyQueryProc(Const strSourcePath, strDestPath: String;
  Const SourceFile, DestFile : TFileRecord; Var Option: TFileAction);

Const
  strMsg = 'Are you sure you want to overwrite the following file?';
  strConsoleMsg = ' Overwrite (Y/N/A/O/C)? ';

Begin
  FileQuery(strMsg, strConsoleMsg,strSourcePath, strDestPath, SourceFile, DestFile,
    Option, False);
End;

(**

  This is an on copy read only query event handler for the sync module.

  @precon  None.
  @postcon Displays a confirmation dialogue to the user to ask whether the read only file should be 
           overwritten.

  @param   strSourcePath as a String as a constant
  @param   strDestPath   as a String as a constant
  @param   SourceFile    as a TFileRecord as a constant
  @param   DestFile      as a TFileRecord as a constant
  @param   Option        as a TFileAction as a reference

**)
Procedure TfrmMainForm.CopyReadOnlyQueryProc(Const strSourcePath, strDestPath: String;
  Const SourceFile, DestFile : TFileRecord; Var Option: TFileAction);

Const
  strMsg = 'Are you sure you want to overwrite the following READ-ONLY file?';
  strConsoleMsg = ' Overwrite READONLY (Y/N/A/O/C)? ';

Begin
  FileQuery(strMsg, strConsoleMsg, strSourcePath, strDestPath, SourceFile, DestFile,
    Option, True);
End;

(**

  This is an on copy start event handler for the sync module.

  @precon  None.
  @postcon Initialises the copy progress dialogue.

  @param   iTotalCount as an Integer as a constant
  @param   iTotalSize  as an int64 as a constant

**)
Procedure TfrmMainForm.CopyStartProc(Const iTotalCount: Integer; Const iTotalSize: int64);

Begin
  FTotalSize := iTotalSize;
  OutputResultLn(Format('Copying %1.0n files (%1.0n bytes)',
      [Int(iTotalCount), Int(iTotalSize)]));
  If iTotalCount > 0 Then
    Begin
      FCopyForm                  := TfrmCopyProgress.Create(Self);
      FCopyForm.OnUpdateProgress := UpdateProgress;
      FCopyForm.Initialise(iTotalCount, iTotalSize, FCopyDlgWidth);
      FDialogueBottom := FCopyForm.Top + FCopyForm.Height;
    End;
End;

(**

  This is an on deleted event handler for the sync module.

  @precon  None.
  @postcon Updates the progress dialogue is successful or outputs an error message.

  @nohint  iCurrentFileToDeleted iTotalFilesToDelete

  @param   iCurrentFileToDeleted          as an Integer as a constant
  @param   iTotalFilesToDelete            as an Integer as a constant
  @param   iCumulativeFileSizeAfterDelete as an Int64 as a constant
  @param   iTotalFileSizeToDelete         as an Int64 as a constant
  @param   iSuccess                       as a TProcessSuccess as a constant

**)
Procedure TfrmMainForm.DeletedProc(Const iCurrentFileToDeleted, iTotalFilesToDelete: Integer;
    Const iCumulativeFileSizeAfterDelete, iTotalFileSizeToDelete: Int64;
    Const iSuccess : TProcessSuccess);

Begin
  FDeleteForm.Progress(iCumulativeFileSizeAfterDelete, iTotalFileSizeToDelete);
  If FTotalSize = 0 Then
    Inc(FTotalSize);
  Case iSuccess Of
    psSuccessed: OutputResultLn();
    psFailed: OutputResultLn(
      Format(' Error Deleting file (error type ignored [%s]).', [FSyncModule.LastError]));
    //psIgnored: OutputResultLn(' Ignored Copying file.');
  End;
End;

(**

  This is an on delete end event handler for the sync module.

  @precon  None.
  @postcon Frees the deletion progress form.

  @param   iDeleted as an Integer as a constant
  @param   iSkipped as an Integer as a constant
  @param   iErrors  as an Integer as a constant

**)
Procedure TfrmMainForm.DeleteEndProc(Const iDeleted, iSkipped, iErrors: Integer);

Begin
  OutputResult(Format('  Deleted %1.0n file(s) (Skipped %1.0n file(s)', [Int(iDeleted),
    Int(iSkipped)]));
  If iErrors > 0 Then
    OutputResult(Format(', Errored %1.0n file(s)', [Int(iErrors)]));
  OutputResultLn(').');
  If FDeleteForm <> Nil Then
    FDeleteDlgWidth := FDeleteForm.Width;
  FreeAndNil(FDeleteForm);
End;

(**

  This is an on delete error event handler for the Sync Module.

  @precon  None.
  @postcon Displays the error message on the screen and asks the user what to do.

  @param   strSource   as a String as a constant
  @param   strErrorMsg as a String as a constant
  @param   iLastError  as a Cardinal as a constant
  @param   iResult     as a TDGHErrorResult as a reference

**)
Procedure TfrmMainForm.DeleteError(Const strSource, strErrorMsg: String; Const iLastError: Cardinal;
  Var iResult: TDGHErrorResult);

Begin
  UpdateTaskBar(ptError, 0, 1);
  OutputResultLn();
  OutputResultLn('    An error has occurred during the deleting of files:');
  OutputResultLn(Format('      Source     : %s', [strSource]));
  OutputResultLn(Format('      OS Error   : (%d) %s', [iLastError, strErrorMsg]));
  OutputResult('    Do you want to [I]gnore Once, Ignore [A]ll the errors or [S]top processing? ');
  Case TfrmErrorDlg.Execute('An error has occurred during the deleting of files',
    strSource, '', Format('(%d) %s', [iLastError, strErrorMsg])) Of
    mrIgnore:
      Begin
        iResult := derIgnoreOnce;
        OutputResultLn('I');
      End;
    mrYesToAll:
      Begin
        iResult := derIgnoreAll;
        OutputResultLn('A');
      End;
    mrAbort:
      Begin
        iResult := derStop;
        OutputResultLn('S');
      End;
  End;
End;

(**

  This is an on delete folders event handler.

  @precon  None.
  @postcon Outputs the folder to be deleted to the log and updates the progress.

  @param   iFolder   as an Integer as a constant
  @param   iFolders  as an Integer as a constant
  @param   strFolder as a String as a constant

**)
Procedure TfrmMainForm.DeleteFolders(Const iFolder, iFolders : Integer; Const strFolder: String);

Begin
  FDeleteForm.InitialiseFileName(dtFiles, iFolder, iFolders, strFolder);
  FDeleteForm.Progress(iFolder, iFolders);
  OutputFileNumber(iFolder, iFolders);
  OutputResultLn(strFolder);
End;

(**

  This is an on delete folders end event handler.

  @precon  None.
  @postcon Frees the memory used by the form.

**)
Procedure TfrmMainForm.DeleteFoldersEnd;

Begin
  If FDeleteForm <> Nil Then  
    FDeleteDlgWidth := FDeleteForm.Width;
  FreeAndNil(FDeleteForm);
End;

(**

  This is an on delete folders start event handler.

  @precon  None.
  @postcon Outputs the number of folders to be deleted to the log and initialises the delete form.

  @param   iFolderCount as an Integer as a constant

**)
Procedure TfrmMainForm.DeleteFoldersStart(Const iFolderCount: Integer);

Begin
  If iFolderCount > 0 Then
    Begin
      OutputResultLn('Deleting empty folders...');
      FDeleteForm                  := TfrmDeleteProgress.Create(Self);
      FDeleteForm.OnUpdateProgress := UpdateProgress;
      FDeleteForm.Initialise(dtFolders, iFolderCount, iFolderCount, FDeleteDlgWidth);
      FDialogueBottom := FDeleteForm.Top + FDeleteForm.Height;
    End;
End;

(**

  This method is an on delete query event handler for the sync module.

  @precon  None.
  @postcon Displays a confirmation dialogue to the user asking whether the file should be deleted.

  @param   strFilePath as a String as a constant
  @param   DeleteFile  as a TFileRecord as a constant
  @param   Option      as a TFileAction as a reference

**)
Procedure TfrmMainForm.DeleteQueryProc(Const strFilePath: String; Const DeleteFile : TFileRecord;
  Var Option: TFileAction);

Const
  strMsg = 'Are you sure you want to delete the file?';
  strConsoleMsg = ' Delete (Y/N/A/O/C)? ';

Begin
  FileQuery(strMsg, strConsoleMsg, strFilePath, '', DeleteFile, Nil, Option, False);
End;

(**

  This method is an on delete readonly query event handler for the sync module.

  @precon  None.
  @postcon Displays a confirmation dialogue to the user asking whether the read only file should be 
           deleted.

  @param   strFilePath as a String as a constant
  @param   DeleteFile  as a TFileRecord as a constant
  @param   Option      as a TFileAction as a reference

**)
Procedure TfrmMainForm.DeleteReadOnlyQueryProc(Const strFilePath: String;
  Const DeleteFile : TFileRecord; Var Option: TFileAction);

Const
  strMsg = 'Are you sure you want to delete the READ-ONLY file?';
  strConsoleMsg = ' Delete READONLY (Y/N/A/O/C)? ';

Begin
  FileQuery(strMsg, strConsoleMsg, strFilePath, '', DeleteFile, Nil, Option, True);
End;

(**

  This is an on delete start event handler for the sync module.

  @precon  None.
  @postcon Creates an instance of the deletion progress form.

  @param   iFileCount as an Integer as a constant
  @param   iTotalSize as an int64 as a constant

**)
Procedure TfrmMainForm.DeleteStartProc(Const iFileCount: Integer; Const iTotalSize: int64);

Begin
  FTotalSize := iTotalSize;
  OutputResultLn(Format('Deleting %1.0n files (%1.0n bytes)...',
      [Int(iFileCount), Int(iTotalSize)]));
  If iFileCount > 0 Then
    Begin
      FDeleteForm                  := TfrmDeleteProgress.Create(Self);
      FDeleteForm.OnUpdateProgress := UpdateProgress;
      FDeleteForm.Initialise(dtFiles, iFileCount, iTotalSize, FDeleteDlgWidth);
      FDialogueBottom := FDeleteForm.Top + FDeleteForm.Height;
    End;
End;

(**

  This is an on delete event handler for the sync module.

  @precon  None.
  @postcon Output the file to be deleted to the deletion progress form.

  @nohint  iCumulativeFileSizeBeforeDelete iTotalFileSizeToDelete

  @param   iCurrentFileToDelete            as an Integer as a constant
  @param   iTotalFilesToDelete             as an Integer as a constant
  @param   iCumulativeFileSizeBeforeDelete as an Int64 as a constant
  @param   iTotalFileSizeToDelete          as an Int64 as a constant
  @param   strDeletePath                   as a String as a constant
  @param   strFileNameToDelete             as a String as a constant

**)
Procedure TfrmMainForm.DeletingProc(Const iCurrentFileToDelete, iTotalFilesToDelete : Integer;
    Const iCumulativeFileSizeBeforeDelete, iTotalFileSizeToDelete: Int64;
    Const strDeletePath, strFileNameToDelete: String);

Begin
  FDeleteForm.InitialiseFileName(dtFiles, iCurrentFileToDelete, iTotalFilesToDelete,
    strDeletePath + strFileNameToDelete);
  OutputFileNumber(iCurrentFileToDelete, iTotalFilesToDelete);
  OutputResult(strDeletePath + strFileNameToDelete);
End;

(**

  This is an on diff size event handler for the sync module.

  @precon  None.
  @postcon Outputs the name of the files that have a difference size but the same date and time.

  @param   iFile       as an Integer as a constant
  @param   iFileCount  as an Integer as a constant
  @param   strLPath    as a String as a constant
  @param   strRPath    as a String as a constant
  @param   strFileName as a String as a constant

**)
procedure TfrmMainForm.DiffSize(Const iFile, iFileCount: Integer; Const strLPath, strRPath,
    strFileName: String);

begin
  OutputFileNumber(iFile, iFileCount);
  OutputResultLn(Format('%s => %s%s', [strLPath, strRPath, strFileName]));
end;

(**

  This is an on diff size end event handler for the sync module.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
procedure TfrmMainForm.DiffSizeEnd;

begin
end;

(**

  This is a Diff Size Start event handler for the sync module.

  @precon  None.
  @postcon Displays a header is there are files to output.

  @param   iFileCount as an Integer as a constant

**)
procedure TfrmMainForm.DiffSizeStart(Const iFileCount: Integer);

begin
  If iFileCount > 0 Then
    OutputResultLn(Format('%1.0n File(s) with Differing Size...', [Int(iFileCount)]));
end;

(**

  This method disables all actions.

  @precon  None.
  @postcon All actions are disabled.

**)
Procedure TfrmMainForm.DisableActions;

Var
  i: Integer;

Begin
  For i := 0 To ComponentCount - 1 Do
    If Components[i] Is TAction Then
      (Components[i] As TAction).Enabled := False;
End;

(**

  This method enables all actions.

  @precon  None.
  @postcon All actions are enabled.

  @param   boolSuccess as a Boolean as a constant

**)
Procedure TfrmMainForm.EnableActions(Const boolSuccess: Boolean);

Var
  i: Integer;
  A: TAction;

Begin
  For i := 0 To ComponentCount - 1 Do
    If Components[i] Is TAction Then
      Begin
        A := Components[i] As TAction;
        If boolSuccess Then
          A.Enabled := True
        Else If A.Name <> 'actFileProcessFiles' Then
          A.Enabled := True;
      End;
End;

(**

  This is an on error message event handler for outputting error message from the deleting ans copying 
  process.

  @precon  None.
  @postcon Outputs an error message to the log file.

  @param   strErrorMsg as a String as a constant

**)
Procedure TfrmMainForm.ErrorMessage(Const strErrorMsg : String);

Begin
  OutputResultLn(Format('  %s', [strErrorMsg]));
End;

(**

  This is an on error message end event handler for the outputting of error messages from
  the deleting and copying processes.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod

**)
Procedure TfrmMainForm.ErrorMessageEnd;

Begin
End;

(**

  This is an on error message start event handler for the outputting of error messages from the deleting 
  and copyin processes.

  @precon  None.
  @postcon Ouputs an error message header to the log file if there are errors to output.

  @param   iFileCount as an Integer as a constant

**)
Procedure TfrmMainForm.ErrorMessageStart(Const iFileCount: Integer);

Begin
  If iFileCount > 0 Then
    OutputResultLn(Format('%1.0n Files had errors...', [Int(iFileCount)]));
End;

(**

  This is an on exceeeds size limit event handler.

  @precon  None.
  @postcon Outputs the given file to the log.

  @param   iFile       as an Integer as a constant
  @param   iFileCount  as an Integer as a constant
  @param   strLPath    as a String as a constant
  @param   strRPath    as a String as a constant
  @param   strFileName as a String as a constant

**)
Procedure TfrmMainForm.ExceedsSizeLimit(Const iFile, iFileCount: Integer; Const strLPath, strRPath,
    strFileName: String);

Begin
  OutputFileNumber(iFile, iFileCount);
  OutputResultLn(Format('%s => %s%s', [strLPath, strRPath, strFileName]));
End;

(**

  This is an on exceeds size limit end event handler.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod

**)
Procedure TfrmMainForm.ExceedsSizeLimitEnd;

Begin
End;

(**

  This is an on exceeds size limit start event handler.

  @precon  None.
  @postcon Outputs a header for the list of over sizeed files in the log.

  @param   iFileCount as an Integer as a constant

**)
Procedure TfrmMainForm.ExceedsSizeLimitStart(Const iFileCount: Integer);

Begin
  If iFileCount > 0 Then
    OutputResultLn(Format('%1.0n Files exceeding Size Limit...', [Int(iFileCount)]));
End;

(**

  This is an exception handling message for the BuildRootKey method.

  @precon  None.
  @postcon Displays the exception message in a dialogue.

  @param   strExceptionMsg as a String as a constant

**)
Procedure TfrmMainForm.ExceptionProc(Const strExceptionMsg: String);

Begin
  MessageDlg(strExceptionMsg, mtError, [mbOK], 0);
End;

(**

  This method displays a dialogue asking the user to confirm the file action they wish to take based on 
  the message displayed. The answer to the dialogue is returned to the sync module.

  @precon  None.
  @postcon The users response to the query is passed back to the sync module.

  @param   strMsg        as a String as a constant
  @param   strConsoleMsg as a String as a constant
  @param   strSourcePath as a String as a constant
  @param   strDestPath   as a String as a constant
  @param   SourceFile    as a TFileRecord as a constant
  @param   DestFile      as a TFileRecord as a constant
  @param   Option        as a TFileAction as a reference
  @param   boolReadOnly  as a Boolean as a constant

**)
Procedure TfrmMainForm.FileQuery(Const strMsg, strConsoleMsg, strSourcePath,
  strDestPath: String; Const SourceFile, DestFile : TFileRecord; Var Option: TFileAction;
  Const boolReadOnly : Boolean);

Begin
  If (Not (Option In [faYesToAll, faNoToAll])     And Not boolReadOnly) Or
     (Not (Option In [faYesToAllRO, faNoToAllRO]) And     boolReadOnly) Then
    Begin
      OutputResult(strConsoleMsg);
      UpdateTaskBar(ptError, 0, 1);
      Case TfrmConfirmationDlg.Execute(Self, strMsg, strSourcePath, strDestPath,
        SourceFile, DestFile, FDialogueBottom, FConfirmDlgWidth) Of
        mrYes:
          Option := faYes;
        mrNo:
          Option := faNo;
        mrAll:
          If Not boolReadOnly Then
            Option := faYesToAll
          Else
            Option := faYesToAllRO;
        mrIgnore:
          If Not boolReadOnly Then
            Option := faNoToAll
          Else
            Option := faNoToAllRO;
      Else
        Option := faCancel;
      End;
      OutputResult(strFileOptions[Option]);
    End;
End;

(**

  This method outputs the results of the various folder comparisons into the list view for
  actioning.

  @precon  None.
  @postcon Outputs the results of the various folder comparisons into the list view for
           actioning.

**)
Procedure TfrmMainForm.FixUpPanes;

Var
  iProcessItem: Integer;
  P           : TProcessItem;

Begin
  Inc(FProgressSection);
  FProgressForm.InitialiseSection(FProgressSection, 0, FSyncModule.ProcessCount);
  vstFileList.BeginUpdate;
  Try
    For iProcessItem := 0 To FSyncModule.ProcessCount - 1 Do
      Begin
        P := FSyncModule.Process[iProcessItem];
        InsertListItem(iProcessItem, P);
        If iProcessItem Mod 10 = 0 Then
          FProgressForm.Progress(FProgressSection, iProcessItem,
            Format('Building ListView... %1.0n in %1.0n', [Int(iProcessItem),
                Int(FSyncModule.ProcessCount)]), '');
      End;
  Finally
    vstFileList.EndUpdate;
  End;
End;

(**

  This is an on form activate event handler for the main form.

  @precon  None.
  @postcon Ensures that any child forms that are visible are brought to the front.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.FormActivate(Sender: TObject);

Var
  iForm: Integer;

Begin
  For iForm := 0 To ComponentCount - 1 Do
    If Components[iForm] Is TForm Then
      If (Components[iForm] As TForm).Visible  Then
        (Components[iForm] As TForm).BringToFront;
End;

(**

  This is the forms on create event handler.

  @precon  None.
  @postcon Creates various objects for the life time of the application and
           loads the applications settings from the registry.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.FormCreate(Sender: TObject);

Begin
  vstFileList.NodeDataSize := SizeOf(TFSFileListNode);
  FParams                         := TStringList.Create;
  FCloseTimer                     := TTimer.Create(Nil);
  FCloseTimer.Enabled             := False;
  FCloseTimer.Interval            := 2000;
  FCloseTimer.OnTimer             := CloseTimerEvent;
  FStartTimer                     := TTimer.Create(Nil);
  FStartTimer.Enabled             := False;
  FStartTimer.Interval            := 1000;
  FStartTimer.OnTimer             := StartTimerEvent;
  DGHMemoryMonitor.HighPoint      := 100;
  DGHMemoryMonitor.HalfPoint      := 90;
  DGHMemoryMonitor.LowPoint       := 80;
  DGHMemoryMonitor.UpdateInterval := 500;
  FRootKey                        := TFSFunctions.BuildRootKey(FParams, ExceptionProc);
  FFolders                        := TFolders.Create;
  FProgressForm                   := TfrmProgress.Create(Self);
  FProgressForm.OnUpdateProgress  := UpdateProgress;
  Application.HelpFile := ExtractFilePath(ParamStr(0)) + 'FldrSync.chm';
  LoadSettings();
  Application.OnHint                := ApplicationHint;
  FIconFiles                        := TStringList.Create;
  FIconFiles.Sorted                 := True;
  Caption := Caption + StringReplace(TFSFunctions.GetConsoleTitle(' %d.%d%s (Build %s)'), #13#10,
    ' - ', []);
  If IsDebuggerPresent Then
    Caption := Format('%s [DEBUGGING]', [Caption]);
  {$IFDEF WIN64}
  Caption := Caption + ' [64-bit]';
  {$ELSE}
  Caption := Caption + ' [32-bit]';
  {$ENDIF}
  Caption                             := Format('%s: %s', [Caption, FRootKey]);
  FDialogueBottom                     := 0;
  FSyncModule                         := TCompareFoldersCollection.Create(Self.Handle);
  FSyncModule.OnSearchStart           := SearchStartProc;
  FSyncModule.OnSearch                := SearchProc;
  FSyncModule.OnSearchEnd             := SearchEndProc;
  FSyncModule.OnCompareStart          := CompareStartProc;
  FSyncModule.OnCompare               := CompareProc;
  FSyncModule.OnCompareEnd            := CompareEndProc;
  FSyncModule.OnMatchListStart        := MatchListStartProc;
  FSyncModule.OnMatchList             := MatchListProc;
  FSyncModule.OnMatchListEnd          := MatchListEndProc;
  FSyncModule.OnDeleteStart           := DeleteStartProc;
  FSyncModule.OnDeleting              := DeletingProc;
  FSyncModule.OnDeleted               := DeletedProc;
  FSyncModule.OnDeleteQuery           := DeleteQueryProc;
  FSyncModule.OnDeleteReadOnlyQuery   := DeleteReadOnlyQueryProc;
  FSyncModule.OnDeleteEnd             := DeleteEndProc;
  FSyncModule.OnCopyStart             := CopyStartProc;
  FSyncModule.OnCopying               := CopyingProc;
  FSyncModule.OnCopyContents          := CopyContentsProc;
  FSyncModule.OnCopied                := CopiedProc;
  FSyncModule.OnCopyQuery             := CopyQueryProc;
  FSyncModule.OnCopyReadOnlyQuery     := CopyReadOnlyQueryProc;
  FSyncModule.OnCopyEnd               := CopyEndProc;
  FSyncModule.OnDiffSizeStart         := DiffSizeStart;
  FSyncModule.OnDiffSize              := DiffSize;
  FSyncModule.OnDiffSizeEnd           := DiffSizeEnd;
  FSyncModule.OnNothingToDoStart      := NothingToDoStart;
  FSyncModule.OnNothingToDo           := NothingToDo;
  FSyncModule.OnNothingToDoEnd        := NothingToDoEnd;
  FSyncModule.OnExceedsSizeLimitStart := ExceedsSizeLimitStart;
  FSyncModule.OnExceedsSizeLimit      := ExceedsSizeLimit;
  FSyncModule.OnExceedsSizeLimitEnd   := ExceedsSizeLimitEnd;
  FSyncModule.OnErrorMsgsStart        := ErrorMessageStart;
  FSyncModule.OnErrorMsgs             := ErrorMessage;
  FSyncModule.OnErrorMsgsEnd          := ErrorMessageEnd;
  FSyncModule.OnDeleteFoldersStart    := DeleteFoldersStart;
  FSyncModule.OnDeleteFolders         := DeleteFolders;
  FSyncModule.OnDeleteFoldersEnd      := DeleteFoldersEnd;
  FSyncModule.OnCopyError             := CopyError;
  FSyncModule.OnDeleteError           := DeleteError;
  FTaskbarList  := Nil;
  FTaskbarList3 := Nil;
  If CheckWin32Version(6, 1) Then
    Begin
      FTaskbarList := CreateComObject(CLSID_TaskbarList) As ITaskbarList;
      FTaskbarList.HrInit;
      Supports(FTaskbarList, IID_ITaskbarList3, FTaskbarList3);
    End;
  actEditCopyRightToLeft.Tag := Integer(foRightToLeft);
  actEditCopyLeftToRight.Tag := Integer(foLeftToRight);
  actEditDelete.Tag := Integer(foDelete);
  actEditDoNothing.Tag := Integer(foNothing);
  DGHMemoryMonitor.OnContextPopup := MemoryPopupMenu;
  FStartTimer.Enabled := True;
End;

(**

  This is the forms on destroy event handler.

  @precon  None.
  @postcon Save the applications settings and free memory used by various
           objects.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.FormDestroy(Sender: TObject);

Begin
  SaveSettings();
  FSyncModule.Free;
  FIconFiles.Free;
  FProgressForm.Free;
  FFolders.Free;
  FCloseTimer.Free;
  FParams.Free;
End;

(**

  This is a Form on Resize event handler.

  @precon  None.
  @postcon Makes sure that the two sides of the interface are evenly spaced.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.FormResize(Sender: TObject);

Var
  H: TVTHeader;
  i: Integer;

Begin
  H := vstFileList.Header;
  i := vstFileList.ClientWidth Div 2
     - H.Columns[0].Width Div 2
     - H.Columns[Integer(vfLeftAttr)].Width
     - H.Columns[Integer(vfLeftSize)].Width
     - H.Columns[Integer(vfLeftDate)].Width
     - 2;
  If i > 0 Then
    Begin
      H.Columns[Integer(vfLeftFileName)].Width := i;
      H.Columns[Integer(vfRightFilename)].Width := i;
    End;
End;

(**

  This method retrieves from the system the icon for the specified file, places it in a list and returns 
  the index of the index in the list.

  @precon  None.
  @postcon Retrieves from the system the icon for the specified file, places it in a list and returns 
           the index of the index in the list

  @param   strFileName as a String as a constant
  @return  a NativeInt

**)
Function TfrmMainForm.GetImageIndex(Const strFileName: String): NativeInt;

Var
  strExt        : String;
  strClassName  : String;
  strFName      : String;
  iPos          : Integer;
  iIndex        : Integer;
  iIcon         : Word;
  szIconFile    : PChar;
  objIcon       : TIcon;
  iLIcon, iSIcon: HICON;
  strIndex      : String;
  strExpanded   : String;
  iOutputSize   : Integer;

Begin
  strExt := LowerCase(ExtractFileExt(strFileName));
  If FIconFiles.Find(strExt, iIndex) Then
    Begin
      Result := NativeInt(FIconFiles.Objects[iIndex]);
      Exit;
    End;
  // Get Icon information from Registry
  strClassName := GetRegStringValue(strExt, '');
  If strClassName <> '' Then
    Begin
      strFName := GetRegStringValue(strExt + '\DefaultIcon', '');
      If strFName = '' Then
        strFName := GetRegStringValue(strClassName + '\DefaultIcon', '');
      If strFName = '' Then
        Begin
          strFName := GetRegStringValue(strClassName + '\shell\open\command', '');
          strFName := StringReplace(strFName, ' "%1"', '', [rfReplaceAll]);
          strFName := StringReplace(strFName, ' %1', '', [rfReplaceAll]);
          strFName := strFName + ',0';
        End;
      SetLength(strExpanded, 1024);
      iOutputSize := ExpandEnvironmentStrings(PChar(strFName), PChar(strExpanded), 1024);
      SetLength(strExpanded, iOutputSize - 1);
      strFName := strExpanded;
    End;
  iPos := Pos(',', strFName);
  If iPos <> 0 Then
    Begin
      strIndex := Copy(strFName, iPos + 1, Length(strFName) - iPos);
      If strIndex <> '' Then
        If strIndex[Length(strIndex)] = '"' Then
          strIndex := Copy(strIndex, 1, Length(strIndex) - 1);
      iIndex       := StrToInt(strIndex);
      strFName     := Copy(strFName, 1, iPos - 1);
      ExtractIconEx(PChar(strFName), iIndex, iLIcon, iSIcon, 1);
    End;
  // If the registry technique fail to get an icon use the ExtractAssociatedIcon
  // function instead
  If iSIcon = 0 Then
    Begin
      iIcon      := 0;
      szIconFile := StrAlloc(1024);
      Try
        StrCopy(szIconFile, PChar(strFileName));
        iSIcon := ExtractAssociatedIcon(Handle, szIconFile, iIcon);
      Finally
        StrDispose(szIconFile);
      End;
    End;
  objIcon := TIcon.Create;
  Try
    objIcon.Handle := iSIcon;
    Result         := ilFileTypeIcons.AddIcon(objIcon);
  Finally
    objIcon.Free;
  End;
  If strExt <> '.exe' Then
    FIconFiles.AddObject(strExt, TObject(Result));
End;

(**

  This method adds a pair of filenames with sizes and dates to the list view.

  @precon  None.
  @postcon Adds a pair of filenames with sizes and dates to the list view.

  @param   iProcessItem as an Integer as a constant
  @param   ProcessItem  as a TProcessItem as a constant

**)
Procedure TfrmMainForm.InsertListItem(Const iProcessItem : Integer; Const ProcessItem : TProcessItem);

Var
  Node : PVirtualNode;
  NodeData : PFSFileListNode;
  strFileName: String;

Begin
  Node := vstFileList.AddChild(Nil);
  NodeData := vstFileList.GetNodeData(Node);
  NodeData.FProcessIndex := iProcessItem;
  // Left File
  NodeData.FLeftFileIndex := -1;
  NodeData.FRightFileIndex := -1;
  If Assigned(ProcessItem.LeftFile) Then
    Begin
      NodeData.FLeftFilename := ProcessItem.LPath + ProcessItem.LeftFile.FileName;
      NodeData.FLeftAttr := ProcessItem.LeftFile.Attributes;
      NodeData.FLeftSize := ProcessItem.LeftFile.Size;
      NodeData.FLeftDate := FileDateToDateTime(ProcessItem.LeftFile.DateTime);
      strFileName := ProcessItem.LeftFile.FileName;
    End Else
    Begin
      NodeData.FLeftFilename := '';
      NodeData.FLeftAttr := 0;
      NodeData.FLeftSize := 0;
      NodeData.FLeftDate := 0;
    End;
  // Right File
  If Assigned(ProcessItem.RightFile) Then
    Begin
      NodeData.FRightFilename := ProcessItem.RPath + ProcessItem.RightFile.FileName;
      NodeData.FRightAttr := ProcessItem.RightFile.Attributes;
      NodeData.FRightSize := ProcessItem.RightFile.Size;
      NodeData.FRightDate := FileDateToDateTime(ProcessItem.RightFile.DateTime);
      strFileName := ProcessItem.RightFile.FileName;
    End Else
    Begin
      NodeData.FRightFilename := '';
      NodeData.FRightAttr := 00;
      NodeData.FRightSize := 0;
      NodeData.FRightDate := 0;
    End;
  NodeData.FLeftFullFileName := ProcessItem.LPath + strFileName;
  NodeData.FRightFullFileName := ProcessItem.RPath + strFileName;
  NodeData.FState := ProcessItem.FileOp;
End;

(**

  This method loads the applications seetings from the registry.

  @precon  None.
  @postcon Loads the applications settings from the registry.

**)
Procedure TfrmMainForm.LoadSettings;

Var
  sl : TStringList;
  i  : Integer;
  j  : TFldrSyncOption;
  k : TFileOpFont;
  strLog : String;
  iSyncOptions : TSyncOptions;
  strMaxValue : String;
  iMaxValue : TInt64Ex;
  strLeftFldr, strRightFldr : String;
  iniMemFile : TMemIniFile;
  iDefaultOps : TFileOpStats;
  ComOps: TCommandLineOptionsRec;
  astrMaxValues: TArray<String>;

Begin
  iniMemFile := TMemIniFile.Create(FRootKey);
  Try
    TStyleManager.SetStyle(iniMemFile.ReadString('Setup', 'Theme',
      TStyleManager.ActiveStyle.Name));
    Top := iniMemFile.ReadInteger('Setup', 'Top', 100);
    Left := iniMemFile.ReadInteger('Setup', 'Left', 100);
    Height := iniMemFile.ReadInteger('Setup', 'Height', 300);
    Width := iniMemFile.ReadInteger('Setup', 'Width', 450);
    vstFileList.Header.Columns[Integer(vfLeftAttr)].Width := iniMemFile.ReadInteger('ColumnWidths', 'LAttr', 50);
    vstFileList.Header.Columns[Integer(vfLeftSize)].Width := iniMemFile.ReadInteger('ColumnWidths', 'LSize', 85);
    vstFileList.Header.Columns[Integer(vfLeftDate)].Width := iniMemFile.ReadInteger('ColumnWidths', 'LDAte', 150);
    vstFileList.Header.Columns[Integer(vfRightAttr)].Width := iniMemFile.ReadInteger('ColumnWidths', 'RAttr', 50);
    vstFileList.Header.Columns[Integer(vfRightSize)].Width := iniMemFile.ReadInteger('ColumnWidths', 'RSize', 85);
    vstFileList.Header.Columns[Integer(vfRightDate)].Width := iniMemFile.ReadInteger('ColumnWidths', 'RDate', 150);
    FInterfaceFonts[ifTableFont].FFontName :=
      iniMemFile.ReadString('ListViewFont', 'Name', InterfaceFontDefaults[ifTableFont].FFontName);
    vstFileList.Font.Name               := FInterfaceFonts[ifTableFont].FFontName;
    FInterfaceFonts[ifTableFont].FFontSize :=
      iniMemFile.ReadInteger('ListViewFont', 'Size', InterfaceFontDefaults[ifTableFont].FFontSize);
    vstFileList.Font.Size               := FInterfaceFonts[ifTableFont].FFontSize;
    FInterfaceFonts[ifLogFont].FFontName :=
      iniMemFile.ReadString('LogFont', 'Name', InterfaceFontDefaults[ifLogFont].FFontName);
    redtOutputResults.Font.Name        := FInterfaceFonts[ifLogFont].FFontName;
    FInterfaceFonts[ifLogFont].FFontSize :=
      iniMemFile.ReadInteger('LogFont', 'Size', InterfaceFontDefaults[ifLogFont].FFontSize);
    redtOutputResults.Font.Size        := FInterfaceFonts[ifLogFont].FFontSize;
    For k := Low(TFileOpFont) To High(TFileOpFont) Do
      Begin
        FFileOpFonts[k].FFontColour := StringToColor(
          iniMemFile.ReadString('FileOpFonts', FileOperationFontDefaults[k].FININame + 'Colour',
            ColorToString(FileOperationFontDefaults[k].FFontColour)));
        FFileOpFonts[k].FBGFontColour := StringToColor(
          iniMemFile.ReadString('FileOpFonts', FileOperationFontDefaults[k].FININame + 'BGColour',
            ColorToString(FileOperationFontDefaults[k].FBGFontColour)));
        FFileOpFonts[k].FFontStyle := TFontStyles(Byte(
          iniMemFile.ReadInteger('FileOpFonts', FileOperationFontDefaults[k].FININame + 'Style',
            Byte(FileOperationFontDefaults[k].FFontStyle))));
      End;
    strLog := ChangeFileExt(FRootKey, '_log.txt');
    If FileExists(strLog) Then
      redtOutputResults.Lines.LoadFromFile(strLog);
    LogSize;
    WindowState := TWindowState(iniMemFile.ReadInteger('Setup', 'WindowState', Byte(wsNormal)));
    redtOutputResults.Height := iniMemFile.ReadInteger('Setup', 'OutputResultsHeight', 100);
    FCompareEXE := iniMemFile.ReadString('Setup', 'CompareEXE', '');
    FCopyDlgWidth := iniMemFile.ReadInteger('DlgWidths', 'CopyDlg', 0);
    FDeleteDlgWidth := iniMemFile.ReadInteger('DlgWidths', 'DeleteDlg', 0);
    FConfirmDlgWidth := iniMemFile.ReadInteger('DlgWidths', 'ConfirmDlg', 0);
    iDefaultOps := [fosDelete..fosDifference];
    FFileOpStats := TFileOpStats(Byte(iniMemFile.ReadInteger('Setup', 'FileOpStats',
      Byte(iDefaultOps))));
    UpgradeINIFolderOptions(iniMemFile);
    If FParams.Count = 0 Then
      Begin
        FFldrSyncOptions := [];
        For j := Low(TFldrSyncOption) To High(TFldrSyncOption) Do
          If iniMemFile.ReadBool(strFldrSyncOptions[j].FINISection, strFldrSyncOptions[j].FINIKey,
            strFldrSyncOptions[j].fDefault) Then
            Include(FFldrSyncOptions, j);
        sl := TStringList.Create;
        Try
          iniMemFile.ReadSection('NewFolders', sl);
          For i := 0 To sl.Count - 1 Do
            Begin
              iSyncOptions := TSyncOptions(SmallInt(iniMemFile.ReadInteger('NewFolderStatus', sl[i],
                SmallInt(iSyncOptions))));
              strMaxValue := iniMemFile.ReadString('NewFolderMaxFileSize', sl[i], '0,0,0,0');
              astrMaxValues := strMaxValue.Split([',']);
              iMaxValue.Value := 0;
              iMaxValue.iFirst := StrToInt(astrMaxValues[0]);
              iMaxValue.iSecond := StrToInt(astrMaxValues[1]);
              iMaxValue.iThird := StrToInt(astrMaxValues[2]);
              iMaxValue.iFourth := StrToInt(astrMaxValues[3]);
              strLeftFldr := GetShortHint(iniMemFile.ReadString('NewFolders', sl[i], ''));
              strRightFldr := GetLongHint(iniMemFile.ReadString('NewFolders', sl[i], ''));
              FFolders.Add(
                TFolder.Create(
                  ExtractFilePath(strLeftFldr),
                  ExtractFilePath(strRightFldr),
                  ExtractFileName(strLeftFldr),
                  iSyncOptions,
                  iMaxValue.Value
                )
              );
            End;
        Finally
          sl.Free;
        End;
        FExclusions := StringReplace(iniMemFile.ReadString('Setup', 'Exclusions', ''), '|', #13#10,
          [rfReplaceAll]);
      End Else
      Begin
        ComOps.iSyncOptions := [soEnabled];
        ProcessCommandLine(FParams, ComOps);
        FFolders.Add(
          TFolder.Create(
            ComOps.strSourceFldr,
            ComOps.strDestFldr,
            ComOps.strFilePatterns,
            ComOps.iSyncOptions,
            ComOps.iMaxFileSize
          ));
        If clsDeletePermentently In ComOps.iCommandLineOptions Then
          Include(FFldrSyncOptions, fsoPermanentlyDeleteFiles)
        Else
          Exclude(FFldrSyncOptions, fsoPermanentlyDeleteFiles);
        If clsBatchRecycleFiles In ComOps.iCommandLineOptions Then
          Include(FFldrSyncOptions, fsoBatchRecycleFiles)
        Else
          Exclude(FFldrSyncOptions, fsoBatchRecycleFiles);
        If clsProceedAutomatically In ComOps.iCommandLineOptions Then
          Include(FFldrSyncOptions, fsoStartProcessingAutomatically)
        Else
          Exclude(FFldrSyncOptions, fsoStartProcessingAutomatically);
      End;
    DGHMemoryMonitor.UpdateInterval := iniMemFile.ReadInteger('MemoryMonitor', 'UpdateInterval',
      DGHMemoryMonitor.UpdateInterval);
    DGHMemoryMonitor.BackColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'BackColour', ColorToString(DGHMemoryMonitor.BackColour)));
    DGHMemoryMonitor.BackFontColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'BackFontColour', ColorToString(DGHMemoryMonitor.BackFontColour)));
    DGHMemoryMonitor.HighColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'HighColour', ColorToString(DGHMemoryMonitor.HighColour)));
    DGHMemoryMonitor.HalfColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'HalfColour', ColorToString(DGHMemoryMonitor.HalfColour)));
    DGHMemoryMonitor.LowColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'LowColour', ColorToString(DGHMemoryMonitor.LowColour)));
    DGHMemoryMonitor.HighFontColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'HighFontColour', ColorToString(DGHMemoryMonitor.HighFontColour)));
    DGHMemoryMonitor.HalfFontColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'HalfFontColour', ColorToString(DGHMemoryMonitor.HalfFontColour)));
    DGHMemoryMonitor.LowFontColour := StringToColor(iniMemFile.ReadString('MemoryMonitor',
      'LowFontColour', ColorToString(DGHMemoryMonitor.LowFontColour)));
    DGHMemoryMonitor.LowPoint := iniMemFile.ReadInteger('MemoryMonitor', 'LowPoint',
      DGHMemoryMonitor.LowPoint);
    DGHMemoryMonitor.HalfPoint := iniMemFile.ReadInteger('MemoryMonitor', 'HalfPoint',
      DGHMemoryMonitor.HalfPoint);
    DGHMemoryMonitor.HighPoint := iniMemFile.ReadInteger('MemoryMonitor', 'HighPoint',
      DGHMemoryMonitor.HighPoint);
    DGHMemoryMonitor.Font.Name := iniMemFile.ReadString('MemoryMonitor', 'FontName',
      DGHMemoryMonitor.Font.Name);
    DGHMemoryMonitor.Font.Size := iniMemFile.ReadInteger('MemoryMonitor', 'FontSize',
      DGHMemoryMonitor.Font.Size);
    DGHMemoryMonitor.Font.Style := TFontStyles(Byte(iniMemFile.ReadInteger('MemoryMonitor', 'FontStyle',
      Byte(DGHMemoryMonitor.Font.Style))));
    DGHMemoryMonitor.Width := iniMemFile.ReadInteger('MemoryMonitor', 'Width',
      DGHMemoryMonitor.Width);
  Finally
    iniMemFile.Free;
  End;
End;

(**

  This methiohd updates the status bar with the current log size.

  @precon  None.
  @postcon The status bar is updated with the current log size.

**)
Procedure TfrmMainForm.LogSize;

Begin
  stbrStatusBar.Panels[0].Text := Format('Log size: %1.1n kbytes',
    [Int(Length(redtOutputResults.Lines.Text)) / 1024.0]);
  stbrStatusBar.Canvas.Font.Assign(stbrStatusBar.Font);
  stbrStatusBar.Panels[0].Width := stbrStatusBar.Canvas.TextWidth(stbrStatusBar.Panels[0].Text) + 25;
End;

(**

  This is an on match list end event handler for the sync module.

  @precon  None.
  @postcon None.

**)
Procedure TfrmMainForm.MatchListEndProc;

Begin
  OutputResultLn('Done.');
  FProgressForm.Progress(FProgressSection, 100, 'Done!', '');
End;

(**

  This is an on match list event handler for the sync module.

  @precon  None.
  @postcon Updates the progress dialogue with the progress through the matching process.

  @param   iPosition as an Integer as a constant
  @param   iMaxItems as an Integer as a constant

**)
Procedure TfrmMainForm.MatchListProc(Const iPosition, iMaxItems: Integer);

Begin
  If iPosition Mod 10 = 0 Then
    FProgressForm.Progress(FProgressSection, Trunc(iPosition / iMaxItems * 100.0),
      Format('Matching items... %1.0n in %1.0n', [Int(iPosition), Int(iMaxItems)]), '');
End;

(**

  This is an on match list start event handler for the sync module.

  @precon  None.
  @postcon Increments the progress sections and initialises the dialogue for progress.

**)
Procedure TfrmMainForm.MatchListStartProc;

Begin
  OutputResult('Match Lists: ');
  Inc(FProgressSection);
  FProgressForm.InitialiseSection(FProgressSection, 1, 100);
End;

(**

  This is an on context popup event handler for the memory monitor graphics control.

  @precon  None.
  @postcon Invokes the dialogue to editing the configuration of the Memory Monitor.

  @param   Sender   as a TObject
  @param   MousePos as a TPoint
  @param   Handled  as a Boolean as a reference

**)
Procedure TfrmMainForm.MemoryPopupMenu(Sender: TObject; MousePos: TPoint;
  Var Handled: Boolean);

Begin
  actToolsConfigMemMonExecute(Sender);
End;

(**

  This is an on Nothing To Do event handler for the sync module.

  @precon  None.
  @postcon Outputs the name of the files that have a difference size but the same date and time.

  @param   iFile       as an Integer as a constant
  @param   iFileCount  as an Integer as a constant
  @param   strLPath    as a String as a constant
  @param   strRPath    as a String as a constant
  @param   strFileName as a String as a constant

**)
procedure TfrmMainForm.NothingToDo(Const iFile, iFileCount: Integer; Const strLPath, strRPath,
    strFileName: String);

begin
  OutputFileNumber(iFile, iFileCount);
  OutputResultLn(Format('%s => %s%s', [strLPath, strRPath, strFileName]));
end;

(**

  This is an on nothingToDo end event handler for the sync module.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
Procedure TfrmMainForm.NothingToDoEnd;

Begin
End;

(**

  This is a Nothing To Do Start event handler for the sync module.

  @precon  None.
  @postcon Displays a header is there are files to output.

  @param   iFileCount as an Integer as a constant

**)
procedure TfrmMainForm.NothingToDoStart(Const iFileCount: Integer);

begin
  If iFileCount > 0 Then
    OutputResultLn(Format('%1.0n Files with Nothing to do...',
      [Int(iFileCount)]));
end;

(**

  This method outputs the file number and the total number of files to the output window.

  @precon  None.
  @postcon Outputs the file number and the total number of files to the output window.

  @param   iCurrentFile as an Integer as a constant
  @param   iTotal       as an Integer as a constant

**)
Procedure TfrmMainForm.OutputFileNumber(Const iCurrentFile, iTotal : Integer);

Var
  strTotal : String;
  iSize : Integer;

Begin
  strTotal := Format('%1.0n', [Int(iTotal)]);
  iSize    := Length(strTotal);
  OutputResult(#32#32 + Format('(%*.0n/%*.0n) ',
      [iSize, Int(iCurrentFile), iSize, Int(iTotal)]));
End;

(**

  This method outputs the given message to the output window with the optional colour but without a line 
  feed and carraige return.

  @precon  None.
  @postcon The messages is output to the end of output window.

  @param   strMsg as a String as a constant

**)
Procedure TfrmMainForm.OutputResult(Const strMsg: String = '');

Begin
  redtOutputResults.SelLength           := 0;
  redtOutputResults.SelStart            := Length(redtOutputResults.Text);
  redtOutputResults.SelText             := strMsg;
  LogSize;
End;

(**

  This method outputs the given message to the output window with the optional colour but WITH a line 
  feed and carraige return.

  @precon  None.
  @postcon The messages is output to the end of output window.

  @param   strMsg as a String as a constant

**)
Procedure TfrmMainForm.OutputResultLn(Const strMsg: String = '');

Begin
  OutputResult(strMsg + #13#10);
End;

(**

  This method adds panels to the status bar for each of the statistics returned from the
  sync module.

  @precon  None.
  @postcon Panels are added to the status bar for each available statistic.

**)
Procedure TfrmMainForm.OutputStats;

Const
  strTemplate = '%s: %1.0n files in %1.1n kbytes';

Var
  i: TFileOpStat;
  P: TStatusPanel;

Begin
  stbrStatusBar.Panels.BeginUpdate;
  Try
    While stbrStatusBar.Panels.Count > 1 Do
      stbrStatusBar.Panels.Delete(stbrStatusBar.Panels.Count - 1);
    FSyncModule.BuildStats;
    For i := Low(TFileOpStat) To High(TFileOpStat) Do
      If i In FFileOpStats Then
        Begin
          P := stbrStatusBar.Panels.Add;
          P.Text := Format(strTemplate, [
            FSyncModule.Statistics[i].FName,
            Int(FSyncModule.Statistics[i].FCount),
            Int(FSyncModule.Statistics[i].FSize / 1024.0)
          ]);
          P.Width := stbrStatusBar.Canvas.TextWidth(P.Text) + 25;
          P.Style := psOwnerDraw;
        End;
  Finally
    stbrStatusBar.Panels.EndUpdate;
  End;
End;

(**

  This method saves the applications seetings to the registry.

  @precon  None.
  @postcon Save the applications settings to the registry.

**)
Procedure TfrmMainForm.SaveSettings;

Var
  i         : Integer;
  recWndPlmt: TWindowPlacement;
  j         : TFldrSyncOption;
  iMaxValue : TInt64Ex;
  k: TFileOpFont;
  Ops: TSyncOptions;
  iniMemFile: TMemIniFile;

Begin
  iniMemFile := TMemIniFile.Create(FRootKey);
  Try
    recWndPlmt.Length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Handle, @recWndPlmt);
    iniMemFile.WriteInteger('Setup', 'Top', recWndPlmt.rcNormalPosition.Top);
    iniMemFile.WriteInteger('Setup', 'Left', recWndPlmt.rcNormalPosition.Left);
    iniMemFile.WriteInteger('Setup', 'Height', recWndPlmt.rcNormalPosition.Bottom -
        recWndPlmt.rcNormalPosition.Top);
    iniMemFile.WriteInteger('Setup', 'Width', recWndPlmt.rcNormalPosition.Right -
        recWndPlmt.rcNormalPosition.Left);
    iniMemFile.WriteInteger('ColumnWidths', 'LAttr', vstFileList.Header.Columns[Integer(vfLeftAttr)].Width);
    iniMemFile.WriteInteger('ColumnWidths', 'LSize', vstFileList.Header.Columns[Integer(vfLeftSize)].Width);
    iniMemFile.WriteInteger('ColumnWidths', 'LDAte', vstFileList.Header.Columns[Integer(vfLeftDate)].Width);
    iniMemFile.WriteInteger('ColumnWidths', 'RAttr', vstFileList.Header.Columns[Integer(vfRightAttr)].Width);
    iniMemFile.WriteInteger('ColumnWidths', 'RSize', vstFileList.Header.Columns[Integer(vfRightSize)].Width);
    iniMemFile.WriteInteger('ColumnWidths', 'RDate', vstFileList.Header.Columns[Integer(vfRightDate)].Width);
    iniMemFile.WriteString('ListViewFont', 'Name', FInterfaceFonts[ifTableFont].FFontName);
    iniMemFile.WriteInteger('ListViewFont', 'Size', FInterfaceFonts[ifTableFont].FFontSize);
    iniMemFile.WriteString('LogFont', 'Name', FInterfaceFonts[ifLogFont].FFontName);
    iniMemFile.WriteInteger('LogFont', 'Size', FInterfaceFonts[ifLogFont].FFontSize);
    For k := Low(TFileOpFont) To High(TFileOpFont) Do
      Begin
        iniMemFile.WriteString('FileOpFonts', FileOperationFontDefaults[k].FININame + 'Colour',
          ColorToString(FFileOpFonts[k].FFontColour));
        iniMemFile.WriteString('FileOpFonts', FileOperationFontDefaults[k].FININame + 'BGColour',
          ColorToString(FFileOpFonts[k].FBGFontColour));
        iniMemFile.WriteInteger('FileOpFonts', FileOperationFontDefaults[k].FININame + 'Style',
          Byte(FFileOpFonts[k].FFontStyle));
      End;
    redtOutputResults.Lines.SaveToFile(ChangeFileExt(FRootKey, '_log.txt'));
    iniMemFile.WriteInteger('Setup', 'WindowState', Byte(WindowState));
    iniMemFile.WriteInteger('Setup', 'OutputResultsHeight', redtOutputResults.Height);
    iniMemFile.WriteString('Setup', 'CompareEXE', FCompareEXE);
    iniMemFile.WriteInteger('DlgWidths', 'CopyDlg', FCopyDlgWidth);
    iniMemFile.WriteInteger('DlgWidths', 'DeleteDlg', FDeleteDlgWidth);
    iniMemFile.WriteInteger('DlgWidths', 'ConfirmDlg', FConfirmDlgWidth);
    iniMemFile.WriteInteger('Setup', 'FileOpStats', Byte(FFileOpStats));
    iniMemFile.WriteString('Setup', 'Theme', TStyleManager.ActiveStyle.Name);
    If FParams.Count = 0 Then
      Begin
        For j := Low(TFldrSyncOption) To High(TFldrSyncOption) Do
          iniMemFile.WriteBool(strFldrSyncOptions[j].FINISection, strFldrSyncOptions[j].FINIKey,
            j In FFldrSyncOptions);
        iniMemFile.EraseSection('Folders');
        iniMemFile.EraseSection('FolderStatus');
        iniMemFile.EraseSection('FolderMaxFileSize');
        iniMemFile.EraseSection('NewFolders');
        iniMemFile.EraseSection('NewFolderStatus');
        iniMemFile.EraseSection('NewFolderMaxFileSize');
        For i := 0 To FFolders.Count - 1 Do
          Begin
            iniMemFile.WriteString('NewFolders', Format('Folder%2.2d', [i]),
              FFolders.Folder[i].LeftFldr + FFolders.Folder[i].Patterns + '|' +
              FFolders.Folder[i].RightFldr + FFolders.Folder[i].Patterns);
            Ops := FFolders.Folder[i].SyncOptions;
            Exclude(Ops, soTempDisabled);
            iniMemFile.WriteInteger('NewFolderStatus', Format('Folder%2.2d', [i]), SmallInt(Ops));
            iMaxValue.Value := FFolders.Folder[i].MaxFileSize;
            iniMemFile.WriteString('NewFolderMaxFileSize', Format('Folder%2.2d', [i]),
              Format('%d,%d,%d,%d', [iMaxValue.iFirst, iMaxValue.iSecond,
                iMaxValue.iThird, iMaxValue.iFourth]));
          End;
        iniMemFile.WriteString('Setup', 'Exclusions', StringReplace(FExclusions, #13#10, '|',
            [rfReplaceAll]));
        //FExclusions := StringReplace(ReadString('Setup', 'Exclusions', ''), '|', #13#10,
        //  [rfReplaceAll]);
      End;
    iniMemFile.WriteInteger('MemoryMonitor', 'UpdateInterval', DGHMemoryMonitor.UpdateInterval);
    iniMemFile.WriteString('MemoryMonitor', 'BackColour', ColorToString(DGHMemoryMonitor.BackColour));
    iniMemFile.WriteString('MemoryMonitor', 'BackFontColour', ColorToString(DGHMemoryMonitor.BackFontColour));
    iniMemFile.WriteString('MemoryMonitor', 'HighColour', ColorToString(DGHMemoryMonitor.HighColour));
    iniMemFile.WriteString('MemoryMonitor', 'HalfColour', ColorToString(DGHMemoryMonitor.HalfColour));
    iniMemFile.WriteString('MemoryMonitor', 'LowColour', ColorToString(DGHMemoryMonitor.LowColour));
    iniMemFile.WriteString('MemoryMonitor', 'HighFontColour', ColorToString(DGHMemoryMonitor.HighFontColour));
    iniMemFile.WriteString('MemoryMonitor', 'HalfFontColour', ColorToString(DGHMemoryMonitor.HalfFontColour));
    iniMemFile.WriteString('MemoryMonitor', 'LowFontColour', ColorToString(DGHMemoryMonitor.LowFontColour));
    iniMemFile.WriteInteger('MemoryMonitor', 'LowPoint', DGHMemoryMonitor.LowPoint);
    iniMemFile.WriteInteger('MemoryMonitor', 'HalfPoint', DGHMemoryMonitor.HalfPoint);
    iniMemFile.WriteInteger('MemoryMonitor', 'HighPoint', DGHMemoryMonitor.HighPoint);
    iniMemFile.WriteString('MemoryMonitor', 'FontName', DGHMemoryMonitor.Font.Name);
    iniMemFile.WriteInteger('MemoryMonitor', 'FontSize', DGHMemoryMonitor.Font.Size);
    iniMemFile.WriteInteger('MemoryMonitor', 'FontStyle', Byte(DGHMemoryMonitor.Font.Style));
    iniMemFile.WriteInteger('MemoryMonitor', 'Width', DGHMemoryMonitor.Width);
    iniMemFile.UpdateFile;
  Finally
    iniMemFile.Free;
  End;
End;

(**

  This is an on search end event handler for the sync module.

  @precon  None.
  @postcon Outputs the results of the search.

  @param   iFileCount as an Integer as a constant
  @param   iTotalSize as an int64 as a constant

**)
Procedure TfrmMainForm.SearchEndProc(Const iFileCount: Integer; Const iTotalSize: int64);

Begin
  FProgressForm.Progress(FProgressSection, 1, ' Done!', '');
  OutputResultLn(Format('%1.0n files in %1.0n bytes.',
      [Int(iFileCount), Int(iTotalSize)]));
End;

(**

  This is an on search event handler for the sync module.

  @precon  None.
  @postcon Updates the progress dialogue with the current number of files found and the current folder 
           and filename being searched.

  @param   strFolder   as a String as a constant
  @param   strFileName as a String as a constant
  @param   iCount      as an Integer as a constant
  @param   Update      as a TUpdateType as a constant

**)
Procedure TfrmMainForm.SearchProc(Const strFolder, strFileName: String; Const iCount: Integer;
  Const Update : TUpdateType);

Begin
  If strFolder = FLastFolder Then
    Begin
      If (iCount Mod 10 = 0) Or (Update = utImmediate) Then
        FProgressForm.Progress(FProgressSection, 0, Format('Searching... %1.0n',
            [Int(iCount)]), strFolder + strFileName);
    End Else
      FLastFolder := strFolder;
End;

(**

  This is an on search start event handler for the sync module.

  @precon  None.
  @postcon Increments the progress section and initialises the progress bar.

  @nohint  strFolder

  @param   strFolder as a String as a constant

**)
Procedure TfrmMainForm.SearchStartProc(Const strFolder: String);

Begin
  OutputResult('Searching...');
  Inc(FProgressSection);
  FProgressForm.InitialiseSection(FProgressSection, 0, 1);
End;

(**

  This method sets the file operations for the selected list view items.

  @precon  None.
  @postcon Sets the selected items in the list view to the File op state passed while also checking that
           the operation is valid for that item.

  @param   FileOp as a TFileOp as a constant

**)
Procedure TfrmMainForm.SetFileOperation(Const FileOp: TFileOp);

Var
  Node : PVirtualNode;
  NodeData : PFSFileListNode;

Begin
  Node := vstFileList.GetFirstSelected();
  While Assigned(Node) Do
    Begin
      NodeData := vstFileList.GetNodeData(Node);
      Case FileOp Of
        foLeftToRight:
          If NodeData.FLeftFilename = '' Then
            Continue;
        foRightToLeft:
          If NodeData.FRightFilename = '' Then
            Continue;
      End;
      NodeData.FState := FileOp;
      FSyncModule.Process[NodeData.FProcessIndex].FileOp := FileOp;
      vstFileList.InvalidateNode(Node);
      Node := vstFileList.GetNextSelected(Node);
    End;
  OutputStats;
End;

(**

  This is an on timer event handler for the start timer of the application.

  @precon  None.
  @postcon The start timer is stopped and the comparison of the folders is started.

  @param   Sender as a TObject

**)
Procedure TfrmMainForm.StartTimerEvent(Sender: TObject);

Begin
  FStartTimer.Enabled := False;
  actFileCompareExecute(Self);
End;

(**

  This is an on draw panel event handler for the status bar.

  @precon  None.
  @postcon Renders the Panels with bold text for the title and the rest as panel text.

  @param   Sender as a TStatusBar
  @param   Panel  as a TStatusPanel
  @param   Rect   as a TRect as a constant

**)
Procedure TfrmMainForm.stbrStatusBarDrawPanel(Sender: TStatusBar; Panel: TStatusPanel;
  Const Rect: TRect);

Var
  R : TRect;
  iPos:  Integer;
  strText : String;

Begin
  iPos := Pos(':', Panel.Text);
  strText := Copy(Panel.Text, 1, iPos);
  R := Rect;
  Sender.Canvas.Font.Assign(Sender.Font);
  Sender.Canvas.FillRect(R);
  Inc(R.Left, 4);
  Sender.Canvas.Font.Color := StyleServices.GetSystemColor(clWindowText);
  Sender.Canvas.Font.Style := [fsBold];
  Sender.Canvas.TextRect(R, strText, [tfLeft, tfBottom]);
  Inc(R.Left, Sender.Canvas.TextWidth(strText) + 2);
  Sender.Canvas.Font.Style := [];
  strText := Copy(Panel.Text, iPos + 1, Length(Panel.Text) - iPos);
  Sender.Canvas.TextRect(R, strText, [tfLeft, tfBottom]);
End;

(**

  This is an on update progress event handler for the application.

  @precon  None.
  @postcon Updates the applications task bar with the current progress position.

  @param   iPosition    as an Integer as a constant
  @param   iMaxPosition as an Integer as a constant

**)
Procedure TfrmMainForm.UpdateProgress(Const iPosition, iMaxPosition: Integer);

Begin
  UpdateTaskBar(ptNormal, iPosition, iMaxPosition);
End;

(**

  This method updates the windows taskbar progress in windows 7 and above. The progress of the bar is 
  only updated IF the progress type is normal.

  @precon  None.
  @postcon The progress bar on the applications taskbar is updated.

  @param   ProgressType as a TTaskbarProgressType as a constant
  @param   iPosition    as an Integer as a constant
  @param   iMaxPosition as an Integer as a constant

**)
Procedure TfrmMainForm.UpdateTaskBar(Const ProgressType: TTaskbarProgressType;
  Const iPosition: Integer = 0; Const iMaxPosition: Integer = 100);

Const
  ProgressTypeValues: Array [Low(TTaskbarProgressType) .. High(TTaskbarProgressType)
    ] Of Cardinal = (TBPF_NOPROGRESS, TBPF_NORMAL, TBPF_ERROR);

Begin
  If Assigned(FTaskbarList3) Then
    Begin
      FTaskbarList3.SetProgressState(Application.Handle,
        ProgressTypeValues[ProgressType]);
      If ProgressType In [ptNormal] Then
        FTaskbarList3.SetProgressValue(Application.Handle, iPosition, iMaxPosition);
    End;
End;

(**

  This method upgrades the old limited Folder options in the INI file to more flexible options.

  @precon  iniMemFile must be a valid instance.
  @postcon Upgrades the old limited Folder options in the INI file to more flexible options.

  @param   iniMemFile as a TMemIniFile as a constant

**)
Procedure TfrmMainForm.UpgradeINIFolderOptions(Const iniMemFile: TMemIniFile);

Var
  sl : TStringList;
  i : Integer;
  iSyncOptions : TSyncOptions;
  
Begin
  If Not iniMemFile.SectionExists('Folders') Then
    Exit;
  sl := TStringList.Create;
  Try
    iniMemFile.ReadSection('Folders', sl);
    For i := 0 To sl.Count - 1 Do
      Begin
        iniMemFile.WriteInteger('NewFolderStatus', Format('Folder%2.2d', [i]), 
          iniMemFile.ReadInteger('FolderStatus', sl[i], SmallInt(iSyncOptions)));
        iniMemFile.WriteString('NewFolderMaxFileSize', Format('Folder%2.2d', [i]), 
          iniMemFile.ReadString('FolderMaxFileSize', sl[i], '0,0,0,0'));
        iniMemFile.WriteString('NewFolders', Format('Folder%2.2d', [i]), 
          sl[i] + '|' + iniMemFile.ReadString('Folders', sl[i], ''));
      End;
  Finally
    sl.Free;
  End;
  iniMemFile.UpdateFile;
End;

(**

  This is an on before cell paint event handler for the virtual treeview.

  @precon  None.
  @postcon Paints the cell background according to the left or right file status.

  @param   Sender        as a TBaseVirtualTree
  @param   TargetCanvas  as a TCanvas
  @param   Node          as a PVirtualNode
  @param   Column        as a TColumnIndex
  @param   CellPaintMode as a TVTCellPaintMode
  @param   CellRect      as a TRect
  @param   ContentRect   as a TRect as a reference

**)
Procedure TfrmMainForm.vstFileListBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; Var ContentRect: TRect);

Var
  NodeData : PFSFileListNode;
  
Begin
  NodeData := Sender.GetNodeData(Node);
  If CellPaintMode = cpmPaint Then
    Case TFSVTVFields(Column) Of
      vfLeftFileName..vfLeftDate:
        Begin
          If Sender.Selected[Node] Then
            TargetCanvas.Brush.Color := StyleServices.GetSystemColor(clHighlight)
          Else If (NodeData.FLeftAttr And FILE_ATTRIBUTE_READONLY <> 0) Then
            TargetCanvas.Brush.Color :=
              StyleServices.GetSystemColor(FFileOpFonts[fofReadOnly].FBGFontColour)
          Else
            TargetCanvas.Brush.Color :=
              StyleServices.GetSystemColor(FFileOpFonts[TFileOpFont(NodeData.FState)].FBGFontColour);
          TargetCanvas.FillRect(CellRect);
        End;
      vfRightFilename..vfRightDate:
        Begin
          If Sender.Selected[Node] Then
            TargetCanvas.Brush.Color := StyleServices.GetSystemColor(clHighlight)
          Else If (NodeData.FRightAttr And FILE_ATTRIBUTE_READONLY <> 0) Then
            TargetCanvas.Brush.Color :=
              StyleServices.GetSystemColor(FFileOpFonts[fofReadOnly].FBGFontColour)
          Else
            TargetCanvas.Brush.Color :=
              StyleServices.GetSystemColor(FFileOpFonts[TFileOpFont(NodeData.FState)].FBGFontColour);
          TargetCanvas.FillRect(CellRect);
        End;
    End;
End;

(**

  This is an on free event handler for the virtual treeview.

  @precon  None.
  @postcon Ensures that managed types are freed from the node data.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode

**)
Procedure TfrmMainForm.vstFileListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

Var
  NodeData : PFSFileListNode;
  
Begin
  NodeData := vstFileList.GetNodeData(Node);
  Finalize(NodeData^);
End;

(**

  This is an on get image index event handler for the tree view.

  @precon  None.
  @postcon Returns the image index for the appropriate column.

  @param   Sender     as a TBaseVirtualTree
  @param   Node       as a PVirtualNode
  @param   Kind       as a TVTImageKind
  @param   Column     as a TColumnIndex
  @param   Ghosted    as a Boolean as a reference
  @param   ImageIndex as a TImageIndex as a reference
  @param   ImageList  as a TCustomImageList as a reference

**)
Procedure TfrmMainForm.vstFileListGetImageIndexEx(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; Var Ghosted:
  Boolean; Var ImageIndex: TImageIndex; Var ImageList: TCustomImageList);

Var
  NodeData : PFSFileListNode;
  
Begin
  NodeData := Sender.GetNodeData(Node);
  If Kind In [ikNormal, ikSelected] Then
    Case TFSVTVFields(Column) Of
      vfState:
        Begin
          ImageIndex := Integer(NodeData.FState);
          ImageList := ilActionImages;
        End;
      vfLeftFileName:
        Begin
          If (NodeData.FLeftFileIndex = -1) And (Length(NodeData.FLeftFilename) > 0) Then
            NodeData.FLeftFileIndex := GetImageIndex(NodeData.FLeftFilename);
          ImageIndex := NodeData.FLeftFileIndex;
          ImageList := ilFileTypeIcons;
        End;
      vfRightFilename:
        Begin
          If (NodeData.FRightFileIndex = -1) And (Length(NodeData.FRightFilename) > 0) Then
            NodeData.FRightFileIndex := GetImageIndex(NodeData.FRightFilename);
          ImageIndex := NodeData.FRightFileIndex;
          ImageList := ilFileTypeIcons;
        End;
    End;
End;

(**

  This is an on get text event handler for the vstTreeView.

  @precon  None.
  @postcon Returns the apporpriate text for the specified node and column.

  @param   Sender   as a TBaseVirtualTree
  @param   Node     as a PVirtualNode
  @param   Column   as a TColumnIndex
  @param   TextType as a TVSTTextType
  @param   CellText as a String as a reference

**)
Procedure TfrmMainForm.vstFileListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);

Const
  strDateFmt = 'dd/mmm/yyyy hh:nn';

Var
  NodeData : PFSFileListNode;
  boolHasLeftFile: Boolean;
  boolHasRightFile: Boolean;
  
Begin
  If TextType = ttNormal Then
    Begin
      NodeData := Sender.GetNodeData(Node);
      boolHasLeftFile := Length(NodeData.FLeftFilename) > 0;
      boolHasRightFile := Length(NodeData.FRightFilename) > 0;
      Case TFSVTVFields(Column) Of
        vfState:         CellText := '';
        vfLeftFileName:  CellText := NodeData.FLeftFilename;
        vfLeftAttr:      CellText := IfThen(boolHasLeftFile, GetAttributeString(NodeData.FLeftAttr), '');
        vfLeftSize:      CellText := IfThen(boolHasLeftFile, Format('%1.0n', [Int(NodeData.FLeftSize)]), '');
        vfLeftDate:      CellText := IfThen(boolHasLeftFile, FormatDateTime(strDateFmt, NodeData.FLeftDate), '');
        vfRightFileName: CellText := NodeData.FRightFilename;
        vfRightAttr:     CellText := IfThen(boolHasRightFile, GetAttributeString(NodeData.FRightAttr), '');
        vfRightSize:     CellText := IfThen(boolHasRightFile, Format('%1.0n', [Int(NodeData.FRightSize)]), '');
        vfRightDate:     CellText := IfThen(boolHasRightFile, FormatDateTime(strDateFmt, NodeData.FRightDate), '');
      End;
    End;
End;

(**

  This is n on paint text event handler for the virtual treeview.

  @precon  None.
  @postcon Sets the font colour and style the specific state of the left of right files.

  @param   Sender       as a TBaseVirtualTree
  @param   TargetCanvas as a TCanvas as a constant
  @param   Node         as a PVirtualNode
  @param   Column       as a TColumnIndex
  @param   TextType     as a TVSTTextType

**)
Procedure TfrmMainForm.vstFileListPaintText(Sender: TBaseVirtualTree; Const
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType:
  TVSTTextType);

Var
  NodeData : PFSFileListNode;
  
Begin
  NodeData := Sender.GetNodeData(Node);
  Case TFSVTVFields(Column) Of
    vfLeftFileName..vfLeftDate:
      Begin
        If Sender.Selected[Node] Then
          TargetCanvas.Font.Color := StyleServices.GetSystemColor(clHighlightText)
        Else If (NodeData.FLeftAttr And FILE_ATTRIBUTE_READONLY <> 0) Then
          TargetCanvas.Font.Color :=
            StyleServices.GetSystemColor(FFileOpFonts[fofReadOnly].FFontColour)
        Else     
          TargetCanvas.Font.Color :=
            StyleServices.GetSystemColor(FFileOpFonts[TFileOpFont(NodeData.FState)].FFontColour);
        TargetCanvas.Font.Style := FFileOpFonts[TFileOpFont(NodeData.FState)].FFontStyle;
      End;
    vfRightFilename..vfRightDate:
      Begin
        If Sender.Selected[Node] Then
          TargetCanvas.Font.Color := StyleServices.GetSystemColor(clHighlightText)
        Else If (NodeData.FRightAttr And FILE_ATTRIBUTE_READONLY <> 0) Then
          TargetCanvas.Font.Color :=
            StyleServices.GetSystemColor(FFileOpFonts[fofReadOnly].FFontColour)
        Else     
          TargetCanvas.Font.Color :=
            StyleServices.GetSystemColor(FFileOpFonts[TFileOpFont(NodeData.FState)].FFontColour);
        TargetCanvas.Font.Style := FFileOpFonts[TFileOpFont(NodeData.FState)].FFontStyle;
      End;
  End;
End;

End.
